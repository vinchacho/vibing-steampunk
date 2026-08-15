//nolint:bodyclose // The client transport owns and closes all synthetic responses.
package adt

// Block-mode enforcement in checkMutation step 4 (impact gate Task 8).
// These tests pin the confirmation contract from the design's
// §Confirmation flow:
//
//   - gate "block" at/above the threshold refuses the write BEFORE any
//     lock or PUT, with an *ImpactBlockedError carrying a fresh
//     single-use token;
//   - retrying the same call with WithImpactConfirm(ctx, token)
//     succeeds end-to-end — including multi-step workflows (rename)
//     whose sub-steps re-enter checkMutation with the same origin
//     (op, objectURL);
//   - tokens are single-use and expire after 10 minutes;
//   - threshold "high" never gates risk "unknown" (a broken where-used
//     lookup cannot brick writes); threshold "medium" does;
//   - gate "advise" never blocks, regardless of risk.

import (
	"context"
	"errors"
	"fmt"
	"net/http"
	"strings"
	"testing"
	"time"
)

// highImpactUsageXML builds a usageReferences response with n distinct
// callers spread over two packages — n >= 25 classifies as risk "high".
func highImpactUsageXML(n int) string {
	var b strings.Builder
	b.WriteString(`<?xml version="1.0" encoding="UTF-8"?>
<usageReferences:usageReferenceResult xmlns:usageReferences="http://www.sap.com/adt/ris/usageReferences" xmlns:adtcore="http://www.sap.com/adt/core">
  <usageReferences:referencedObjects>`)
	for i := 0; i < n; i++ {
		pkg := "Z_PKG_A"
		if i%2 == 0 {
			pkg = "Z_PKG_B"
		}
		fmt.Fprintf(&b, `
    <usageReferences:referencedObject usageReferences:uri="/sap/bc/adt/oo/classes/zcl_demo_caller%03d" usageReferences:isResult="true">
      <adtcore:adtObject adtcore:uri="/sap/bc/adt/oo/classes/zcl_demo_caller%03d" adtcore:type="CLAS/OC" adtcore:name="ZCL_DEMO_CALLER%03d">
        <adtcore:packageRef adtcore:name="%s"/>
      </adtcore:adtObject>
    </usageReferences:referencedObject>`, i, i, i, pkg)
	}
	b.WriteString(`
  </usageReferences:referencedObjects>
</usageReferences:usageReferenceResult>`)
	return b.String()
}

func isPutCall(c recordedCall) bool {
	return c.method == http.MethodPut
}

// writeSourceBlockMockRoutes stages the complete WriteSource update flow
// (probe, impact legs, syntax check, lock, PUT, activation). Staging the
// full sequence even for blocked calls is deliberate: an absent LOCK/PUT
// in the call log then proves the gate refused the write, not that the
// mock had no route.
func writeSourceBlockMockRoutes(usageStatus int, usageBody string) []routedResponse {
	return []routedResponse{
		{method: http.MethodGet, pathSubstring: "/source/main", status: http.StatusOK, body: "REPORT ztest."},
		{method: http.MethodPost, pathSubstring: "usageReferences", status: usageStatus, body: usageBody},
		{method: http.MethodPost, pathSubstring: "datapreview/freestyle", status: http.StatusOK, body: impactEmptyE071XML},
		{method: http.MethodPost, pathSubstring: "/checkruns", status: http.StatusOK, body: impactCleanSyntaxXML},
		{method: http.MethodPost, pathSubstring: "/programs/programs/ZTEST", status: http.StatusOK, body: syntheticLocalLockXML},
		{method: http.MethodPut, pathSubstring: "/source/main", status: http.StatusOK, body: ""},
		{method: http.MethodPost, pathSubstring: "/activation", status: http.StatusOK, body: ""},
	}
}

func blockedWriteSource(ctx context.Context, t *testing.T, client *Client) *ImpactBlockedError {
	t.Helper()
	result, err := client.WriteSource(ctx, "PROG", "ZTEST", "REPORT ztest.", &WriteSourceOptions{
		Mode: WriteModeUpdate,
	})
	if err == nil {
		t.Fatalf("WriteSource() error = nil, want impact block (result: %#v)", result)
	}
	var blocked *ImpactBlockedError
	if !errors.As(err, &blocked) {
		t.Fatalf("errors.As(*ImpactBlockedError) = false for %T: %v", err, err)
	}
	return blocked
}

// (a) Gate "block", threshold "high", high-impact object: the write is
// refused with an *ImpactBlockedError whose text carries a well-formed
// token and the caller count, and no lock/PUT was issued.
func TestWriteSourceBlockedAtHighRisk(t *testing.T) {
	mock := &methodPathMock{routes: writeSourceBlockMockRoutes(http.StatusOK, highImpactUsageXML(30))}
	client := newImpactWorkflowClient(mock)
	client.Safety().ImpactGate = ImpactGateBlock
	client.Safety().ImpactThreshold = ImpactThresholdHigh

	blocked := blockedWriteSource(context.Background(), t, client)

	if !impactTokenPattern.MatchString(blocked.Token) {
		t.Fatalf("Token %q does not match %s", blocked.Token, impactTokenPattern)
	}
	if blocked.Summary == nil || blocked.Summary.Risk != riskHigh {
		t.Fatalf("Summary = %+v, want risk %q", blocked.Summary, riskHigh)
	}
	msg := blocked.Error()
	if !strings.Contains(msg, blocked.Token) {
		t.Fatalf("Error() %q must contain the confirmation token", msg)
	}
	if !strings.Contains(msg, "30 callers") {
		t.Fatalf("Error() %q must cite the caller count", msg)
	}
	if !strings.Contains(msg, "IMPACT GATE") {
		t.Fatalf("Error() %q must render the IMPACT GATE refusal", msg)
	}

	if idx := callIndex(mock.calls, isImpactRefsCall); idx < 0 {
		t.Fatal("no usageReferences request recorded — impact was never computed")
	}
	if idx := callIndex(mock.calls, isLockCall); idx >= 0 {
		t.Fatalf("LOCK request at call %d — blocked writes must stop before the lock", idx)
	}
	if idx := callIndex(mock.calls, isPutCall); idx >= 0 {
		t.Fatalf("PUT request at call %d — blocked writes must never reach the PUT", idx)
	}
}

// (b) + (c) Retrying with the issued token succeeds; the token is
// single-use — a third call reusing it re-blocks with a fresh token.
func TestWriteSourceConfirmRetryAndSingleUse(t *testing.T) {
	mock := &methodPathMock{routes: writeSourceBlockMockRoutes(http.StatusOK, highImpactUsageXML(30))}
	client := newImpactWorkflowClient(mock)
	client.Safety().ImpactGate = ImpactGateBlock
	client.Safety().ImpactThreshold = ImpactThresholdHigh

	// First call: blocked, token issued.
	blocked := blockedWriteSource(context.Background(), t, client)

	// Second call: same call, confirm token in ctx → full write succeeds.
	result, err := client.WriteSource(WithImpactConfirm(context.Background(), blocked.Token),
		"PROG", "ZTEST", "REPORT ztest.", &WriteSourceOptions{Mode: WriteModeUpdate})
	if err != nil {
		t.Fatalf("confirmed WriteSource() error = %v, want success", err)
	}
	if !result.Success || result.Mode != "updated" {
		t.Fatalf("confirmed write did not complete: %#v", result)
	}
	if result.Impact == nil || result.Impact.Risk != riskHigh {
		t.Fatalf("Impact = %+v, want attached summary with risk %q", result.Impact, riskHigh)
	}
	if idx := callIndex(mock.calls, isPutCall); idx < 0 {
		t.Fatal("no PUT recorded — confirmed write never reached SAP")
	}

	// Third call reusing the consumed token: blocked again, NEW token.
	reblocked := blockedWriteSource(WithImpactConfirm(context.Background(), blocked.Token), t, client)
	if !impactTokenPattern.MatchString(reblocked.Token) {
		t.Fatalf("reissued token %q does not match %s", reblocked.Token, impactTokenPattern)
	}
	if reblocked.Token == blocked.Token {
		t.Fatal("reissued token equals the consumed one — tokens must be single-use and fresh per block")
	}
}

// (d) Threshold "high" does not gate risk "unknown": a broken where-used
// lookup must not brick writes under the default threshold.
func TestWriteSourceUnknownRiskPassesAtThresholdHigh(t *testing.T) {
	mock := &methodPathMock{routes: writeSourceBlockMockRoutes(http.StatusInternalServerError, "boom")}
	client := newImpactWorkflowClient(mock)
	client.Safety().ImpactGate = ImpactGateBlock
	client.Safety().ImpactThreshold = ImpactThresholdHigh

	result, err := client.WriteSource(context.Background(), "PROG", "ZTEST", "REPORT ztest.", &WriteSourceOptions{
		Mode: WriteModeUpdate,
	})
	if err != nil {
		t.Fatalf("WriteSource() error = %v, want unknown risk to pass at threshold high", err)
	}
	if !result.Success {
		t.Fatalf("write did not complete: %#v", result)
	}
	if result.Impact == nil || result.Impact.Risk != riskUnknown || result.Impact.Available {
		t.Fatalf("Impact = %+v, want attached degraded summary with risk %q", result.Impact, riskUnknown)
	}
	if idx := callIndex(mock.calls, isPutCall); idx < 0 {
		t.Fatal("no PUT recorded — write never reached SAP")
	}
}

// (e) Threshold "medium" does gate risk "unknown".
func TestWriteSourceUnknownRiskBlockedAtThresholdMedium(t *testing.T) {
	mock := &methodPathMock{routes: writeSourceBlockMockRoutes(http.StatusInternalServerError, "boom")}
	client := newImpactWorkflowClient(mock)
	client.Safety().ImpactGate = ImpactGateBlock
	client.Safety().ImpactThreshold = ImpactThresholdMedium

	blocked := blockedWriteSource(context.Background(), t, client)
	if blocked.Summary == nil || blocked.Summary.Risk != riskUnknown {
		t.Fatalf("Summary = %+v, want risk %q", blocked.Summary, riskUnknown)
	}
	if !impactTokenPattern.MatchString(blocked.Token) {
		t.Fatalf("Token %q does not match %s", blocked.Token, impactTokenPattern)
	}
	if idx := callIndex(mock.calls, isLockCall); idx >= 0 {
		t.Fatalf("LOCK request at call %d — blocked writes must stop before the lock", idx)
	}
	if idx := callIndex(mock.calls, isPutCall); idx >= 0 {
		t.Fatalf("PUT request at call %d — blocked writes must never reach the PUT", idx)
	}
}

// (f) Gate "advise" never blocks, whatever the risk.
func TestAdviseModeNeverBlocksHighRisk(t *testing.T) {
	mock := &methodPathMock{routes: writeSourceBlockMockRoutes(http.StatusOK, highImpactUsageXML(30))}
	client := newImpactWorkflowClient(mock)
	client.Safety().ImpactGate = ImpactGateAdvise
	client.Safety().ImpactThreshold = ImpactThresholdMedium // even the strict threshold must not block in advise mode

	result, err := client.WriteSource(context.Background(), "PROG", "ZTEST", "REPORT ztest.", &WriteSourceOptions{
		Mode: WriteModeUpdate,
	})
	if err != nil {
		t.Fatalf("WriteSource() error = %v, want advise mode to never block", err)
	}
	if !result.Success {
		t.Fatalf("write did not complete: %#v", result)
	}
	if result.Impact == nil || result.Impact.Risk != riskHigh {
		t.Fatalf("Impact = %+v, want attached summary with risk %q", result.Impact, riskHigh)
	}
}

// (g) An expired token re-blocks: the TTL is enforced at consume time.
func TestExpiredConfirmTokenReblocks(t *testing.T) {
	now := time.Date(2026, 8, 15, 12, 0, 0, 0, time.UTC)
	prev := impactNow
	impactNow = func() time.Time { return now }
	t.Cleanup(func() { impactNow = prev })

	mock := &methodPathMock{routes: writeSourceBlockMockRoutes(http.StatusOK, highImpactUsageXML(30))}
	client := newImpactWorkflowClient(mock)
	client.Safety().ImpactGate = ImpactGateBlock
	client.Safety().ImpactThreshold = ImpactThresholdHigh

	blocked := blockedWriteSource(context.Background(), t, client)

	now = now.Add(impactTokenTTL + time.Second)

	reblocked := blockedWriteSource(WithImpactConfirm(context.Background(), blocked.Token), t, client)
	if reblocked.Token == blocked.Token {
		t.Fatal("re-block after expiry must issue a fresh token")
	}
	if idx := callIndex(mock.calls, isPutCall); idx >= 0 {
		t.Fatalf("PUT request at call %d — expired token must not authorize the write", idx)
	}
}

// (h) THE RENAME TEST (plan Task 8, rename/multi-step constraint): a
// high-risk rename under gate "block" is refused once with a token, and a
// single confirmed retry completes the ENTIRE multi-step workflow — the
// OpCreate/UpdateSource sub-steps have a different origin and skip the
// gate, and the step-6 DeleteObject (same (OpDelete, oldURL) origin as
// the first gate) honors the already-consumed confirmation instead of
// re-blocking un-confirmably.
func TestRenameUnderBlockSingleConfirmSuffices(t *testing.T) {
	mock := &methodPathMock{routes: []routedResponse{
		{method: http.MethodPost, pathSubstring: "usageReferences", status: http.StatusOK, body: highImpactUsageXML(30)},
		{method: http.MethodPost, pathSubstring: "datapreview/freestyle", status: http.StatusOK, body: impactEmptyE071XML},
		{method: http.MethodGet, pathSubstring: "/programs/programs/zold/source/main", status: http.StatusOK, body: "REPORT zold."},
		{method: http.MethodPost, pathSubstring: "nodestructure", status: http.StatusOK, body: packageNodeStructureXML},
		{method: http.MethodPost, pathSubstring: "/programs/programs/zold", status: http.StatusOK, body: syntheticLocalLockXML},
		{method: http.MethodDelete, pathSubstring: "/programs/programs/zold", status: http.StatusOK, body: ""},
		{method: http.MethodPut, pathSubstring: "/programs/programs/znew/source/main", status: http.StatusOK, body: ""},
		{method: http.MethodPost, pathSubstring: "/programs/programs/znew", status: http.StatusOK, body: syntheticLocalLockXML},
		{method: http.MethodPost, pathSubstring: "/activation", status: http.StatusOK, body: ""},
		// Create posts to the collection URL — must stay below the more
		// specific /zold and /znew routes (first match wins).
		{method: http.MethodPost, pathSubstring: "/programs/programs", status: http.StatusCreated, body: ""},
	}}
	client := newImpactWorkflowClient(mock)
	client.Safety().ImpactGate = ImpactGateBlock
	client.Safety().ImpactThreshold = ImpactThresholdHigh

	// First call: blocked before any mutation-side request.
	result, err := client.RenameObject(context.Background(), ObjectTypeProgram, "ZOLD", "ZNEW", "$TMP", "")
	if err == nil {
		t.Fatalf("RenameObject() error = nil, want impact block (result: %#v)", result)
	}
	var blocked *ImpactBlockedError
	if !errors.As(err, &blocked) {
		t.Fatalf("errors.As(*ImpactBlockedError) = false for %T: %v", err, err)
	}
	if !impactTokenPattern.MatchString(blocked.Token) {
		t.Fatalf("Token %q does not match %s", blocked.Token, impactTokenPattern)
	}
	if idx := callIndex(mock.calls, isLockCall); idx >= 0 {
		t.Fatalf("LOCK request at call %d — blocked rename must stop before any lock", idx)
	}
	if idx := callIndex(mock.calls, isDeleteCall); idx >= 0 {
		t.Fatalf("DELETE request at call %d — blocked rename must not delete the old object", idx)
	}
	if idx := callIndex(mock.calls, isPutCall); idx >= 0 {
		t.Fatalf("PUT request at call %d — blocked rename must not write the new object", idx)
	}

	// Retry with the token: the whole rename must complete end-to-end —
	// create, source write, activate, AND the final delete of the old
	// object — on this single confirmation.
	result, err = client.RenameObject(WithImpactConfirm(context.Background(), blocked.Token),
		ObjectTypeProgram, "ZOLD", "ZNEW", "$TMP", "")
	if err != nil {
		t.Fatalf("confirmed RenameObject() error = %v, want success", err)
	}
	if !result.Success {
		t.Fatalf("confirmed rename did not complete: %#v (errors: %v)", result, result.Errors)
	}
	if result.Impact == nil || result.Impact.Risk != riskHigh {
		t.Fatalf("Impact = %+v, want attached summary with risk %q", result.Impact, riskHigh)
	}
	if n := countCalls(mock.calls, isDeleteCall); n != 1 {
		t.Fatalf("DELETE requests = %d, want exactly 1 (old object deleted once, on the confirmed retry)", n)
	}
	if idx := callIndex(mock.calls, isPutCall); idx < 0 {
		t.Fatal("no PUT recorded — new object source was never written")
	}
}
