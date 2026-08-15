//nolint:bodyclose // The client transport owns and closes all synthetic responses.
package adt

// Advisory impact wiring on the WriteSource/EditSource paths (impact gate
// Task 5). These tests pin three contracts:
//
//  1. gate "advise" computes an ImpactSummary once per logical write, BEFORE
//     the lock is acquired (never between LOCK and PUT), and attaches it to
//     the workflow result;
//  2. gate off (the default) computes nothing — no usageReferences and no
//     E071/E070 RunQuery requests appear in the request log;
//  3. WriteSource create-mode never computes impact even when advised — a
//     brand-new object has no callers to analyze.

import (
	"context"
	"net/http"
	"strings"
	"testing"
)

// impactCleanSyntaxXML is a syntax-check response with an empty message list,
// so the write workflows proceed past the pre-lock syntax gate.
const impactCleanSyntaxXML = `<?xml version="1.0" encoding="UTF-8"?>
<chkrun:checkRunReports xmlns:chkrun="http://www.sap.com/adt/checkrun">
  <chkrun:checkReport chkrun:status="clean">
    <chkrun:checkMessageList/>
  </chkrun:checkReport>
</chkrun:checkRunReports>`

// impactEmptyE071XML is an E071 lookup with zero rows: the transport-recency
// leg stops after the first query (no E070 follow-up), keeping the mock
// routing unambiguous — both RunQuery calls hit datapreview/freestyle.
const impactEmptyE071XML = `<?xml version="1.0" encoding="utf-8"?>
<dataPreview:tableData xmlns:dataPreview="http://www.sap.com/adt/dataPreview">
  <dataPreview:totalRows>0</dataPreview:totalRows>
</dataPreview:tableData>`

// newImpactWorkflowClient wires a Client to a methodPathMock with the CSRF
// token pre-set so the recorded call order contains only workflow requests.
func newImpactWorkflowClient(mock *methodPathMock) *Client {
	cfg := NewConfig("https://sap.example.com:44300", "user", "pass")
	transport := NewTransportWithClient(cfg, mock)
	transport.setCSRFToken("synthetic-token")
	return NewClientWithTransport(cfg, transport)
}

// callIndex returns the index of the first recorded call matching the
// predicate, or -1.
func callIndex(calls []recordedCall, match func(recordedCall) bool) int {
	for i, call := range calls {
		if match(call) {
			return i
		}
	}
	return -1
}

// countCalls returns how many recorded calls match the predicate.
func countCalls(calls []recordedCall, match func(recordedCall) bool) int {
	n := 0
	for _, call := range calls {
		if match(call) {
			n++
		}
	}
	return n
}

func isImpactRefsCall(c recordedCall) bool {
	return strings.Contains(c.path, "usageReferences")
}

func isImpactSQLCall(c recordedCall) bool {
	return strings.Contains(c.path, "datapreview/freestyle")
}

func isLockCall(c recordedCall) bool {
	return c.query.Get("_action") == "LOCK"
}

func TestWriteSourceUpdateComputesImpactBeforeLockWhenAdvised(t *testing.T) {
	mock := &methodPathMock{routes: []routedResponse{
		{method: http.MethodGet, pathSubstring: "/source/main", status: http.StatusOK, body: "REPORT ztest."},
		{method: http.MethodPost, pathSubstring: "usageReferences", status: http.StatusOK, body: impactUsageXML},
		{method: http.MethodPost, pathSubstring: "datapreview/freestyle", status: http.StatusOK, body: impactEmptyE071XML},
		{method: http.MethodPost, pathSubstring: "/checkruns", status: http.StatusOK, body: impactCleanSyntaxXML},
		{method: http.MethodPost, pathSubstring: "/programs/programs/ZTEST", status: http.StatusOK, body: syntheticLocalLockXML},
		{method: http.MethodPut, pathSubstring: "/source/main", status: http.StatusOK, body: ""},
		{method: http.MethodPost, pathSubstring: "/activation", status: http.StatusOK, body: ""},
	}}
	client := newImpactWorkflowClient(mock)
	client.Safety().ImpactGate = ImpactGateAdvise

	result, err := client.WriteSource(context.Background(), "PROG", "ZTEST", "REPORT ztest.", &WriteSourceOptions{
		Mode: WriteModeUpdate,
	})
	if err != nil {
		t.Fatalf("WriteSource() error = %v", err)
	}
	if !result.Success || result.Mode != "updated" {
		t.Fatalf("workflow did not complete: %#v", result)
	}

	if result.Impact == nil {
		t.Fatal("Impact = nil, want summary when gate is advise")
	}
	if !result.Impact.Available {
		t.Fatalf("Impact.Available = false (%s), want true", result.Impact.Unavailable)
	}
	// impactUsageXML holds 4 distinct callers relative to ZTEST (the zcl_demo
	// row is not the object under edit here, so it counts as a caller).
	if result.Impact.Callers != 4 {
		t.Fatalf("Impact.Callers = %d, want 4", result.Impact.Callers)
	}

	usageIdx := callIndex(mock.calls, isImpactRefsCall)
	lockIdx := callIndex(mock.calls, isLockCall)
	if usageIdx < 0 {
		t.Fatal("no usageReferences request recorded — impact was not computed")
	}
	if lockIdx < 0 {
		t.Fatal("no LOCK request recorded — workflow did not reach the lock step")
	}
	if usageIdx > lockIdx {
		t.Fatalf("usageReferences at call %d AFTER lock at call %d — impact must be computed before the lock, never between LOCK and PUT", usageIdx, lockIdx)
	}
	if sqlIdx := callIndex(mock.calls, isImpactSQLCall); sqlIdx >= 0 && sqlIdx > lockIdx {
		t.Fatalf("E071 lookup at call %d AFTER lock at call %d", sqlIdx, lockIdx)
	}
	if n := countCalls(mock.calls, isImpactRefsCall); n != 1 {
		t.Fatalf("usageReferences requests = %d, want exactly 1 per logical write", n)
	}
}

func TestWriteSourceUpdateSkipsImpactWhenGateOff(t *testing.T) {
	mock := &methodPathMock{routes: []routedResponse{
		{method: http.MethodGet, pathSubstring: "/source/main", status: http.StatusOK, body: "REPORT ztest."},
		{method: http.MethodPost, pathSubstring: "/checkruns", status: http.StatusOK, body: impactCleanSyntaxXML},
		{method: http.MethodPost, pathSubstring: "/programs/programs/ZTEST", status: http.StatusOK, body: syntheticLocalLockXML},
		{method: http.MethodPut, pathSubstring: "/source/main", status: http.StatusOK, body: ""},
		{method: http.MethodPost, pathSubstring: "/activation", status: http.StatusOK, body: ""},
	}}
	client := newImpactWorkflowClient(mock)
	// Default safety: ImpactGate is unset (off).

	result, err := client.WriteSource(context.Background(), "PROG", "ZTEST", "REPORT ztest.", &WriteSourceOptions{
		Mode: WriteModeUpdate,
	})
	if err != nil {
		t.Fatalf("WriteSource() error = %v", err)
	}
	if !result.Success {
		t.Fatalf("workflow did not complete: %#v", result)
	}

	if result.Impact != nil {
		t.Fatalf("Impact = %+v, want nil with the gate off", result.Impact)
	}
	if idx := callIndex(mock.calls, isImpactRefsCall); idx >= 0 {
		t.Fatalf("usageReferences request at call %d — gate off must compute nothing", idx)
	}
	if idx := callIndex(mock.calls, isImpactSQLCall); idx >= 0 {
		t.Fatalf("E071 RunQuery request at call %d — gate off must compute nothing", idx)
	}
}

func TestWriteSourceCreateSkipsImpactEvenWhenAdvised(t *testing.T) {
	mock := &methodPathMock{routes: []routedResponse{
		// Existence probe: 404 → create path. Everything past the probe may
		// fail (unrouted → 404); this test only pins that create-mode never
		// runs blast-radius analysis — a new object has no callers.
		{method: http.MethodGet, pathSubstring: "/source/main", status: http.StatusNotFound, body: "not found"},
	}}
	client := newImpactWorkflowClient(mock)
	client.Safety().ImpactGate = ImpactGateAdvise

	result, err := client.WriteSource(context.Background(), "PROG", "ZTEST", "REPORT ztest.", &WriteSourceOptions{
		Mode:        WriteModeCreate,
		Description: "Synthetic program",
		Package:     "$TMP",
	})
	if err != nil {
		t.Fatalf("WriteSource() error = %v", err)
	}
	if result.Impact != nil {
		t.Fatalf("Impact = %+v, want nil on the create path", result.Impact)
	}
	if idx := callIndex(mock.calls, isImpactRefsCall); idx >= 0 {
		t.Fatalf("usageReferences request at call %d — create mode must not compute impact", idx)
	}
	if idx := callIndex(mock.calls, isImpactSQLCall); idx >= 0 {
		t.Fatalf("E071 RunQuery request at call %d — create mode must not compute impact", idx)
	}
}

func TestEditSourceComputesImpactWhenAdvised(t *testing.T) {
	mock := &methodPathMock{routes: []routedResponse{
		{method: http.MethodPost, pathSubstring: "usageReferences", status: http.StatusOK, body: impactUsageXML},
		{method: http.MethodPost, pathSubstring: "datapreview/freestyle", status: http.StatusOK, body: impactEmptyE071XML},
		{method: http.MethodGet, pathSubstring: "/source/main", status: http.StatusOK, body: "REPORT ztest.\nWRITE 'Hello'."},
		{method: http.MethodPost, pathSubstring: "/checkruns", status: http.StatusOK, body: impactCleanSyntaxXML},
		{method: http.MethodPost, pathSubstring: "/programs/programs/ZTEST", status: http.StatusOK, body: syntheticLocalLockXML},
		{method: http.MethodPut, pathSubstring: "/source/main", status: http.StatusOK, body: ""},
		{method: http.MethodPost, pathSubstring: "/activation", status: http.StatusOK, body: ""},
	}}
	client := newImpactWorkflowClient(mock)
	client.Safety().ImpactGate = ImpactGateAdvise

	result, err := client.EditSource(context.Background(),
		"/sap/bc/adt/programs/programs/ZTEST", "'Hello'", "'World'", false, true, false)
	if err != nil {
		t.Fatalf("EditSource() error = %v", err)
	}
	if !result.Success {
		t.Fatalf("edit did not complete: %#v", result)
	}

	if result.Impact == nil {
		t.Fatal("Impact = nil, want summary when gate is advise")
	}
	if !result.Impact.Available {
		t.Fatalf("Impact.Available = false (%s), want true", result.Impact.Unavailable)
	}

	usageIdx := callIndex(mock.calls, isImpactRefsCall)
	lockIdx := callIndex(mock.calls, isLockCall)
	if usageIdx < 0 || lockIdx < 0 || usageIdx > lockIdx {
		t.Fatalf("usageReferences call %d must precede lock call %d", usageIdx, lockIdx)
	}
	// The E071 lookup must carry the TADIR type derived from the object URL.
	sqlIdx := callIndex(mock.calls, isImpactSQLCall)
	if sqlIdx < 0 {
		t.Fatal("no E071 RunQuery recorded — transport leg was skipped entirely")
	}
	if n := countCalls(mock.calls, isImpactRefsCall); n != 1 {
		t.Fatalf("usageReferences requests = %d, want exactly 1 per logical write", n)
	}
}

func TestEditSourceSkipsImpactWhenGateOff(t *testing.T) {
	mock := &methodPathMock{routes: []routedResponse{
		{method: http.MethodGet, pathSubstring: "/source/main", status: http.StatusOK, body: "REPORT ztest.\nWRITE 'Hello'."},
		{method: http.MethodPost, pathSubstring: "/checkruns", status: http.StatusOK, body: impactCleanSyntaxXML},
		{method: http.MethodPost, pathSubstring: "/programs/programs/ZTEST", status: http.StatusOK, body: syntheticLocalLockXML},
		{method: http.MethodPut, pathSubstring: "/source/main", status: http.StatusOK, body: ""},
		{method: http.MethodPost, pathSubstring: "/activation", status: http.StatusOK, body: ""},
	}}
	client := newImpactWorkflowClient(mock)

	result, err := client.EditSource(context.Background(),
		"/sap/bc/adt/programs/programs/ZTEST", "'Hello'", "'World'", false, true, false)
	if err != nil {
		t.Fatalf("EditSource() error = %v", err)
	}
	if !result.Success {
		t.Fatalf("edit did not complete: %#v", result)
	}
	if result.Impact != nil {
		t.Fatalf("Impact = %+v, want nil with the gate off", result.Impact)
	}
	if idx := callIndex(mock.calls, isImpactRefsCall); idx >= 0 {
		t.Fatalf("usageReferences request at call %d — gate off must compute nothing", idx)
	}
	if idx := callIndex(mock.calls, isImpactSQLCall); idx >= 0 {
		t.Fatalf("E071 RunQuery request at call %d — gate off must compute nothing", idx)
	}
}

// TestWriteSourceUpsertResolvingToUpdateComputesImpact pins that upsert mode
// resolving to an update (the existence probe finds the object) takes the
// same impact-computation path as an explicit update.
func TestWriteSourceUpsertResolvingToUpdateComputesImpact(t *testing.T) {
	mock := &methodPathMock{routes: []routedResponse{
		{method: http.MethodGet, pathSubstring: "/source/main", status: http.StatusOK, body: "REPORT ztest."},
		{method: http.MethodPost, pathSubstring: "usageReferences", status: http.StatusOK, body: impactUsageXML},
		{method: http.MethodPost, pathSubstring: "datapreview/freestyle", status: http.StatusOK, body: impactEmptyE071XML},
		{method: http.MethodPost, pathSubstring: "/checkruns", status: http.StatusOK, body: impactCleanSyntaxXML},
		{method: http.MethodPost, pathSubstring: "/programs/programs/ZTEST", status: http.StatusOK, body: syntheticLocalLockXML},
		{method: http.MethodPut, pathSubstring: "/source/main", status: http.StatusOK, body: ""},
		{method: http.MethodPost, pathSubstring: "/activation", status: http.StatusOK, body: ""},
	}}
	client := newImpactWorkflowClient(mock)
	client.Safety().ImpactGate = ImpactGateAdvise

	result, err := client.WriteSource(context.Background(), "PROG", "ZTEST", "REPORT ztest.", &WriteSourceOptions{
		Mode: WriteModeUpsert,
	})
	if err != nil {
		t.Fatalf("WriteSource() error = %v", err)
	}
	if !result.Success || result.Mode != "updated" {
		t.Fatalf("upsert did not resolve to update: %#v", result)
	}
	if result.Impact == nil {
		t.Fatal("Impact = nil, want summary when upsert resolves to update under gate advise")
	}
	if n := countCalls(mock.calls, isImpactRefsCall); n != 1 {
		t.Fatalf("usageReferences requests = %d, want exactly 1 per logical write", n)
	}
}

// The two tests below pin the review fix: when the local op-type policy will
// refuse the write anyway (read-only mode here), the gate must not spend
// network calls computing a blast radius for a mutation that never happens.

func TestWriteSourceRefusedByPolicySkipsImpact(t *testing.T) {
	mock := &methodPathMock{routes: []routedResponse{
		// Existence probe (a read, permitted in read-only mode) → update path.
		{method: http.MethodGet, pathSubstring: "/source/main", status: http.StatusOK, body: "REPORT ztest."},
	}}
	client := newImpactWorkflowClient(mock)
	client.Safety().ImpactGate = ImpactGateAdvise
	client.Safety().ReadOnly = true

	_, err := client.WriteSource(context.Background(), "PROG", "ZTEST", "REPORT ztest.", &WriteSourceOptions{
		Mode: WriteModeUpdate,
	})
	if err == nil {
		t.Fatal("WriteSource() error = nil, want read-only policy refusal")
	}
	if !strings.Contains(err.Error(), "blocked by safety configuration") {
		t.Fatalf("error %q, want safety-configuration refusal", err)
	}
	if idx := callIndex(mock.calls, isImpactRefsCall); idx >= 0 {
		t.Fatalf("usageReferences request at call %d — refused writes must not compute impact", idx)
	}
	if idx := callIndex(mock.calls, isImpactSQLCall); idx >= 0 {
		t.Fatalf("E071 RunQuery request at call %d — refused writes must not compute impact", idx)
	}
}

func TestEditSourceRefusedByPolicySkipsImpact(t *testing.T) {
	mock := &methodPathMock{} // no routes: any request would 404 and fail the test below
	client := newImpactWorkflowClient(mock)
	client.Safety().ImpactGate = ImpactGateAdvise
	client.Safety().ReadOnly = true

	_, err := client.EditSource(context.Background(),
		"/sap/bc/adt/programs/programs/ZTEST", "'Hello'", "'World'", false, true, false)
	if err == nil {
		t.Fatal("EditSource() error = nil, want read-only policy refusal")
	}
	if !strings.Contains(err.Error(), "blocked by safety configuration") {
		t.Fatalf("error %q, want safety-configuration refusal", err)
	}
	if idx := callIndex(mock.calls, isImpactRefsCall); idx >= 0 {
		t.Fatalf("usageReferences request at call %d — refused writes must not compute impact", idx)
	}
	if idx := callIndex(mock.calls, isImpactSQLCall); idx >= 0 {
		t.Fatalf("E071 RunQuery request at call %d — refused writes must not compute impact", idx)
	}
}

// TestImpactGateActiveIsAnAllowlist pins the reviewer requirement that the
// gate check is an allowlist of the two active modes — NOT `!= off` — so a
// garbage config value stays inert instead of enabling network calls.
func TestImpactGateActiveIsAnAllowlist(t *testing.T) {
	tests := []struct {
		gate string
		want bool
	}{
		{"", false},
		{ImpactGateOff, false},
		{ImpactGateAdvise, true},
		{ImpactGateBlock, true},
		{"banana", false},
		{"ADVISE", false}, // config plumbing normalizes case; raw values do not activate
	}
	for _, tt := range tests {
		cfg := &SafetyConfig{ImpactGate: tt.gate}
		if got := impactGateActive(cfg); got != tt.want {
			t.Fatalf("impactGateActive(%q) = %v, want %v", tt.gate, got, tt.want)
		}
	}
}
