//go:build integration

package adt

// Impact-gated writes — live integration coverage (impact gate Task 11,
// docs/plans/2026-08-15-impact-gated-writes.md).
//
// What IS covered here against a real system:
//   - the advisory attach on the WriteSource update path (gate "advise");
//   - the create path staying impact-free;
//   - the live usageReferences round-trip that feeds ComputeWriteImpact,
//     including a probe of the isResult assumption its counting loop
//     rests on (impact.go: rows with isResult="false" are structural
//     grouping nodes and are skipped);
//   - block mode NOT false-positiving on a low-risk (fresh, caller-less)
//     object at the strict "medium" threshold — plus, when the live
//     where-used lookup degrades (risk "unknown", which "medium" gates
//     fail-closed), the real block → token → confirmed-retry round trip;
//   - the confirmation-token store round trip on the live client instance
//     (issue, wrong-token reject, canonicalized-URL keying, single-use) —
//     in-process, no SAP calls.
//
// What is deliberately NOT covered: a guaranteed block on a genuinely
// high-risk object. Forcing one needs an object with >= 25 callers or a
// recent transport touch — no such object can be assumed to exist on an
// arbitrary test system, and writing to a real high-caller object to
// manufacture one is exactly what the gate exists to prevent. True
// block-path verification needs a seeded fixture package and is deferred
// to the sandbox canary run (plan Task 11 as-built note).

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"testing"
	"time"
)

// tallyImpactUsageRows logs up to limit usageReferences rows and tallies
// them by their isResult flag.
func tallyImpactUsageRows(t *testing.T, label string, refs []UsageReference, limit int) (resultRows, groupingRows int) {
	t.Helper()
	for i, r := range refs {
		if r.IsResult {
			resultRows++
		} else {
			groupingRows++
		}
		if i < limit {
			t.Logf("  [%s] row %d: isResult=%v type=%s name=%s pkg=%s uri=%s",
				label, i, r.IsResult, r.Type, r.Name, r.PackageName, r.URI)
		}
	}
	if len(refs) > limit {
		t.Logf("  [%s] ... and %d more row(s)", label, len(refs)-limit)
	}
	return resultRows, groupingRows
}

// TestIntegration_ImpactAdvisorySummary exercises gate "advise" end-to-end
// on a live system: create a $TMP program (no impact block expected), update
// it (impact block expected — available, or with a stated degradation
// reason), then verify the underlying usageReferences round-trip and probe
// the isResult assumption ComputeWriteImpact's caller counting relies on.
func TestIntegration_ImpactAdvisorySummary(t *testing.T) {
	client := getIntegrationClient(t)
	client.Safety().ImpactGate = ImpactGateAdvise
	ctx := context.Background()

	timestamp := time.Now().Unix() % 100000
	programName := fmt.Sprintf("ZMCPI_%05d", timestamp)
	objectURL := fmt.Sprintf("/sap/bc/adt/programs/programs/%s", programName)
	t.Logf("Test program name: %s", programName)

	createSource := fmt.Sprintf(`REPORT %s.

* Impact advisory integration fixture (v1)
WRITE: / 'impact-advisory-v1'.`, strings.ToLower(programName))

	created, err := client.WriteSource(ctx, "PROG", programName, createSource, &WriteSourceOptions{
		Mode:        WriteModeCreate,
		Description: "Impact gate advisory integration test",
		Package:     "$TMP",
	})
	if err != nil {
		t.Fatalf("WriteSource(create) failed: %v", err)
	}

	// Cleanup at end
	defer func() {
		lock, _ := client.LockObject(ctx, objectURL, "MODIFY")
		if lock != nil {
			client.DeleteObject(ctx, objectURL, lock.LockHandle, "")
		}
	}()

	if !created.Success {
		t.Fatalf("WriteSource(create) did not succeed: %s", created.Message)
	}
	// The create path must NOT carry an impact block: a brand-new object has
	// no callers to analyze (design §Exemptions).
	if created.Impact != nil {
		t.Errorf("create result unexpectedly carries an impact summary: %+v", created.Impact)
	}

	updateSource := fmt.Sprintf(`REPORT %s.

* Impact advisory integration fixture (v2 — updated)
WRITE: / 'impact-advisory-v2'.`, strings.ToLower(programName))

	updated, err := client.WriteSource(ctx, "PROG", programName, updateSource, &WriteSourceOptions{
		Mode: WriteModeUpdate,
	})
	if err != nil {
		t.Fatalf("WriteSource(update) failed: %v", err)
	}
	if !updated.Success {
		t.Fatalf("WriteSource(update) did not succeed: %s", updated.Message)
	}

	imp := updated.Impact
	if imp == nil {
		t.Fatal("gate=advise update result carries no impact summary")
	}
	if imp.Available {
		if imp.Unavailable != "" {
			t.Errorf("available summary carries an unavailable_reason: %q", imp.Unavailable)
		}
		if imp.Risk != riskLow && imp.Risk != riskMedium && imp.Risk != riskHigh {
			t.Errorf("available summary has unexpected risk tier %q", imp.Risk)
		}
		if imp.Callers != 0 {
			t.Logf("NOTE: fresh $TMP program reports %d caller(s) — unexpected but not impossible (name reuse, index artifacts)", imp.Callers)
		}
	} else {
		if imp.Unavailable == "" {
			t.Error("unavailable summary states no reason")
		}
		if imp.Risk != riskUnknown {
			t.Errorf("unavailable summary has risk %q, want %q", imp.Risk, riskUnknown)
		}
		t.Logf("impact analysis degraded on this system: %s", imp.Unavailable)
	}
	t.Logf("advisory summary: available=%v risk=%s callers=%d packages=%v crossPackage=%v transports=%d",
		imp.Available, imp.Risk, imp.Callers, imp.Packages, imp.CrossPackage, len(imp.RecentTransports))
	if imp.Advice != "" {
		t.Logf("advice: %s", imp.Advice)
	}

	// --- isResult assumption probe (plan Task 11 verification note) ---
	// ComputeWriteImpact counts only rows with isResult="true", assuming
	// object-level usageReferences RESULT rows carry isResult=true while
	// structural grouping rows (package nodes) carry isResult=false. Probe
	// that live. This is an assumption probe, not a contract test: a failed
	// assumption logs loudly instead of failing the run.
	refs, err := client.FindReferences(ctx, objectURL, 0, 0)
	if err != nil {
		if imp.Available {
			t.Errorf("impact summary was available but direct FindReferences on %s failed: %v", objectURL, err)
		} else {
			t.Logf("isResult probe unavailable: FindReferences failed (consistent with degraded summary): %v", err)
		}
		return
	}
	t.Logf("usageReferences round-trip OK: %d row(s) for %s", len(refs), objectURL)
	resultRows, groupingRows := tallyImpactUsageRows(t, programName, refs, 10)
	switch {
	case len(refs) == 0:
		t.Logf("isResult probe INCONCLUSIVE on %s: zero rows for a fresh caller-less object", programName)
	case resultRows == 0:
		t.Logf("!!! ASSUMPTION PROBE FAILED on this system: %d usageReferences row(s), NONE with isResult=true — "+
			"ComputeWriteImpact's counting loop would report 0 callers for every object here; "+
			"re-check the isResult semantics of this system's where-used service", len(refs))
	default:
		t.Logf("isResult assumption holds on %s: %d result row(s) (isResult=true), %d grouping row(s)",
			programName, resultRows, groupingRows)
	}

	if len(refs) == 0 {
		// Fallback probe against a standard class with guaranteed callers, so
		// an inconclusive fresh-object probe still checks the assumption.
		// Same non-fatal timeout tolerance as TestIntegration_FindReferences:
		// whole-object queries can be slow on heavily-used standard objects.
		probeCtx, cancel := context.WithTimeout(ctx, 90*time.Second)
		defer cancel()
		stdRefs, err := client.FindReferences(probeCtx, "/sap/bc/adt/oo/classes/CL_ABAP_STRUCTDESCR", 0, 0)
		if err != nil {
			t.Logf("fallback isResult probe skipped (FindReferences on CL_ABAP_STRUCTDESCR failed): %v", err)
			return
		}
		resultRows, groupingRows = tallyImpactUsageRows(t, "CL_ABAP_STRUCTDESCR", stdRefs, 5)
		if len(stdRefs) > 0 && resultRows == 0 {
			t.Logf("!!! ASSUMPTION PROBE FAILED (fallback object): %d row(s), none with isResult=true", len(stdRefs))
		} else {
			t.Logf("fallback isResult probe: %d result row(s), %d grouping row(s)", resultRows, groupingRows)
		}
	}
}

// TestIntegration_ImpactBlockConfirmRoundTrip exercises gate "block" at
// threshold "medium" (the strictest setting) against a live system.
//
// A fresh $TMP program has 0 callers and no transport history, so a healthy
// where-used lookup classifies it risk "low" — which even threshold "medium"
// does not gate. The update must therefore PROCEED: that is the
// no-false-positive half of block-mode coverage. If the live where-used
// lookup degrades instead, risk is "unknown", which "medium" deliberately
// gates fail-closed — not a false positive but the documented degraded-mode
// behavior, and it hands us a real live block → token → retry round trip,
// which the test then exercises.
//
// The guaranteed-block path (a genuinely high-risk object refused, then
// confirmed) cannot be forced here — see the file comment; it is deferred to
// the sandbox canary with a seeded fixture package. The confirmation-token
// mechanics are still verified on the live client instance, in-process.
func TestIntegration_ImpactBlockConfirmRoundTrip(t *testing.T) {
	client := getIntegrationClient(t)
	client.Safety().ImpactGate = ImpactGateBlock
	client.Safety().ImpactThreshold = ImpactThresholdMedium
	ctx := context.Background()

	timestamp := time.Now().Unix() % 100000
	programName := fmt.Sprintf("ZMCPB_%05d", timestamp)
	objectURL := fmt.Sprintf("/sap/bc/adt/programs/programs/%s", programName)
	t.Logf("Test program name: %s", programName)

	// Create under gate=block: the create path and its fill PUT are exempt
	// (withImpactCreateFill). If that exemption regressed, this create would
	// block whenever the where-used service degrades (risk "unknown" gated
	// by threshold "medium"), so its success is itself part of the coverage.
	createSource := fmt.Sprintf(`REPORT %s.

* Impact block-mode integration fixture (v1)
WRITE: / 'impact-block-v1'.`, strings.ToLower(programName))

	created, err := client.WriteSource(ctx, "PROG", programName, createSource, &WriteSourceOptions{
		Mode:        WriteModeCreate,
		Description: "Impact gate block-mode integration test",
		Package:     "$TMP",
	})
	if err != nil {
		var blocked *ImpactBlockedError
		if errors.As(err, &blocked) {
			t.Fatalf("create path was impact-blocked — create/create-fill exemption regressed: %v", err)
		}
		t.Fatalf("WriteSource(create) failed: %v", err)
	}

	// Cleanup at end (raw DeleteObject is deliberately ungated at the
	// primitive, so cleanup cannot be impact-blocked).
	defer func() {
		lock, _ := client.LockObject(ctx, objectURL, "MODIFY")
		if lock != nil {
			client.DeleteObject(ctx, objectURL, lock.LockHandle, "")
		}
	}()

	if !created.Success {
		t.Fatalf("WriteSource(create) did not succeed: %s", created.Message)
	}

	updateSource := fmt.Sprintf(`REPORT %s.

* Impact block-mode integration fixture (v2 — updated)
WRITE: / 'impact-block-v2'.`, strings.ToLower(programName))

	updated, err := client.WriteSource(ctx, "PROG", programName, updateSource, &WriteSourceOptions{
		Mode: WriteModeUpdate,
	})
	if err != nil {
		var blocked *ImpactBlockedError
		if !errors.As(err, &blocked) {
			t.Fatalf("WriteSource(update) failed: %v", err)
		}
		risk := riskUnknown
		if blocked.Summary != nil {
			risk = blocked.Summary.Risk
		}
		if risk != riskUnknown {
			// A fresh caller-less $TMP program must classify "low"; "medium"
			// or "high" here would mean the counting loop miscounts (e.g.
			// grouping rows tallied as callers) or the tiering regressed.
			t.Fatalf("BLOCK-MODE FALSE POSITIVE: fresh caller-less $TMP program blocked with risk %q — summary %+v; error:\n%v",
				risk, blocked.Summary, err)
		}
		// Degraded-mode block: where-used failed live, threshold "medium"
		// gated risk "unknown" fail-closed as designed. Exercise the real
		// block → token → confirmed-retry round trip.
		t.Logf("where-used degraded live (risk unknown) — threshold medium gated it as designed; exercising token retry:\n%v", err)
		if !strings.HasPrefix(blocked.Token, "impact-confirm-") {
			t.Fatalf("blocked error carries malformed token %q", blocked.Token)
		}
		retryCtx := WithImpactConfirm(ctx, blocked.Token)
		retried, err := client.WriteSource(retryCtx, "PROG", programName, updateSource, &WriteSourceOptions{
			Mode: WriteModeUpdate,
		})
		if err != nil {
			t.Fatalf("confirmed retry still failed: %v", err)
		}
		if !retried.Success {
			t.Fatalf("confirmed retry did not succeed: %s", retried.Message)
		}
		t.Log("live block -> token -> confirmed retry round trip PASSED (degraded-mode path)")
	} else {
		if !updated.Success {
			t.Fatalf("WriteSource(update) did not succeed: %s", updated.Message)
		}
		imp := updated.Impact
		if imp == nil {
			t.Error("block mode is gate-active; the update result should still carry the advisory impact summary")
		} else {
			t.Logf("update proceeded at threshold=medium: available=%v risk=%s callers=%d packages=%v",
				imp.Available, imp.Risk, imp.Callers, imp.Packages)
			// Proceeding at threshold "medium" implies the risk was "low":
			// "medium"/"high" are gated, and so is "unknown" at this
			// threshold. Anything else means tiering and gating disagree.
			if imp.Risk != riskLow {
				t.Errorf("write proceeded at threshold=medium but summary says risk %q (want %q) — tiering vs gating inconsistency",
					imp.Risk, riskLow)
			}
		}
		t.Log("no-false-positive path PASSED: low-risk write proceeded under gate=block threshold=medium")
	}

	// --- Confirmation-token store round trip (in-process; no SAP calls) ---
	// The live update above only exercises token redemption when the system
	// happens to degrade; the token mechanics themselves are verified here
	// on the same live client instance, deterministically.
	op := impactTokenOp(OpUpdate)
	token := client.IssueImpactToken(objectURL, op)
	if !strings.HasPrefix(token, "impact-confirm-") || len(token) != len("impact-confirm-")+32 {
		t.Fatalf("issued token has unexpected shape: %q", token)
	}
	if client.consumeImpactToken(objectURL, op, "impact-confirm-ffffffffffffffffffffffffffffffff") {
		t.Error("wrong token value was consumed successfully")
	}
	// The store keys on the canonicalized object URL: an equivalent
	// /source/main form must address the same token.
	if !client.consumeImpactToken(objectURL+"/source/main", op, token) {
		t.Error("valid token failed to consume via a canonically equivalent URL")
	}
	if client.consumeImpactToken(objectURL, op, token) {
		t.Error("token consumed twice — must be single-use")
	}
	t.Log("token store round trip PASSED: issue, wrong-token reject, canonicalized consume, single-use")
}
