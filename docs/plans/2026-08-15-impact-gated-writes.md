# Impact-Gated Writes Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Every update/edit/delete/rename returns an agent-directed blast-radius summary (`impact` block), with an opt-in blocking gate above a risk threshold.

**Architecture:** Compute once per logical write in the `pkg/adt` workflow layer (the same choke point as the package pre-check, before LOCK), attach to existing result structs, gate in `checkMutation` step 4. MCP, CLI, and DSL inherit it because they all call these workflows.

**Tech Stack:** Go; existing `FindReferences` (ADT usageReferences), `RunQuery` (E071/E070), mock-transport test pattern from `pkg/adt/devtools_activation_test.go` / `http_test.go`.

**Design:** [2026-08-15-impact-gated-writes-design.md](2026-08-15-impact-gated-writes-design.md)

---

### Task 1: ImpactSummary type + risk tiering (pure logic)

**Files:**
- Create: `pkg/adt/impact.go`
- Test: `pkg/adt/impact_test.go`

**Step 1: Write the failing tests**

```go
package adt

import "testing"

func TestClassifyImpactRisk(t *testing.T) {
	tests := []struct {
		name string
		s    ImpactSummary
		want string
	}{
		{"many callers", ImpactSummary{Available: true, Callers: 25}, "high"},
		{"cross-package with recent transport", ImpactSummary{Available: true, Callers: 6, CrossPackage: true,
			RecentTransports: []TransportTouch{{Transport: "TR-EXAMPLE"}}}, "high"},
		{"few callers", ImpactSummary{Available: true, Callers: 5}, "medium"},
		{"transport touch only", ImpactSummary{Available: true, Callers: 0,
			RecentTransports: []TransportTouch{{Transport: "TR-EXAMPLE"}}}, "medium"},
		{"quiet object", ImpactSummary{Available: true, Callers: 4}, "low"},
		{"unavailable", ImpactSummary{Available: false}, "unknown"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := classifyImpactRisk(&tt.s); got != tt.want {
				t.Fatalf("classifyImpactRisk() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestImpactAdviceMentionsPackages(t *testing.T) {
	s := &ImpactSummary{Available: true, Callers: 30, CrossPackage: true,
		Packages: []string{"Z_BILLING", "Z_ORDERS"}, Risk: "high"}
	advice := impactAdvice(s)
	if advice == "" || !containsAll(advice, "30", "Z_BILLING") {
		t.Fatalf("advice %q must cite caller count and a package", advice)
	}
}
```

(`containsAll` = tiny test helper using `strings.Contains`.)

**Step 2: Run to verify failure** — `go test ./pkg/adt/ -run 'TestClassifyImpactRisk|TestImpactAdvice' -v` → FAIL: undefined types.

**Step 3: Minimal implementation** in `pkg/adt/impact.go`:

```go
package adt

// TransportTouch is one recent transport that carried the object.
type TransportTouch struct {
	Transport string `json:"transport"`
	Type      string `json:"type"`
	Status    string `json:"status"`
	Owner     string `json:"owner"`
	Date      string `json:"date"`
}

// ImpactSummary is the blast-radius block attached to write results.
type ImpactSummary struct {
	Object           string           `json:"object"`
	Callers          int              `json:"callers"`
	Packages         []string         `json:"packages,omitempty"`
	CrossPackage     bool             `json:"cross_package"`
	RecentTransports []TransportTouch `json:"recent_transports,omitempty"`
	Risk             string           `json:"risk"`
	Advice           string           `json:"advice,omitempty"`
	Available        bool             `json:"available"`
	Unavailable      string           `json:"unavailable_reason,omitempty"`
	TransitiveDepth  int              `json:"transitive_depth,omitempty"` // reserved for Phase 2
}

const (
	impactHighCallers   = 25
	impactMediumCallers = 5
)

func classifyImpactRisk(s *ImpactSummary) string {
	if !s.Available {
		return "unknown"
	}
	touched := len(s.RecentTransports) > 0
	switch {
	case s.Callers >= impactHighCallers || (s.CrossPackage && touched):
		return "high"
	case s.Callers >= impactMediumCallers || touched:
		return "medium"
	default:
		return "low"
	}
}

func impactAdvice(s *ImpactSummary) string { /* per-tier fmt.Sprintf naming callers, packages, transport recency, and next actions (read callers, run tests on affected packages) */ }
```

**Step 4: Run to verify pass** — same command → PASS.
**Step 5: Commit** — `git add pkg/adt/impact.go pkg/adt/impact_test.go && git commit -m "feat(adt): impact summary type and risk tiering"`

---

### Task 2: ComputeWriteImpact — where-used leg (mocked HTTP)

**Files:**
- Modify: `pkg/adt/impact.go`
- Test: `pkg/adt/impact_test.go`

**Step 1: Failing test** using the existing `mockHTTPClient`/`NewTransportWithClient` pattern (see `pkg/adt/devtools_activation_test.go:63-75` for the boilerplate): mock one usageReferences response with 3 references across 2 packages; call `client.ComputeWriteImpact(ctx, "/sap/bc/adt/oo/classes/zcl_demo", "ZCL_DEMO", "CLAS", "Z_PKG_A")`; assert `Callers==3`, `CrossPackage==true`, `Available==true`, risk computed, and — crucial — a second test where the mock returns HTTP 500 and the function returns `Available:false, Risk:"unknown"` with **no error**.

**Step 2:** Run → FAIL (undefined `ComputeWriteImpact`).

**Step 3: Implementation:**

```go
// ComputeWriteImpact builds the blast-radius summary for objectURL. It never
// returns an error: failures degrade to Available=false (see design §Degradation).
// ownPackage is the object's package from the mutation context ("" if unknown).
func (c *Client) ComputeWriteImpact(ctx context.Context, objectURL, objectName, tadirType, ownPackage string) *ImpactSummary {
	s := &ImpactSummary{Object: objectName}
	refs, err := c.FindReferences(ctx, objectURL, 0, 0) // object-level usage; position ignored
	if err != nil {
		s.Unavailable = fmt.Sprintf("where-used lookup failed: %v", err)
		s.Risk = classifyImpactRisk(s)
		return s
	}
	s.Available = true
	pkgs := map[string]bool{}
	for _, r := range refs {
		if r.Name == objectName { continue } // self-reference
		s.Callers++
		if r.PackageName != "" {
			pkgs[r.PackageName] = true
			if ownPackage != "" && r.PackageName != ownPackage { s.CrossPackage = true }
		}
	}
	for p := range pkgs { s.Packages = append(s.Packages, p) }
	sort.Strings(s.Packages)
	s.RecentTransports = c.recentTransportTouches(ctx, tadirType, objectName) // Task 3; returns nil for now
	s.Risk = classifyImpactRisk(s)
	s.Advice = impactAdvice(s)
	return s
}
```

Stub `recentTransportTouches` returning nil so this task compiles.

**Step 4:** Run → PASS. **Step 5: Commit** — `feat(adt): ComputeWriteImpact where-used leg`.

---

### Task 3: Transport-recency leg (E071→E070 via RunQuery)

**Files:**
- Modify: `pkg/adt/impact.go`
- Test: `pkg/adt/impact_test.go`

**Step 1:** Read `cmd/vsp/cli_extra.go:897-960` (the `vsp graph co-change` command) — it is the canonical 2-query pattern: `SELECT TRKORR,PGMID,OBJECT,OBJ_NAME FROM E071 WHERE PGMID = 'R3TR' AND OBJECT = '<type>' AND OBJ_NAME = '<name>'`, then `SELECT TRKORR,STRKORR,TRFUNCTION,TRSTATUS,AS4USER,AS4DATE FROM E070 WHERE TRKORR IN (...)`. Mirror its `RunQuery` result parsing exactly (do not invent field access; copy the row-iteration idiom).

**Step 2: Failing test:** mock transport returning the two query responses; assert one `TransportTouch` with the E070 fields, filtered to `AS4DATE` within 90 days (inject `now` via an unexported package var `impactNow = time.Now` so the test can pin it). Second case: `RunQuery` errors → method returns nil, summary stays refs-only (`Available` still true).

**Step 3: Implement** `recentTransportTouches`: skip immediately (return nil) when `c.safety != nil && c.safety.BlockFreeSQL`; collapse tasks to parent request via `STRKORR` (same as `aggregateChangelogEntries` in `cmd/vsp/changelog.go:261` — object level here, so just dedupe on parent). Never cache (E07x rule, `cmd/vsp/audit_cache.go:19-21`).

**Step 4:** `go test ./pkg/adt/ -run TestRecentTransportTouches -v` → PASS. **Step 5: Commit** — `feat(adt): transport-recency leg of impact summary`.

---

### Task 4: Gate config plumbing

**Files:**
- Modify: `pkg/adt/safety.go` (struct + validation), `pkg/config/systems.go` (fields `impact_gate`, `impact_threshold` — follow the safety-fields block added by PR #156 at `SystemConfig`), `cmd/vsp/main.go` (persistent flags `--impact-gate`, `--impact-threshold`, env `SAP_IMPACT_GATE`, `SAP_IMPACT_THRESHOLD` — register exactly where the PR-#156 safety flags are declared so propagation-to-subcommands is inherited)
- Test: extend `cmd/vsp/cli_safety_test.go` (inheritance/precedence tests exist there — add the two new fields to its table) and `pkg/adt/safety_test.go` (invalid value rejected)

**Steps:** failing test (`ImpactGate: "banana"` → error from a new `SafetyConfig.Validate()` call or normalize-to-off with warning — match however existing invalid enum values are handled in `main.go`; if none, reject at flag parse) → implement → `go test ./cmd/vsp/ ./pkg/adt/ -run 'Safety|Impact' -v` → commit `feat(safety): impact gate configuration`.

---

### Task 5: Advisory wiring — WriteSource/EditSource paths

**Files:**
- Modify: `pkg/adt/mutation_gate.go` (ctx marker), `pkg/adt/workflows_source.go` (compute at :261 area — the outer site that already calls `withMutationPackageChecked`; add `Impact *ImpactSummary \`json:"impact,omitempty"\`` to `WriteSourceResult` at :159), `pkg/adt/workflows_edit.go` (same at :163 / `EditSourceResult` :12)
- Test: `pkg/adt/workflows_test.go` additions

**Step 1: ctx marker** (mirror `withMutationPackageChecked`, `mutation_gate.go:15`):

```go
type impactComputedKey struct{}
func withImpactComputed(ctx context.Context, s *ImpactSummary) context.Context {
	return context.WithValue(ctx, impactComputedKey{}, s)
}
func impactFromContext(ctx context.Context) *ImpactSummary {
	s, _ := ctx.Value(impactComputedKey{}).(*ImpactSummary)
	return s
}
```

**Step 2: Failing test:** mock full WriteSource flow (existing mock sequences in `pkg/scripting/write_source_test.go` and `pkg/adt/workflows_test.go` show the response order) with `SafetyConfig{ImpactGate: "advise"}` → assert `result.Impact != nil`; with gate `off` → assert `result.Impact == nil` **and** the mock saw no usageReferences request (count requests).

**Step 3: Implement:** at the outer site, immediately before the existing package pre-check: `if gate != off { s := c.ComputeWriteImpact(...); ctx = withImpactComputed(ctx, s); result.Impact = s }`. Compute BEFORE lock; never between LOCK and PUT.

**Steps 4–5:** run, commit `feat(adt): advisory impact on source writes`.

---

### Task 6: Advisory wiring — delete and rename paths

**Files:**
- Modify: `pkg/adt/crud.go` (outer sites :473 and :1207), `internal/mcp/handlers_crud.go:530` (replace the bare `"Object deleted successfully"` string with a small `DeleteResult{Success, Object, Impact}` marshaled like other handlers)
- Test: `pkg/adt/crud_reconcile_test.go`-style mock test for delete; rename covered by existing `TestRenameStopsBeforeDeleting...` extended with an impact assertion

Same pattern as Task 5. Commit `feat(adt): advisory impact on delete and rename`.

---

### Task 7: Confirm-token store

**Files:**
- Create: `pkg/adt/impact_confirm.go`
- Test: `pkg/adt/impact_confirm_test.go`

**Step 1: Failing tests:** issue → validate consumes (second use fails); expiry (inject clock var) → invalid; token bound to objectURL+op (token for update of A rejected for delete of A).

**Step 3: Implementation:** `impactTokenStore` struct (map + `sync.Mutex`) on `Client`; `IssueImpactToken(objectURL, op string) string` (`"impact-confirm-" + 8 hex from crypto/rand`), `consumeImpactToken(objectURL, op, token string) bool`; 10-minute TTL; opportunistic sweep on issue.

**Commit:** `feat(adt): impact confirmation tokens`.

---

### Task 8: Block mode — checkMutation step 4

**Files:**
- Modify: `pkg/adt/mutation_gate.go` (`checkMutation` :89 — after transportable-edit check), `pkg/adt/impact.go` (render block error text per design §Confirmation flow)
- Test: `pkg/adt/impact_gate_test.go`

**Step 1: Failing tests:** gate `block`, threshold `high`, mocked high-impact object → write fails, error contains `impact-confirm-` token and caller count; retry with `ctx = WithImpactConfirm(ctx, token)` → succeeds; threshold `high` + `unknown` risk → passes; threshold `medium` + `unknown` → blocks.

**Step 3: Implement:** step 4 reads `impactFromContext(ctx)`; exported `WithImpactConfirm(ctx, token)` ctx setter; blocked error type `*ImpactBlockedError` with `Summary` and `Token` fields, `Error()` renders the design's text.

**Commit:** `feat(adt): impact gate block mode`.

---

### Task 9: Surface the confirm parameter (MCP + CLI)

**Files:**
- Modify: `internal/mcp/tools_register.go` (optional `confirm` string on WriteSource, EditSource, DeleteObject, Rename tools), `internal/mcp/handlers_source.go` / `handlers_crud.go` (read `confirm`, apply `adt.WithImpactConfirm`), `internal/mcp/handlers_universal.go` (pass `params.confirm` through — params already flow to handlers, verify only), `cmd/vsp/cli.go` (`--confirm-impact` on the source write / delete / rename commands; print one-line risk summary on gated ops when gate ≠ off)
- Test: `internal/mcp/handlers_source_test.go` (added by PR #156 — extend: blocked write returns MCP tool error containing the token; retry with `confirm` arg succeeds against mock)

**Commit:** `feat(mcp,cli): impact confirm parameter`.

---

### Task 10: Docs, skills, counts

**Files:**
- Modify: `README_TOOLS.md` (confirm param note), `CLAUDE.md` (flags table: two new rows; re-derive counts per "Reconciling counts"), `plugin/skills/deploy/SKILL.md` + `plugin/skills/abap-developer/SKILL.md` (one line each: on `impact.risk: high`, read 2–3 key callers and run tests on affected packages before proceeding), design pointer report `reports/2026-08-15-004-impact-gated-writes-design.md` (metadata header + link + 5-line summary)
- **Sanitize scan** staged diff per CLAUDE.md before commit.

**Commit:** `docs: impact-gated writes documentation`.

---

### Task 11: Integration test + full verification

**Files:**
- Modify: `pkg/adt/integration_test.go` (tag `integration`): with gate `advise`, edit a `$TMP` object, assert the impact block exists and `available` is true or reason states why.

**Verification:**
- `go build ./... && go vet ./... && go test ./...` → all green
- Manual (sandbox): `--impact-gate advise` → edit object with known callers → `impact` block in response; `--impact-gate block --impact-threshold medium` → block → token retry succeeds
- `grep -c 'shouldRegister("' internal/mcp/tools_register.go` unchanged (no new tools, only params)

**Commit:** `test(adt): impact gate integration coverage`.
