# Impact-Gated Writes — Design (as built)

**Date:** 2026-08-15
**Status:** Shipped (feature/impact-gated-writes, 2026-08-15)
**Companion:** [2026-08-15-impact-gated-writes.md](2026-08-15-impact-gated-writes.md) (implementation plan) · pointer report [reports/2026-08-15-004-impact-gated-writes-design.md](../../reports/2026-08-15-004-impact-gated-writes-design.md)

## Problem

vsp's dependency analysis — where-used, package boundaries, transport history — is its clearest differentiator, but it was invisible at the moment it matters most: when an agent writes to the system. An agent editing `ZCL_PAYMENT_HANDLER` learned nothing about its 47 callers until something broke. Competing servers gate writes on *permission* (read-only flags, package allowlists). Nobody gates on *impact*, because nobody else has the analysis.

## What shipped

Every risky write returns a blast-radius summary (`impact` block) the **agent** acts on mid-task: read key callers before editing, run tests on affected packages after, or stop and ask. Operators escalate the same signal into an enforcement gate (`--impact-gate block`) that refuses high-impact writes until confirmed with a single-use token.

## Architecture: two-tier enforcement

The original "one choke point" framing did not survive implementation review — raw primitives are reachable without passing any workflow. What shipped is two tiers:

**Advisory tier — four workflow sites.** `WriteSource` (update path), `EditSource`, `RenameObject`, and `DeleteObjectWithResult` compute the summary once per logical write — before any lock is acquired, at the same point as the package pre-check, so no stateless call lands between LOCK and PUT — stash it in ctx (`withImpactComputed`, carrying the origin `(op, objectURL)`), and attach it to their result structs (`WriteSourceResult`, `EditSourceResult`, `RenameObjectResult`, `DeleteResult`). A `checkSafety` pre-guard skips computation when the local op-type policy would refuse the write anyway — no impact traffic on policy-refused writes. `DeleteObjectWithResult` is the MCP-facing delete wrapper (the `DeleteObject` tool calls it); the raw `Client.DeleteObject` keeps its plain-error signature for internal cleanup paths.

**Enforcement tier — checkMutation step 4 plus two primitive guards.** `checkImpactGate` (step 4 of the unified `checkMutation` policy gate) enforces in block mode, but only at the gate whose `(Op, ObjectURL)` matches the stashed marker's origin (URLs compared canonicalized) — sub-step gates of a multi-step workflow (rename's create/write/activate legs) inherit the marker but skip enforcement, so one confirmation covers the whole logical write. Because the marker-only design left verified bypasses (expert `UpdateSource` tool, hyperfocused `UPDATE_SOURCE` route, DeployZip phase 2, `Create`/`Update`/`DeployFromFile`, `dsl.Import`, `WriteProgram`/`WriteClass`, the install tools), block mode additionally enforces at the two write primitives themselves: `Client.UpdateSource` and `Client.UpdateClassInclude` each compute the blast radius when reached markerless (and not create-fill-exempted) and stash their own origin marker, so every wrapper inherits the refusal and honors a `WithImpactConfirm` retry. This is **block mode only** — advise does not compute at the primitives, because an advisory summary needs a result struct to attach to and the primitives return only an error. Advisory coverage therefore remains exactly the four workflow sites.

## The summary

Attached as `"impact"` on write results:

```json
"impact": {
  "object": "ZCL_PAYMENT_HANDLER",
  "callers": 47,
  "packages": ["Z_BILLING", "Z_ORDERS", "Z_MIGRATION"],
  "cross_package": true,
  "recent_transports": [
    {"transport": "TR-EXAMPLE", "type": "K", "status": "R", "owner": "TESTUSER", "date": "2026-08-03"}
  ],
  "risk": "high",
  "advice": "High impact: 47 callers across 3 package(s) (Z_BILLING, Z_MIGRATION, Z_ORDERS); transport TR-EXAMPLE touched this object on 2026-08-03 — read 2-3 key callers before editing and run unit tests on Z_BILLING after activation.",
  "available": true
}
```

When computation fails: `{"available": false, "unavailable_reason": "...", "risk": "unknown"}`. Computation failure never fails a write by itself.

Sources: one `FindReferences` POST at line 0/column 0 (whole-object usage; grouping rows filtered by `isResult`, self-reference excluded by URI, callers deduped by URI), then two `RunQuery` lookups (E071 for the object's transports, E070 for their headers, tasks collapsed to parent requests via STRKORR, 90-day window). E07x results are never cached.

**Identity derivation** (`deriveWriteImpactIdentity`): source URLs collapse to the whole-object identity — `/source/main` suffixes are stripped and class-include URLs map to the parent class, while program includes keep their own identity (E071 keys them as standalone `R3TR PROG` objects). The workbench type (`CLAS/OC`) is cut to the bare R3TR TADIR type (`CLAS`) for the E071 leg; an unmappable URL degrades inside `ComputeWriteImpact` instead of erroring.

## Risk tiers

- **high** — callers ≥ 25, or cross-package spread combined with a transport touch within 90 days
- **medium** — callers ≥ 5, or any transport touch within 90 days
- **low** — otherwise
- **unknown** — the where-used call failed

## Configuration

| Setting | Env / flag / per-system field | Values | Default |
|---|---|---|---|
| Gate | `SAP_IMPACT_GATE` / `--impact-gate` / `impact_gate` | `off` · `advise` · `block` | `off` |
| Threshold | `SAP_IMPACT_THRESHOLD` / `--impact-threshold` / `impact_threshold` | `high` · `medium` | `high` |

`off` computes nothing. `advise` computes once per logical write at the four workflow sites and attaches the summary. `block` additionally refuses writes at or above the threshold until confirmed — at the origin-matching workflow gate and at the two primitives. Threshold `medium` also gates `unknown`; the default `high` does not, so a broken where-used lookup cannot brick writes. Invalid values are rejected at startup (`NormalizeImpactGate` / `NormalizeImpactThreshold`); mode checks are allowlists, so an unrecognized value that slipped past normalization stays inert rather than silently enabling network calls.

## Confirmation flow (block mode)

The refusal renders the report and the exact retry:

```
IMPACT GATE: refusing update of ZCL_PAYMENT_HANDLER (risk: high).
47 callers across 3 package(s) (Z_BILLING, Z_MIGRATION, Z_ORDERS); released
transport TR-EXAMPLE touched this object on 2026-08-03.
To proceed, retry the same call with: confirm: "impact-confirm-3f9a2c1d8e07b64a5c21d9f04b83e7a6"
Token expires in 10 minutes and is valid only for this object and operation.
```

Tokens are `impact-confirm-` plus 32 hex chars (16 bytes from `crypto/rand`; a broken CSPRNG panics rather than falling back to a guessable token). They live in a mutex-guarded in-memory map keyed by **canonicalized object URL + operation letter** — canonicalization normalizes case, trailing slashes, percent-encoding, `/source/main` suffixes, and class-include paths, so issue and consume sites may pass any equivalent URL form. They expire after 10 minutes, are compared constant-time, and are consumed on use. A process restart invalidates them — the same trade-off the codebase already accepts for lock-to-transport context.

**Single confirm per logical write.** When a token is consumed at an origin-matching gate, the ctx marker flips to `confirmed`, and later origin-matching gates within the same logical write honor it instead of demanding a second, un-issuable token. This is what lets a confirmed high-risk rename run to completion: the confirm is consumed at the first `(OpDelete, oldURL)` gate, the create/write/activate legs skip on origin mismatch, and step 6's delete of the old object is authorized by the confirmed marker.

Surfacing: all 13 MCP write tools that can hit the gate carry an optional `confirm` parameter (see README_TOOLS.md); the hyperfocused SAP tool forwards `params.confirm`. The CLI has `--confirm-impact` on `source write`, `source edit`, and `deploy`, and prints a one-line risk summary to stderr when a result carries an advisory impact block.

Rationale for the sub-decisions: retrying with a parameter beats a new confirmation tool (smallest protocol; the error shows the exact retry). In-memory beats signed tokens (matches existing precedent; restart fails safe). A risk tier beats a raw caller count (folds in spread and production recency). Unknown gating only at `medium` keeps defaults unbrickable.

## Exemptions

| Path | Gated? | Rationale |
|---|---|---|
| Creates | No | A brand-new object has no callers. |
| Create-fill writes (`writeSourceCreate`, `CreateFromFile`, `CreateAndActivateProgram`, `CreateClassWithTests`, `ExecuteABAP`'s temp program) | No — marked `withImpactCreateFill`, honored by both primitive guards | The fill PUT targets an object created moments earlier; a degraded-mode block (risk `unknown` under threshold `medium` during a where-used outage) would strand a partial create between LOCK and PUT. |
| Internal cleanup deletes (`reconcileFailedCreate`, `ExecuteABAP` rollback, rename step 6) | No — raw `Client.DeleteObject` is deliberately ungated at the primitive | These delete zombie or just-replaced objects; blocking error-recovery would strand them. The user-facing delete goes through `DeleteObjectWithResult`, which is gated. |
| i18n / UI5 / gCTS writes | No | Out of scope — no ABAP where-used blast radius to compute. |
| DeployZip phase 2 | **Yes — per object, at the primitive** | Phase 1 tolerates "already exists", so phase 2 uploads source to pre-existing objects too; a blanket create-fill exemption would bypass block-mode gating on them. |

Rename step 6 must keep calling raw `DeleteObject`, never `DeleteObjectWithResult`: the wrapper would recompute and restash a fresh *unconfirmed* marker over the confirmed origin marker (same origin identity), re-blocking the delete un-confirmably after the new object already exists.

## Degradation ladder

1. Full: refs + E071/E070 → complete summary
2. Free SQL blocked or failing: refs only — no `recent_transports`, tiering without the transport signal
3. Refs failing: `available: false`, risk `unknown`
4. Gate `off`: nothing runs

## Documented trade-offs

Each of these is a reviewed, deliberate decision — not an open bug.

- **Token burns before write I/O.** The token is consumed at the policy gate, before the PUT/DELETE. A confirmed call that fails mid-flight (lock lost, transport error) has spent its confirmation; the retry goes through a fresh block round-trip. Failing in the safe direction is the point.
- **Reissue overwrites.** Issuing a token for an `(object, op)` key invalidates any previous token for that key, so two agents blocked on the same object race — the last block's token wins. The gate assumes a single operator per server process.
- **Risk is recomputed per retry.** A confirm-carrying retry recomputes the blast radius before the gate; if risk has dropped below the threshold the token is never consumed and simply ages out — the token authorizes a risk level, not a bypass. Consequence of the unknown-semantics choice: at the default threshold `high`, a where-used flake on the retry drops risk to `unknown`, which `high` does not gate, letting a previously-blocked write through. Operators who want fail-closed behavior run `--impact-threshold medium`, which gates `unknown`.
- **Class-include tokens are class-wide.** Canonicalization maps every include URL to the parent class, so a token issued for a main-source update is consumable for a testclasses-include update (same operation). Accepted: the alternative — per-include keys — would break EditSource's include path, whose origin marker must match the primitive's identity.
- **Delete-window CSRF caveat.** `DeleteObjectWithResult` computes impact between the caller's earlier lock call and the DELETE; the impact legs issue CSRF-bearing POSTs, and a token expiring exactly in that window triggers a stateless CSRF refresh that can unbind the lock session, failing the DELETE loudly. Narrow, loud, gate-on only; hardening (a stateful CSRF fetch while a lock handle is live) is deferred.
- **One-shot CLI cannot redeem tokens.** The token store is per-process; a fresh `vsp` run can never redeem a token issued by a previous run's refusal. `--confirm-impact` exists for interface parity and a future long-running serve mode; block-mode one-shot CLI operators lower `--impact-gate` for the invocation instead.
- **DeployZip confirms are per object.** An N-object zip over pre-existing objects blocks per object at the primitive; each run's failures carry their own tokens, a `confirm` retry can redeem exactly one (tokens are keyed per object), and every rerun re-prints the full deployment report — N gated objects mean N sequential confirm round-trips with re-report noise. Accepted over a blanket exemption, which would un-gate bulk overwrites of existing code.
- **Batch amplification.** Every gated computation costs ~3 stateless reads (one usageReferences POST, two `RunQuery` lookups). Batches funneling per object through `WriteSource` pay it per updated object in advise and block alike. Batches reaching the primitives markerless — `dsl.Import` → `DeployFromFile` → `UpdateFromFile`, DeployZip phase 2 — pay it per pre-existing object in **block mode only** (advise computes nothing on those paths), and there it lands in the LOCK→PUT window (precedent: the AllowedPackages lookup already does). CI operators running large imports under a gate should budget for it.

## Out of scope (Phase 2)

Transitive impact via the graph engine (`builder_adt.go` feeding `graph.Impact`; the schema reserves `transitive_depth`), caching, method-level granularity, and changing the default from `off` to `advise` (bundle with the safe-by-default flip).

## Verification status

Unit coverage is complete (`pkg/adt/impact*_test.go`, `workflows_impact_test.go`, `internal/mcp/handlers_source_test.go`). Live integration coverage exists in `pkg/adt/integration_impact_test.go` (build tag `integration`): the advisory attach and create exemption, the usageReferences round-trip with an `isResult` assumption probe, block mode's no-false-positive path at threshold `medium` on a fresh `$TMP` object (plus the real block → token → retry round trip whenever the live where-used lookup degrades), and the in-process token-store round trip. Deliberately unverified live: a guaranteed block on a genuinely high-risk object — that requires a seeded fixture package (≥ 25 callers or a recent transport touch) and is deferred to a manual sandbox canary (plan, Task 11 as-built note).
