# Impact-Gated Writes — Design

**Date:** 2026-08-15
**Status:** Validated (brainstorming session with maintainer)
**Companion:** [2026-08-15-impact-gated-writes.md](2026-08-15-impact-gated-writes.md) (implementation plan) · pointer report `reports/2026-08-15-004-impact-gated-writes-design.md`

## Problem

vsp's dependency analysis — where-used, package boundaries, transport history — is its clearest differentiator, but it is invisible at the moment it matters most: when an agent writes to the system. An agent editing `ZCL_PAYMENT_HANDLER` learns nothing about its 47 callers until something breaks. Competing servers gate writes on *permission* (read-only flags, package allowlists). Nobody gates on *impact*, because nobody else has the analysis.

## Goal

Every risky write returns a blast-radius summary the **agent** acts on mid-task: read key callers before editing, run tests on affected packages after, or stop and ask. Operators can escalate the same signal into an enforcement gate.

## Decisions (each validated in review)

| Decision | Choice | Why |
|---|---|---|
| Gate mode | Advisory by default when enabled; blocking opt-in | No workflow breakage; enterprises get enforcement |
| Depth | Where-used + transport recency (~3 HTTP calls) | Sub-second; transitive graph impact deferred to Phase 2 |
| Scope | Update, edit, delete, rename | Creates have no callers; keeps the dev loop fast |
| Audience | The agent | Terse structured output plus one agent-directed advice sentence |
| Architecture | Compute in `pkg/adt` workflow layer | MCP, CLI, and DSL all funnel through it; one choke point |

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
  "advice": "High impact: 47 callers across 3 packages; a released transport touched this object 12 days ago. Read 2-3 key callers before editing and run unit tests on Z_BILLING after activation.",
  "available": true
}
```

When computation fails: `{"available": false, "unavailable_reason": "...", "risk": "unknown"}`. Computation failure never fails a write by itself.

Sources: one `FindReferences` POST (caller count and packages), then two `RunQuery` lookups (E071 for the object's transports, E070 for their headers, 90-day window). E07x results are never cached.

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

`off` computes nothing. `advise` computes once per logical write — before lock acquisition, at the same point as the package pre-check, so no stateless call lands between LOCK and PUT — and attaches the summary. `block` additionally refuses writes at or above the threshold until confirmed. Threshold `medium` also gates `unknown`; the default `high` does not, so a broken E071 lookup cannot brick writes.

## Confirmation flow (block mode)

The refusal renders the report and the exact retry:

```
IMPACT GATE: refusing update of ZCL_PAYMENT_HANDLER (risk: high).
47 callers across 3 packages (Z_BILLING, Z_ORDERS, Z_MIGRATION); released
transport TR-EXAMPLE touched this object on 2026-08-03.
To proceed, retry the same call with: confirm: "impact-confirm-3f9a2c1d"
Token expires in 10 minutes and is valid only for this object and operation.
```

Tokens come from `crypto/rand`, live in a mutex-guarded in-memory map keyed by object URL + operation, expire after 10 minutes, and are consumed on use. A process restart invalidates them — the same trade-off the codebase already accepts for lock-to-transport context. MCP write tools gain an optional `confirm` parameter; the CLI gains `--confirm-impact`.

Rationale for the sub-decisions: retrying with a parameter beats a new confirmation tool (smallest protocol; the error shows the exact retry). In-memory beats signed tokens (matches existing precedent; restart fails safe). A risk tier beats a raw caller count (folds in spread and production recency). Unknown gating only at `medium` keeps defaults unbrickable.

## Degradation ladder

1. Full: refs + E071/E070 → complete summary
2. Free SQL blocked or failing: refs only — no `recent_transports`, tiering without the transport signal
3. Refs failing: `available: false`, risk `unknown`
4. Gate `off`: nothing runs

## Out of scope (Phase 2)

Transitive impact via the graph engine (`builder_adt.go` feeding `graph.Impact`; the schema reserves `transitive_depth`), caching, method-level granularity, and changing the default from `off` to `advise` (bundle with the safe-by-default flip).
