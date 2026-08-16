---
name: status
description: "Show the connected SAP system and VSP session state: system ID and release, detected features, VSP mode, safety restrictions, ZADT_VSP availability. Use for 'what system am I on', 'what can vsp do here', 'is debugging available', 'show status'. Triggers: status, system info, features, mode. Scope: a read-only snapshot — for a persisted profile that other skills reuse, run bootstrap-system-context."
---

Display a comprehensive status report of the connected SAP system.

## Workflow

1. Run **GetSystemInfo** — show system ID, release, kernel version, database
2. Run **GetFeatures** — show which features are available:
   - HANA, abapGit, RAP, AMDP, UI5, Transport
   - For each: Available (yes/no), Mode (auto/on/off)
3. Run **ListDependencies** — show ZADT_VSP installation status
4. Report the current VSP mode (`focused` / `expert` / `hyperfocused`) and any safety restrictions
5. Report the auth method in use (basic / cookie / browser SSO)

## Output Format

```
System: <SID> (<release>) on <database>
Mode: hyperfocused (1 universal SAP tool, default) | focused (~103 tools) | expert (~154 tools)
Auth: basic | cookie | browser SSO
Safety: read-only | restricted to <packages> | unrestricted

Features:
  HANA:      available | not available
  abapGit:   available | not available
  RAP:       available | not available
  Transport: available | not available
  UI5:       available | not available
  AMDP:      available | not available

ZADT_VSP: installed | not installed
  WebSocket debugging: available | requires ZADT_VSP
  Report execution:    available | requires ZADT_VSP
  RFC calls:           available | requires ZADT_VSP
```

## Example Usage

```
/vsp:status
```

## Layered Checking with Gating

Run the checks in this order. Each layer has prerequisites; when a prerequisite failed, mark the downstream layer **SKIP** and print the rationale — **never run a check that can only false-negative.** A ZADT_VSP object probe against a dead connection reports "not installed" for objects that exist, and that misleads the user into reinstalling. SKIP with a reason is correct; a false FAIL is not.

**Layer 1 — Connectivity**
- **GetSystemInfo** — does the system answer at all?
- FAIL → report the transport-level error (DNS, TLS, HTTP status, credentials rejected) and mark Layers 2-4 `[SKIP — no connection; fix Layer 1 first]`.

**Layer 2 — Authorization probes** (gated on Layer 1)
Each probe tests one authorization. Run all four even if one fails — they are independent. On failure, **name the missing authorization**, never just "error":

| Probe | Tests | On failure report |
|-------|-------|-------------------|
| **SearchObject** (any simple query, max 1 result) | Repository read access | "Missing S_DEVELOP (DISPLAY) — object search denied" |
| **GetInactiveObjects** | Developer worklist access | "Missing S_DEVELOP — developer session denied" |
| **ListTransports** | Transport read access | "Missing S_TRANSPRT — transport queries denied" |
| **GetTableContents** on `T000` | Generic table read | "Missing S_TABU_DIS — table reads (and RunQuery) denied" |

**Layer 3 — Feature detection** (gated on Layer 1)
- **GetFeatures** — HANA, abapGit, RAP, AMDP, UI5, Transport, each with mode auto/on/off.
- If the Layer-2 transport probe failed, annotate the Transport feature line: availability may be misreported for this user.

**Layer 4 — ZADT_VSP objects** (gated on Layer 1 AND the Layer-2 repository-read probe)
- **ListDependencies** — ZADT_VSP installation status, which unlocks WebSocket debugging, report execution, and RFC calls.
- If **SearchObject** failed in Layer 2, mark `[SKIP — repository read denied; object checks would report existing objects as missing]`.

### Layered Output Format

Prefix each line of the report (see Output Format above) with its verdict, and always print the rationale on SKIP:

```
[PASS] Connectivity: <SID> (<release>) reachable
[FAIL] Auth: ListTransports denied — missing S_TRANSPRT
[PASS] Features: HANA, abapGit, Transport available
[SKIP] ZADT_VSP: repository read denied in Layer 2 — cannot distinguish "missing" from "unreadable"
```

An auth-probe FAIL is a finding about *this user's* authorizations, not about the system: say so explicitly, so the user takes the report to their security team rather than to Basis.

---
