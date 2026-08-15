---
name: rca
description: "Structured root-cause analysis for a runtime error, dump, or failed job: collect evidence first (dumps, traces, version history, recent transports), then hypothesize, then verify on the system. Use for 'why did this fail in prod', 'analyze this dump', 'root cause', 'RCA for the incident'. Triggers: root cause, RCA, incident, production error, failed job. Scope: an evidence-driven workflow producing a findings write-up — free-form interactive debugging is abap-debugger."
---

Investigate a runtime error using the 4-phase Root Cause Analysis workflow.

## Workflow

1. **Discover the failure:**
   - If dump ID provided: Run **GetDump** directly
   - If exception type provided: Run **ListDumps** filtered by exception type
   - If program name provided: Run **ListDumps** filtered by program
   - If no argument: Run **ListDumps** for the current user, last 24 hours
2. Present the dump details: exception type, failing program/method, line number, stack trace
3. Read the failing source code with **GetSource** (method-level for classes)
4. Analyze the code and propose likely root cause
5. If WebSocket debugging is available (check **GetFeatures**):
   - Offer to reproduce with breakpoints (Phase 2-3 of RCA)
   - Guide through: SetBreakpoint → RunUnitTests → DebuggerListen → DebuggerAttach → inspect variables
6. Propose a fix and offer to apply it

## Prerequisites

- Phase 1 (discovery + static analysis): Always available
- Phases 2-3 (reproduction + debugging): Requires ZADT_VSP WebSocket handler
- Phase 4 (fix): Requires write permissions

## Example Usage

```
/vsp:rca                           # Recent dumps for current user
/vsp:rca CX_SY_ZERODIVIDE         # Find zero-division dumps
/vsp:rca ZADT_TEST_REPORT          # Dumps from specific program
/vsp:rca 20260329_142355_DEV_001   # Specific dump ID
```

## Root-Cause Categories

Every hypothesis you present MUST declare exactly one of these 8 categories. If a hypothesis doesn't fit a category, it isn't specific enough yet — keep investigating. Narrow to 2-3 hypotheses maximum, each with a confidence level (High/Medium/Low) and a concrete confirmation step.

| Category | Typical symptoms | Key signals |
|----------|------------------|-------------|
| **Master / input data** | Only specific records, values, or documents fail; others succeed | Variable values in the dump, **RunQuery** on the involved master/transaction tables |
| **Authorization** | Only specific users fail with the same input | SU53 capture (ask user), recent role changes, auth-check statements near the failure line |
| **Customizing** | Only specific org units, company codes, or document types affected | Config table reads via **RunQuery**, recent customizing transports in **ListTransports** |
| **Interface / RFC / Batch** | External integration, IDoc, or background job fails; dialog works | RFC/tRFC queues, job logs, IDoc status (ask user — see evidence matrix), RFC-related exception types in **GetDump** |
| **Custom development** | Z*/Y* objects in the call stack | **GetDump** stack trace, **FindReferences** on the failing object, recent Z* transports |
| **Performance** | Timeouts (TIME_OUT), long runtimes, lock waits | **TraceExecution**, **ListSQLTraces**, lock entries (ask user for SM12) |
| **Transport / version** | Worked yesterday; broke right after an import, patch, or upgrade | **ListTransports** timing vs incident start, version/revision comparison of the failing object |
| **Infrastructure** | System-wide or instance-wide failures, DB/host-level errors | Kernel/DB error text in the dump, system log (ask user for SM21) |

## Evidence Matrix — tool-answerable vs tool-unreachable

Split every piece of needed evidence into two buckets **before** asking the user anything:

**Tool-answerable — query it, never ask the user for it:**

| Evidence | Tool |
|----------|------|
| Runtime errors, stack traces, dump variables | **ListDumps** / **GetDump** |
| Recent changes / imports correlated with the incident | **ListTransports** |
| Execution path and hot spots | **TraceExecution** |
| SQL behavior, slow statements | **ListSQLTraces** |
| Callers of the failing object | **FindReferences** |
| Data values in involved tables | **RunQuery** |

**Tool-unreachable — this list generates your user questions:**

Authorization traces (SU53/STAUTHTRACE), application log (SLG1), system log (SM21), update errors (SM13), RFC/tRFC/qRFC queues (SM58/SMQ1/SMQ2), background job logs (SM37), IDoc status (WE02/BD87), OData/Fiori error logs (/IWFND/ERROR_LOG), lock entries (SM12).

Rules:
- Ask the user **only** about tool-unreachable gaps, and only those that discriminate between your remaining hypotheses. Max 3 questions per round.
- Each question should name the transaction the user needs and what to report back (e.g. "In SM58, are there stuck tRFC entries for destination X from around the incident time?").
- 0 results from a tool query means "nothing found in this window/filter", not "ruled out" — say which filter you used.

## BLOCKED, not "probably"

Speculation is forbidden. A root cause is either **supported by evidence in hand** (tool output or an explicit user answer) or it is **not a conclusion**.

- Never write "probably", "most likely", or "it seems" as a final verdict without naming the evidence.
- If every remaining hypothesis depends on tool-unreachable evidence the user cannot supply, stop and return **BLOCKED** with exactly three things: (1) what is confirmed so far, (2) which evidence is missing, (3) the precise artifact that would unblock (e.g. "SU53 screenshot from the affected user, taken immediately after the failure").
- A BLOCKED result with a sharp evidence request is a good outcome. A confident-sounding guess is a failure of this skill.

---

Category framework, evidence-matrix split, and no-speculation rule adapted from [babamba2/superclaude-for-sap](https://github.com/babamba2/superclaude-for-sap) (MIT).
