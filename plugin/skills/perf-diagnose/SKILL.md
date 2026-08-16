---
name: perf-diagnose
description: "Find the root cause of slow ABAP SQL, CDS views, reports, or transactions with a cheapest-first diagnostic ladder — read the source for scan patterns, probe selectivity with RunQuery, check ST05 SQL-trace state, analyze ABAP profiler traces (hitlist / dbAccesses), hunt the generator of framework-built SQL via the call graph, and correlate runtime dumps — stopping at the rung that explains it. Use when the user says 'why is this query slow', 'this report takes forever', 'the CDS view times out', 'find the slow SELECT', 'performance problem in ZDEMO_REPORT', 'where is the time going'. Triggers: slow, performance, SQL trace, ST05, full table scan, selectivity, N+1, SELECT in LOOP, index, timeout, TIME_OUT. Scope: read-only diagnosis ending in a concrete fix proposal — stepping through code interactively belongs to abap-debugger, the full incident root-cause workflow (timeline, dumps, transports correlation) is rca, and applying the fix is abap-developer."
---

> **System profile:** before generating release-specific syntax or proposing transport/feature workflows, read `.claude/vsp-system-profile.md` if it exists; if it's missing and the task is release- or feature-sensitive, run the **bootstrap-system-context** skill first. The ST05 and profiler ADT APIs used below do not exist on every release (NW 7.50 lacks `/sap/bc/adt/st05/trace`) — knowing the release up front tells you which rungs are reachable.

You are diagnosing a **slow ABAP SQL statement, CDS view, report, or transaction**. The goal is not "it's slow" but *why*: which statement, what it scans, and the cheapest change that fixes it. This skill is **read-only** — it never writes source, never activates, never changes trace state on the system.

## When to use / when NOT to use

**Use for:** a slow report or transaction, a long-running CDS view or SELECT, a query that times out under load, "it got slow after <change>", a `TIME_OUT` dump chain.

**Do NOT use for:**
- Stepping through code with breakpoints to watch values — that is **abap-debugger**.
- A production incident needing a full timeline (what changed, which transport, which dumps, who) — that is **rca**. This skill answers only the narrower "why is this statement slow".
- Functional bugs (wrong results, not slow results) — **abap-debugger** or **rca**.

## Inputs (ask only for what's missing)

- **What is slow** — an object name (program / class / CDS view / transaction) or a table + access pattern. If all you have is "X is slow", get the object name first — everything below keys off it.
- **How slow / how often** — single slow call vs. slow-under-load vs. intermittent.
- **Reproducible?** — can the user re-trigger it on demand (needed for a live ST05 or profiler capture)? On which system, as which SAP user?
- **Recent change?** — new code, data growth, a transport, an index drop. Narrows the search fast.

## Smart defaults (apply silently, do NOT ask)

| Setting | Default |
|---------|---------|
| `RunQuery` `max_rows` | 100 (never raise above 1000 for a probe) |
| `ListTraces` / `ListDumps` `user` filter | the connected SAP user |
| `ListDumps` date range | last 7 days |
| `GetTrace` analysis order | `hitlist` first, then `dbAccesses` |
| ATC variant for the static pass | a performance variant if `GetATCCustomizing` shows one; otherwise the default variant |
| Probe SQL dialect | ABAP SQL — `ASCENDING`/`DESCENDING` not `ASC`/`DESC`, `max_rows` parameter not `LIMIT` |

## Hard scope caps

- **≤ 5 `RunQuery` probes per hypothesis.** If five probes haven't pinned it, descend a rung instead of probing more.
- **Never `SELECT *` a suspect table.** Probe with `COUNT(*)` or the specific columns under test.
- **≤ 20 objects read per diagnosis.** A package-wide perf audit is not this skill — refuse with: *"That's a package-wide audit, not a single-statement diagnosis — run `RunATCCheck` with a performance variant on the package (see atc-remediation), or name the one slow object and I'll take it down the ladder."*
- **No blind optimization.** If the user asks "just make it faster" with no measurement, refuse to guess: *"I won't propose a fix without evidence of where the time goes — give me the object name and I'll run the ladder first."*

---

## Reachability — don't promise a rung you can't reach

Before descending past rung 1, know what this system and this vsp configuration allow:

- **`RunQuery` may be blocked** (`SAP_BLOCK_FREE_SQL`, `--read-only`, or `--allowed-ops` without `Q`). If it errors as blocked, skip rung 1's probes and lean on rungs 0, 3, and the static pass — say so, don't retry.
- **vsp cannot arm or disarm the ST05 SQL trace, and cannot read trace records.** `GetSQLTraceState` and `ListSQLTraces` are read-only: state + file directory. Arming, reproducing, disarming, and reading the records (the exact SQL, duration, rows fetched, EXPLAIN plan) is a **user-driven SAP GUI ST05 step** — hand over the precise steps (rung 2), never claim you captured a trace yourself.
- **The ST05 ADT API 404s on NW 7.50.** If `GetSQLTraceState` fails with 404, the whole of rung 2's tooling is unreachable on this release — the ST05 steps below still work in SAP GUI; say that and move on.
- If a rung is unreachable, **stop at the deepest reachable rung** and state exactly what evidence is missing and how the user can get it. A diagnosis that names its evidence gap beats a guessed execution plan.

## The diagnostic ladder — stop at the rung that explains it

Work top-down. Each rung is cheaper than the next and usually tells you whether to descend.

### 0. Orient (token-cheap, no execution)

- **GetSource** on the object (for classes: **GetObjectStructure** first, then method-level reads). Eyeball for the usual suspects *before* measuring: `LIKE '%term%'` (leading wildcard = index unusable), `SELECT` with no `WHERE` on a key field, `SELECT *` feeding two used fields, `SELECT` inside a `LOOP` (N+1), missing `FOR ALL ENTRIES` empty-table guard, `ORDER BY` on an unindexed column, client-side filtering of a big result.
- **GetCDSDependencies** for a CDS view — the slow view is often a thin projection over a heavy base stack; know what it actually reads from.
- **GrepObject** for `SELECT` statements when the object is large — cheaper than reading everything.

If rung 0 alone names an unambiguous cause (e.g. a `SELECT ... ENDSELECT` inside a nested `LOOP` over a large table), you may stop here — but label the finding *statically inferred, not measured*.

### 1. Statement-level evidence — RunQuery selectivity probes

`RunQuery` returns **columns and rows only — no execution time, no total match count, no HANA plan**. The evidence it gives you is *selectivity* and *observed responsiveness*, and both are real signals:

```
RunQuery(sql_query="SELECT COUNT(*) FROM zdemo_orders")                          -- table volume
RunQuery(sql_query="SELECT COUNT(*) FROM zdemo_orders WHERE status = 'OPEN'")    -- filter selectivity
RunQuery(sql_query="SELECT COUNT(*) FROM zdemo_orders WHERE descr LIKE '%term%'")-- the suspect predicate
```

- **Selectivity verdict:** matched rows ≫ rows the feature actually needs = a scan/selectivity problem. A `WHERE` that eliminates almost nothing cannot use an index usefully no matter what indexes exist.
- **Equality also slow? It's the view, not your filter.** On a CDS view, probe an exact `WHERE key = '...'` alongside the suspect `LIKE`. If both are equally slow, the `LIKE` is a red herring — the cost is the view itself (deep joins the filter can't lead into, `DISTINCT`, aggregation). Route the fix at the view (rung 0's `GetCDSDependencies` stack), not the predicate.
- **A probe that times out is itself evidence** of an unbounded scan — record the timeout as the finding; don't silently retry with smaller `max_rows` and report success.
- **Index check:** `RunQuery(sql_query="SELECT indexname, fieldname FROM dd17s WHERE sqltab = 'ZDEMO_ORDERS'")` — does the slow `WHERE` match any index's leading fields?
- What this rung **cannot** prove: the execution plan, buffer state, or exact statement duration. If the fix decision needs those (e.g. "add index A vs invert join B"), descend to rung 2 — never present a guessed plan as measured.

### 2. The exact SQL + plan — ST05 SQL trace (state check via vsp, capture via user)

```
GetSQLTraceState()                 -- is a trace already on? (someone may be mid-capture — don't collide)
ListSQLTraces(user="TESTUSER")     -- trace files already recorded for the user
```

vsp stops there by design. To capture the ground truth — the recorded `SELECT`s with **duration**, **rows fetched**, and the **EXPLAIN plan** — hand the user this exact sequence:

1. SAP GUI **ST05** → *Activate Trace with Filter* → filter to their user.
2. Reproduce the slow action **once**, immediately.
3. ST05 → *Deactivate Trace* (always — an armed trace left on is a support incident).
4. ST05 → *Display Trace* → sort by **Duration** descending → for the top statement read the SQL text, Duration, Records (rows fetched), Object; double-click → **Explain** for the DB plan.

What to look for in the records: repeated identical `SELECT`s (N+1), Records ≫ rows the screen shows (selectivity), extra hits on text/association tables the feature doesn't need. Then confirm the file appeared with `ListSQLTraces` and fold the user's readings into the verdict — cited as *user-captured ST05*, not as tool output.

### 3. App-bound or N+1? — ABAP profiler traces

When the SQL itself probes fast but the feature is slow, the time is in ABAP or in call *count*, not per-statement cost:

```
ListTraces(user="TESTUSER")                       -- recent profiler traces
GetTrace(trace_id="...", tool_type="hitlist")     -- hottest ABAP call paths
GetTrace(trace_id="...", tool_type="dbAccesses")  -- which tables, how many times, buffered?
```

- `dbAccesses` is the N+1 detector: one table with a huge access count from inside a loop.
- `hitlist` names the ABAP hot path when the DB isn't the cost at all.
- No usable trace listed? **TraceExecution** (`object_uri`, optionally `run_tests=true`) drives the whole loop for testable code: static call graph → run unit tests to generate a trace → compare static vs actual edges. This is the GUI-free way to *create* a trace; for dialog flows without tests, ask the user to record one in SAT/ST12 and re-run `ListTraces`.

### 4. The generator hunt — who builds this SQL?

A slow statement found in a trace often exists **nowhere in the source** — it is generated by a framework (SADL/RAP, a search help, dynamic `WHERE` assembly), so a literal grep comes up empty. Trace the generator instead:

- **GetCallersOf** on the method that owns the `SELECT` (or the framework entry point) — who reaches it, and with what upstream loop?
- **AnalyzeCallGraph** on the entry object — depth and fan-out show where a per-row call multiplies into thousands of statements.
- **FindReferences** / **GrepPackage** for the table name — every access path to the suspect table, including the ones that build SQL dynamically.
- **SearchObject** for the service/DPC/helper classes when the entry point is a generated artifact; **GetCDSImpactAnalysis** for which consumers drag a heavy CDS view into their list pages.

The fix for generated SQL targets the generator's input (the CDS model, the search-help definition, the RAP behavior), not the statement text.

### 5. Did it dump? — runtime errors as evidence

```
ListDumps(exception_type="TIME_OUT", date_from="...", program="...")
GetDump(dump_id="...")
```

`TIME_OUT` and `TSV_*` (memory) dumps are the performance problem's crash signature: the dump's stack trace names the exact statement that was running when the axe fell — often the fastest route to the culprit for "it dies under load". **Zero dumps in the window means no dumps found in that range and filter — not that the system is healthy.**

### Anytime: the static pass

`RunATCCheck` (performance variant per Smart Defaults) and `AnalyzeABAPCode` flag perf anti-patterns for free — SELECT-in-loop, missing WHERE, obsolete constructs. They can't see data-dependent scans, but they're the cheapest corroboration for a rung-0 suspicion.

---

## Root-cause catalog (pattern → confirm → fix)

| Symptom | Likely cause | Confirm with | Typical fix |
|---------|--------------|--------------|-------------|
| Matched rows ≫ rows needed | Full scan / poor selectivity | Rung-1 `COUNT(*)` probes; ST05 Records | `WHERE` on indexed fields; secondary index; push filter into the CDS |
| `LIKE '%term%'` | Leading wildcard defeats the index | Rung-0 source read + rung-1 probe | Anchor the pattern; search help / full-text index; pre-filter on an indexed field |
| Same table hit thousands of times | N+1 (SELECT in LOOP) | `GetTrace dbAccesses` giant count | `FOR ALL ENTRIES` / join / read-once-then-loop; RAP: prefetch |
| `SELECT *`, two fields used | Over-fetch | Rung-0 source read | Field list; trim the CDS projection |
| Equality probe as slow as the `LIKE` | The view itself (joins/`DISTINCT`/aggregation) | Rung-1 equality-vs-`LIKE` comparison; `GetCDSDependencies` | Restructure so the selective table leads; drop `DISTINCT`/aggregate from list projections |
| Fast in DEV, slow in PROD | Data volume / stale stats / different plan | User-captured ST05 Explain on PROD | Refresh optimizer stats; index; partition |
| Slow but SQL probes fast | ABAP logic / call count above the DB | `GetTrace hitlist` | Move logic into the DB/CDS; cache; collapse per-row calls |
| Slow SQL not in any source | Generated SQL (SADL/search help/dynamic) | Rung-4 generator hunt | Fix the generator's input, not the statement |
| `TIME_OUT` / `TSV_*` dumps under load | Unbounded result / missing paging | `ListDumps` + `GetDump` stack | Server-side paging; filters; package the work |

## SAP GUI escalation (what vsp cannot reach)

| Tool | Gives you what vsp can't |
|------|--------------------------|
| **ST05** | Recorded statements with duration + rows; **Explain** → the DB execution plan; identical-statement grouping |
| **ST12 / SAT** | Combined ABAP+SQL trace with aggregation for dialog flows without unit tests |
| **DBACOCKPIT / HANA studio** | `EXPLAIN PLAN`, PlanViz, plan cache, table/index sizes, optimizer-stats freshness |
| **SE11 / SE14** | Index maintenance (vsp can *read* `DD17S`, not change indexes) |
| **SM50 / SM66** | What work processes are stuck on right now, under load |

## Epistemic honesty — hard rules

- **Never invent measurements.** `RunQuery` yields no timings or plans; only user-captured ST05/SAT output does. Cite every number to the tool or GUI step that produced it, and label rung-0-only conclusions *statically inferred, not measured*.
- **Absence of evidence is not evidence of health.** No traces listed = nothing recorded; no dumps = none in that filter window; ATC clean = the active variant found nothing.
- **A blocked or missing rung is reported, not papered over.** "`RunQuery` blocked by safety config — selectivity unverified" is a valid finding; a guessed selectivity is not.
- **A probe timeout is a result.** Record it as scan evidence; don't shrink the probe until it succeeds and call the query fine.
- **Fix-proposal impact gate:** if the proposed fix touches a shared object (a base CDS view, a widely-used table's index, a central method), read the key consumers first — **GetCallersOf** / **GetCDSImpactAnalysis** — and name the blast radius in the proposal. High-risk fix with unread callers = incomplete diagnosis.
- **Synthetic identifiers only in examples** (`ZDEMO_*`, `ZCL_DEMO_*`, `$ZDEMO`, `TESTUSER`); quote real names only when citing the system under diagnosis.

## Error handling

| Error | Cause | Fix |
|-------|-------|-----|
| `RunQuery` rejected / blocked | `SAP_BLOCK_FREE_SQL`, `--read-only`, or op not in `--allowed-ops` | Expected in hardened setups — skip rung-1 probes, note the gap, use rungs 0/3/static |
| `RunQuery` syntax error near `ASC`/`DESC` or `LIMIT` | Standard-SQL habits; ADT wants ABAP SQL | `ASCENDING`/`DESCENDING`; `max_rows` parameter instead of `LIMIT` |
| `GetSQLTraceState` → 404 | Release has no ST05 ADT API (e.g. NW 7.50) | Expected — rung 2 is GUI-only on this system; hand over the ST05 steps |
| `GetTrace` → 400 or empty `statements` | Profiler is weak for some trace kinds (esp. HTTP) | Expected — use `dbAccesses` for counts, ST05 for statement timings |
| `ListTraces` returns nothing | No trace recorded for that user/filter | Not "no problem" — generate one via `TraceExecution` or SAT, then re-list |
| `TraceExecution` fails on `run_tests` | Object has no unit tests | Ask the user to record a SAT/ST12 trace of the real flow instead |
| Probe hangs / times out | The scan under diagnosis | Record as evidence (see hard rules); don't retry-and-forget |

## Output

Deliver a tight diagnosis, not a tool log:

````
# Performance Diagnosis — <OBJECT NAME>

**Verdict:** <DB-bound scan | view cost | N+1 | app-bound ABAP | generated SQL | dump-confirmed overload>
**Deepest rung reached:** <0–5> — <why you stopped there / what was unreachable>

## The statement
<the offending SQL or call site, and what it scans — with the probe/trace/dump numbers that prove it, each cited to its source>

## Root cause
<one sentence, mapped to the catalog>

## Fix (cheapest first)
1. <concrete minimal change — the index, the filter push-down, the loop collapse — with its trade-off>
2. <next option if 1 is refused>
**Blast radius:** <consumers read via GetCallersOf / GetCDSImpactAnalysis, or "low — object-local">

## Evidence
- <exact tool calls run, in order>
- <user-captured ST05/SAT readings, if any>
- <evidence gaps: what was unreachable and how to close it>
````

## Follow-up options

- **abap-developer** — implement the fix (index, rewritten SELECT, CDS change) and activate it.
- **abap-debugger** — step through the hot path interactively when the profiler hitlist isn't conclusive.
- **rca** — widen to a full incident workflow (timeline, transports, dump correlation) when "slow" turns out to be "broken since <transport>".
- **abap-architect** — impact analysis before a fix that restructures a shared CDS stack.
- **atc-remediation** — work down the findings list when the static pass surfaced more than the one statement.
