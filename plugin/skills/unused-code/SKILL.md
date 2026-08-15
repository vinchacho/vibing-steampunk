---
name: unused-code
description: "Find custom code that never runs by fusing SCMON/SUSG runtime usage data (RunQuery) with static where-used (FindReferences) and vsp's native Slim dead-code analysis into one USED/LIKELY_UNUSED/UNUSED verdict per object. Use when the user says 'find unused custom code', 'what can we delete safely', 'dead Z objects', 'identify retirement candidates', 'is anyone still running this report'. Triggers: unused, dead code, retirement, decommission, cleanup, SCMON, SUSG, usage statistics, slim. Scope: read-only evidence and classification — never deletes; executing deletions belongs to abap-developer, dependency deep-dives to abap-architect, migrate-vs-delete triage of survivors to clean-core-check."
---

> **System profile:** before generating release-specific syntax or proposing transport/feature workflows, read `.claude/vsp-system-profile.md` if it exists; if it's missing and the task is release- or feature-sensitive, run the **bootstrap-system-context** skill first. (SCMON exists on NW ≥ 7.50, SUSG on ≥ 7.52 — on older releases only the static half of this skill works.)

You find Z/Y custom objects that are **never called at runtime** — the prerequisite for any credible custom-code retirement project. You fuse three independent signals into one verdict per object:

1. **Runtime evidence** — SAP call-monitor data (SCMON raw tables or SUSG aggregates) read via **RunQuery**
2. **Static where-used** — **FindReferences** against the live cross-reference index
3. **Slim dead-code analysis** — `vsp slim` (zero static incoming references in CROSS/WBCROSSGT, batch over a whole package)

An object condemned by all three is a far stronger deletion candidate than any single signal can produce. This skill is strictly **read-only**: it reports, it never deletes.

## Hard scope rules (refuse, don't comply)

**Refuse to run without a filter.** A "list all unused code" report across a full system returns tens of thousands of rows and is useless. Require at least one of:

- **Package** (e.g., `$ZDEMO`) — analyze only objects in this package
- **Namespace prefix** (e.g., `ZDEMO_*`) — only objects matching the pattern
- **Object list** — explicit names to check
- **Intent statement** — e.g., "find Z reports in FI that haven't run in 90 days" — narrow by purpose

If none is given, ask:

> Which package or namespace do you want to audit? E.g., "the $ZDEMO package", "everything starting with ZDEMO_", or "these 15 reports I'm planning to delete".

**Refuse scopes over 500 candidates.** A 500-object deletion list is not actionable. Tell the user the count, ask them to narrow (subpackage, prefix, object type), and stop.

## Smart Defaults (apply silently, do NOT ask)

| Setting | Default | Rationale |
|---|---|---|
| Runtime source | Probe SCMON first; use it if rows exist, else SUSG | SCMON is fresher; SUSG has months of history |
| Candidate types | PROG, CLAS, FUGR/FUNC with Z/Y prefix | Only these are "called at runtime" in a way SCMON/SUSG attributes |
| Slim pass | `vsp slim '<package>' --level objects --format json` | One batch call covers the whole scope |
| FindReferences | Only for objects with zero runtime hits | Expensive per object — don't spend it on proven-USED code |
| Report shape | Headline + UNUSED table + LIKELY_UNUSED with caller hints; top 20 per table | Full list on request |
| SQL chunking | `IN (...)` lists of ≤ 100 names per RunQuery | ADT FreeSQL query-length limits |

## Prerequisites — probe before planning

1. **RunQuery must be allowed.** If the server runs with `--block-free-sql` / `SAP_BLOCK_FREE_SQL` (or `--read-only` excludes it via `--allowed-ops`), degrade to **static-only mode** (see below) — do not fight the policy.
2. **User needs `S_TABU_NAM` on the SCMON/SUSG tables** — a 403 on them means an auth gap, not absent data.
3. **Monitoring must have been active at some point.** Probe:

```
RunQuery(sql_query="SELECT COUNT(*) AS cnt FROM scmon_data")
RunQuery(sql_query="SELECT COUNT(*) AS cnt FROM susg_data")
```

| SCMON rows | SUSG rows | Runtime source |
|---|---|---|
| > 0 | > 0 | SCMON (fresher) — mention SUSG is available for longer history |
| > 0 | 0 | SCMON (SUSG hasn't aggregated yet) |
| 0 | > 0 | SUSG (SCMON's ~7-day window expired; SUSG keeps months) |
| 0 | 0 | **No runtime data.** Say so, offer static-only mode, and give the activation steps below. Never present static-only results as "unused". |

If the customer has no data yet, tell them: activate the monitor in transaction `SCMON`, let it run ≥ 7 days (mind the coverage-window caveat below), and for permanent history schedule `SCMON_COLLECT` daily followed by `SUSG_COLLECT_FROM_SCMON`. Then re-run this skill.

## Procedure (token-cheap first)

### Step 1: Probe runtime data (two COUNT queries above)

### Step 2: Enumerate the scope → set S

- **Package:** `GetPackage(package_name="$ZDEMO")`; keep PROG/CLAS/FUGR objects with Z/Y prefix.
- **Prefix:** `SearchObject(query="ZDEMO_*", maxResults=500)`; filter the result list to runtime types — SearchObject has no type parameter, filter after.
- **Object list:** use the names as given.

Count |S|. Over 500 → refuse (rule above). Note objects that don't resolve on the system — they classify as INDETERMINATE later, never silently dropped.

### Step 3: Slim pass (one batch call) → set D

```bash
vsp slim '$ZDEMO' --level objects --format json      # add --exact-package to skip subpackages
```

Slim flags objects with **zero static incoming references** in the CROSS/WBCROSSGT indexes. `--level methods` additionally reports dead methods inside live classes — offer it as a follow-up, not by default. If the `vsp` binary isn't available, skip this step and rely on FindReferences alone (note the loss: single-engine static evidence).

### Step 4: Runtime usage → set U

**SCMON** (column caveat — SAP's naming is inverted: `scmon_data~trigid` joins `scmon_prog~progid` and identifies the *executed* object; `scmon_data~subid` joins `scmon_sub` and identifies the *trigger root*):

```
RunQuery(sql_query="SELECT p~progname, p~object, p~obj_name, SUM( d~counter ) AS execs FROM scmon_data AS d INNER JOIN scmon_prog AS p ON d~trigid = p~progid WHERE p~obj_name IN ( 'ZDEMO_REPORT_A', 'ZCL_DEMO_B' ) GROUP BY p~progname, p~object, p~obj_name ORDER BY execs DESCENDING", max_rows=500)
```

Also check the trigger side — some Z objects run only as transactions/RFC entry points, never as inner callees:

```
RunQuery(sql_query="SELECT s~rootname, s~roottype, SUM( d~counter ) AS execs FROM scmon_data AS d INNER JOIN scmon_sub AS s ON d~subid = s~subid WHERE s~rootname IN ( 'ZDEMO_REPORT_A' ) GROUP BY s~rootname, s~roottype", max_rows=500)
```

**SUSG** (longer window; prefer the pre-joined CDS view `susg_i_data` — it exists on ≥ 7.52 with `authorizationCheck: #NOT_REQUIRED`; if it's missing, join the base tables `susg_data`/`susg_prog` analogously to SCMON after checking their fields with GetStructure):

```
RunQuery(sql_query="SELECT progname, obj_type, obj_name, SUM( counter ) AS execs, MAX( last_used ) AS last_seen FROM susg_i_data WHERE obj_name IN ( ... ) GROUP BY progname, obj_type, obj_name", max_rows=500)
```

RunQuery speaks **ABAP SQL**: `DESCENDING` not `DESC`, `max_rows` parameter not `LIMIT`. Chunk `IN` lists at 100 names. Union results into set U (object → exec count, and for SUSG the last-used date where available). Record the observation window.

### Step 5: Static where-used for objects not in U → set W

For each object in S but **not** in U:

```
FindReferences(object_url="/sap/bc/adt/oo/classes/ZCL_DEMO_B")
```

If |S \ U| > 50, don't burn a call per object: take slim's verdict as the static signal and run FindReferences only on (a) objects where slim and the candidate's presence in the package disagree, and (b) the final UNUSED table before publishing it. For LIKELY_UNUSED caller hints, `GetCallersOf(object_uri=...)` gives the caller tree.

### Step 6: Classify (first-match-wins)

| Evidence | Classification | Meaning |
|---|---|---|
| In U (executed ≥ 1 time in window) | **USED** | Runs in production; keep |
| Not in U, static references exist (W) | **LIKELY_UNUSED** | Someone calls it in source, but nothing exercised it in the window — show the callers |
| Not in U, no refs in W, **and** slim flags it (D) | **UNUSED** | Runtime silence + two independent static engines agree — strongest deletion candidate |
| Not in U, no refs in W, but slim disagrees (or slim unavailable) | **LIKELY_UNUSED** | Static engines conflict — stale index or dynamic-call hints; investigate before deleting |
| Doesn't resolve on the system | **INDETERMINATE** | Manual review |

**Static-only mode** (RunQuery blocked or no SCMON/SUSG rows): the top row can never fire. Use **STATIC_UNREFERENCED** / **STATIC_REFERENCED** labels instead and say prominently that runtime usage was **not measured** — a statically unreferenced object may still run daily as a job or RFC. Never print "UNUSED" without runtime evidence.

### Step 7: Annotate the UNUSED table

For each UNUSED object, `GetRevisions(type="PROG", name="ZDEMO_REPORT_A")` gives last change date, author, and transport. Old + unused = safest to retire.

### Step 8: Emit the report

```
Unused Code Audit — <scope>
Runtime source: SCMON (2,843 rows, window 2026-08-08 → 2026-08-15)
   — or — SUSG (aggregated 2026-03-01 → 2026-08-15, 168 days)
   — or — NONE (static-only mode: runtime usage NOT measured)
Static sources: vsp slim (objects level) + FindReferences

Scope:          42 objects
  USED:           18  (43%)  — keep
  LIKELY_UNUSED:  12  (29%)  — static callers or conflicting evidence; investigate
  UNUSED:         10  (24%)  — deletion candidates (all three signals agree)
  INDETERMINATE:   2   (5%)  — manual review
```

UNUSED table:

```
Object              Type  Package   Last change (author / TR)
ZDEMO_OLD_POSTING   PROG  $ZDEMO    2024-02-15 (TESTUSER / TR-EXAMPLE)
ZCL_DEMO_LEGACY     CLAS  $ZDEMO    2023-11-03 (TESTUSER / TR-EXAMPLE)
```

LIKELY_UNUSED with caller hints — a LIKELY_UNUSED object whose callers are all themselves UNUSED is transitively deletable; one runtime-USED caller blocks deletion:

```
ZCL_DEMO_UTILS  (CLAS)
  Called by:  ZDEMO_OLD_JOB   (PROG — also UNUSED → cascade candidate)
              ZCL_DEMO_ACTIVE (CLAS — USED at runtime) ← blocks deletion
```

## Error Handling

| Error | Cause | Fix |
|---|---|---|
| RunQuery refused / blocked | `--block-free-sql`, `--read-only` op policy, or ops whitelist without `Q` | Expected under safety config — switch to static-only mode and label the report accordingly; don't ask for the flag to be lifted unless the user wants runtime evidence |
| `scmon_data` unknown table / activation error | Release < 7.50 (no SCMON) or no ST-PI backport | Static-only mode; note the release limitation |
| Both COUNT probes return 0 | Monitoring never activated | Expected on dev boxes — stop, give SCMON activation steps, offer static-only mode |
| SCMON has rows but SUSG is 0 | SUSG batch never ran / can't aggregate open slices | Use SCMON directly; suggest scheduling `SCMON_COLLECT` + `SUSG_COLLECT_FROM_SCMON` |
| SCMON stopped collecting mid-window | Auto-deactivated at record threshold | Note the gap in the report window; user reactivates via `SCMON` |
| 403 on `scmon_*` / `susg_*` | Missing `S_TABU_NAM` | Name the tables in the auth request; static-only mode meanwhile |
| `vsp slim` fails or binary missing | CLI not installed / no CROSS index | Proceed with FindReferences only; state that the verdict rests on a single static engine |
| Scope > 500 candidates | Filter too broad | Refuse — a 500-object deletion list is not actionable; ask the user to narrow |

## Epistemic honesty (hard rules)

- **Absence of SCMON/SUSG data means "not measured", never "unused."** No runtime rows for an object is only evidence within the stated observation window.
- **Coverage gap:** a year-end report won't appear in a 7-day SCMON window. Always print the window and flag objects whose names suggest periodic use (`*YEAR*`, `*ANNUAL*`, `*MIGR*`).
- **Dynamic calls lie to static analysis:** `SUBMIT` with dynamic names, RTTI method calls, and background RFCs can make a live object look dead in both slim and FindReferences. That's why UNUSED requires runtime silence *too*.
- **0 candidates found ≠ package is clean** — if a probe or tool call failed along the way, say which signal is missing rather than reporting a green result.
- **Never invent counts.** Every exec count, reference, and revision in the report must come from an actual tool result.
- **Synthetic identifiers only in examples** (`ZDEMO_*`, `ZCL_DEMO_*`, `$ZDEMO`, `TR-EXAMPLE`, `TESTUSER`); quote real names only when citing the audited system itself.

## What this skill does NOT do

- **No deletion.** Report only. Deleting is a human decision — via **abap-developer** (DeleteObject on `$TMP` prototypes) or a proper transport for anything transportable, after business-owner sign-off.
- **No cross-system consolidation** (DEV+QAS+PRD usage in one verdict). One system per run; run it per system and merge manually.
- **No SUSG XML imports.** If the customer only has SUSG GUI exports, they need an offline parser — out of scope here.

## When to use / when NOT

**Use for:** scoping a custom-code retirement, pre-migration cleanup ("don't migrate what you don't use"), post-rollback feature removal, dead-code inventories for license or compliance reviews.

**Not for:** dead *methods inside live classes* as the primary question (start with `vsp slim --level methods` or **abap-architect**'s CompareCallGraphs instead), performance analysis of hot code, or ATC-driven quality cleanup (**atc-remediation**).

## Follow-up Options

- "Deeper dependency graph for the LIKELY_UNUSED cluster?" → **abap-architect** (GetCallersOf / CheckBoundaries / `vsp health`)
- "Migrate-vs-delete triage for the USED survivors?" → **clean-core-check**
- "Method-level dead code inside the USED classes?" → `vsp slim '<package>' --level methods`
- "Ready to delete the UNUSED set?" → **abap-developer**, one transport, after business sign-off — this skill won't do it
- "Park the audit for the next session?" → **handoff**

---

Adapted from [arc-mcp/arc-1](https://github.com/arc-mcp/arc-1) `sap-unused-code` (MIT).
