---
name: transport-overview
description: "System-wide register of open transport requests — every modifiable request with owner, age, size, and risk flags (same object in two open requests, local/$TMP-package objects that will not import, stale >90 days, empty requests). Headers and catalog queries only — never source diffs. Use when the user says 'what transports are open in the system', 'show all open transports', 'transport backlog', 'what is everyone working on', 'which requests are ready to release', 'find transport conflicts before the import wave'. Triggers: transports, open requests, backlog, transport register, release readiness, import conflicts, basis overview. Scope: breadth-first inventory — what exactly changed inside one transport (object-by-object diffs) is transport-review; releasing/importing as part of a delivery is deploy."
---

> **System profile:** before generating release-specific syntax or proposing transport/feature workflows, read `.claude/vsp-system-profile.md` if it exists; if it's missing and the task is release- or feature-sensitive, run the **bootstrap-system-context** skill first.

You are producing a basis-/lead-oriented **register of every open transport** in the system: who owns what, how large each request is, how old it is, and where the risks are — without ever pulling source or diffs. Deliberately breadth-first and token-cheap.

This is the **breadth** companion to **transport-review** (depth — what exactly changed inside one request). The pairing:

| | transport-overview (this skill) | transport-review |
|---|---|---|
| Question | "What's open and risky across the system?" | "What exactly changed in this one request?" |
| Unit | all open requests | one request's objects |
| Reads | headers, counts, catalog tables | object sources and diffs |
| Cost | cheap (KBs) | expensive (source reads per object) |

Never diff here. If the user wants contents-level judgment on one request, hand off to transport-review.

## Smart Defaults (apply silently, do NOT ask)

| Setting | Default | Rationale |
|---|---|---|
| Users | all (`user="*"`) | Basis cares about the whole system, not one developer |
| Status | modifiable only | `ListTransports` returns open requests; released history is `vsp changelog` territory |
| Payload | headers + counts first | Inventory needs counts, never source |
| Diffs | never | transport-review's job; this skill stays breadth-only |
| Grouping | by owner | The basis mental model: "whose requests, how many, how big, how old" |
| `RunQuery` `max_rows` | 500 for catalog queries | The default 100 truncates a busy system's E070/E071 silently |

## Tool routing

| Step | Primary vsp tool | Fallback |
|---|---|---|
| Register of open requests | **ListTransports** (`user="*"`) | **GetUserTransports** per known user (expert mode) |
| Age / staleness | **RunQuery** on E070 (`AS4DATE`) | none — report "age unavailable", never guess |
| Expand one request's objects | **GetTransport** | **RunQuery** on E071 for that TRKORR |
| Object package resolution ($TMP flag) | **RunQuery** join E071 × TADIR | **SearchObject** per flagged object |
| Duplicate object across requests | **RunQuery** join E070 × E071 | compare explicitly expanded **GetTransport** manifests |
| One object's current lock/assignment | **GetTransportInfo** (expert mode; needs `object_url` + `dev_class`) | **RunQuery** on E071 |

## Step 1: The register (cheap, always)

```
ListTransports(user="*")
```

One row per request: number, owner, description, status, target, tasks with objects. That alone answers "what's open and how big" — count objects across tasks per request; do not print full object lists for every request.

**Scope guard (hard cap):** on a busy system this can be hundreds of requests. Aggregate (counts per owner, top-N by size); expand at most **10** requests in later steps. If more are flagged, report the register and say: *"N requests flagged — tell me which to expand, I won't pull all of them."*

## Step 2: Enrich with catalog data (where free SQL is allowed)

`ListTransports` carries no dates. Ages, cross-request duplicates, and package checks need `RunQuery` (ABAP SQL — `ASCENDING`/`DESCENDING`, `max_rows` instead of LIMIT):

```sql
-- Requests and their tasks, with age (STRKORR links a task to its parent request)
SELECT trkorr, strkorr, trfunction, trstatus, as4user, as4date
  FROM e070 WHERE trstatus IN ( 'D', 'L' )
```

```sql
-- All object entries in open requests/tasks (aggregate client-side, mapping task → parent via STRKORR)
SELECT a~trkorr, a~strkorr, b~pgmid, b~object, b~obj_name
  FROM e070 AS a INNER JOIN e071 AS b ON b~trkorr = a~trkorr
  WHERE a~trstatus IN ( 'D', 'L' )
```

```sql
-- Local-package objects sitting in a transport (they will NOT import)
SELECT a~trkorr, b~object, b~obj_name, c~devclass
  FROM e070 AS a
  INNER JOIN e071 AS b ON b~trkorr = a~trkorr
  INNER JOIN tadir AS c ON c~pgmid = b~pgmid AND c~object = b~object AND c~obj_name = b~obj_name
  WHERE a~trstatus IN ( 'D', 'L' ) AND c~devclass LIKE '$%'
```

If `RunQuery` is blocked (`--block-free-sql`, read-only policy), skip this step and **say so in the report**: "age/overlap/package flags unavailable — free SQL blocked". Not checked ≠ clean.

## Step 3: Risk / health flags (the basis value)

- **Same object in two open requests** — same (`pgmid`, `object`, `obj_name`) under two different parent requests (from the E070×E071 query, task entries mapped to their parent). Import-order risk: whichever releases second wins. Without SQL, only manifests you explicitly expanded via `GetTransport` count — label the result "checked among expanded requests only". Raw-key matching is conservative: an `R3TR` entry and a `LIMU` sub-object of the same class won't match, so call it "exact CTS-key overlap", not exhaustive conflict detection.
- **Stale (> 90 days)** — `AS4DATE` older than 90 days from today. Long-open requests accumulate drift and block reassignment. SQL-only flag.
- **$TMP / local-package objects** — object's TADIR `DEVCLASS` starts with `$` yet it sits in a transport: it will not import downstream. TADIR matching covers `R3TR` entries only; `LIMU` entries resolve via their enclosing object. Never infer package from the manifest alone.
- **Empty requests** — zero objects across all tasks. Cleanup candidates (delete or release).
- **No target** — local request that cannot go anywhere (often a mistake for work meant to ship).
- **Locked objects blocking others** — for a specific contested object, `GetTransportInfo` gives current lock holder and task. It is *current assignment*, not history — full historical membership is E070/E071 (or `vsp changelog`), not this tool.

## Step 4: Report

Use this structure (identifiers below are synthetic):

```markdown
# Open transports — <SID>  (<N> requests, <M> objects, <K> owners)

Data sources: ListTransports + E070/E071/TADIR via RunQuery   ← or: "ListTransports only — free SQL blocked; age/overlap/package flags NOT checked"

## By owner
| Owner | Requests | Objects | Oldest | Notable |
|---|---|---|---|---|
| TESTUSER  | 6 | 41 | 210d | 1 empty, 1 stale, 1 local-only |
| TESTUSER2 | 2 | 8  | 12d  | |

## Register
| Request | Owner | Description | Objects | Age | Target | Flags |
|---|---|---|---|---|---|---|
| TR-EXAMPLE-1 | TESTUSER  | Demo RAP service | 12 | 210d | (none) | stale; no target — won't ship |
| TR-EXAMPLE-2 | TESTUSER2 | Pricing fix      | 3  | 12d  | QAS    | ZCL_DEMO_PRICE also in TR-EXAMPLE-1 |

## Needs attention
- R3TR CLAS ZCL_DEMO_PRICE is in 2 open requests (TR-EXAMPLE-1, TR-EXAMPLE-2) → import-order risk; sequence or consolidate.
- TR-EXAMPLE-3 (TESTUSER) is empty → delete or release.
- TR-EXAMPLE-1 contains 2 objects in $TMP → they will not import; move to a transportable package first.
```

Write to disk only if asked; otherwise return inline.

## Error Handling

| Error | Cause | Fix |
|---|---|---|
| `ListTransports` blocked / "transport operations not enabled" | Server started without `--enable-transports` / `--allow-transportable-edits`, or `SAP_FEATURE_TRANSPORT=off` | Ask the user to restart vsp with transports enabled; report what could not be checked |
| `RunQuery` blocked | `--block-free-sql` or policy | Expected on locked-down systems — degrade per Step 2; flag the gaps explicitly |
| `user="*"` returns only my requests | Backend authorization (`S_TRANSPRT`) restricts cross-user listing | State the register covers the current user only; do not present it as system-wide |
| E070/E071 rows exactly at `max_rows` | Silent truncation | Re-run with a higher `max_rows`; never aggregate a truncated result without saying so |
| `GetTransportInfo` / `GetUserTransports` not found | Focused mode — expert-only tools | Use `ListTransports`/`GetTransport`/`RunQuery` instead |
| `GetTransport` slow across many requests | Expanding too much | Respect the 10-request cap; expand only flagged/in-focus requests |

## When to use this skill

- Basis / release manager: "what's open across the system, and what's risky to import?"
- Team lead: "what is everyone working on?" — backlog and cleanup review.
- Pre-import / pre-go-live: find overlaps, stale requests, and local-only requests before a transport wave.

## When NOT to use this skill

- **What exactly changed in one request** (source diffs, object-by-object judgment) → **transport-review**.
- **One object's release/import history** → `vsp changelog` / `vsp changes` CLI (E070/E070A/E07T-driven), not the ADT transport tools.
- **Releasing or importing as part of a delivery** → **deploy**. `ReleaseTransport` is irreversible — never release from an overview pass without the owner's explicit confirmation.
- **Cross-system comparison** (is DEV ahead of QAS) → out of scope: vsp binds one system per server; run against each profile (`vsp systems`) and compare.

## Follow-up Options

- "Review the actual changes in one of these?" → **transport-review**.
- "Release the ready ones?" → `ReleaseTransport` (irreversible; confirm request number and owner first).
- "Clean up the empty ones?" → `DeleteTransport` (modifiable requests only; confirm first).
- "Which packages/boundaries does a request touch?" → `vsp tr-boundaries` / `vsp changes`.
