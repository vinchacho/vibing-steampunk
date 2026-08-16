---
name: transport-review
description: "Review what actually changed — in a transport request or in your unreleased/unactivated work — as per-object unified diffs with honest baseline labelling, built from VSP version history (GetRevisions + CompareVersions) and transport metadata. Use when the user says 'review this transport', 'what changed in TR X', 'diff the objects in a transport', 'show my pending changes before I activate/release', 'what am I about to ship', 'prepare a change review'. Triggers: transport review, TR diff, change set, pending changes, pre-release gate, release review, what changed, transport contents. Scope: depth on ONE change set with real diffs — a breadth inventory of every open transport (no diffs) is transport-overview and the two are mutually exclusive for a given question; quality review of the code itself is clean-abap-review; dependency/impact deep-dives are abap-architect; releasing or importing is deploy."
---

> **System profile:** before generating release-specific syntax or proposing transport/feature workflows, read `.claude/vsp-system-profile.md` if it exists; if it's missing and the task is release- or feature-sensitive, run the **bootstrap-system-context** skill first.

Answer "what actually changed?" for one transport request or for the user's in-flight work, as a **reviewable report**: a per-object unified diff plus risk flags — not a wall of full source, and never a guessed diff.

VSP has an advantage generic ADT clients lack: **every revision entry from GetRevisions carries the transport request that cut it** (`transport` field). That means you can locate *the* snapshot a released TR produced and its immediate predecessor — a true per-object baseline — instead of approximating with today's source. Use it.

## When to use this skill

- Pre-release / pre-activation gate — "show me everything I'm about to ship."
- Code review of a colleague's transport without leaving the chat.
- Hand-off / audit — a written delta of a change set.
- "What did released TR-EXAMPLE actually change?" (released-TR mode).
- "I've been editing for an hour — what have I actually changed?" (pending-work mode).

## When NOT to use this skill

- **System-wide inventory of every open transport** (who has what open, sizes, conflicts — *no diffs*) → **transport-overview**. These two skills are a mutually exclusive pair: this one is depth-on-one-change-set, that one is breadth-across-the-system. Never run both to answer one question — pick by whether the user wants *diffs of one set* or *a map of all sets*.
- **Understanding one object deeply** (structure, call graph, design) → **abap-architect**.
- **Judging code quality** of the changed source → **clean-abap-review** (this skill only flags, it doesn't grade).
- **Releasing / importing** the transport → **deploy** (this skill is read-only).
- **Cross-system compare** (DEV vs QAS source): out of scope — run vsp against each system profile and diff the two exports as a separate orchestration.

## Smart Defaults (apply silently, do NOT ask)

| Setting | Default | Rationale |
|---|---|---|
| Transport scope | current user's modifiable requests (`ListTransports`) | The work in progress, not released history |
| Order of operations | list → one `GetTransport` → per-object revisions → diffs | Token-cheap first; never pull all sources up front |
| Baseline for a **released** TR | revision *before* the TR → revision *tagged with* the TR | The TR's own snapshot — **never substitute today's active source for an old TR** |
| Baseline for **open/pending** work | latest released revision → `current` | Labelled "since released snapshot" — see coverage rules |
| Diffable types | PROG, CLAS, INTF, FUNC, INCL, DDLS, BDEF, SRVD | The types `GetRevisions`/`CompareVersions` support |
| Object-diff cap | ~40 diffable objects | Above that: summary table only, then ask |
| Impact | On for changed DDLS/BDEF/SRVD in a risk-focused review; otherwise opt-in | Focus extra reads where dependency risk lives |
| ATC | Opt-in (`+atc`), bounded to the changed set | ATC is workload-producing; never fan it out silently |

## Input

The user provides **one of**:

- **A transport id** (e.g. `TR-EXAMPLE`) — review everything in that request (open or released).
- **"my pending changes" / "before I activate"** — review the user's unreleased work (`ListTransports` + `GetInactiveObjects`).
- **An object list or package** — review those objects' recent changes (`vsp changelog '<package>'` finds the transports that touched it).
- **Nothing specific** ("what changed") — list the user's modifiable transports and ask which, or default to pending-work mode.

Optional: `+impact` (consumers of changed CDS/RAP objects), `+atc` (quality findings on the changed set), an output path for a Markdown file.

## Step 1: Resolve scope (cheap reads only)

- **Transport id given** → `GetTransport(transport="<id>")` → tasks + object entries.
- **Pick a transport** → `ListTransports` (current user's modifiable requests; `user="*"` only if asked) → present the table, let the user pick, then `GetTransport`.
- **Another user's requests** → `GetUserTransports(user_name="TESTUSER")`.
- **Package history** → CLI `vsp changelog '<package>' --since YYYYMMDD` (transport-aggregated change history; add `--include-subpackages` for hierarchies) or `vsp changes '<package>'` to group by change-request attribute. Pick the transport(s) to review from that list.
- **Pending-work mode** → `ListTransports` for the object lists, plus `GetInactiveObjects` to know which objects also carry an *unactivated* draft.
- **Lock questions** ("which request holds ZCL_DEMO_ORDER?") → `GetTransportInfo(object_url=…, dev_class=…)` — returns the object's available transports and current lock status; useful for the risk-flags section when an object sits in two open requests.

These tools require the server to run with `--enable-transports` (or `--allow-transportable-edits`); if they are absent, say so instead of improvising — see Error Handling.

## Step 2: Normalize CTS entries, then classify

`GetTransport` returns **CTS identities**, not guaranteed read-tool inputs. Real transports contain sub-object and metadata entries: `LIMU/METH`, `LIMU/REPS`, `LANG/*` translation entries, `R3TR/DEVC` package entries.

Before diffing:

1. Flatten the tasks' object lists, but keep the task id and the original CTS key for the report.
2. Treat supported `R3TR` entries (`R3TR CLAS`, `R3TR DDLS`, …) as direct repository objects and deduplicate exact repeats.
3. **Never pass a `LIMU`/`LANG` pgmid or a CTS subtype (`METH`, `REPS`, `CLSD`, `CPUB`, `PROG` include entries) as the `type` of `GetSource`/`GetRevisions`/`CompareVersions`.** Those tools take repository object types only.
4. Fold a sub-object into a parent **only when the parent is unambiguous** (e.g. `LIMU METH ZCL_DEMO_ORDER=====CM001` → parent class `ZCL_DEMO_ORDER` appears as `R3TR CLAS` in the same request). If the parent cannot be established from the entry itself, report the entry as `parent resolution unavailable` — a coverage limitation, **not** evidence that nothing changed.
5. Count both raw CTS entries and unique resolved repository objects; never present the entry count as an object count.

Then split resolved objects into:

- **Diffable** — PROG, CLAS, INTF, FUNC, INCL, DDLS, BDEF, SRVD → real diffs in Step 4.
- **Metadata-only** — DOMA, DTEL, TABL, MSAG, VIEW, SRVB, DDLX, DCLS, ENHO, SUSH, AUTH, DEVC, … → no version diff exists in VSP. Do not fake one. Where `GetSource` supports the type (VIEW, SRVB, MSAG, FUGR), read it so the report can *name* what the object is — listed as "metadata — no source diff". This is an ADT coverage boundary, not a VSP bug.

## Step 3: Establish baselines — the snapshot-sparsity doctrine

ABAP cuts a version snapshot **only when a transport is released**. For an open request, an object usually has just its current source (plus maybe an inactive draft) — there may be *no* "before" revision. Handle it honestly:

1. Per diffable object: `GetRevisions(type=…, name=…)` (add `parent` for FUNC, `include` for class includes). Each revision has a date, author, and — crucially — the tagging **transport**.
2. **Released TR**: find the revision tagged with that TR and its immediate predecessor by **date, not feed order** (confirm ordering from the timestamps). If either side cannot be established, report the gap — do not substitute today's active source for an old TR.
3. **Open TR / pending work**: diff latest released revision → `current`, labelled **"since released snapshot"** — it can include changes from *other* open requests touching the same object, and `current` is the working source (it includes any unactivated draft; `GetInactiveObjects` tells you which objects have one — flag those lines as containing unactivated work).
4. **No prior revision at all**: report `baseline unavailable`. **Never guess that the object is new.** Call it an *add* only when independent evidence proves creation in this request (e.g. `vsp changelog` shows no earlier transport ever touched it, or the revision feed's first entry is tagged with this very TR).

Every reviewed object gets a **coverage label**: `released snapshot` · `since released snapshot (may span requests)` · `pending draft included` · `baseline unavailable`.

## Step 4: Diff each object — pick the sides by intent

```
CompareVersions(type="<type>", name="<name>",
                version1_uri="<older revision uri>",
                version2_uri="<newer revision uri | current>")
```

| Intent | version1 → version2 | Coverage label |
|---|---|---|
| **Released TR** ("what did TR-EXAMPLE change") | revision immediately before the TR → revision tagged with the TR | `released snapshot` |
| **Open TR / "what am I about to ship"** | latest released revision → `current` | `since released snapshot (may span requests)`; add `pending draft included` if the object is in `GetInactiveObjects` |
| **Specific revisions** | any two URIs from `GetRevisions` | quote the two revision ids |
| **Object created in this TR** | (no diff) read `GetSource` once, report as *add* | only with the Step-3 evidence bar met |

For FUNC pass `parent`; for class test/local includes pass `include`. `GetRevisionSource(version_uri=…)` fetches a single historical version when the user wants to *read* rather than diff. `CompareSource` diffs two *different* objects (e.g. a Z-copy against its original) — not versions of one object.

**Hard scope cap:** if the resolved set exceeds ~40 diffable objects, do not start diffing. Show the normalized object table (type, name, task, coverage label) and say: *"This request has N diffable objects — a full diff of all of them would be unreviewable. Which objects or which task should I expand?"* A review nobody reads is worse than no review.

## Step 5 (opt-in): impact, boundaries, quality — bounded to the changed set

- **Impact** — a changed DDLS/BDEF/SRVD can break consumers: `GetCDSImpactAnalysis` per changed CDS artifact; `GetCallersOf` / `FindReferences` for changed classes/FMs the user flags as risky. **Impact gate:** when the blast radius comes back high (many consumers, cross-package), read the key callers' source before endorsing any follow-up write or release — never wave a high-impact change through on counts alone.
- **Boundaries** — CLI `vsp tr-boundaries <transport> [--format json]` reports cross-package boundary crossings introduced by the request (`vsp cr-boundaries <cr-id>` for a whole change request).
- **Quality** — on `+atc`: `RunATCCheck` only on the changed objects, never the whole package. `AnalyzeABAPCode` is the cheaper offline pass. 0 findings on objects you did not check = **not checked, not clean** — say which objects were checked.
- **Pre-release validity** — `SyntaxCheck` (read-only) on changed objects. `Activate`/`ActivateMultiple` mutate system state: only on explicit user request, never as part of the review.

## Step 6: Write the report

````markdown
# Change review — <transport id | "pending work"> on <system>

_<owner> · <status> · <description>_
_CTS entries: <n raw> → <m> unique repository objects · Coverage: see per-object labels_
_Checked with ATC: <list | none — findings above are diff-based only>_

## Summary

| Object | Type | Task | Change | +/− | Coverage | Flags |
|---|---|---|---|---|---|---|
| ZCL_DEMO_ORDER | CLAS | <task> | changed | +12 −3 | released snapshot | |
| ZDEMO_I_ORDER | DDLS | <task> | changed | +4 −0 | since released snapshot | impacts 3 consumers |
| ZDEMO_NEW_RPT | PROG | <task> | unknown | — | baseline unavailable | |
| ZDEMO_STATUS | DOMA | <task> | changed | — | — | metadata — no source diff |
| LIMU METH …CM001 | — | <task> | unresolved | — | — | parent resolution unavailable |

## Diffs

### ZCL_DEMO_ORDER (CLAS) — v<prev> → v<TR> (+12 −3)
```diff
<unified diff from CompareVersions>
```
…one block per diffable object…

## Risk flags
- ⚠ ZDEMO_I_ORDER (DDLS) has 3 downstream consumers — activation order matters.
- ⚠ Request has no target system — local request, cannot be imported onward.
- ⚠ 2 objects carry unactivated drafts (GetInactiveObjects) — the diff includes unshipped work.

## Verdict
<2–3 lines: what this change set does, what to review first, what is risky or not yet activated.>
````

Return inline by default; write to disk only if the user gave a path.

## Error Handling

| Error | Cause | Fix |
|---|---|---|
| `ListTransports`/`GetTransport` tool not available | Server started without `--enable-transports` / `--allow-transportable-edits`, or `--feature-transport off` | Tell the user which flag to add; do not reconstruct transport contents another way |
| `GetTransport` 404 / empty | Wrong id, deleted, or already released and purged from the modifiable view | Re-run `ListTransports`, or `vsp changelog` on the package to find the released request |
| `GetRevisions` unsupported for type X | Type outside PROG/CLAS/INTF/FUNC/INCL/DDLS/BDEF/SRVD | Expected — classify as "metadata — no source diff", don't retry |
| `GetRevisions` returns 1 revision | Snapshots only cut on release (sparsity) | Coverage = `baseline unavailable` unless the single revision is tagged with the TR under review (then it's a candidate *add* — apply the Step-3 evidence bar) |
| `CompareVersions` → "Sources are identical" | Object transported without source change (e.g. re-activation entry) | Report as "in request, no source delta" — a finding, not an error |
| FUNC diff fails | Missing `parent` function group | Take the FUGR from the CTS entry or `SearchObject` |
| CTS entry is `LIMU/*`, `LANG/*`, `R3TR DEVC` | CTS identity, not a read-tool type | Fold into an unambiguous parent or report `parent resolution unavailable`; never guess a parent name |
| >40 diffable objects | Review too large to read | Summary table only + refusal text from Step 4; ask which to expand |

## Follow-up Options

- "Looks good — release it" → **deploy** (`ReleaseTransport` is irreversible; on any high-impact flag from Step 5, read the key callers first and re-confirm with the user).
- "Is the changed code any good?" → **clean-abap-review** on the changed objects, or **atc-remediation** for the ATC findings.
- "Who breaks if this CDS changes?" → **abap-architect** (full impact/dependency workflow).
- "Show me all open transports on the system instead" → **transport-overview** (the breadth twin — switch, don't combine).
