---
name: clean-core-check
description: "Audit ABAP objects, packages, or transports for Clean Core / ABAP Cloud compliance — unreleased API use, direct SELECT on SAP-owned tables, dynpro, SUBMIT/CALL TRANSACTION, FORM/PERFORM, SAP-standard modification — with release state verified on the live system and every object graded Clean-Core Level A–D. Use when the user says 'is this clean core compliant', 'check cloud readiness', 'can this run on BTP', 'audit for ABAP Cloud', 'which APIs here are unreleased'. Triggers: clean core, ABAP Cloud, cloud-ready, cloudification, BTP, released API, tier 1, side-by-side extension. Scope: read-only audit — fixing the findings is atc-remediation; general code style is clean-abap-review."
---

> **System profile:** before generating release-specific syntax or proposing transport/feature workflows, read `.claude/vsp-system-profile.md` if it exists; if it's missing and the task is release- or feature-sensitive, run the **bootstrap-system-context** skill first.

You audit ABAP objects for compliance with the **clean core** principles of the ABAP Cloud development model. You pull source and release states from the connected system, never from memory, and produce a graded compliance report. **Read-only — no writes back, ever.**

The rules you check against live in the shared rules file: [`../clean-abap-review/references/rules.md`](../clean-abap-review/references/rules.md), section **"ABAP Cloud / clean core"** (`released-apis-only`, `no-direct-select-on-sap-owned-tables`, `abap-cloud-language-scope-only`, `no-modification-of-sap-standard`, `interface-entity-required-annotations`). Cite rules by name; do not restate or extend them here.

For Cloudification Repository lookups — exact JSON content URLs per target release, the Level A–D ↔ API-state mapping, ATC check names, prerequisite SAP Notes, and the two-independent-axes rule (extensibility vs integration) — use [`references/cloudification-data.md`](references/cloudification-data.md) as the authority; never quote those values from memory.

## Tool routing

| Step | Primary vsp tool | Fallback |
|---|---|---|
| 0. Establish target model (BTP vs on-prem cloud scope) | `.claude/vsp-system-profile.md` — run **bootstrap-system-context** first if missing | `GetSystemInfo` + `GetFeatures`, then ask the user |
| 1. Resolve scope to an object list | `GetPackage` (package) · `GetTransport` (transport contents) | `SearchObject`; ask the user for the list |
| 2. Read each object's source | `GetSource` (method-level for classes) | ask the user to paste from ADT |
| 3. Pattern-scan for forbidden statements | `GrepPackage` (package scope) · `GrepObject` (single object) | scan the `GetSource` output by hand |
| 4. Verify release state of every consumed SAP API | `GetAPIReleaseState` (object URI from `SearchObject`) | `RunQuery` on release-state views where the system exposes them; otherwise mark the finding *unverified* |
| 5. Corroborate with ATC | `RunATCCheck` with a cloud-readiness variant (e.g. `ABAP_CLOUD_DEVELOPMENT_DEFAULT`) — confirm the variant exists via `GetATCCustomizing` first | `AnalyzeABAPCode` (offline abaplint — flags obsolete statements, cannot judge release state) |
| 6. Trace what a custom CDS view sits on | `GetCDSDependencies` | read the DDLS source and follow `select from` by hand |
| 7. Prioritisation context (who uses the offender) | `FindReferences` · `GetCallersOf` | `GrepPackages` |

`GetAPIReleaseState` replaces guesswork: **never assert that an SAP object is or is not released from memory** — check it, or mark the finding *unverified* and say so.

## Inputs

If not provided, ask. Default to the narrowest scope.

- An ABAP object name (class, program, function group, include, CDS entity, BDEF)
- A package name
- A transport request — audit every object in the transport (`GetTransport`, or `ListTransports` to find it)

**Target model is mandatory.** Whether a finding is a hard violation or a soft warning depends on whether the code targets BTP ABAP Environment or on-prem/private-cloud ABAP with cloud language scope. Take it from the system profile; if the profile is missing and the user cannot answer, run bootstrap-system-context before grading anything.

## What to check

Each check maps to a rule in the shared rules file — cite the rule name in every finding.

### Hard violations — never allowed in either BTP or on-prem cloud scope

| Check | Rule | Detection |
|---|---|---|
| Use of an SAP class/interface/FM with no released (C1) contract | `released-apis-only` | source read + `GetAPIReleaseState` per consumed SAP object |
| Direct `SELECT` from an SAP-owned database table | `no-direct-select-on-sap-owned-tables` | `GrepPackage` pattern `SELECT` (case-insensitive), then judge each `FROM` target: not in the customer namespace → verify with `GetAPIReleaseState` |
| Classic dynpro (`CALL SCREEN`, `MODULE ... INPUT/OUTPUT`, screen exits) | `abap-cloud-language-scope-only` | `GrepPackage` `CALL SCREEN\|MODULE\s+\w+\s+(INPUT\|OUTPUT)` |
| `SUBMIT`, `CALL TRANSACTION`, `LEAVE TO TRANSACTION` | `abap-cloud-language-scope-only` | `GrepPackage` `^\s*(SUBMIT\|CALL TRANSACTION\|LEAVE TO TRANSACTION)` |
| `FORM` / `PERFORM` | `abap-cloud-language-scope-only` | `GrepPackage` `^\s*(FORM\|PERFORM)\b` |
| `CALL FUNCTION ... DESTINATION 'NONE'` for internal logic | `abap-cloud-language-scope-only` | `GrepPackage` `DESTINATION 'NONE'` |
| Modification of SAP standard objects, or cloning SAP source into the customer namespace | `no-modification-of-sap-standard` | scope listing (SAP-namespace objects in a customer transport), source read |
| Use of an SAP enhancement point that is not released | `no-modification-of-sap-standard` | source read + `GetAPIReleaseState` on the enhancement spot |
| Legacy `define view` instead of `define view entity` | `interface-entity-required-annotations` | `GrepPackage` `@AbapCatalog\.sqlViewName` — present only in legacy DDL views (Go regexp has no lookahead, so grep the marker annotation, not the `define view` phrase) |

### Soft warnings — tolerated on-prem for transitional reasons, hard on BTP

| Check | Note |
|---|---|
| SAP API released only with a system-internal use contract | Allowed on-prem during migration, blocked on BTP — the contract level comes from `GetAPIReleaseState`, not from memory |
| Released-API use that is deprecated with a named successor | Will become a hard violation; record the successor |
| Custom CDS view selecting from another unreleased *custom* view | Works today, but the chain breaks when the base view changes — trace with `GetCDSDependencies` |

When the target model is BTP, skip the soft pass: every soft finding is reported as hard.

## Procedure

1. **Establish the target model** (step 0 above). State it at the top of the report.
2. **Resolve the scope** to a concrete object list and read every object. Do not paraphrase or imagine source code — if you cannot read an object, exclude it and say so.
3. **Run the grep passes** from the hard-violation table across the scope, then **walk each object's source** for the checks grep cannot catch (unreleased API use, SAP-standard modification, enhancement points).
4. **Verify release state** for every SAP object the code consumes: `SearchObject` → URI → `GetAPIReleaseState`. Record the returned state verbatim. A release-state claim with no tool evidence is marked *unverified* — no exceptions.
5. **Run ATC** with a cloud-readiness variant (confirm availability via `GetATCCustomizing`). Match ATC findings to your own: label each tool-verifiable finding *confirmed by ATC* or *not raised by ATC*. "Not raised" usually means the check is missing from the active variant — say that rather than dropping the finding.
6. **Walk the soft checks** (on-prem targets only).
7. **Grade** every finding and object per the level scheme below, and emit the report.
8. **Do not write changes.** Point the user at **atc-remediation** for fixes.

## Clean-Core Levels

Grade each **finding** B–D by remediation class; an object's **grade** is its worst finding, and objects with no findings are Level A.

| Level | Meaning | Criteria |
|---|---|---|
| **A — cloud-ready** | Compliant as-is | No hard violations, no soft warnings |
| **B — mechanical migration** | Bounded, local fixes | Every finding has a released successor verified on this system (table → released CDS view, unreleased FM → released class, `define view` → `define view entity`) |
| **C — API gap** | Blocked on SAP | At least one finding consumes SAP functionality with no released successor found on this system — requires redesign, waiting for SAP, or (on-prem only) staying in classic scope. Claim a gap only after `GetAPIReleaseState` came back unreleased *and* you searched for a successor; otherwise the finding is *unverified*, not Level C |
| **D — architectural conflict** | Re-architecture required | SAP-standard modification, dynpro/GUI process flow, SUBMIT-driven job chains — the construct itself cannot be migrated line-by-line |

## Output format

```
# Clean Core Compliance Report — <SCOPE>

Target model: BTP | on-prem cloud scope        (source: system profile | user)
Objects audited: <N>   Hard: <N>   Soft: <N>   Unverified: <N>
ATC variant used: <name> | none available (per GetATCCustomizing)

## Findings by level
(D first, then C, then B. Within a level, tool-verifiable findings first.)

### Level D — architectural conflicts
#### D-001 — <one-line finding> — *confirmed by ATC | not raised by ATC | unverified*
- **Object / where:** <object>, <include/method>, line <N>
- **Rule:** <rule name from rules.md>
- **Evidence:** <offending statement; for API findings, the verbatim GetAPIReleaseState result>
- **Direction:** <successor API or redesign direction — the fix itself belongs to atc-remediation>

### Level C — API gaps
(same structure; Evidence must include the unreleased state AND the successor search that came up empty)

### Level B — mechanical migrations
(same structure; Direction names the released successor. For table→CDS-view moves, list EVERY
renamed field used in the surrounding code — half a migration is worse than none.)

## Soft warnings (on-prem targets only)
(same structure)

## Summary
| Object | Level | Hard | Soft | Unverified |
|---|---|---|---|---|
| <name> | A–D | N | N | N |

## Next actions
1. Level D findings first — they gate the architecture, not just activation.
2. Tool-verifiable B findings: run atc-remediation to fix in batches.
3. Semantic migrations (table → released view with field renames): fix by hand, then re-run this audit.
```

## Hard rules

- **Read-only.** Never call a write, activate, or delete tool from this skill.
- **Never assert an object's release or API state from memory.** Look it up (`GetAPIReleaseState`, `RunQuery`, or an actual ATC finding) or mark the claim *unverified*.
- **Never invent ATC findings.** Only `RunATCCheck` output counts as ATC evidence, and only for the variant you actually ran — verify variant availability with `GetATCCustomizing` before claiming a check exists.
- **Sort tool-verifiable findings first** and label each *confirmed by ATC* or *not raised by ATC*.
- **Always state the target model** — hard vs soft depends on it, and grading without it is guessing.
- **Cite the rule name** from the shared rules file in every finding. No finding without a rule — this is a compliance check, not a free-form review.
- **Point at the direction, don't apply it.** Remediation belongs to **atc-remediation**; pseudo-comment suppression (`"#EC`) is never a remediation here.
