---
name: atc-remediation
description: "Triage and fix ATC (ABAP Test Cockpit) findings methodically: group into six categories, remediate in priority order under an auto-apply / ask-first / manual-only severity ladder, re-run ATC after every batch and report resolved/remaining/new deltas. Use when the user says things like 'fix the ATC findings', 'remediate ATC', 'clean up the ATC results for this package', 'get this transport ATC-clean', 'triage the ATC worklist'. Triggers: ATC, Test Cockpit, findings, remediate, priority 1, check variant, quickfix, pseudo-comment, suppression, exemption. Scope: fixing findings an actual RunATCCheck run produced — running checks without fixing is test; a rules-based review without an ATC run is clean-abap-review; general feature development is abap-developer."
---

> **System profile:** before generating release-specific syntax or proposing transport/feature workflows, read `.claude/vsp-system-profile.md` if it exists; if it's missing and the task is release- or feature-sensitive, run the **bootstrap-system-context** skill first.

You take real ATC results from the connected SAP system, group them by check category, and fix them in priority order — one surgical, verified change at a time. The rule set you cite lives in [`../clean-abap-review/references/rules.md`](../clean-abap-review/references/rules.md); cite rules by their `RULE:` name. Pseudo-comment suppressions are off-limits unless the user explicitly justifies and approves them.

## Tool routing

| Step | Primary vsp tool | Fallback |
|------|------------------|----------|
| Discover default variant + exemption reasons | GetATCCustomizing | ask the user for the variant name |
| Run the ATC baseline | RunATCCheck (`object_url`, `variant`, `max_results`) | AnalyzeABAPCode (offline abaplint — label its findings *not raised by ATC*); last resort: ask the user to paste ADT results |
| Resolve a package scope to objects | GetPackage / SearchObject | GrepPackage for pattern-scoped subsets |
| Resolve a transport scope to objects | GetTransportInfo / ListTransports | ask the user for the object list |
| Read the offending source | GetSource (method-level for classes) | GrepObject to locate the finding's exact lines |
| Verify release/API state of a replacement | GetAPIReleaseState | RunQuery on release-state views; otherwise mark the claim *unverified* |
| Check callers before a risky fix | GetCallersOf / FindReferences | ListDependencies |
| Apply a fix | EditSource (surgical string replacement) | WriteSource (full replace — only when the diff is too broad for a unique string match); PrettyPrint the source first when formatting itself is the finding |
| Validate | SyntaxCheck | — |
| Activate | Activate / ActivateMultiple (mutually dependent objects) | ActivatePackage after a bulk batch |
| Regression gate | RunUnitTests | — |
| Re-run per batch | RunATCCheck (same scope, same variant) | — |
| Back out a change that introduced new findings | GetRevisionSource + CompareSource, then EditSource to restore | — |

RunATCCheck takes **one object URL per call**. For a package or transport scope, resolve the object list first and run object-by-object, aggregating counts. **Never invent ATC findings** — if no ATC result is available from a tool or a user paste, say so and stop; do not fill the gap from memory.

## Inputs to collect

If not provided, ask which of these to target. Default to the narrowest scope.

- A single ABAP object (e.g. `ZCL_DEMO_ORDER_PROCESSOR`)
- A package (e.g. `$ZDEMO` — list its contents first so the user can confirm scope)
- A transport request (e.g. `TR-EXAMPLE` — resolve to its object list via GetTransportInfo)

Also confirm the check variant. Call GetATCCustomizing for the system default; for cloud-readiness work the usual choice is `ABAP_CLOUD_DEVELOPMENT_DEFAULT` — but confirm it exists on this system rather than assuming.

## Procedure

1. **Baseline.** Run RunATCCheck per object in scope with the agreed variant. Record total findings and the priority split (1=Error, 2=Warning, 3=Info). This baseline is what every later delta is measured against — same scope, same variant, always.
2. **Group findings into six categories**, and remediate in exactly this order:
   1. **Clean Core / Cloud compatibility** — unreleased API usage, direct SELECT on SAP-owned tables, language-version violations (rules: `released-apis-only`, `no-direct-select-on-sap-owned-tables`, `abap-cloud-language-scope-only`, `no-modification-of-sap-standard`)
   2. **Security & Authorization** — missing authorization checks, unsafe/dynamic SQL, hard-coded user IDs
   3. **Performance** — `SELECT *`, SELECT inside loops, nested loops over internal tables, missing keys
   4. **Code quality / Clean ABAP** — magic numbers, method length, parameter count, `CREATE OBJECT`, chained declarations, obsolete statements
   5. **CDS / RAP modelling** — missing mandatory annotations, incorrect composition/association, missing draft setup (rule: `interface-entity-required-annotations`)
   6. **Testing** — missing ABAP Unit coverage, test classes that hit live data (rule: `no-database-access-in-unit-tests`)
3. **For each finding:** state the violation in one sentence; cite the ATC check title and priority; name the applicable `RULE:` from the rules file; show a Before/After snippet; assign a rung on the severity ladder. If you notice an adjacent problem ATC did not flag, you may report it — labeled ***not raised by ATC*** — but ATC-confirmed findings always sort first.
4. **Severity ladder** — every fix gets exactly one rung:
   - **Auto-apply** — mechanical rewrites that cannot change behavior: `CREATE OBJECT` → `NEW` (`prefer-new-to-create-object`), `READ TABLE` + `sy-subrc` → `line_exists( )`/table expression (`use-table-expressions-not-read-table`), splitting chained `DATA:` declarations (`no-chained-declarations`), `CONCATENATE` → string template (`use-string-templates-not-concatenate`), adding a missing `@EndUserText.label`. Still batched and confirmed once per batch — never silently written.
   - **Ask-before-applying** — touches code shape or has a caller-visible edge: `EXPORTING` → `RETURNING` (`prefer-returning-to-exporting` — run FindReferences first; refuse if any caller uses `IS SUPPLIED`), magic literal → named constant (the *name* needs domain input), extracting a long method, narrowing a `CATCH cx_root` to specific exceptions, adding `@AccessControl.authorizationCheck: #CHECK` where an access control may not yet exist.
   - **Manual-only** — changes semantics, interfaces, or data-access paths: replacing a direct `SELECT FROM` an SAP table with a released CDS view (column names differ — e.g. `vbak-vbeln` vs `I_SalesOrder-SalesOrder`), replacing an unreleased function module, adding a missing `AUTHORITY-CHECK`, anything that moves code across a `COMMIT WORK`/`ROLLBACK` boundary. Present the full fix with context; the user applies or explicitly delegates it.
5. **Fix loop per change:** GetSource → EditSource → SyntaxCheck (must be clean) → Activate → RunUnitTests (must pass). If SyntaxCheck fails or a test breaks, fix it yourself before touching the next finding — do not hand a broken state back to the user. One object's fixes = one reviewable unit; never write across many objects in a single irreversible batch.
6. **Impact gate.** vsp may attach an impact block to write results. If EditSource/WriteSource returns `impact.risk: high`, stop before the next write: read the key callers (GetCallersOf, FindReferences) and confirm the change is safe for them — downgrade the fix to ask-before-applying if any caller looks affected.
7. **Never suppress with pseudo-comments unsolicited.** `"#EC CI_USAGE_OK`, `"#EC NOTEXT`, and friends are not fixes. Refuse to insert them by default. If the user requests a suppression, require: (a) the exact ATC check name, and (b) a one-sentence written justification. Emit the justification as a comment directly above the pseudo-comment, and list it in the report. Where the finding is legitimate but unfixable now, prefer proposing a proper ATC exemption (GetATCCustomizing lists the system's exemption reasons) over a source-level suppression.
8. **After each batch, re-run ATC on the same scope and variant** and report the delta: findings **resolved**, findings **remaining**, findings **newly introduced**. Any newly introduced finding means backing out the offending change (GetRevisionSource/CompareSource to identify it, EditSource to restore) before proceeding.

## Anti-hallucination rules (non-negotiable)

- **Never invent ATC findings.** Only a RunATCCheck result (or output the user pasted) is an ATC finding. Everything else is labeled *not raised by ATC*.
- **Never assert an object's release or API state from memory.** Whether a SAP class, function module, or CDS entity is released is system-specific — look it up (GetAPIReleaseState, or RunQuery where available) or mark the claim *unverified*.
- **Sort tool-verifiable findings first**, each labeled *confirmed by ATC* or *not raised by ATC*. The second label usually means the check is missing from the active variant, not that the finding is wrong — say which you believe and why.

## Output format

```
# ATC Remediation — <SCOPE>

Variant: <check variant> (source: GetATCCustomizing | user)
Baseline: <N> findings (P1: n / P2: n / P3: n) across <M> objects

## Category breakdown
| Category                   | Total | Auto-apply | Ask | Manual |
|----------------------------|-------|------------|-----|--------|
| Clean Core / Cloud         | N     | N          | N   | N      |
| Security & Authorization   | N     | N          | N   | N      |
| Performance                | N     | N          | N   | N      |
| Code quality / Clean ABAP  | N     | N          | N   | N      |
| CDS / RAP modelling        | N     | N          | N   | N      |
| Testing                    | N     | N          | N   | N      |

## Findings (grouped by category, priority order)
### F-001 — <one-line violation>            [confirmed by ATC]
- Object: ZCL_DEMO_ORDER_PROCESSOR, line <N>
- ATC check: <check title> — priority <1|2|3>
- Rule: <RULE: name from rules.md>
- Before / After: <snippets>
- Disposition: auto-apply | ask | manual

## Batch results
| Batch | Fixed | ATC re-run: resolved | remaining | newly introduced |
|-------|-------|----------------------|-----------|------------------|

## Suppressions (empty unless the user requested any)
| Check name | Line | User justification |
```
