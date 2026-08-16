---
name: clean-abap-review
description: "Review ABAP code against the Clean ABAP rule set (references/rules.md) and produce a severity-graded report with ATC-verified findings first. Use when the user says things like 'review this class', 'audit ZCL_DEMO_X for clean code', 'check this package for Clean ABAP violations', 'is this code clean', 'code review ZDEMO_REPORT'. Triggers: review, clean ABAP, code review, audit, violations, code smells, quality report. Scope: read-only — never writes; applying the fixes belongs to clean-abap-refactor, and working down ATC result lists belongs to atc-remediation; dedicated cloud-readiness audits are clean-core-check; merely running checks and reading their results is test."
---

> **System profile:** before generating release-specific syntax or proposing transport/feature workflows, read `.claude/vsp-system-profile.md` if it exists; if it's missing and the task is release- or feature-sensitive, run the **bootstrap-system-context** skill first.

You are reviewing ABAP source code for compliance with the Clean ABAP rule set in `references/rules.md` (relative to this skill's directory). Your job is to produce a **structured, prioritised, evidence-backed review** — not to change the code. This skill is strictly read-only.

## Tool routing

| Step | Primary vsp tool | Fallback |
|------|------------------|----------|
| Read object source | **GetSource** (for classes: `GetObjectStructure` first, then method-level reads) | ask the user to paste the source |
| Enumerate a package | **GetPackage** | **SearchObject** with a package-scoped pattern |
| Run ATC | **RunATCCheck** | **AnalyzeABAPCode** (offline abaplint — weaker, but never invents ATC results) |
| Discover active ATC variant / available checks | **GetATCCustomizing** | state that the variant is unknown; do not guess |
| Local static analysis (no SAP round-trip) | **AnalyzeABAPCode** | none |
| Release / API state of a SAP object | **GetAPIReleaseState** | **RunQuery** on release-state views where available; otherwise mark *unverified* |
| Blast radius of a Critical finding | **FindReferences**, **GetCallersOf** | **ListDependencies** |
| Pattern search across a package | **GrepPackage** | **GrepObject** per object |
| System context (release, features) | **GetSystemInfo**, **GetFeatures** | ask the user |

If no vsp server is connected, the skill still works on **pasted** source — but then no finding may be labelled as ATC-verified. Never invent source, and never invent tool output.

## Inputs

Accept any of the following. If none are provided, ask the user which they want.

- **An object name** (class, program, interface, function module, CDS view) — read it with GetSource.
- **A package name** — enumerate with GetPackage, then review object by object.
- **A code block pasted into the conversation** — always works, including with no system connected.

## Procedure

1. **Load the rule set.** Read every `## RULE:` block in `references/rules.md`, including its "How to apply this rule set" preamble. These are the only rules in scope for this review.
2. **Establish system context.** Check `.claude/vsp-system-profile.md` (or GetSystemInfo / GetFeatures) to decide whether the "ABAP Cloud / clean core" rule section applies. On classic on-prem development it produces observations, not findings, unless the user asked for cloud readiness.
3. **Get the source.** If pasted inline, use it. If an object name was given, read it with GetSource — for a class, GetObjectStructure first for the shape, then read the includes/methods that matter rather than pulling the whole class.
4. **Run the tools first, before the manual scan.**
   - **RunATCCheck** on the object. Record every finding with its check name and priority.
   - **AnalyzeABAPCode** for the cheap local pass (naming, obsolete statements, common smells).
   - If a rule's `**ATC**:` tag names a check that did not appear, use **GetATCCustomizing** to see whether the check is even in the active variant before drawing conclusions.
5. **Scan the source against every rule.** Record each violation with: the rule name, the exact line or block, and a one-sentence diagnosis. Do not stop at what the tools flagged — the tools bound the *verified* set, not the *complete* set.
6. **Reconcile.** For every finding whose rule carries an `**ATC**:` tag, label it ***confirmed by ATC*** (the check fired) or ***not raised by ATC*** (it did not — worth stating explicitly, since it usually means the check is missing from the active variant, not that the finding is wrong). Findings from rules tagged `not ATC-checkable` get no label. Tool-verifiable findings sort first within each severity.
7. **Assign a severity to each finding:**
   - **Critical** — produces a runtime exception, silently corrupts data, or fails activation (e.g. `catch-specific-exceptions-not-cx-root` with processing continuing, `no-empty-catch-blocks`, a `released-apis-only` violation in cloud language scope).
   - **Major** — unambiguous, ATC-checkable Clean ABAP violation that survives activation (e.g. `no-magic-numbers-or-literals`, `methods-do-one-thing-and-stay-small`, `prefer-returning-to-exporting`, `use-table-expressions-not-read-table`).
   - **Minor** — style preference with no hard ATC check (e.g. `prefer-inline-declarations`, `comments-explain-why-not-what`, naming improvements).
8. **Suggest a concrete fix for every finding.** Show a short Do/Avoid snippet citing the rule by name — never a vague instruction. The fix must obey every other rule in the set; do not fix one violation by introducing another.
9. **Do not modify the source.** If the user wants the fixes applied, point them at **clean-abap-refactor** (rule-by-rule rewrite) or **atc-remediation** (working down an ATC result list).

## Output format

Use this exact structure. One report per object reviewed; for a package, add a single summary table at the end with totals per object.

````
# Clean ABAP Review — <OBJECT NAME>

ATC run: <variant name, or "not run — findings unverified">

## Critical (N)

### 1. <rule-name> — <one-line diagnosis> [confirmed by ATC | not raised by ATC]
**Location:** <include/method> line <N>
**Found:**
```abap
<offending code>
```
**Fix (per RULE: <rule-name>):**
```abap
<concrete replacement>
```

## Major (N)

(same format, ATC-labelled findings first)

## Minor (N)

(same format)

## Summary
- Critical: N
- Major:    N
- Minor:    N
- Total:    N  (of which confirmed by ATC: N)

## Out-of-scope observations
<anything real that no rule in references/rules.md covers — clearly separated, no severity>

## Recommended next step
<one sentence — usually "run clean-abap-refactor", "run atc-remediation on the confirmed findings", or "address the Criticals manually first because …">
````

## Hard rules for this skill

- **Never invent ATC findings.** Only actual RunATCCheck output counts as an ATC finding. An `**ATC**:` tag in the rule set means the rule is tool-verifiable — it does not mean ATC flagged this code.
- **Never assert an object's release or API state from memory.** Look it up (GetAPIReleaseState, RunQuery, or RunATCCheck with a cloud variant) or mark the claim *unverified*.
- **Sort tool-verifiable findings first**, and label each one *confirmed by ATC* or *not raised by ATC*.
- **Cite the rule by name.** Every finding starts from a `## RULE:` in `references/rules.md`. No findings without a rule.
- **Do not invent rules.** Anything real but uncovered goes in "Out-of-scope observations" at the very end — not in the main report.
- **Do not skip findings to be polite.** A real review names everything; the severity scale handles the noise.
- **Read-only.** Never call WriteSource, EditSource, Activate, PrettyPrint, or any other mutating tool from this skill — fixes belong to clean-abap-refactor or atc-remediation.
- **Synthetic identifiers only in examples.** Use `ZDEMO_*` / `ZCL_DEMO_*` / `$ZDEMO` in illustrative snippets; quote the system's real names only when citing the reviewed source itself.
