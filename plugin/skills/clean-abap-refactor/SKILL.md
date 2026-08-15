---
name: clean-abap-refactor
description: "Refactor existing ABAP to Clean ABAP style without changing behavior — six deterministic passes (naming, declarations, expressions, method shape, error handling, class shape) with syntax-check dry-runs, per-object confirmation, read-back after write, and unit-test proof via VSP MCP tools. Use when the user says things like 'refactor this class', 'clean up this code', 'modernize this ABAP', 'apply Clean ABAP', 'tidy this method'. Triggers: refactor, clean up, modernize, Clean ABAP, tidy, style, rewrite. Scope: behavior-preserving style rewrites that write back — findings-only analysis without edits is clean-abap-review; new features and bug fixes are abap-developer; ATC-finding remediation is atc-remediation."
---

> **System profile:** before generating release-specific syntax or proposing transport/feature workflows, read `.claude/vsp-system-profile.md` if it exists; if it's missing and the task is release- or feature-sensitive, run the **bootstrap-system-context** skill first.

You refactor ABAP source code to comply with the Clean ABAP rule set in
[`../clean-abap-review/references/rules.md`](../clean-abap-review/references/rules.md).
**Style and structure only — business logic stays identical.** If a change would
alter what the code does, you do not make it; you flag it instead. This matters
doubly here: most rules in the expression pass need ABAP ≥ 7.40 syntax, so the
system profile is not optional decoration.

## Tool Routing

| Step | Primary vsp tool | Fallback |
|------|------------------|----------|
| Read the source | **GetSource** (method-level for classes — `method` / `include`) | ask the user to paste the code |
| Enumerate a package target | **GetPackage** / **SearchObject** | ask the user which objects to include |
| Find callers before touching a signature | **FindReferences** | **GetCallersOf**; if neither runs, **skip the change** |
| Check for subclasses before adding FINAL | **GetTypeHierarchy** | **FindReferences** on the class; if unknown, skip |
| Offline lint of proposed source | **AnalyzeABAPCode** | skip (advisory only) |
| Dry-run refactored source before any write | **SyntaxCheck** (pass new source as `content`) | none — a write without a dry-run is not allowed |
| Write back | **EditSource** (surgical, keeps `syntax_check=true`) / **WriteSource** (full replace) | hand the diff to the user to paste in ADT |
| Read back after write | **GetSource** | **CompareSource** |
| Activate | **Activate** | **ActivateMultiple** for mutually dependent objects |
| Prove behavior preserved | **RunUnitTests** | none — report "no test evidence" honestly |
| Final quality gate (optional) | **RunATCCheck** (variant from **GetATCCustomizing**) | **AnalyzeABAPCode** offline |
| Release/API state of a used object | **GetAPIReleaseState** | **RunQuery** on release-state views; else mark *unverified* |

## Inputs

Accept any of the following. If nothing is provided, ask which target the user wants.

- An ABAP object name — read it with **GetSource** (for classes, per include or method; never pull a whole large class when three methods change)
- A package name — enumerate with **GetPackage**, then refactor **object by object**, never as a single bulk change
- A code block pasted into the conversation — always works; the result is a diff for the user when no system is connected

## Procedure

1. **Load the rule set.** Read every `## RULE:` block in
   `../clean-abap-review/references/rules.md`, including its "How to apply this
   rule set" section. Those blocks are the only refactoring targets.
2. **Get the source.** Pasted code is used as-is; object names are read with **GetSource**.
3. **Build the plan.** Analyze the source against the rule set (the same
   analysis clean-abap-review performs) and produce a prioritized occurrence
   list — that list is the refactor plan. Sort tool-verifiable (ATC-tagged)
   rules first.
4. **Refactor in six passes, in this exact order.** Do not skip ahead; later
   passes assume the earlier ones are done.
   1. **Naming pass** — `use-problem-domain-names`, `no-magic-numbers-or-literals`
   2. **Declaration pass** — `prefer-inline-declarations`, `no-chained-declarations`, `no-default-key-on-internal-tables`
   3. **Expression pass** — `use-string-templates-not-concatenate`, `prefer-is-not-to-not-is`, `prefer-case-to-elseif`, `keep-nesting-shallow`, `use-table-expressions-not-read-table`, `functional-call-style-not-call-method`, `avoid-obsolete-statements`
   4. **Method shape pass** — `methods-do-one-thing-and-stay-small`, `at-most-three-importing-parameters`, `prefer-returning-to-exporting`, `split-methods-instead-of-boolean-input`
   5. **Error handling pass** — `class-based-exceptions-not-sy-subrc`, `catch-specific-exceptions-not-cx-root`, `no-empty-catch-blocks`
   6. **Class shape pass** — `final-classes-and-private-members-by-default`, `prefer-new-to-create-object`

   `comments-explain-why-not-what` may be applied opportunistically in any pass
   that already touches a section — delete redundant *what*-comments, never a
   *why*-comment. Rules **not** listed above (testing rules, clean-core/cloud
   rules, `depend-on-interfaces-inject-dependencies`) are design changes:
   **flag them in the report, do not apply them.**
5. **Preserve behavior.** After every pass, mentally diff the program logic
   against the original. If you cannot *prove* a change is behavior-preserving,
   **stop and ask** instead of guessing.
6. **Dry-run before every write.** Run **SyntaxCheck** with the full refactored
   source as `content` — it validates without writing, so a broken refactor
   never reaches the system. Optionally run **AnalyzeABAPCode** first for
   offline feedback. Only after the dry-run passes may a write be proposed.
7. **Show the diff and confirm per object.** Produce a per-method before/after
   diff, each change annotated with the rule name that motivated it. Ask for
   explicit confirmation **for this object** — a yes for a previous object is
   never a blanket yes.
8. **Write, then read back.** Write via **EditSource** (preferred, surgical) or
   **WriteSource** (full replace). Then **GetSource** the object again and
   confirm the system content matches the diff you showed. The tool output is
   the source of truth, not your memory of what you wrote.
9. **Activate.** Run **Activate** (or **ActivateMultiple** for mutually
   dependent objects). If activation fails: report the error, restore the
   original source, and stop. Never patch through activation errors.
10. **Run the tests.** **RunUnitTests** for the object after activation. A
    behavior-preserving refactor with no test evidence is a claim, not a proof
    — if the object has no tests, say so in the report.

## Behavior preservation — non-negotiable

These changes are out of scope for this skill — they alter behavior:

- **Public exception contracts.** Never add, remove, or replace exception
  classes raised or declared by a public method — callers catch them.
- **SELECT ordering and result sets.** Never change the order or set of rows a
  SELECT returns — no added `ORDER BY`, no rewritten `WHERE`, no aggregate
  "simplifications".
- **AUTHORITY-CHECKs.** Never remove or relocate one, even if it looks redundant.
- **COMMIT/ROLLBACK boundaries.** Never inline or extract code that crosses a
  `COMMIT WORK` or `ROLLBACK WORK` — LUW boundaries are behavior.
- **Signatures.** Never change a public method signature, with one exception:
  `EXPORTING` → `RETURNING` for a single output — and only after
  **FindReferences** has shown every caller and none of them uses
  `IS SUPPLIED` on that parameter. If FindReferences cannot run, skip the change.
- **FINAL.** Add `FINAL` only after **GetTypeHierarchy** (or FindReferences)
  confirms no subclass exists.

If a Clean ABAP rule appears to require one of the above, **flag it in the
report and skip the change**. Behavior change belongs in a separate task, not
in a style refactor.

## Hard rules — evidence, not memory

- **Never assert an object's release or API state from memory.** Whether an
  API is released is system-specific: look it up with **GetAPIReleaseState**
  (or **RunQuery** on release-state views), or mark the claim *unverified*.
- **Never invent ATC findings.** Only actual **RunATCCheck** output produces
  ATC findings; verify a check exists in the active variant with
  **GetATCCustomizing** before claiming it does.
- **Sort tool-verifiable findings first.** When ATC was run, label each
  ATC-taggable finding *confirmed by ATC* or *not raised by ATC* — the second
  label usually means the check is missing from the variant, not that the
  finding is wrong.
- **One object at a time.** No silent batching across a package; each object
  gets its own plan, diff, confirmation, write, read-back, activation, and test run.
- **No rule-by-rule chatter.** Group changes by method; one diff per change.

## Output format

```
# Clean ABAP Refactor — ZCL_DEMO_ORDER_READER

## Plan
1. <rule-name> — <count> occurrences   [ATC-taggable]
2. <rule-name> — <count> occurrences
...

## Changes

### <method name>
**Rule:** <rule-name>
**Before:** ```abap …``` **After:** ```abap …```
**Rationale:** <one sentence>

## Behavior-preservation check
- [ ] No public signature changed (except verified EXPORTING→RETURNING)
- [ ] No exception classes added/removed on public methods
- [ ] No SELECT reordered or filtered differently
- [ ] No AUTHORITY-CHECK removed or moved
- [ ] Nothing moved across COMMIT/ROLLBACK
- [ ] FINAL only added with verified empty subclass list

## Skipped
<rule-name> at <location> — <one-line reason it would have changed behavior>

## Verification
- SyntaxCheck dry-run: pass / fail
- Read-back after write (GetSource): matches diff / MISMATCH
- Activation: clean / failed (reverted)
- RunUnitTests: N passed, N failed / object has no tests / not run

## Confirmation
Write these changes back to <OBJECT NAME>? (yes / no / per-method)
```

---

Adapted from [matt1as/claude-abap-skills](https://github.com/matt1as/claude-abap-skills) (Apache-2.0).
