---
name: generate-unit-tests
description: "Generate ABAP Unit test classes for a class — dependency classification, interface-based test doubles, testclasses-include creation via CreateTestInclude/UpdateClassInclude, and a run-until-green RunUnitTests loop. Use when the user says things like 'generate unit tests for ZCL_DEMO_X', 'write tests for this class', 'add ABAP Unit tests', 'create test doubles for this method', 'I want regression tests before refactoring'. Triggers: generate tests, unit test, test class, test double, mock, FOR TESTING, cl_abap_unit_assert, coverage. Scope: authoring new test code — running existing tests and ATC is test; general object development is abap-developer; a graded review of the class itself is clean-abap-review; the pre-release gate is deploy."
---

> **System profile:** before generating release-specific syntax or proposing transport/feature workflows, read `.claude/vsp-system-profile.md` if it exists; if it's missing and the task is release- or feature-sensitive, run the **bootstrap-system-context** skill first.

> **Impact gate:** when a write result carries `impact.risk: "high"`, read 2-3 key callers and run unit tests on the affected packages before proceeding; on an `IMPACT GATE` refusal, surface the report to the user and retry with the `confirm` token only once proceeding is justified.

Generate ABAP Unit tests for an existing (or new) class: analyze its dependencies, build interface-based test doubles, write the local test class into the `testclasses` include, and run the tests until green. Assertion signatures, DURATION/RISK semantics, lifecycle invariants, and the CDS/OSQL/RAP test-environment templates live in [references/abap-unit-reference.md](references/abap-unit-reference.md) (relative to this skill's directory) — read it before writing assertion or test-environment code; do not recall those signatures from memory.

## Smart Defaults (apply silently, do NOT ask)

| Setting | Default | Rationale |
|---|---|---|
| Test class name | `ltc_<short_class_name>` | Local test class in the class's `testclasses` include |
| Test double name | `ltd_<short_dep_name>` | Local test double, defined in the same include |
| Methods to test | All public methods | User can narrow after seeing the proposal list |
| Risk level / duration | `RISK LEVEL HARMLESS` / `DURATION SHORT` | Doubles touch no real data; RunUnitTests skips DANGEROUS/LONG by default |
| Mock strategy | Interface-based constructor injection | Cleanest pattern; test environments only when DB/CDS/RAP access exists |
| Transport | Omitted for `$TMP` / local packages | Required only for transportable packages — then ask, never invent one |

## Hard scope caps

- **One class per invocation.** For a package, refuse with: *"This skill generates tests for one class at a time. Tell me which class to start with — for a package-wide coverage picture, run the test skill on the package first."*
- **Max 15 test methods per pass.** If the analysis proposes more, present all candidates but generate the top 15 by risk (error paths and branched logic first) and say so.
- **Max 3 fix iterations** in the verification loop, then stop and report (see Step 5).

## Step 1 — Gather context (token-cheap first)

1. **SearchObject** — confirm the class exists; never guess the spelling.
2. **GetObjectStructure** or **GetClassComponents** — list methods with visibility and signatures; pick the public ones.
3. **GetSource** with `object_type="CLAS"`, `include="testclasses"` — read existing tests. An error here usually just means the include doesn't exist yet (expected on a fresh class — note it and move on).
4. **GetSource** with `include_context=true` — the appended dependency context gives the public API contracts of referenced interfaces/classes without extra calls. For deeper fan-out use **ListDependencies**.
5. Read only the methods you will test (`method` parameter) — pulling a whole large class wastes tokens.

If existing tests are found, extend them: reuse their fixtures and naming, never duplicate covered scenarios, and never delete a passing test.

## Step 2 — Classify dependencies and propose test cases

### Dependency classification (what to double vs. what to call)

| Category | What to look for | Test strategy |
|---|---|---|
| **Mockable** | Constructor/setter injection via interfaces (`if_*`, `zif_*`) | Local `ltd_*` test double implementing the interface |
| **Stubbable (DB)** | `SELECT` / `INSERT` / `UPDATE` / `DELETE`, CDS reads, EML on RAP BOs | OSQL / CDS / RAP test environment — templates in the reference file |
| **Transparent** | Private/protected helpers of the class itself | Do not mock — they execute normally under the public method |
| **Framework** | Static calls to SAP classes (`cl_abap_context_info`, `sy-datum`, …) | Wrap in an injectable interface if editing the class is in scope; otherwise state the limitation in the proposal |

For each public method identify: branches (IF/CASE/COND — one test each), error paths (RAISE, TRY-CATCH), return values, and state changes. Then present the proposal in exactly this shape and ask **"Which test cases should I generate? (all / numbers / skip some)"**:

```
Identified test cases for ZCL_DEMO_INVOICE:

Method: CALCULATE_TOTAL (public)
  1. [HAPPY]  test_total_simple      — 2 items, obvious sum
  2. [BRANCH] test_total_discounted  — discount > 0 path
  3. [ERROR]  test_total_no_items    — empty input, expect ZCX_DEMO_ERROR

Dependencies: zif_demo_pricing (mockable), ZDEMO_ITEMS table (stubbable via OSQL env)
Not mockable without refactoring: cl_abap_context_info=>get_system_date( ) in CALCULATE_TOTAL
```

## Step 3 — Generate the test code

Read [references/abap-unit-reference.md](references/abap-unit-reference.md) now for exact assertion names and the lifecycle/environment templates. Generation rules:

- Skeleton: `CLASS ltc_x DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.` with `class_setup` / `setup` / `teardown` / `class_teardown` only where needed; fresh CUT per test in `setup`.
- Lifecycle invariants for any test environment: **create in `class_setup`, `clear_doubles( )` in `setup`, `destroy( )` in `class_teardown`** — never skip any of the three.
- Arrange → Act → Assert in every method; one behavior per test; `test_<method>_<scenario>` snake_case names ≤ 30 chars.
- **Test data rules:** minimal (1-3 records), obvious values (`100.00 - 10.00 = 90.00`, not `37.83 - 4.27`), type-correct literals (`'001'` for NUMC, `'20260101'` for DATS, `abap_true`), deterministic (never `sy-datum`, `sy-uzeit`, or randoms), self-contained per method.
- Exception tests: call, then `cl_abap_unit_assert=>fail( )` if no exception, `CATCH` the specific class and assert on it.
- `cl_abap_testdouble` exists on 7.51+ only, and on ABAP Cloud only released APIs may be doubled — check the system profile; on older systems always use manual `ltd_*` doubles.
- Synthetic identifiers only in examples: `ZCL_DEMO_*`, `ZIF_DEMO_*`, `$ZDEMO`.

Show the complete generated include to the user and ask **"Write this to the system? (yes / edit first / cancel)"** before any write.

## Step 4 — Write, activate

Optional pre-flight: **AnalyzeABAPCode** with `source=<generated include>` — fix findings before writing. Zero findings means the lint pass found nothing, not that the tests are correct.

Choose the write path:

| Situation | Path |
|---|---|
| Class does not exist yet | **CreateClassWithTests** (`class_name`, `description`, `package_name`, `class_source`, `test_source`) — creates, writes the test include, activates, and runs the tests in one call. Done: skip to Step 5's reporting. |
| Existing class, writing/replacing the test include | **LockObject** on `/sap/bc/adt/oo/classes/<name>` → **CreateTestInclude** (`class_name`, `lock_handle`) if the include is missing → **UpdateClassInclude** (`class_name`, `include_type="testclasses"`, `source`, `lock_handle`) → **UnlockObject** → **SyntaxCheck** → **Activate** |
| Existing class where the main source is also being rewritten | **WriteSource** (`object_type="CLAS"`) with `test_source` set — auto-creates the include and runs the tests |
| Small fix to one existing test method | **EditSource** on the class URL with a unique `old_string`/`new_string` (or `method` constraint) — handles lock/syntax-check/activate itself |

Pass `transport` on every write for non-local packages; ask the user for the number, never fabricate one.

## Step 5 — Run and verify

**RunUnitTests** with `object_url="/sap/bc/adt/oo/classes/<name>"`. Defaults exclude DANGEROUS-risk and LONG-duration tests — generated tests are HARMLESS/SHORT, so defaults are right; only set `include_dangerous`/`include_long` when running pre-existing tests that need them.

Report in this shape:

```
Unit test run — ZCL_DEMO_INVOICE
Passed: 5   Failed: 1   Skipped: 0

FAIL test_total_discounted
  Assert: 'Discounted total wrong'  act = 95.00  exp = 90.00
  Diagnosis: mock pricing double returns list price, discount never applied — mock setup issue, not CUT logic
  Fix: configure ltd_pricing to return the discounted price
```

Failure loop (max 3 iterations): classify each failure as *mock setup*, *test data/assertion*, or *genuine CUT defect*; fix the first two with **EditSource** on the failing test method, re-run. A genuine CUT defect is a finding, not something to paper over — report it to the user and suggest the abap-developer skill (or abap-debugger for runtime analysis) instead of weakening the assertion.

**Epistemic honesty — never claim more than the tools showed:**
- A run reporting **0 executed tests** means the include didn't activate or `FOR TESTING` is missing — report "no tests executed", never "all green".
- Tests you did not run are "not run", not "passing". If RunUnitTests errors out, say so and stop.
- Green on `$TMP` with doubles proves the logic under the doubled contracts — say that, not "the class is verified".

## Error handling

| Error | Cause | Fix |
|---|---|---|
| GetSource `include="testclasses"` fails | No test include yet — **expected on a fresh class** | Proceed; CreateTestInclude in Step 4 creates it |
| CreateTestInclude fails "already exists" | Include exists — **expected when extending tests** | Skip straight to UpdateClassInclude |
| UpdateClassInclude lock error | Missing/stale `lock_handle`, or someone else holds the lock | LockObject again on the class; if held by a user, report and stop |
| Activation error in the include | Syntax error in generated code | Read the error line, fix, re-activate; SyntaxCheck first next time |
| `cl_abap_testdouble` unknown | Release < 7.51 or ABAP Cloud restriction | Manual `ltd_*` double implementing the interface |
| CUT method not visible from test | Method is protected/private | Test through public methods; only add a `FRIENDS ltc_*` clause with the user's consent |
| `class_setup` dump | CUT constructor signature mismatch — a required dependency not injected | Re-read the constructor via GetSource `method="constructor"` and fix the injection |
| Test env create fails | Entity/table name wrong, or RAP BDEF not found | Verify the name with SearchObject; check the environment templates in the reference file |

## When to use / when NOT

**Use for:** classes with non-trivial public logic, dependency-injected classes (ideal), building a regression net before a refactor, TDD starting points.

**NOT for:** running existing tests (→ **test**), CDS view data tests as the primary goal beyond the bundled template, performance measurement, pure data containers. If Step 2 finds no branches, no error paths, and no meaningful return values, say: *"This class has no non-trivial public behavior to test — unit tests would only assert getters. Proceed anyway?"*

## Follow-up options

- **test** — run the new tests together with ATC on the whole package
- **clean-abap-review** — review the class (and the new tests) against the Clean ABAP rule set
- **abap-developer** — fix genuine CUT defects the tests uncovered
- **deploy** — full pre-release gate once the package is green
