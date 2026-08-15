---
name: test
description: "Run ABAP Unit tests and ATC checks for a package, class, or object and interpret the failures. Use for 'run the tests', 'does it pass ATC', 'test ZCL_X', 'check code quality'. Triggers: unit test, ABAP Unit, ATC, code check, quality. Scope: executing and reading existing checks — writing new test classes is abap-developer; the full pre-release gate sequence is deploy."
---

> **System profile:** before generating release-specific syntax or proposing transport/feature workflows, read `.claude/vsp-system-profile.md` if it exists; if it's missing and the task is release- or feature-sensitive, run the **bootstrap-system-context** skill first.

Run unit tests and code quality checks for the specified ABAP package or class.

## Workflow

1. If no argument provided, ask the user what to test
2. Use **SearchObject** to verify the target exists
3. Run **RunUnitTests** on the target
4. Run **RunATCCheck** on the target
5. Report results clearly:
   - Total tests: passed / failed / skipped
   - ATC findings by priority (P1 = critical, P2 = important, P3 = info)
   - If failures: show the failing test names and error messages
   - If ATC P1/P2 findings: show the finding details with object and line

## Example Usage

```
/vsp:test $TMP
/vsp:test ZCL_INVOICE_PROCESSOR
/vsp:test $ZADT_VSP
```
