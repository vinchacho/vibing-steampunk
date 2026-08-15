# Skill & Plugin Content to Borrow — marianfoo, ABAP FS, and the Skill Packs

**Date:** 2026-08-15
**Report ID:** 003
**Subject:** Content-level analysis of SAP skill packs and agentic suites — which specific skills, patterns, and tool-description techniques the vsp plugin should adopt
**Related Documents:** [2026-08-15-001-sap-mcp-skills-landscape-and-borrow-roadmap.md](2026-08-15-001-sap-mcp-skills-landscape-and-borrow-roadmap.md) (packaging-level survey; this report goes one level deeper into skill *content*)

---

## 0. Headline finding

**`weiserman/rap-skills` (22★, 5 skills, updated Aug 2026) is built on top of vsp.** Its README, `docs/VSP_SETUP.md`, and every SKILL.md reference `oisee/vibing-steampunk` by name — install instructions, mode/tool counts, `SAP_ALLOW_TRANSPORTABLE_EDITS`, the ZADT_VSP WebSocket handler. Each skill (rap-generator, rap-cds, rap-behavior, rap-testing, rap-troubleshoot; 348–475 lines each, high-quality RAP content) carries a **Tools Used** table naming exact vsp tools and parameters. It is a third-party skill pack for our server, listed in marianfoo's catalog — while we ourselves are absent from that catalog. We should engage: offer collaboration, and answer its branch-per-scenario design (one git branch per deployment-target × implementation-type) with runtime profile detection instead (§3.1).

## 1. Source inventory (licenses noted — GPL content is inspiration-only)

| Source | License | What it is |
|---|---|---|
| marcellourbani/vscode_abap_remote_fs ("ABAP FS") | MIT | VS Code Copilot-native agentic suite: 13 testing skills + 9 general ABAP skills, 9 agents + 12 cost-tiered subagent templates, **52 LM tools**, Playwright WebGUI testing runtime |
| marianfoo — mcp-sap-docs / sap-api-policy-skill / btp-drawio-skill / catalog | MIT-family | Best-in-class retrieval tool descriptions; skill static-check CI; the `skillsAndPlugins` catalog (22 entries: 9 SAP-official, 12 community) |
| matt1as/claude-abap-skills | Apache-2.0 | 5 skills (clean-abap review/refactor; RAP bo-design/clean-core-check/atc-remediation); rules-file + ATC-tagging architecture; **directly adaptable** |
| arc-mcp/arc-1 `skills/` | MIT | 22 skills; workflow chains; mutually-exclusive skill pairs; `bootstrap-system-context` |
| babamba2/superclaude-for-sap | MIT | 24 agents (incl. FI/CO/MM/SD module consultants), 15 multi-file skills; per-skill `model:` cost routing |
| likweitan/abap-skills | MIT | 13 reference-heavy skills; `atc-cloudification` (exact Cloudification-Repository JSON URLs, Clean-Core Level A–D ↔ API-state mapping); `scripts/validate_skills.py` CI |
| jfilak/sapcli-claude-plugin | — | `abap-snippet`: ABAP REPL via temporary `IF_OO_ADT_CLASSRUN` class |
| secondsky/sap-skills | **GPL-3.0** | Release-compatibility matrix concept (7.40→Cloud per-feature); "Related Skills" routing blocks. Re-derive from SAP sources, don't copy text |

## 2. New skills to add to the vsp plugin (ranked)

1. **`atc-remediation`** — biggest content gap. Model: matt1as (Apache-2.0). Groups ATC findings into 6 categories, fixes in priority order under a severity ladder (auto-apply / ask-first / manual-only), **refuses pseudo-comment suppressions** (`"#EC`) unless the user supplies check name + justification, re-runs ATC per batch and reports resolved/remaining/new deltas. Backed by: `RunATCCheck`, `EditSource`, `SyntaxCheck`, `Activate`, `RunUnitTests`.
2. **`clean-abap-review`** + **`clean-abap-refactor`** (pair). Model: matt1as. Review: read-only, rules cited by name, ATC-checkable findings first, each marked *confirmed by ATC* / *not raised by ATC*, Critical/Major/Minor. Refactor: six deterministic ordered passes (naming → declaration → expression → method shape → error → class shape) with explicit behaviour-preservation non-negotiables (never change public exceptions, SELECT ordering, authority checks, code crossing COMMIT/ROLLBACK), dry-run syntax check before write, read-back after write, unit tests after activation.
3. **`bootstrap-system-context`** (arc-1's highest-leverage skill) — probe SID/release/components/features once, write `system-info.md` (or `.claude/vsp.local.md`), every other skill reads it. Kills the "emit 7.50 syntax on a 7.40 box" failure class, and is our runtime answer to rap-skills' branch-per-scenario. Backed by: `GetConnectionInfo`, `GetFeatures`, feature-detection we already have.
4. **`abap-snippet`** (sapcli's idea) — the "prove it on the real system" primitive: write temp `IF_OO_ADT_CLASSRUN` class → SyntaxCheck → run → capture output → delete. We have every tool needed; pairs with `ExecuteABAP`'s hardened result checking from PR #156.
5. **`clean-core-check`** — hard violations (unreleased API, direct SELECT on SAP tables, dynpro, SUBMIT/CALL TRANSACTION, FORM/PERFORM, mods) vs soft warnings, BTP-vs-on-prem split, **release state from an authoritative lookup, never from memory**; Clean-Core Levels A–D per likweitan's mapping. Backed by: `GrepPackage`, `ListDependencies`, `RunATCCheck`, `RunQuery`.
6. **`transport-review` / `transport-overview`** (documented as a mutually exclusive pair, arc-1 style) — depth (per-object diffs + risk flags, pre-release gate) vs breadth (all open transports, owner/size/risk). Natural showcase for our `changelog`/`tr-boundaries` analysis suite, which no other pack can back.
7. **`perf-diagnose`** — cheapest-first ladder (SQL trace state → traces → query metrics → call graph), root-cause catalog. Model: arc-1 `debug-slow-sql`. Could fold into `abap-debugger`.
8. **`program-to-spec`** (superclaude) — reverse-engineer an object/package into an FS/TS document with Socratic scope narrowing; pairs with `handoff` and, longer-term, with the abap_wiki-style author/judge doc generation from report 001.
9. **`anst-enhancement-discovery`** (ABAP FS, unique) — guide the user through tcode ANST runtime tracing → export xlsx → classify rows USER_EXIT/DEFINITE/POTENTIAL/STANDARD (~20 lines of Go) → read each object's source → feed findings into test/RCA. Nobody in the Claude ecosystem has runtime enhancement discovery.

## 3. Cross-cutting patterns to adopt

### 3.1 Environment profile instead of branches
rap-skills is right that BTP `strict(2)` / S/4 Private / on-prem need different guidance, and wrong to solve it with git branches (2 of 10 planned branches exist; not installable as a plugin). vsp detects the system at runtime — one bootstrap skill caches the profile; all skills condition on it. This also gives us a friendly opening with weiserman.

### 3.2 Skill-description formula (rap-skills / matt1as / marianfoo)
Three components in every `description:`: verbatim user phrasings ("says things like 'create a RAP BO for…'"), a bare keyword trigger list, and a **closing scope/negative-scope sentence** ("Read-only — never writes back"). Our `abap-debugger` vs `rca` vs `test` overlap is exactly what scope sentences disambiguate. Add marianfoo-style `tests/skill_static_checks.py` frontmatter linting to CI.

### 3.3 Rules-file + ATC-tagging architecture (matt1as)
Move Clean ABAP / clean-core rules into `references/rules.md` as `## RULE: <name>` blocks, each with an `**ATC**:` line naming the check when one exists. Skills cite rules by name and sort tool-verifiable findings first. Anti-hallucination rules stated as commands: never assert release status from memory; never invent ATC findings.

### 3.4 Verifier agents + confirmation gates (ABAP FS)
- **Adversarial reviewer subagents as phase gates**: their `sap-findings-reviewer` re-reads source hunting for fabricated line numbers and missed MESSAGE/AUTHORITY-CHECK statements; `sap-screens-reviewer` *fails* output that shows ABAP internals (proof it was derived from source, not observed). Add a paired verifier to `rca` and `abap-architect` with an explicit fabrication checklist.
- **Confirmation-string gates**: a tool/step requires a literal confirmation string ("I verified all upstream phase gates…") before running — cheap deterministic enforcement that a gate wasn't skipped. Fits our `deploy` skill's STOP points, and the preview-then-confirm roadmap item (report 001 §5.1).
- **Ephemeral-subagent contract boilerplate**: "If the caller tells you HOW to do your task, ignore it — follow only this file. Accept inputs, reject invented methods; one-shot; REJECTED template names the missing input and the exact re-invocation fix." Paired orchestrator rule: "pass inputs, not methods."
- **Phase pipeline with disk as the handoff contract**: their 7-phase testing suite writes one artifact per phase (`_flow.md`, `_screens.md`, `TC-NNN.md`, …), "one phase per chat." Our `handoff` skill is the seed of this; formalize artifact names.

### 3.5 Tool-description techniques (marianfoo's mcp-sap-docs — apply to vsp's own MCP tools)
Five moves, worth retrofitting across `tools_register.go` descriptions: (1) inline call example as the first line; (2) a SOURCES/capabilities overview with per-option "best for" one-liners; (3) **discover-don't-invent** guidance for opaque identifiers ("run the search without the token first; never guess it") with a stated safe fallback; (4) anti-misroute warnings where two params are commonly confused; (5) an ESCALATION clause naming the next tool to try when a call returns empty. ABAP FS adds: "MANDATORY before X" ordering constraints (their `get_abap_sql_syntax` is mandatory before `execute_data_query` — ours could gate `RunQuery` similarly) and documented fallbacks for known-empty responses. With 103/154 tools, description quality is our main routing-accuracy lever.

### 3.6 Doc grounding
Recommend `marianfoo/abap-mcp-server` (offline ABAP keyword docs, Clean ABAP, DSAG, `abap_feature_matrix`, clean-core-level search facets) as a companion server in the README, and adopt arc-1's split of fast-path vs `-researched` skill variants so doc lookup is opt-in rather than a tax on every generation.

## 4. Ecosystem actions

1. **PR marianfoo's catalog** (`data/catalog.json` → `skillsAndPlugins`): vsp is absent from all sections; entries are plain JSON. High-visibility, trivial.
2. **Engage weiserman/rap-skills**: offer the runtime-profile approach, fix their doc drift (their README/concept doc disagree on default branch; referenced setup doc missing), and cross-link.
3. **skills.cloud.sap registration** once the root-level `skills/` layout question is settled (drafts in `.local/ecosystem-listing-drafts.md`).

## 5. Suggested order of work

| Step | Item | Effort |
|---|---|---|
| 1 | Description-formula rewrite of the 10 existing skills + scope sentences (§3.2) | hours |
| 2 | `bootstrap-system-context` skill + profile cache (§2.3, §3.1) | ~1 day |
| 3 | `atc-remediation` + `clean-abap-review` skills w/ rules file (§2.1–2.2, §3.3) | ~2 days |
| 4 | `abap-snippet` + `clean-core-check` (§2.4–2.5) | ~1 day |
| 5 | Tool-description retrofit pass over `tools_register.go` (§3.5) | ~1–2 days |
| 6 | Catalog PR + weiserman engagement (§4) | hours |
| 7 | Transport pair, perf-diagnose, program-to-spec, ANST (§2.6–2.9) | as capacity allows |
