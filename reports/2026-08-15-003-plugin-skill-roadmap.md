# Plugin Skill Roadmap and Authoring Standards

**Date:** 2026-08-15
**Report ID:** 003
**Subject:** Capability-gap analysis for the vsp Claude Code plugin — skills shipped 2026-08-15, the forward queue, and the house authoring standards
**Related Documents:** [2026-08-15-001-sap-mcp-skills-landscape-and-roadmap.md](2026-08-15-001-sap-mcp-skills-landscape-and-roadmap.md) (ecosystem survey; this report covers the plugin's own skill surface)

---

## 0. Ecosystem headline

**`weiserman/rap-skills` (22★, 5 skills, updated Aug 2026) is built on top of vsp.** Its README, `docs/VSP_SETUP.md`, and every SKILL.md reference the upstream repo by name — install instructions, mode/tool counts, safety flags, the ZADT_VSP WebSocket handler — and each skill carries a **Tools Used** table naming exact vsp tools and parameters. It is a third-party skill pack for our server, listed in marianfoo's catalog while we ourselves are absent. We should engage: offer collaboration, and answer its branch-per-deployment-scenario design with vsp's runtime system-profile detection (`bootstrap-system-context`) instead.

## 1. Skill surface as of 2026-08-15 (21 skills, CI-linted)

**Core workflow:** `abap-developer` (dev loop), `abap-debugger` (live runtime investigation), `abap-architect` (structure/impact/design), `deploy` (gated pipeline), `test` (run checks), `rca` (evidence-first incident workflow with an 8-category root-cause framework and a no-speculation BLOCKED rule), `handoff`, `status` (layered health checks with gated skips), `vsp-knowledge`, `bootstrap-system-context` (system profile probe + syntax-level derivation), `cba-enterprise`.

**Code quality:** `clean-abap-review` (rules-file-driven read-only review, ATC-verified findings first, 29-rule reference with ATC check tags), `clean-abap-refactor` (six deterministic passes with behaviour-preservation non-negotiables), `atc-remediation` (six-category triage, auto-apply/ask/manual severity ladder, pseudo-comment-suppression refusal, per-batch ATC deltas), `clean-core-check` (hard-violation vs soft-warning audit, Levels A–D, authoritative release-state lookups via the bundled Cloudification-Repository data reference — never from memory).

**Transports:** `transport-review` (depth: per-object diffs with CTS identity normalization, snapshot-sparsity handling — "baseline unavailable" is a first-class answer — and coverage declarations) and `transport-overview` (breadth: all open transports with owner/age/risk flags); documented as a mutually exclusive pair.

**Analysis & generation:** `perf-diagnose` (cheapest-first diagnostic ladder with an explicit stop rule and generator-hunting), `unused-code` (runtime SCMON/SUSG evidence fused with static where-used and Slim V2 dead-code, with hard refusal of unbounded scans), `generate-unit-tests` (dependency classification, test-double selection, full assert/lifecycle reference), `rap-bo-design` (structured design interview → whole-stack scaffold with dependency-aware batch activation).

**Infrastructure:** `scripts/validate-skills.py` lints every manifest (frontmatter, name/folder agreement, link resolution) in CI.

## 2. Forward queue

**Batch B (next):**
1. **`program-to-spec`** — reverse-engineer an object into a functional/technical spec with a four-level depth ladder (Quick/Standard/Deep/Audit), a single bundled question opener, and a numeric ambiguity gate as the interview exit condition. Backed by `GetSource`, `FindReferences`, `GetRevisions`, the graph engine.
2. **`package-to-process`** — reverse-engineer a package into an end-to-end business-process document: cluster objects by shared core tables, label clusters with canonical module flows (PR→PO→GR→IR style), render BPML. The graph engine computes natively what this otherwise costs N tool calls — our strongest differentiator applied to documentation.
3. **`abap-snippet` as a Go MCP tool** — execute an ABAP snippet via a temporary `IF_OO_ADT_CLASSRUN` class with create→check→run→capture→delete atomicity guaranteed server-side; a thin skill documents usage. Fills the "prove it on the real system" gap between `RunQuery` and `RunUnitTests`.
4. **`object-documenter`** — batch per-object documentation enriched by the abaplint port and graph health/api-surface data.

**Later (sequenced):** a merged clean-core migration skill (audit → classify by level → blast radius → wrapper generation → re-scan); CDS/analytics generation chain; program-variant comparison; BAdI/enhancement guidance including debugger-driven BAdI discovery (breakpoint on the BAdI factory, enumerate what fires); authorization/IAM handler templates; a released-APIs routing reference (curated shortlist + live lookup, no vendored dumps); ANST-driven runtime enhancement discovery feeding test scope and RCA.

## 3. Authoring standards (house rules for every vsp skill)

1. **Description formula:** capability + verbatim user phrasings + bare keyword triggers + a closing scope sentence naming the sibling skills that own adjacent intents.
2. **System-profile awareness:** release- or feature-sensitive skills read `.claude/vsp-system-profile.md` and route to `bootstrap-system-context` when it's missing.
3. **Tool reality:** every named tool must exist in `internal/mcp/tools_register.go`; CI lints the manifests.
4. **Smart defaults, stated:** a defaults table the skill applies silently instead of interrogating the user.
5. **Hard scope caps with refusal text:** bounded object counts with an explicit "this list is not actionable" refusal beyond the cap.
6. **Error-handling tables** including expected-failure rows ("diff not supported for this type — expected, list as metadata").
7. **Epistemic honesty as rules:** "0 findings on `$TMP` = not checked, not clean"; "baseline unavailable", never a guessed diff; release state from a lookup or marked *unverified*; never invent ATC findings.
8. **Literal output contracts:** fenced report templates with exact headings so results are reproducible across sessions.
9. **Token-cheap-first ordering:** summaries before full reads, server-side diffs before source dumps, diagnostic ladders with a stop rule.
10. **Impact-gate awareness:** on `impact.risk: "high"` read key callers and run tests on affected packages before proceeding; on an `IMPACT GATE` refusal, surface the report and retry with the confirm token only once justified.
11. **Data hygiene:** explicit column lists on configuration-table queries; never read connection-secret columns (e.g. `RFCDES.RFCOPTIONS`).
12. **Progressive disclosure:** SKILL.md stays a short contract; heavyweight content lives in `references/` files loaded on demand.

Third-party license notices for the plugin are consolidated in [NOTICE.md](../NOTICE.md).

## 4. Ecosystem actions

1. **Catalog listing** (marianfoo/sap-ai-mcp-servers): the fork is absent; a PR adding it is small and high-visibility.
2. **Engage weiserman/rap-skills**: offer the runtime-profile approach and cross-link.
3. **skills.cloud.sap registration** once the root-level `skills/` layout question is settled (drafts in `.local/ecosystem-listing-drafts.md`).
