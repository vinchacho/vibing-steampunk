# SAP MCP / Skills / Plugins Landscape — and What to Borrow

**Date:** 2026-08-15
**Report ID:** 001
**Subject:** Survey of the SAP MCP-server, agent-skills, and plugin ecosystem (mid-2026), competitive position, upstream PR reviews (#150, #156), and a prioritized borrow roadmap
**Related Documents:** [2026-07-11-001-improvement-plan-and-landscape.md](2026-07-11-001-improvement-plan-and-landscape.md), [2026-08-15-002-upstream-vsp-bug-audit.md](2026-08-15-002-upstream-vsp-bug-audit.md), [2026-06-15-001-issue-pr-triage-and-roadmap.md](2026-06-15-001-issue-pr-triage-and-roadmap.md)

---

## 1. Executive summary

Since the July report, three things changed materially:

1. **Upstream is stalled while a rival accelerates.** `oisee/vibing-steampunk` (440★) has had no commit since 2026-06-15 while good PRs pile up. **ARC-1** is now the frontier community project and publicly positions us as "functionality over governance." Two upstream PRs were cherry-picked into this fork as part of this work (§4).
2. **SAP went official.** The ABAP MCP Server GA'd inside ADT for VS Code, and Joule for Developers' agentic ABAP is GA — but both are cloud-first. The on-prem/ECC niche this fork serves remains open.
3. **The distribution channel matters as much as the server.** SAP now runs an official skills portal (skills.cloud.sap); the most-starred community SAP AI project is a *skills pack with no server at all* (secondsky/sap-skills, 409★). We ship a plugin but distribute nothing: no releases, no marketplace, no registry, no listing.

The borrow roadmap (§5) covers four themes the user prioritized: security posture, plugin quality/evals, distribution, and tool surface/DX.

## 2. Landscape by category (August 2026)

The best single map is [marianfoo/sap-ai-mcp-servers](https://github.com/marianfoo/sap-ai-mcp-servers) — an auto-regenerated catalog (last generation 2026-08-13). Upstream is listed there at 440★ — still #1 among community ABAP MCP servers by stars — but the entry mislabels the project "Python" and does not mention this fork.

### 2.1 Official SAP

| Offering | State | Notes for us |
|---|---|---|
| **ABAP MCP Server** (in ADT for VS Code, `SAPSE.adt-vscode`) | GA'd Q2 2026, still flagged experimental | Runs locally inside the VSIX; headless reuse of Eclipse ADT libraries; ~11 repo object types; auth inherited from IDE destinations (RFC on-prem, HTTP BTP). Zero credential handling — its main DX advantage over us. |
| **Joule for Developers / agentic ABAP** | GA at Sapphire 2026 | Flagship Mass S/4 Custom Code Conversion Agent (ATC → deterministic quick-fixes → AI fixes → human-in-loop). **Not available for on-prem S/4** — which is exactly our niche. |
| **SAP-ABAP-1 model** | Shipping | Lost every public benchmark, including code *explanation* (Zeis, Mar 2026). Frontier models + good tools beat a fine-tuned model + weak tools. |
| **MCP Gateway** (Integration Suite Premium) | Live | Translates API specs into MCP schemas *augmented with natural-language semantics*. |
| **Other official MCP servers** | Active | fiori-mcp-server (155★), cap-js/mcp-server (109★), UI5/mcp-server (97★), mdk-mcp-server (34★). Official Claude plugins for UI5, LeanIX, Theme Designer, Automation Pilot. |
| **SAP AI Skills Library** (skills.cloud.sap, `SAP/ai-skills-library`) | Live, 134+ entries | Official portal + CLI for skills AND MCP servers, for Joule *and* Claude Code, with verification badges and a "Submit Skills" path. **We are absent.** |

### 2.2 Community ABAP ADT MCP servers

- **[arc-mcp/arc-1](https://github.com/arc-mcp/arc-1)** (160★, 566 commits, MIT) — the main rival; credits vibing-steampunk as prior art. Distinctives: **secure-by-default** (read-only until flags flip, `SAP_ALLOW_DATA_PREVIEW`/`SAP_ALLOW_FREE_SQL` gates, package allowlists, action-level deny lists like `SAPWrite.delete`, 3-tier rate limiting); **12 intent-based tools** (SAPRead/Search/Write/Activate/Navigate/Query/Transport/Git/Context/Lint/Diagnose/Manage) whose **schemas auto-adapt to detected system capability**; auth breadth (API key, OIDC/JWT via Entra/Keycloak, OAuth, XSUAA, per-user identity via BTP Destination Service); 3,474 unit + 262 integration + 141 E2E tests; `.mcpb` one-click install; Claude Code marketplace; docs site with ADRs. Satellites: `adt-ls` (headless language-server SDK), `xsuaa-auth`, `mcp-hub` (multi-system).
- **mario-andreschak** — `mcp-abap-abap-adt-api` (179★), `mcp-abap-adt` (170★). In npm + MCP Registry. Maintained but no longer the frontier.
- **[fr0ster/mcp-abap-adt](https://github.com/fr0ster/mcp-abap-adt)** (78★, active Aug 2026) — full CRUD, JWT/XSUAA + service-key auth, split connection layer.
- **[DataZooDE/erpl-adt](https://github.com/DataZooDE/erpl-adt)** (16★) — C++17 single native binary, no JVM/RFC SDK; closest architectural analogue to our Go build. Notable: **DuckDB metadata catalog with full-text + embedding semantic search** over the repository.
- **[abap-ai/mcp](https://github.com/abap-ai/mcp)** (79★) — MCP server SDK written *in ABAP*, on the 2025-11-25 spec (prompts, resources, long-running tasks with polling, completions, DDIC→JSON-Schema). Frozen pending V2. Direct comparison point for our ZADT_VSP embedded service.
- **[Coaspe/sap-abap-mcp](https://github.com/Coaspe/sap-abap-mcp)** (3★ but idea-rich) — 115 tools in named **toolsets** (core/write/analysis/debug/operations/artifacts), prod profiles read-only, **preview-then-confirm** for destructive ops with **10-minute stale-plan expiry**, JSONL audit log, SARIF/JUnit evidence output for CI.
- **AWS ABAP Accelerator for Amazon Q** (54★) — 15 tools, ECS Fargate, X.509 principal propagation + OAuth/OIDC.
- Long tail: Dassian, babamba2/abap-mcp-adt-powerup, Hochfrequenz/aibap.mcp, YahorNovik/mcp-adt, Eclipse-as-MCP with SSO (Erhan Keseli).

### 2.3 Adjacent MCP servers

- **[marianfoo/mcp-sap-docs](https://github.com/marianfoo/mcp-sap-docs)** (213★) — 20+ offline sources (ABAP keyword docs, DSAG Leitfaden, style guides) + live SAP Help/Community; **hybrid BM25 + MiniLM embeddings fused by reciprocal-rank fusion**; `abap_feature_matrix` for release availability; public no-auth endpoint; air-gapped Docker. Variants: abap-mcp-server (85★), mcp-sap-notes (56★).
- **[ClementRingot/ROSA](https://github.com/ClementRingot/ROSA)** (25★) — Clean Core advisor over SAP's Cloudification Repository, fetched from public GitHub at runtime, **no SAP connection needed** ("is MARA allowed? → use I_PRODUCT"); **auto-detects 4 auth modes from env**; one codebase serving MCP + REST; simultaneous npm + Docker + native-binary releases.
- OData/BTP: `oisee/odata_mcp_go` (138★, upstream author's), lemaiwo/btp-sap-odata-to-mcp-server (129★), gavdilabs/cap-mcp-plugin (61★). HANA: HatriGt/hana-mcp-server (63★). BW: dnic-dev/bw-modeling-mcp (55★). GUI automation: mario-andreschak/mcp-sap-gui (124★, stale), kts982 (24★, active).

### 2.4 Skills / plugins / agent packaging

- **[secondsky/sap-skills](https://github.com/secondsky/sap-skills)** (409★ — the most-starred community SAP AI repo, and it ships **no server**): 40 plugins, 67 commands, 31 agents. Layout worth copying: per-plugin `.claude-plugin/plugin.json` + `.codex-plugin/plugin.json` + `agents/openai.yaml` generated **from one source of truth** by `scripts/sync-plugins.sh`; dual marketplaces; `npm run audit:skills` staleness/capability checks in CI; per-skill `README.md` holding discovery keywords separate from `SKILL.md`.
- **[matt1as/claude-abap-skills](https://github.com/matt1as/claude-abap-skills)** — skills as verbs (review, refactor, rap-bo-design, atc-remediation, clean-core-check); a `templates/AGENTS.md` for non-Claude agents; an explicit **tool-routing table** per skill (official SAP MCP primary → ARC-1 fallback → pasted code last); a hard **"no fabrication without a system connection"** rule; documents the `MCP_TIMEOUT=600000` gotcha.
- **naveenkumarbaskaran/abap-ai-workspace** — 15 `/abap-*` skills + 92 crawled SAP Help pages as an offline corpus with a refresh script; SessionStart hook auto-detects connected SAP systems.
- **marcellourbani/vscode_abap_remote_fs** — five releases in the week of Aug 10–14. v2.8.0 added a **SAP Testing agentic suite**: skills + agents in-box, Playwright-driven SAP testing, VS Code LM Tools (native Copilot integration), ANST skill/agent/tool, web GUI auto-login, system debugging. A pre-MCP extension turning itself into an agentic platform — more evidence that packaging is the battleground. (Also the read-only access layer `abap_wiki` builds on.)
- Best-in-class outside SAP: **obra/superpowers** (skills as mandatory workflow gates; SessionStart hook injects the skill index; RED-GREEN-REFACTOR applied to *prose* — baseline a subagent without the skill, record the failure, write the skill to close it); **Anthropic skill-creator** (executor/grader/comparator/analyzer eval agents, N-run benchmarks with variance analysis, description-field optimization for trigger accuracy); the **marketplace eval harness** pattern (JSON test suites with `contains`/`not_regex`/file-effect assertions, fresh temp workspace per test, committed baselines, `compare.py` regression gate, results posted to the PR).

### 2.5 Agentic ABAP beyond MCP

- **ABAP LLM benchmark** (timkoehne/marianfoo; Zeis writeups) — 180 tasks, generated code **activated on a live ABAP system**, ABAP Unit run, errors fed back for up to 5 rounds, scored as cumulative pass rate. GPT-5 ≈ Claude Opus 4.5 at top.
- **[Gixsy95/abap_wiki](https://github.com/Gixsy95/abap_wiki)** (42★) — Z-object → Markdown knowledge base: L0 deterministic TADIR inventory (no LLM) → L1 **author agent + independent judge agent on a different model**, fail-closed, every claim tagged `[VERIFIED: path:N-M]` → L2 human slices. Reports ~68% downstream token reduction. The author/judge pattern fits our graph engine unusually well.
- **abaplint** remains the standard fast static-feedback loop for LLM-generated ABAP (we already embed a Go port in `pkg/abaplint`).

## 3. Competitive position

**Our moat (unchanged, still real):** ECC/on-prem coverage that SAP's cloud-first tooling ignores; the analysis suite nobody else has (`boundaries`, `slim` dead-code, `health`, E070-driven `changelog`, side-effects/LUW, `cr-config-audit`); CLI + YAML/Go/Lua DSL for headless CI; single Go binary; ZADT_VSP optional on-SAP companion (WebSocket debug/RFC).

**Our exposure:** ARC-1 wins on everything *around* the server — security defaults, auth breadth, test/eval credibility, packaging, docs, distribution. SAP wins on zero-config identity. The July report's conclusion stands and has sharpened: CRUD-over-MCP is a commodity; analysis, on-prem, and governance are the contested ground, and we currently concede governance.

## 4. Upstream PR reviews and cherry-picks (done in this fork)

Upstream main is frozen at `83b9699` (2026-06-15) — exactly our sync point — so both PRs applied with only fork-local conflicts (module paths, plus our d409a98/1ce43be changes).

- **PR #150 — `ActivateMultiple`** (txape10, 5 files): batch activation in a single ADT POST so SAP resolves mutual dependencies (a program and its includes activate together, matching Eclipse); `ResolveObjectRef("TYPE NAME")` shorthand; `ActivatePackage` rewired to one batch request; full MCP wiring including the hyperfocused `ACTIVATE_MULTI` route. Live-tested by the author on S/4HANA. **Cherry-picked as-is.**
- **PR #156 — write-safety/session hardening audit** (Augusto42, 44 files, 15 commits, draft): ten defect groups; see [2026-08-15-002-upstream-vsp-bug-audit.md](2026-08-15-002-upstream-vsp-bug-audit.md) for the author's full audit. Highlights: **CSRF fetch now stays in the same stateful session as lock/write** (with HEAD→GET fallback) — this **supersedes old cherry-pick candidates #120 and #125** (package check moved before lock via a context marker; operation/transport gates preserved); logical `Success=false` now fails closed across ExecuteABAP, copy, deploy/rename/table/batch workflows, DSL, and MCP `WriteSource`; CLI safety flags are persistent and propagate to every subcommand client; installers verify package/deploy/activation/read-back; per-system safety fields in the systems config; Windows recording-ID and jseval portability fixes. **Cherry-picked commit-by-commit**, with one merge fixup: batch-activation logical failures are classified per-object (preserving `ActivatePackage`'s contract) instead of erroring.
  - Caveat carried forward: the author verified with mocks only. A live sandbox canary (their "safe manual plan" checklist) is still worth running before trusting the write paths in anger.
  - Follow-up: upstream PRs **#108, #145, #152** overlap with #156's session/transport work and need re-triage; #148 (activation response root element) and #157 (CGO-free SQLite) remain independent candidates. Issues #153/#154 (HTTP 406 on DTEL / namespaced function groups — Accept-header bugs) look small and worth fixing here directly.

Both cherry-picks are verified: `go build ./... && go vet ./... && go test ./...` green (1,363 test functions across 16 packages).

## 5. Borrow roadmap

Ranked within each theme; "quick" ≈ a session, "medium" ≈ days, "large" ≈ sustained.

### 5.1 Security posture (ARC-1's wedge — and our stated priority)

1. **Flip to safe-by-default** (medium, breaking): `internal/mcp/server.go` still defaults to `adt.UnrestrictedSafetyConfig()`; the safe default in `pkg/adt/safety.go` sits unused. The stderr warning (1ce43be) was step one; #156's persistent-flag propagation was step two. Flip the default at the next major version, with `--unrestricted` as the explicit opt-out. ARC-1 proves this sells rather than repels.
2. **Preview-then-confirm for destructive ops** (medium): borrow Coaspe's pattern — destructive tools return a preview + plan token; execution requires the token; tokens expire after 10 minutes. Natural fit for the hyperfocused universal tool.
3. **JSONL audit log** (quick): append-only log of every mutating operation (op type, object, package, transport, outcome). Enterprise evaluators ask for this first.
4. **Action-level deny lists** (quick): extend `SAP_DISALLOWED_OPS` semantics to sub-actions (e.g. allow write but deny delete).

### 5.2 Plugin quality / evals

1. **Eval suite with committed baselines** (medium, highest ROI — nobody in the ABAP space has one): `plugin/evals/{suites,fixtures,baselines,out}`; JSON tests (`id`, `input`, `tags`, assertions of type contains/not_regex/file-effect); runner via `claude -p ... --plugin-dir` in a fresh temp workspace; `compare.py` gates CI against the baseline.
2. **Baseline-first (RED-GREEN) skill authoring** (quick per skill): before editing `abap-debugger`/`rca`, run a subagent *without* the skill on a fixture task, record the failure modes, write only what closes them.
3. **Tool-routing table + no-fabrication rule** (quick): each SKILL.md states the tool order (vsp MCP → official SAP ADT MCP fallback → pasted code last) and a hard "never fabricate ABAP without a live connection" rule (matt1as pattern).
4. **Dual manifests + AGENTS.md from one source** (quick): we already carry `plugin/.codex-plugin/` — formalize with a `scripts/sync-plugins.sh` that generates Claude + Codex manifests and a root `AGENTS.md` from one source of truth (secondsky pattern).

### 5.3 Distribution (we currently ship none of it)

1. **GitHub releases for the fork** (quick): da46af5 documented that our README pointed at nonexistent binaries. `release.yml` exists; run it.
2. **Get listed** (quick): fix the marianfoo catalog entry (wrong language, fork absent); submit the plugin to skills.cloud.sap via `SAP/ai-skills-library`.
3. **`.mcpb` bundle + MCP Registry + Homebrew tap** (medium): one-click install is table stakes now (ARC-1, mario-andreschak both have it).
4. **Docs site with ADRs** (medium): we have `docs/adr/` content; publishing it is credibility.

### 5.4 Tool surface / DX

1. **Capability-adaptive schemas** (medium): we already probe features (Safety Network `auto` flags); feed detection into tool registration so schemas only advertise what the connected system supports (ARC-1 pattern; also fixes cloud-vs-on-prem confusion).
2. **Live-system eval harness** (medium): port the 180-task benchmark loop — generate → deploy to `$TMP` → activate → ABAP Unit → feed errors back ≤5 rounds — as an opt-in integration suite. Doubles as regression testing for write paths (exactly what #156's mocks can't cover) and as publishable marketing.
3. **Hybrid doc index** (large, or partner): either embed a BM25+embeddings index over ABAP keyword docs à la mcp-sap-docs / erpl-adt's DuckDB catalog, or simply document running mcp-sap-docs alongside vsp. Start with the latter.
4. **Author/judge doc generation** (large, differentiating): abap_wiki's two-model verify-or-fail pattern on top of our graph engine — package → verified Markdown knowledge base with `[VERIFIED]` claims.

## 6. Immediate next actions

1. ~~Cherry-pick #150 and #156~~ — done (§4).
2. Ecosystem listings: correct the marianfoo catalog entry; prepare a skills.cloud.sap submission for `vsp-abap-developer` (drafts pending user sign-off — outward-facing).
3. Re-triage upstream #108/#145/#152 against the merged #156; pick up #148, #153/#154 as small fixes.
4. Start §5.2.1 (plugin eval suite) and §5.1.1 (safe-by-default flip plan) as the next substantive work items.
