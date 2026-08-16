# SAP MCP / Skills / Plugins Landscape and Roadmap

**Date:** 2026-08-15
**Report ID:** 001
**Subject:** Survey of the SAP MCP-server, agent-skills, and plugin ecosystem (mid-2026), competitive position, upstream PR reviews (#150, #156), and the resulting improvement roadmap
**Related Documents:** [2026-07-11-001-improvement-plan-and-landscape.md](2026-07-11-001-improvement-plan-and-landscape.md), [2026-08-15-002-upstream-vsp-bug-audit.md](2026-08-15-002-upstream-vsp-bug-audit.md), [2026-08-15-003-plugin-skill-roadmap.md](2026-08-15-003-plugin-skill-roadmap.md), [2026-06-15-001-issue-pr-triage-and-roadmap.md](2026-06-15-001-issue-pr-triage-and-roadmap.md)

---

## 1. Executive summary

Since the July report, three things changed materially:

1. **Upstream is stalled while rivals accelerate.** `oisee/vibing-steampunk` (440★) has had no commit since 2026-06-15 while good PRs pile up. ARC-1 is now the most active community project. Two upstream PRs were cherry-picked into this fork as part of this work (§4).
2. **SAP went official.** The ABAP MCP Server GA'd inside ADT for VS Code, and Joule for Developers' agentic ABAP is GA — but both are cloud-first. The on-prem/ECC niche this fork serves remains open.
3. **Distribution matters as much as the server.** SAP runs an official skills portal (skills.cloud.sap); the most-starred community SAP AI project is a skills pack with no server at all. We ship a plugin but distribute nothing: no releases, no marketplace, no registry, no listing.

The roadmap (§5) covers four themes: security posture, plugin quality, distribution, and tool surface/DX.

## 2. Landscape by category (August 2026)

The best single map is [marianfoo/sap-ai-mcp-servers](https://github.com/marianfoo/sap-ai-mcp-servers) — an auto-regenerated catalog (last generation 2026-08-13). Upstream is listed there at 440★ — still #1 among community ABAP MCP servers by stars — but the entry does not mention this fork.

### 2.1 Official SAP

| Offering | State | Notes |
|---|---|---|
| **ABAP MCP Server** (in ADT for VS Code) | GA'd Q2 2026, still flagged experimental | Runs locally inside the VSIX; ~11 repo object types; auth inherited from IDE destinations. Zero credential handling — its main DX advantage. |
| **Joule for Developers / agentic ABAP** | GA at Sapphire 2026 | Flagship Mass S/4 Custom Code Conversion Agent. **Not available for on-prem S/4** — exactly our niche. |
| **SAP-ABAP-1 model** | Shipping | Lost every public benchmark, including code explanation. Frontier models + good tools beat a fine-tuned model + weak tools. |
| **MCP Gateway** (Integration Suite Premium) | Live | Translates API specs into MCP schemas augmented with natural-language semantics. |
| **Other official MCP servers** | Active | fiori-mcp-server (155★), cap-js/mcp-server (109★), UI5/mcp-server (97★), mdk-mcp-server (34★). Official Claude plugins for UI5, LeanIX, Theme Designer, Automation Pilot. |
| **SAP AI Skills Library** (skills.cloud.sap) | Live, 134+ entries | Official portal + CLI for skills and MCP servers, with a "Submit Skills" path. **We are absent.** |

### 2.2 Community ABAP ADT MCP servers

- **[arc-mcp/arc-1](https://github.com/arc-mcp/arc-1)** (160★, 566 commits, MIT) — the main rival. Secure-by-default (read-only until flags flip, gated data preview/free SQL, package allowlists, action-level deny lists, rate limiting); 12 intent-based tools with capability-adaptive schemas; broad auth (API key, OIDC/JWT, OAuth, XSUAA, BTP Destination per-user identity); large test suite; one-click install; docs site with ADRs.
- **mario-andreschak** — `mcp-abap-abap-adt-api` (179★), `mcp-abap-adt` (170★). In npm + MCP Registry. Maintained but no longer the frontier.
- **[fr0ster/mcp-abap-adt](https://github.com/fr0ster/mcp-abap-adt)** (78★) — full CRUD, JWT/XSUAA + service-key auth.
- **[DataZooDE/erpl-adt](https://github.com/DataZooDE/erpl-adt)** (16★) — C++17 single native binary; DuckDB metadata catalog with full-text + embedding search.
- **[abap-ai/mcp](https://github.com/abap-ai/mcp)** (79★) — MCP server SDK written in ABAP, on the 2025-11-25 spec. Frozen pending V2.
- **[Coaspe/sap-abap-mcp](https://github.com/Coaspe/sap-abap-mcp)** — named toolsets, preview-then-confirm for destructive ops, JSONL audit log, SARIF/JUnit output.
- **AWS ABAP Accelerator for Amazon Q** (54★) — 15 tools, ECS Fargate, X.509 principal propagation.

### 2.3 Adjacent MCP servers

- **[marianfoo/mcp-sap-docs](https://github.com/marianfoo/mcp-sap-docs)** (213★) — 20+ offline ABAP documentation sources with hybrid BM25 + embedding retrieval; public no-auth endpoint; air-gapped Docker.
- **[ClementRingot/ROSA](https://github.com/ClementRingot/ROSA)** (25★) — Clean Core advisor over SAP's Cloudification Repository; no SAP connection needed; auth auto-detection.
- OData/BTP: `oisee/odata_mcp_go` (138★), lemaiwo/btp-sap-odata-to-mcp-server (129★), gavdilabs/cap-mcp-plugin (61★). HANA: HatriGt/hana-mcp-server (63★). GUI automation: mario-andreschak/mcp-sap-gui (124★, stale), kts982 (24★, active).

### 2.4 Skills / plugins / agent packaging

- **[secondsky/sap-skills](https://github.com/secondsky/sap-skills)** (409★ — the most-starred community SAP AI repo, and it ships no server): 40 plugins, 67 commands, 31 agents, dual Claude/Codex manifests generated from one source, CI audit scripts.
- **[matt1as/claude-abap-skills](https://github.com/matt1as/claude-abap-skills)** — clean-code and RAP skills with per-plugin rule files and explicit tool-routing tables.
- **naveenkumarbaskaran/abap-ai-workspace** — 15 skills + a crawled offline SAP Help corpus; SessionStart system detection.
- **marcellourbani/vscode_abap_remote_fs** — five releases in the week of Aug 10–14; v2.8.0 added an agentic SAP-testing suite (skills + agents in-box, Playwright WebGUI testing, VS Code LM tools, ANST integration, system debugging). A pre-MCP extension turning itself into an agentic platform — more evidence that packaging is the battleground.

### 2.5 Agentic ABAP beyond MCP

- **ABAP LLM benchmark** (timkoehne/marianfoo; Zeis writeups) — 180 tasks executed against a live ABAP system with error feedback; frontier general models lead by a wide margin.
- **[Gixsy95/abap_wiki](https://github.com/Gixsy95/abap_wiki)** (42★) — Z-object → Markdown knowledge base with a two-model author/judge verification gate and per-claim source citations.
- **abaplint** remains the standard fast static-feedback loop for LLM-generated ABAP (we embed a native Go port in `pkg/abaplint`).

## 3. Competitive position

**Our moat:** ECC/on-prem coverage that SAP's cloud-first tooling ignores; the analysis suite nobody else has (`boundaries`, `slim` dead-code, `health`, E070-driven `changelog`, side-effects/LUW, `cr-config-audit`); CLI + YAML/Go/Lua DSL for headless CI; single Go binary; ZADT_VSP optional on-SAP companion.

**Our exposure:** ARC-1 leads on everything *around* the server — security defaults, auth breadth, test/eval credibility, packaging, docs, distribution. SAP wins on zero-config identity. CRUD-over-MCP is a commodity; analysis, on-prem, and governance are the contested ground.

## 4. Upstream PR reviews and cherry-picks (done in this fork)

Upstream main is frozen at `83b9699` (2026-06-15) — exactly our sync point — so both PRs applied with only fork-local conflicts.

- **PR #150 — `ActivateMultiple`**: batch activation in a single ADT POST so SAP resolves mutual dependencies; `ResolveObjectRef("TYPE NAME")` shorthand; `ActivatePackage` rewired to one batch request; full MCP wiring. **Cherry-picked as-is.**
- **PR #156 — write-safety/session hardening audit**: ten defect groups; see [2026-08-15-002-upstream-vsp-bug-audit.md](2026-08-15-002-upstream-vsp-bug-audit.md). Highlights: CSRF fetch stays in the same stateful session as lock/write (supersedes old candidates #120 and #125); logical `Success=false` fails closed across ExecuteABAP, copy, deploy/rename/table/batch workflows, DSL, and MCP `WriteSource`; CLI safety flags persist and propagate; installers verify results; per-system safety fields. **Cherry-picked commit-by-commit**, with one merge fixup (batch-activation logical failures classified per-object). Author verified with mocks only — a live sandbox canary remains worthwhile.
- Follow-up: upstream PRs **#108, #145, #152** overlap the session/transport work and need re-triage; #148 and #157 remain independent candidates; issues #153/#154 (HTTP 406 Accept-header bugs) look small and worth fixing directly.

## 5. Improvement roadmap

Ranked within each theme; "quick" ≈ a session, "medium" ≈ days, "large" ≈ sustained.

### 5.1 Security posture

1. **Flip to safe-by-default** (medium, breaking): `internal/mcp/server.go` still defaults to unrestricted; the safe default in `pkg/adt/safety.go` sits unused. The stderr warning was step one; persistent-flag propagation was step two. Flip the default at the next major version, with `--unrestricted` as the explicit opt-out.
2. **Preview-then-confirm for destructive ops** (medium): destructive tools return a preview + plan token; execution requires the token; tokens expire. *(The impact-gate confirm flow shipped 2026-08-15 implements this pattern for high-impact writes.)*
3. **JSONL audit log** (quick): append-only log of every mutating operation.
4. **Action-level deny lists** (quick): extend `SAP_DISALLOWED_OPS` semantics to sub-actions.

### 5.2 Plugin quality

1. **Eval suite with committed baselines** (medium, highest ROI): `plugin/evals/{suites,fixtures,baselines}`; JSON assertions; a compare gate in CI.
2. **Baseline-first skill authoring** (quick per skill): before editing a skill, run a subagent without it on a fixture task, record the failure modes, write only what closes them.
3. **Tool-routing tables + a no-fabrication rule** in each skill (shipped for the 2026-08-15 skill additions; extend to the rest).
4. **Dual manifests + AGENTS.md from one source** (quick): formalize the Codex manifest generation with a sync script.

### 5.3 Distribution

1. **GitHub releases for the fork** (quick): `release.yml` exists; run it.
2. **Get listed** (quick): the marianfoo catalog and skills.cloud.sap.
3. **`.mcpb` bundle + MCP Registry + Homebrew tap** (medium): one-click install is table stakes.
4. **Docs site with ADRs** (medium): `docs/adr/` content exists; publishing it is credibility.

### 5.4 Tool surface / DX

1. **Capability-adaptive schemas** (medium): feed feature detection into tool registration so schemas only advertise what the connected system supports.
2. **Live-system eval harness** (medium): generate → deploy to `$TMP` → activate → ABAP Unit → feed errors back, as an opt-in integration suite. Doubles as regression testing for write paths and publishable evidence.
3. **Offline doc index** (large, or partner): embed a hybrid retrieval index over ABAP keyword docs, or document running a docs server alongside vsp. Start with the latter.
4. **Verified documentation generation** (large): two-model author/judge documentation of customer packages on top of the graph engine, with per-claim source citations.

## 6. Immediate next actions

1. ~~Cherry-pick #150 and #156~~ — done (§4).
2. Ecosystem listings: correct the marianfoo catalog entry; prepare a skills.cloud.sap submission (drafts pending sign-off).
3. Re-triage upstream #108/#145/#152; pick up #148, #153/#154.
4. Plugin eval suite (§5.2.1) and the safe-by-default flip plan (§5.1.1) as the next substantive work items.
