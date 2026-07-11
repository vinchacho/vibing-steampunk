# Improvement Plan & Competitive Landscape — July 2026

**Date:** 2026-07-11
**Report ID:** 001
**Subject:** Fork improvement roadmap informed by the July-2026 ABAP/SAP MCP landscape; internal quality assessment; 2026 MCP protocol gap analysis
**Related Documents:** [2026-06-15-001-issue-pr-triage-and-roadmap.md](2026-06-15-001-issue-pr-triage-and-roadmap.md) (upstream triage), [2026-04-05-002-graph-engine-design.md](2026-04-05-002-graph-engine-design.md), [2026-02-01-001 one-tool mode design]
**Provenance:** Landscape research and code verification performed 2026-07-07 → 2026-07-11. Star counts and GA dates are point-in-time snapshots; SAP dates are *announced* status from vendor/community communications, not independently verified GA.

---

## 1. Executive summary

Basic CRUD-over-MCP for ABAP is being commoditized — by SAP itself and by a half-dozen community servers. vsp's durable advantages are (a) ECC/on-prem coverage that SAP's cloud-first tooling does not address, (b) the deep analysis suite (boundaries, slim, health, co-change, side-effects, transport changelog) that no competitor has, and (c) CLI + DSL automation for headless/CI use. The fork's biggest risks are internal, not external: no CI on push/PR, an untested `internal/mcp` layer, published quickstarts that 404, and documentation whose numbers disagree with the code. This report fixes the priorities: quality foundation first, then DX & distribution, then 2026 MCP protocol parity.

## 2. Competitor matrix

| Project | Stack | ~Stars | Distinguishing features | Threat/lesson for vsp |
|---|---|---|---|---|
| mario-andreschak/mcp-abap-abap-adt-api | TypeScript | 163 | Broad ADT coverage, most-starred community server | Popularity ≠ depth; vsp exceeds it on analysis |
| arc-mcp/ARC-1 | — | 136 | 7 read-only tools, remote HTTP + RBAC, CI/CD via GitHub Actions | Small surface + enterprise auth story sells; CI/CD table stakes |
| jfilak/sapcli | Python CLI | 93 | Mature CLI, scripting-first | Overlaps vsp CLI; no MCP |
| abap-ai/mcp | Native ABAP | 75 | MCP Prompts/Resources/Tasks, OAuth — runs *on* the SAP system | Furthest along on 2026 MCP spec features |
| fr0ster/mcp-abap-adt | — | 65 | JWT/XSUAA auth | Cloud-auth depth |
| AWS ABAP Accelerator | — | 52 | Amazon Q/Kiro integration, ECC→S/4 transform | Hyperscaler distribution channel |
| secondsky/sap-skills | Skills (no server) | 368 | Claude Code skills packaging only | **Distribution winner** — packaging beats features for reach |

Directory note: `github.com/marianfoo/sap-ai-mcp-servers` lists this project under upstream `oisee` and mislabels it "Python". A correction PR is a cheap visibility win (Phase 5).

## 3. SAP official moves (announced status, Sapphire-2026 timeframe)

- **ABAP MCP Server** — GA announced Q2 2026, BTP/cloud-first; on-prem support unconfirmed.
- **ADT for VS Code** — GA announced Q2 2026.
- **SAP-ABAP-1** — purpose-trained ABAP LLM.
- **Joule for Developers** — free through Sept 2026.
- **Custom Code Migration Agent** — ECC→S/4 focus.

Implication: within a year, "read/write ABAP source over MCP" is a checkbox SAP ships. Third-party servers survive on what SAP won't do: on-prem/ECC systems, cross-release analysis depth, open automation, and vendor-neutral integration.

## 4. vsp's durable position

1. **ECC/on-prem coverage** — SAP's tooling is cloud-first; the installed base is not.
2. **Analysis suite** — `vsp boundaries`, `slim` (method-level dead code), `health`, `changelog`/`changes` (E070/E070A co-change), side-effect extraction + LUW classification, `cr-config-audit`. No competitor has an equivalent.
3. **CLI + DSL automation** — YAML workflows, Go fluent API, Lua scripting; headless CI use without an LLM in the loop.
4. **Safety machinery** — package allowlists, op whitelists, read-only mode, transportable-edit gates (needs safe-by-default, see §6).

## 5. Upstream state (verified 2026-07-11)

Upstream `main` has had **one commit since April** (83b9699, docs-only triage report — merged into this fork 2026-07-11 as `b884ea7`). The activity is in the review queue, exactly as upstream's own triage report concedes (PR throughput is their bottleneck): 12+ open PRs from external contributors, updated as recently as 2026-07-03, plus long-lived unmerged branches (`one-tool-mode` +16, `worktree-integration-test-infra` +8 — 23 integration tests for untested ADT methods).

**Cherry-pick candidates for this fork** (pull-only; we do not push to upstream):

| Upstream PR | What it fixes | Why it matters here |
|---|---|---|
| #125 | Skip redundant mutation gate after lock (stateful session survives) | Same lock-handle bug class as `22517d4`; hardens write path |
| #145 | Reuse an object's existing open transport instead of 409-ing | Write-path ergonomics |
| #120 | CSRF HEAD→GET fallback + secure-cookie fix + `SAP_SESSION_TYPE` | Auth robustness on hardened systems |
| #121/#139/#134 | INCL (PROG/I) write support (three competing PRs) | Upstream triage cluster B; pick most complete |
| #126 | Server-side type filter for search | Small, clean |
| #108 | Deploy session ordering + MODIFICATION_SUPPORT handling | Write-path correctness |
| branch `worktree-integration-test-infra` | +23 integration tests | Directly serves our quality-foundation theme |

## 6. Internal quality assessment (all verified by reading code / APIs)

1. **No push/PR CI** — only `claude.yml`, `release.yml` (manual dispatch; tests run only at release), `sync-upstream.yml`. *(Fixed in this change: `ci.yml`.)*
2. **`vsp systems init` writes a dead file** — writes `.vsp-systems.json` (`cmd/vsp/cli.go:452`) but the loader (`pkg/config/systems.go:49-64`) only searches `.vsp.json`, `.vsp/systems.json`, `~/.vsp.json`, `~/.vsp/systems.json`; help text and hint list the wrong paths too. *(Fixed in this change.)*
3. **MCP server defaults to unrestricted** — `internal/mcp/server.go:134` uses `adt.UnrestrictedSafetyConfig()`; a safe `DefaultSafetyConfig` exists unused (`pkg/adt/safety.go:76-82`). *(Stderr warning added in this change; flipping the default is a Phase 4 breaking-change decision.)*
4. **Quickstart 404s** — README curls `vinchacho/.../releases/latest/...` but the fork has zero GitHub releases (verified via API); `.goreleaser.yml:61-62` targets `oisee`. Same broken URLs in `docs/cli-agents/README.md` (+ RU/UA/ES) and CHANGELOG links. *(Fixed in this change.)*
5. **Go version drift** — `go.mod` requires 1.25; `docs/reviewer-guide.md` said "Go 1.23+". *(Fixed in this change.)*
6. **`internal/mcp` is untested** — 46 files, 38 `handlers_*.go`, the default hyperfocused router — only `server_test.go` exists. (Phase 4.)
7. **Doc drift** — tool counts published as 81/122 (`cmd/vsp/main.go:40`) and 100/147 (`main.go:137`) vs code-derived 102 focused / 153 expert registrations; test counts published as 244/499+821/~1,000 vs 1,325 measured `func Test` functions; ROADMAP.md frozen at v2.15.0; ARCHITECTURE.md describes a pre-`internal/mcp` layout; CLAUDE.md had a broken Configuration section and a stale "legacy registerTools in server.go" note (it lives in `tools_register.go`). *(CLAUDE.md fixed in this change; main.go/ROADMAP/ARCHITECTURE are Phase 4 follow-ups.)*
8. **Graph unification half-done** — `cmd/vsp/cli_extra.go` uses `pkg/graph` ✓, but `cmd/vsp/cli_deps.go` keeps its own classifier, `pkg/ctxcomp/analyzer.go` is a third parallel dep-stack, and `pkg/graph/builder_adt.go` doesn't exist (`builder_sql.go`, `builder_transport.go`, `builder_config.go` do — the old "SQL adapters pending" note was stale). (Phase 4.)

## 7. 2026 MCP protocol gap analysis

vsp implements tools-over-stdio only. The 2026 baseline (see abap-ai/mcp, github-mcp-server, playwright-mcp) adds:

| Capability | What it buys | vsp status |
|---|---|---|
| MCP Resources & Prompts | Sources/tables as addressable resources; canned workflows as prompts | Missing |
| Long-running Tasks + polling | Activation, ATC, unit-test runs without timeout gymnastics | Missing |
| Elicitation (human-in-the-loop confirms) | Safe destructive ops (delete, transport release) | Missing |
| Streamable HTTP + OAuth 2.1 | Remote/multi-user deployment, enterprise auth | stdio only (SAML/browser auth exist for the SAP side) |
| Registry + MCPB bundles + Claude Code plugin | One-click install; discoverability | Missing (§2: packaging is how sap-skills won) |
| Token-lean structured outputs | Playwright-style compact trees instead of prose dumps | Partial (hyperfocused mode helps) |

References: anthropic.com/engineering/code-execution-with-mcp · github.com/github/github-mcp-server (composable `--toolsets`) · github.com/microsoft/playwright-mcp · github.com/modelcontextprotocol/mcpb.

## 8. Prioritized roadmap

**Phase 3 — Quick wins (this change):** push/PR CI; `systems init` fix; download/quickstart fixes (point binaries at upstream releases, add `go install` + source builds); `.mcp.json.example`/`.env.example`; unrestricted-safety stderr warning.

**Phase 4 — Quality foundation:** table-driven routing tests for `handlers_universal.go`; graph unification (fold `cli_deps.go` classifier + `ctxcomp/analyzer.go` into `pkg/graph`, add `builder_adt.go`); flip safe-by-default behind `--unrestricted` opt-out (breaking-change note); auto-generate README_TOOLS.md from `tools_register.go`; reconcile `main.go` help-text counts; cherry-pick upstream PRs #125/#120/#126 and the integration-test branch (§5).

**Phase 5 — DX & distribution:** unified `vsp login` (auto-detect basic/cookie/browser-SSO/SAML — pieces exist in `cmd/vsp/main.go`, `pkg/adt/browser_auth.go`, `saml_auth.go`); lean 60-second README quickstart; Claude Code plugin + MCPB bundle + Homebrew tap (goreleaser `brews:`) + MCP Registry listing; correction PR to the marianfoo directory.

**Phase 6 — 2026 MCP parity (cost/benefit order):** Resources + Prompts → elicitation confirms for destructive ops → long-running Tasks (activate/ATC/unit tests) → streamable HTTP + OAuth 2.1 → token-lean structured outputs.

Strategic positioning work (clean-core wedge, public benchmarks) is deliberately deprioritized behind the above.
