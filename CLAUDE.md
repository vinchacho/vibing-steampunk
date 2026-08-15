# CLAUDE.md

**vsp** — Go-native MCP server and CLI for SAP ABAP Development Tools (ADT).

> **Doc intent:** CLAUDE.md = dev context. README.md = user onboarding. reports/ = research/history. contexts/ = session handoff.

---

## Project Status

| Metric | Value |
|--------|-------|
| **Latest version** | v2.39.0 |
| **Modes** | `hyperfocused` (1 universal tool, **default**) · `focused` (103 whitelisted tools) · `expert` (154 registered tools; runtime count varies with feature detection and `--disabled-groups`) |
| **Tests** | 1,363 `func Test` functions across 16 packages (incl. 35 integration tests behind the `integration` build tag) — `go test ./...` green as of 2026-08-15 |
| **Platforms** | 9 (cross-compiled via Makefile) |
| **Reports** | 194 in `reports/` (183 dated `YYYY-MM-DD-NNN-title.md` + 11 reference) |
| **Sync** | 0 commits behind upstream `oisee/vibing-steampunk` (last merge `b884ea7`, 2026-07-11) |

> Counts are code-derived (see "Reconciling counts" below). If a number here disagrees with the code, the code wins — re-measure, don't copy forward.

## Current Priorities

Roadmap and rationale: [reports/2026-07-11-001-improvement-plan-and-landscape.md](reports/2026-07-11-001-improvement-plan-and-landscape.md). Upstream's own triage: [reports/2026-06-15-001-issue-pr-triage-and-roadmap.md](reports/2026-06-15-001-issue-pr-triage-and-roadmap.md).

### 1. Quality foundation
- Routing tests for `internal/mcp/handlers_universal.go` (default mode is effectively untested — only `server_test.go` exists in `internal/mcp`)
- Safe-by-default decision: `internal/mcp/server.go` defaults to `adt.UnrestrictedSafetyConfig()`; unused safe default at `pkg/adt/safety.go` (stderr warning ships now; flipping is a breaking change)
- ~~Cherry-pick candidates #125/#120/#126~~ superseded: upstream PR #156 (safety/session audit) and #150 (ActivateMultiple) cherry-picked 2026-08-15 — see [reports/2026-08-15-001-sap-mcp-skills-landscape-and-borrow-roadmap.md](reports/2026-08-15-001-sap-mcp-skills-landscape-and-borrow-roadmap.md); re-triage #108/#145/#152, pick up #148/#153/#154

### 2. Graph Engine (`pkg/graph/`) — In Progress
- Done: core types, parser dep extraction, boundary analyzer, SQL/transport/config builders (`builder_sql.go`, `builder_transport.go`, `builder_config.go` — all tested), queries (slim/health/rename/impact/api-surface/transport_boundaries)
- Pending: `builder_adt.go` (ADT adapters), unify `cmd/vsp/cli_deps.go` classifier + `pkg/ctxcomp/analyzer.go` into `pkg/graph`
- Design: [002](reports/2026-04-05-002-graph-engine-design.md), [003](reports/2026-04-05-003-graph-engine-alignment-for-claude.md)

### 3. GUI Debugger (Issue #2) — Strategic
Plan: MCP debug sessions → DAP → Web UI. ADT REST API mapped from `CL_TPDA_ADT_RES_APP`. Design: [001](reports/2026-04-05-001-gui-debugger-design.md)

---

## Build & Test

```bash
# Build
go build -o vsp ./cmd/vsp

# Run unit tests
go test ./...

# Run integration tests (requires SAP system)
SAP_URL=http://host:port SAP_USER=user SAP_PASSWORD=pass SAP_CLIENT=001 \
  go test -tags=integration -v ./pkg/adt/

# Cross-compile (via Makefile)
make build              # Current platform → build/vsp
make build-all          # 3 common platforms (linux-amd64, darwin-arm64, windows-amd64)
make build-all-all      # All 9 platforms
```

CI: `.github/workflows/ci.yml` runs build + vet + test on every push/PR. `release.yml` is manual dispatch; `sync-upstream.yml` is the daily upstream check.

Key flags: `--mode focused|expert|hyperfocused`, `--read-only`, `--allowed-packages "Z*"`, `--disabled-groups 5THD`

### Reconciling counts

When updating the Project Status table, derive — never copy:

```bash
grep -c 'shouldRegister("' internal/mcp/tools_register.go        # expert-mode registrations
grep -cE '^\s*"[A-Za-z0-9_]+":\s*true' internal/mcp/tools_focused.go  # focused whitelist
grep -rE '^func Test' --include='*_test.go' . | wc -l             # test functions
ls reports/ | wc -l                                               # reports
```

---

## Configuration

```bash
# Using environment variables
SAP_URL=http://host:50000 SAP_USER=user SAP_PASSWORD=pass ./vsp

# Using cookie authentication
./vsp --url http://host:50000 --cookie-string "sap-usercontext=abc; SAP_SESSIONID=xyz"
./vsp --url http://host:50000 --cookie-file cookies.txt
```

| Variable / Flag | Description |
|-----------------|-------------|
| `SAP_URL` / `--url` | SAP system URL (e.g., `http://host:50000`) |
| `SAP_USER` / `--user` | SAP username |
| `SAP_PASSWORD` / `--password` | SAP password |
| `SAP_CLIENT` / `--client` | SAP client number (default: 001) |
| `SAP_LANGUAGE` / `--language` | SAP language (default: EN) |
| `SAP_INSECURE` / `--insecure` | Skip TLS verification (default: false) |
| `SAP_COOKIE_FILE` / `--cookie-file` | Path to Netscape-format cookie file |
| `SAP_COOKIE_STRING` / `--cookie-string` | Cookie string (key1=val1; key2=val2) |
| `SAP_MODE` / `--mode` | Tool mode: `hyperfocused` (default since `880aa68`) · `focused` · `expert` — tool counts in Project Status |
| `SAP_DISABLED_GROUPS` / `--disabled-groups` | Disable tool groups: `5`/`U`=UI5, `T`=Tests, `H`=HANA, `D`=Debug, `C`=CTS, `G`=Git, `R`=Reports, `I`=Install, `X`=Experimental |
| `SAP_VERBOSE` / `--verbose` | Enable verbose logging to stderr |
| **Safety Configuration** | |
| `SAP_READ_ONLY` / `--read-only` | Block all write operations (default: false) |
| `SAP_BLOCK_FREE_SQL` / `--block-free-sql` | Block RunQuery execution (default: false) |
| `SAP_ALLOWED_OPS` / `--allowed-ops` | Whitelist operation types (e.g., "RSQ") |
| `SAP_DISALLOWED_OPS` / `--disallowed-ops` | Blacklist operation types (e.g., "CDUA") |
| `SAP_ALLOWED_PACKAGES` / `--allowed-packages` | Restrict to packages (supports wildcards: "Z*") |
| `SAP_ALLOW_TRANSPORTABLE_EDITS` / `--allow-transportable-edits` | Allow editing objects in transportable packages (default: false) |
| **Feature Configuration (Safety Network)** | |
| `SAP_FEATURE_ABAPGIT` / `--feature-abapgit` | abapGit integration: auto, on, off (default: auto) |
| `SAP_FEATURE_RAP` / `--feature-rap` | RAP/OData development: auto, on, off (default: auto) |
| `SAP_FEATURE_AMDP` / `--feature-amdp` | AMDP/HANA debugger: auto, on, off (default: auto) |
| `SAP_FEATURE_UI5` / `--feature-ui5` | UI5/Fiori BSP management: auto, on, off (default: auto) |
| `SAP_FEATURE_TRANSPORT` / `--feature-transport` | CTS transport management: auto, on, off (default: auto) |

⚠️ With no safety flags the server runs fully unrestricted (and warns on stderr). For anything beyond a sandbox, start from `--read-only` or `--allowed-packages`.

## Codebase Structure

```
cmd/
├── vsp/                  # Main CLI + MCP server (cli, devops, compile, deps, lsp, lua, workflow, ...)
└── abapgit-pack/         # Standalone abapGit ZIP packer

internal/
├── mcp/                  # MCP server core + 38 handlers_*.go (one per domain: crud, git, graph, health, ...)
└── lsp/                  # ABAP LSP server (online diagnostics, go-to-definition)

pkg/
├── adt/                  # ADT REST client (HTTP, CSRF, sessions, all SAP ops)
├── abaplint/             # Native Go port of abaplint: lexer, statement parser, lint rules (oracle-verified)
├── graph/                # Dependency graph engine: queries (slim/health/rename/impact/api-surface), builders, scopes
├── ctxcomp/              # Context compression: dep resolution + contract injection for GetSource
├── dsl/                  # Fluent API + YAML workflow engine, batch import/export, pipelines
├── cache/                # In-memory + SQLite cache
├── config/               # Multi-system profile management (add/list/switch)
├── scripting/            # Lua VM + 50+ ADT tool bindings, REPL
├── jseval/               # JavaScript evaluator
├── llvm2abap/            # LLVM IR → ABAP compiler (research)
├── ts2abap/              # TypeScript → ABAP transpiler (research)
├── ts2go/                # TypeScript → Go transpiler (used to port abaplint)
└── wasmcomp/             # WASM → ABAP AOT compiler (QuickJS proven on SAP)

embedded/
├── abap/                 # ABAP sources installed on SAP (ZADT_VSP service, debug, git, AMDP, reports, RFC)
└── deps/                 # Embedded abapGit dependency ZIPs

docs/                     # Architecture, ADRs, cli-agents guides (4 langs), reviewer guide
contexts/                 # Session handoff notes (chronological)
reports/                  # Research / design / status reports (YYYY-MM-DD-NNN-title.md)
articles/                 # Published articles (upstream author content — do not rewrite oisee URLs)
abap/src/zadt_vsp/        # ABAP source mirror in abapGit format
scripts/                  # Sync upstream, release helpers

Makefile                  # Cross-compilation (9 platforms)
ARCHITECTURE.md  ROADMAP.md  VISION.md  README_TOOLS.md
```

> File-level detail rots fast. To list current files in a package: `ls pkg/<name>/`. To find a handler: `ls internal/mcp/handlers_*.go`.

### Where things live

| Task | Where |
|------|-------|
| New MCP tool | `tools_register.go` + `handlers_<domain>.go` + (optional) `tools_focused.go` — see "Adding a New MCP Tool" below |
| New ADT operation | `pkg/adt/` — `client.go` (read), `crud.go` (write), `devtools.go` (syntax/activate), `codeintel.go` (def/refs), `debugger.go` / `amdp_debugger.go`, `git.go`, `reports.go`, `transport.go`, `ui5.go`, `workflows.go` |
| New graph query | `pkg/graph/queries_*.go` |
| New ABAP lint rule | `pkg/abaplint/rules.go` |
| New CLI command | `cmd/vsp/cli_*.go` (see existing: `cli_extra.go`, `cli_compile.go`, `cli_deps.go`, `devops.go`) |
| Integration test | `pkg/adt/integration_test.go` (build tag `integration`) |
| One-tool / hyperfocused mode | `internal/mcp/handlers_universal.go` |

---

## Adding a New MCP Tool

1. **Add ADT client method** in `pkg/adt/` (e.g. `client.go`, `crud.go`, `devtools.go`).
2. **Register tool** in `internal/mcp/tools_register.go` with `shouldRegister("ToolName")` and the tool definition. (All registration goes through `registerTools()` in this file — there is no separate legacy path anymore.)
3. **Whitelist for focused mode** (optional): add to `internal/mcp/tools_focused.go`. For tool grouping (`--disabled-groups`), update `tools_groups.go`.
4. **Add handler** in the appropriate `internal/mcp/handlers_<domain>.go` (38 domain files exist; pick the closest fit or create a new one). Handlers are routed from `handleToolCall()` in `server.go`:

```go
// In handlers_read.go (or appropriate domain file)
func (s *Server) handleNewTool(ctx context.Context, args map[string]any) (*mcp.CallToolResult, error) {
    name, _ := getString(args, "name")
    result, err := s.client.NewMethod(ctx, name)
    if err != nil {
        return mcp.NewToolResultError(err.Error()), nil
    }
    return mcp.NewToolResultText(formatResult(result)), nil
}
```

5. **Add integration test** in `pkg/adt/integration_test.go` (build tag `integration`).
6. **Update `README_TOOLS.md`** tool reference table.

### AMDP WebSocket Client Pattern (via ZADT_VSP)

AMDP/HANA debugging uses a WebSocket connection to the ZADT_VSP APC handler instead of plain REST — the debug session is stateful and event-driven (`pkg/adt/amdp_websocket.go` for the Go client, `embedded/abap/zcl_vsp_amdp_service.clas.abap` for the ABAP side). Handlers call the WS client directly (`s.amdpWSClient.Step(...)`) rather than going through the ADT REST client.

## Testing

- **Unit tests:** `go test ./...` — 16 packages, notably `internal/mcp` (server/registration), `pkg/adt` (client, HTTP, safety, transport, codeintel, debugger), `pkg/cache`, `pkg/config`, `pkg/dsl`, `pkg/graph`, `pkg/scripting`.
- **Integration tests:** `go test -tags=integration -v ./pkg/adt/` — create objects in `$TMP`, clean up after. Manual test program: `ZTEST_MCP_CRUD` in `$TMP`.
- **Known gap:** `internal/mcp` handler routing (esp. `handlers_universal.go`) has no dedicated tests — see Current Priorities.

## ADT API Reference

- `/sap/bc/adt/discovery` — API discovery document
- `reports/adt-abap-internals-documentation.md` — detailed endpoint analysis

---

## Common Issues

1. **CSRF errors** — auto-refreshed in `http.go`
2. **Lock conflicts** — edit handler does auto lock/unlock
3. **Session issues** — some CRUD/debugger flows are session-sensitive; verify stateful/stateless before changing transport or auth logic
4. **Auth** — use basic OR cookies, not both
5. **ZADT_VSP** — WebSocket debug/RFC/RunReport require it installed on SAP

## Security

Never commit `.env`, `cookies.txt`, `.mcp.json`, or local agent/MCP config files (all in `.gitignore`). Tracked examples (`.env.example`, `.mcp.json.example`) must contain placeholder values only.

### Sanitize policy for tracked docs, tests, and examples

The public repo must not contain concrete identifiers that tie code or
docs to a live SAP system, a real user, or a customer's ABAP namespace.
Anything that does belongs under `.local/` (gitignored) and never in
`contexts/`, `reports/`, `docs/`, or any tracked test fixture.

**Never in tracked files:**
- Real SAP usernames — use `TESTUSER`
- Real hostnames or IPs — use `dev.example.local`, `prodsys-a.example`, `trialsys.example`
- System aliases that name a live box — use `devsys`, `devsys-adt`, `prodsys-a`, `prodsys-b`
- Live transport numbers (`DEVK[0-9]+`, `R[0-9]{2}K[0-9]+`, `D[0-9]{2}K[0-9]+`) — use `TR-EXAMPLE`
- Live change request IDs — use `CR-EXAMPLE`
- Customer ABAP namespaces from real projects — use synthetic `ZDEMO_*`, `ZCL_DEMO_*`, `ZIF_DEMO_*`, `$ZDEMO`
- Customer transport attribute names — use `Z_CR_ATTR`
- Real passwords, API keys, bearer tokens (obvious, but stated)
- Real person names tied to private systems (OSS attribution for upstream libraries is fine — "user X on private host Y" is not)

**Always OK in tracked files:**
- `$ZHIRTEST*`, `ZCL_HIRT*`, `ZCUSTOM_DEVELOPMENT` — pre-agreed synthetic fixtures
- Public GitHub handles that are already in the Go module path
- Upstream OSS attribution for library authors

**Operational scratch goes under `.local/`** — session notes, live CR
dumps, bug repros with real identifiers, debugging transcripts. The
`.local/` dir is gitignored. If you need to reference it from a
tracked doc, redact first.

**Before every commit that touches `reports/`, `contexts/`, `docs/`,
or test fixtures:** scan the staged diff for the identifier families
above. The detection signature (concrete literal list of past-leaked
strings) lives at `.local/scripts/check-identifiers.sh` and is
gitignored on purpose — the signature itself would otherwise be the
leak it is trying to prevent. Structural patterns safe to commit:

```bash
git diff --cached | grep -nE \
  '\b[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\b|' \
  '\b[A-Z][0-9]{2}K[0-9]{6}\b|' \
  '\bDEVK[0-9]{6,}\b'
```

That catches IPv4 literals and SAP transport IDs without hardcoding
a specific customer's values. Pair it with the private signature
file for the names-based families (usernames, hostnames, ABAP object
prefixes). If either matches, move the content under `.local/` and
replace the tracked version with a synthetic placeholder. Rule of
thumb: "would a stranger reading this file be able to identify the
customer, the system, or a live account?" If yes, redact.

## Conventions

### Object Naming
| Object Type | Pattern | Example |
|-------------|---------|---------|
| Programs | `ZADT_<nn>_<name>` | `ZADT_00_DEBUG_TEST` |
| Classes | `ZCL_ADT_<name>` | `ZCL_ADT_DEBUG_HELPER` |
| Interfaces | `ZIF_ADT_<name>` | `ZIF_ADT_DEBUGGABLE` |
| Function Groups | `ZADT_<nn>_<name>` | `ZADT_00_UTILS` |

### Debugging via Unit Tests
To trigger breakpoints programmatically (without SAP GUI):
1. Create a class with test methods (`lcl_test` pattern)
2. Set external breakpoint on the test code
3. Run `RunUnitTests` to trigger the breakpoint
4. Use `DebuggerListen` → `DebuggerAttach` to catch and debug

This allows AI-driven debugging without manual SAP GUI interaction.

## Reports and Documentation

### Report Naming Convention

**Format:** `./reports/{YYYY-MM-DD-<number>-<title>}.md` — sequential numbers starting from 001 each day, lowercase hyphen-separated titles. Date range so far: 2025-12-02 through 2026-07-11. Browse `reports/` for the full listing; the 11 reference documents (ADT discovery/internals/capability guides, cookie-auth guide, focused-mode proposal, Go-port assessment, rename analysis) are non-numbered.

### Creating New Reports

1. **Determine the date:** ISO format `YYYY-MM-DD` (creation date)
2. **Assign next number:** Continue sequence from last report that day
3. **Use the format:** `reports/{YYYY-MM-DD-<number>-<title>}.md`
4. **Include metadata** at the top:

```markdown
# Report Title

**Date:** 2026-07-11
**Report ID:** 001
**Subject:** Brief description
**Related Documents:** Links to related reports
```

## Feature Status

| Area | Status |
|------|--------|
| Transport Changelog (v2.39.0) | ✅ `vsp changelog` / `vsp changes` — E070/E070A/E07T-driven package & CR-level change correlation |
| `cr-config-audit` | ✅ v2a.1 — value-level literal match, L2 SQLite cache, 1-hop transitive reach, DDIC delivery-class filter |
| RecoverFailedCreate | ✅ MCP primitive + `vsp recover-failed-create` CLI; reconciles partial-create on 5xx |
| Boundary crossing analysis | ✅ `vsp boundaries`, `tr-boundaries`, `cr-boundaries` with `--details` and HTML reports |
| Graph exports | ✅ DOT, PlantUML, GraphML, Mermaid (with package subgraphs, edge coloring) |
| Side-effects + LUW (Phase 1) | ✅ Extracts `CALL TRANSACTION`, `CALL TRANSFORMATION`, `LEAVE TO TRANSACTION` |
| SAML SSO | ✅ `pkg/adt/saml_auth.go` — S/4HANA Public Cloud (PR #97) |
| Package allowlist on mutations | ✅ `SAP_ALLOWED_PACKAGES` enforced on existing-object writes (PR #101) |
| `AnalyzeABAPCode` tool | ✅ abaplint-based static analysis (PR #89) |
| Slim V2 dead-code | ✅ Method-level with `--level` flag, TDEVC hierarchy resolution |
| Package health MVP | ✅ `vsp health <package>` — `--details`, `--format md/html`, `--report` file output |
| Browser auth | ✅ `pkg/adt/browser_auth.go` (chromedp-based interactive login) |
| ABAP LSP server | ✅ `internal/lsp/` — online diagnostics, go-to-definition |
| MCP handler domains | ✅ `cds`, `codeanalysis`, `gcts`, `graph`, `health`, `i18n`, `revisions`, `testing`, `transport_analysis` |
| `pkg/graph/` engine | 🚧 See Current Priorities |
| AMDP Debugger | ⚠️ Experimental — session works, breakpoints need investigation (expert mode only) |
| UI5/BSP Mgmt | ⚠️ Partial — Read ops work; Create needs alternate API |

## DSL & Workflow Usage

```bash
# Run unit tests for a package
vsp workflow test "$TMP"
vsp workflow test "$ZRAY*" --parallel 4 --json

# Run YAML workflow
vsp workflow run examples/workflows/ci-pipeline.yaml --var PACKAGE=\$TMP
```

```go
// Go fluent API - Search & Test
objects, _ := dsl.Search(client).Query("ZCL_*").Classes().InPackage("$TMP").Execute(ctx)
summary, _ := dsl.Test(client).Objects(objects...).IncludeDangerous().Parallel(4).Run(ctx)

// Batch Import (abapGit-compatible; RAPOrder = DDLS → BDEF → Classes → SRVD)
result, _ := dsl.Import(client).FromDirectory("./src/").ToPackage("$ZRAY").RAPOrder().Execute(ctx)

// Batch Export (with all class includes)
result, _ := dsl.Export(client).Classes("ZCL_TRAVEL").ToDirectory("./backup/").Execute(ctx)

// RAP Deployment Pipeline
pipeline := dsl.RAPPipeline(client, "./src/", "$ZRAY", "ZTRAVEL_SB")
```

---

## Upstream Sync Automation

This fork automatically syncs with upstream `oisee/vibing-steampunk`. See [scripts/README.md](scripts/README.md) for details. **Upstream is pull-only — never push to or open PRs against `oisee/vibing-steampunk`.**

### Quick Sync

```bash
# Manual sync (recommended first time)
./scripts/sync-upstream.sh

# Auto-merge and push
./scripts/sync-upstream.sh --auto-merge --push

# Or trigger GitHub Action
gh workflow run sync-upstream.yml
```

### What's Automated

- ✅ Daily checks for upstream changes (2 AM UTC)
- ✅ Auto-merge when no conflicts
- ✅ Fix import paths (`oisee` → `vinchacho`)
- ✅ Update dependencies (`go mod tidy`)
- ✅ Build & test verification
- ✅ Create PR for review
- ⚠️ CLAUDE.md updates (template provided, manual review needed)
- ⚠️ Markdown URL fixes (`oisee` → `vinchacho` in `docs/` only, NOT `articles/`)
- ⚠️ CLAUDE.md/README.md conflict resolution (script only handles `cmd/vsp/main.go`)

### Conflict Resolution Strategy

When resolving fork-vs-upstream conflicts:
- **CLAUDE.md data sections** (test counts, feature lists, codebase structure): keep fork (HEAD) — it has richer, more accurate content
- **CLAUDE.md new content** (new sections from upstream): merge in
- **README.md URLs**: keep `vinchacho` URLs, incorporate new upstream content (links, badges)
- **`docs/` markdown**: fix `oisee` → `vinchacho` in all repo URLs
- **`articles/`**: do NOT change `oisee` references — these are published upstream author content referencing their own repos (`oisee/zork-abap`, `oisee/vivid-vibes`)
