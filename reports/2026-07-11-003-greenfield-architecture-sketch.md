# Greenfield Architecture Sketch — What We'd Build Starting Today

**Date:** 2026-07-11
**Report ID:** 003
**Subject:** If vsp were started from scratch in July 2026: the verb surface, capability tier ladder, MCP-2026 primitive mapping, and repo layout we would choose — written to double as a convergence target for the existing codebase, not a rewrite proposal
**Related Documents:** [2026-07-11-001-improvement-plan-and-landscape.md](2026-07-11-001-improvement-plan-and-landscape.md) · [2026-07-11-002-vision-achievability-review.md](2026-07-11-002-vision-achievability-review.md) · [2026-02-01-001 one-tool mode design] · [VISION.md](../VISION.md)
**Method:** Three parallel read-only design investigations grounded every table below in the actual code (tool inventory, safety model, ADT operation characteristics). All file:line citations verified against the tree.

---

## 1. Purpose and framing

Reports 001 and 002 established what went wrong: tool-count sprawl followed by a retreat to one tool, safety bolted on after an unrestricted default, features shipped as unwired horizontal layers, docs drifting from code, distribution as an afterthought, and a maintainer bottleneck the architecture does nothing to relieve. This report answers "what would we do differently starting today?" — concretely enough that each section is also a convergence milestone for the code that already exists. **Nothing here proposes a rewrite.** The existing `pkg/adt` client, the universal router, and the safety machinery are the raw material; the sketch is the shape they converge to.

Six principles, one line each (rationale in report 002 §5 and the session findings):

1. **One verb surface, designed first** — never 153 tools; the retreat to hyperfocused proved the destination, so start there.
2. **2026 MCP primitives are the skeleton, not parity items** — Tasks, Resources, Prompts, elicitation, OAuth'd HTTP.
3. **Safe by default, capability tiers as the core abstraction** — one dial, not nine knobs.
4. **Vertical slices; the demo is a test** — nothing merges without an end-to-end executable proof.
5. **Position for the post-SAP-MCP world** — analysis depth and on-prem/ECC are the moat; CRUD is commoditized.
6. **Design for the maintainer bottleneck** — research off-trunk, docs generated, CI from commit one.

## 2. The verb surface

### 2.1 What the current router taught us

The universal `SAP(action, target, params)` tool (`internal/mcp/handlers_universal.go:18-42`) chains 26 route functions in fixed order, first-match-wins (`:72-113`). Its real vocabulary is ~12 actions, each re-dispatching on object type — evidence that a 12-verb surface losslessly covers the product. Two findings fall out of the enumeration:

- **Four domains are register-only and unreachable from the default mode**: the 10-tool gCTS suite (`handlers_gcts.go`), revisions (`handlers_revisions.go`), i18n (`handlers_i18n.go`), and `AnalyzeABAPCode` (`handlers_codeanalysis.go`) have no `route*` function — a user on hyperfocused (the default) cannot reach them at all. A from-scratch design closes this by construction: if a capability exists, it is reachable from the one tool.
- **Dead weight is identifiable**: superseded single-type getters (`GetProgram/GetClass/...`, replaced by `GetSource` per its own description at `handlers_source.go:91`), a duplicated legacy debugger route, and installer/bootstrap tools mixed into the development surface.

### 2.2 The 12 verbs

| Verb | Sub-actions | Absorbs (current routes/tools) | Example |
|---|---|---|---|
| **read** | source, table, structure, package, transaction, message-class, type-info, cds, api-state, texts, revision | source/read routes, class-include reads, i18n reads, revisions | `read source "CLAS ZCL_DEMO"` |
| **search** | objects, grep, query | SearchObject, GrepObject/GrepPackage, table-contents/SQL query | `search grep {pattern, package}` |
| **nav** | definition, references, completion, callers, callees, structure | code-intel + call-graph navigation | `nav references "CLAS ZCL_DEMO" {line, col}` |
| **write** | create, edit, patch, delete, rename, clone, move, deploy, save, texts, publish | CRUD, workflow writers, class includes, service bindings, file I/O, i18n writes | `write edit "CLAS ZCL_DEMO" {source}` |
| **check** | syntax, atc, activate, pretty-print, inactive, lint | syntax check, ATC, activation, abaplint | `check atc "CLAS ZCL_DEMO"` |
| **test** | unit, coverage, results | unit tests, coverage, check runs | `test unit "CLAS ZCL_DEMO"` |
| **run** | abap, report, report-async, async-result, rfc | execute_abap, report execution, RFC | `run report {name, variant}` |
| **debug** | breakpoint, step, stack, vars, listen/attach/detach, amdp, dumps, traces, sqltrace, **recording** | both debugger routes (merged), AMDP, dumps/traces/SQL-trace — plus the recorder, MCP-exposed at last (report 002 W3) | `debug breakpoint "PROG ZDEMO" {line}` |
| **analyze** | impact, boundaries, call-graph, trace, health, co-change, where-used, usage, parse, deps | the analysis suite + context compression | `analyze impact "DDLS ZI_DEMO"` |
| **transport** | list, get, create, release, delete, info, compare | CTS ops + version compare | `transport create {desc}` |
| **git** | abapgit-export, gcts-clone/pull/commit/... | abapGit route + the currently-unreachable gCTS suite | `git gcts-pull {repo}` |
| **admin** | system-info, components, connection, features, setup | system route; installers demoted to `admin setup` | `admin system-info` |

Deliberately dropped or demoted: superseded single-type getters (aliases during convergence, then gone), the legacy debugger route (merge into `debug`), `RecoverFailedCreate` as a user-facing tool (becomes internal retry logic), installers (to `admin setup`), i18n write-side and comparison miscellany (extras).

### 2.3 Token-lean output envelope

Today's outputs are two inconsistent shapes: raw text with a dependency prologue *appended into the source body* (`handlers_source.go:196-200` — the "Context stats" comment line pollutes the code the model edits), and `json.MarshalIndent` array-of-objects tables where every column name repeats for all 100 default rows (`handlers_read.go:274-295`). Errors are free text (`"X failed: %v"`), unbranchable by agents.

One envelope for every verb, emitted compact (`json.Marshal`, never `MarshalIndent`):

```json
{ "ok": true,
  "data": { "source": "…raw ABAP, never JSON-escaped into oblivion…" },
  "meta": { "type": "CLAS", "name": "ZCL_DEMO", "lines": 420, "deps": {"found": 8, "resolved": 7} },
  "hints": ["method GET is inactive — run check activate"],
  "cursor": "eyJvZmZzZXQiOjEwMH0" }
```

Rules: source stays raw inside `data.source` with dependency stats moved to `meta`; tabular data goes **columnar** (`{columns:[…], rows:[[…]]}`) killing per-row key repetition; errors use the same envelope (`{ok:false, error:{code:"LOCKED", msg, hint}}`) so agents branch on `code`; `hints[]` surfaces the next-action affordances currently buried in prose; `cursor` standardizes the ad-hoc offset slicing already in `GetTableContents`.

## 3. The capability tier ladder

Today safety is nine orthogonal knobs assembled by hand (`pkg/adt/safety.go:9-73`) on top of an unrestricted runtime default (`internal/mcp/server.go:135`). The 13 op-types (`safety.go:117-131`) are good atoms; the ladder is one ordered dial over them. Tier is orthogonal to `--mode` (which governs tool *visibility*); tier governs what a call may *do*, enforced at the existing `checkMutation` chokepoint (`pkg/adt/mutation_gate.go:73-90`).

| Tier | Op-types | Compiles to | For |
|---|---|---|---|
| **read** *(default)* | R S Q T I | `= DefaultSafetyConfig()` (`safety.go:76-82`) | Browse, understand, run existing tests. Zero writes, zero ad-hoc SQL. |
| **analyze** | + F | `ReadOnly:true, BlockFreeSQL:false` | Adds free SQL for data investigation; still no object writes. |
| **write-local** | + C U A L W | writes on, `DisallowedOps:"DX"`, `AllowedPackages:["$TMP","$*"]` | Create/edit/activate in local packages only; no transports ever. |
| **write-transportable** | + X(read) | `AllowTransportableEdits:true, EnableTransports:true, TransportReadOnly:true` | Edits in transportable packages against an existing transport; can discover transports, cannot create/release them. |
| **ship** | + D, X(write) | everything on | Full lifecycle: delete, create/release transports. |

Each tier strictly contains the one below — a single ordinal, a one-token escalation, one value in the audit trail.

**Elicitation rides on top of the tier** (net-new — no elicitation code exists today): free SQL (analyze+), transport-attached edits (write-transportable+), delete, transport release (ship) each require a per-call human confirm even when the tier permits them. Non-elicitation clients fall back to tier-only enforcement (open question §6.3). `--yes`/`SAP_ELICIT=off` exists for CI, documented as dangerous.

**Surface:** one flag `--tier`, env `SAP_TIER`, default `read`.

**Migration without breakage:** legacy flags compose as *tighteners* on the tier baseline — `--read-only` clamps the tier, `--disallowed-ops` unions, `--allowed-packages` narrows, `--allow-transportable-edits`/`--enable-transports` remain valid mid-ladder opt-ins. The single intentional behavior change is no-flags: unrestricted → `read`. Back-compat shim: if any legacy safety flag is explicitly set and `--tier` isn't, reconstruct the old behavior and skip the tier default; if nothing is set, default `read` with a one-line notice pointing at `--tier ship` for the old behavior, for one deprecation window. No currently-working locked-down deployment gets loosened; no scripted invocation breaks.

## 4. 2026 MCP primitives as the skeleton

### 4.1 Long-running Tasks

The codebase already contains one bespoke async system — `AsyncTask` + in-memory map (`server.go:17,39`) wired only for report execution (`handlers_report.go:157-258`, goroutine + 5-minute poll + client re-poll via `GET_ASYNC_RESULT`). That *is* an MCP Task in disguise. Everything with a status/worklist-id pair migrates to one generic Task registry:

| Operation | Today | Becomes |
|---|---|---|
| Report execution | bespoke asyncTasks map + polling (`reports.go:87,192`) | MCP Task (first to migrate; drop the sync variant) |
| ATC run | blocking chain (`devtools.go:1002→1070→1203`) | Task with progress; findings land as a Resource |
| Unit tests + coverage (package-wide) | blocking round trip (`devtools.go:628`) | Task; single-object stays sync |
| Mass activation | blocking POST (`devtools.go:159`) | Task; single object stays sync |
| gCTS clone/pull/commit | blocking (`gcts.go:211,235,267`) | Task |
| abapGit full-package export | WebSocket round trip (`git.go:59`) | Task |
| Transport release | blocking, can run minutes (`transport.go:298,818`) | Task **+ elicitation** |

Debug sessions are the natural stretch case: a `debug listen` that today long-polls becomes a Task emitting breakpoint-hit progress events.

### 4.2 Resources

Resource capability is *already advertised and empty* — `server.WithResourceCapabilities(true, true)` at `server.go:194` with zero `AddResource` calls anywhere. Fill it:

| Resource | URI |
|---|---|
| Object source | `adt://{SYS}/CLAS/ZCL_DEMO` (system key from `config.SystemsConfig`, native multi-system) |
| Class include | `adt://{SYS}/CLAS/ZCL_DEMO?include=testclasses` |
| Table/DDIC schema | `adt://{SYS}/TABL/SFLIGHT` |
| Package tree | `adt://{SYS}/DEVC/ZPKG?depth=1` |
| ATC worklist / test results | `adt://{SYS}/atc/worklist/{id}` (Task outputs) |
| Dependency graph | `adt://{SYS}/graph/CLAS/ZCL_DEMO` |
| Debug recordings | `vsp://recordings/{id}` (local artifacts, distinct authority) |

Reads as Resources let hosts cache source without burning tool calls — today every read is a round trip.

### 4.3 Prompts

The DSL already models `Pipeline → Stage → Step` (`pkg/dsl/types.go:151-168`) and three YAML workflows ship in `examples/workflows/` (quality-gate, ci-pipeline, test-package). Those are MCP Prompts with parameters — the front door for canned multi-step flows, with the `workflows_*.go` verbs as the library underneath.

### 4.4 Inbound auth — the gap that matters for remote deployment

All existing auth is *outbound* (vsp→SAP: basic, cookies, SAML via IAS, chromedp browser SSO). The MCP HTTP endpoint itself is **unauthenticated** — `ServeHTTP` starts the streamable server with no middleware (`server.go:237-239`). Greenfield: OAuth 2.1 resource-server in front of the HTTP transport (bearer validation + protected-resource metadata), validated principal mapped onto per-system SAP credentials from `config.SystemsConfig`. stdio remains trust-the-parent.

## 5. Repo layout: core / extras / siblings

| Component | Verdict | Why |
|---|---|---|
| `pkg/adt`, `internal/mcp`, `pkg/config`, `pkg/cache` | **core** | The client, the server, connections, persistence. |
| `embedded/abap` (ZADT_VSP) | **core, optional at runtime** | Powers WebSocket features (reports/debug/git/RFC); install stays strictly opt-in (`admin setup`), never assumed. |
| `pkg/graph`, `pkg/ctxcomp`, `pkg/dsl`, `pkg/abaplint`, `pkg/scripting`, `internal/lsp` | **extras** | Optional modules behind build tags/flags; graph/ctxcomp feed Resources, dsl feeds Prompts. |
| `pkg/jseval`, `pkg/ts2abap`, `pkg/ts2go`, `pkg/llvm2abap`, `pkg/wasmcomp` | **siblings** | Research compilers sharing nothing with the ADT client; own repos, own cadence, out of the 26 MB binary. |

## 6. Open design questions

Curated from the investigations — real decisions, not bikeshed:

1. **`--allowed-ops` semantics under tiers:** intersect with the tier whitelist (safer) vs replace it (preserves today's exact semantics). Recommendation: replace during the deprecation window, intersect after.
2. **Task durability:** in-memory registry (status quo) vs durable across server restarts. SAP-side ids (jobs, worklists, transports) are already durable; recommendation: persist the id map in `pkg/cache` SQLite, cheap and sufficient.
3. **Elicitation fallback:** on non-elicitation clients, do delete/release/free-SQL hard-fail or degrade to tier-only? Recommendation: tier-only + stderr notice, so stdio pipelines keep working.
4. **`write-transportable` default package scope:** open, or conservative `Z*`/`Y*` customer-namespace whitelist? Recommendation: `Z*,Y*,$*` default, override with `--allowed-packages`.
5. **Git backend:** abapGit export vs the gCTS suite as canonical — keeping both doubles the surface. Needs a maintainer call; gCTS is the strategic SAP direction, abapGit is the installed-base reality.
6. **UI5/BSP:** first-class verb vs `admin` parking (chosen here) vs folding into read/write with `UI5_*` targets. Low frequency argues for parking.
7. **Multi-tenant OAuth:** one principal ↔ one SAP system, or token claims selecting among systems? Defer until remote deployment is real.

## 7. Convergence path for the existing fork (strangler, not rewrite)

Ordered so each step is independently shippable and none breaks users:

1. **Envelope + tier ladder** (small, high leverage): add `adt.SafetyConfigForTier()` beside the existing presets, `--tier` flag with the back-compat shim, and the output envelope behind the universal tool first. This alone fixes the two worst DX/safety defects. *(Extends report 001's Phase 4 "safe-by-default" decision with the full spec.)*
2. **Close the reachability gap:** route gCTS/revisions/i18n/lint through the universal router; retire superseded getters to aliases; merge the legacy debugger route. Exit criterion: every registered capability reachable from hyperfocused mode.
3. **Primitives:** migrate `asyncTasks` → MCP Tasks (reports first, then ATC/tests/activation/transport-release), register the Resources (capability bit is already on), ship the three YAML workflows as Prompts, add recording tools to `debug` (report 002 W3 — same milestone).
4. **Layout + inbound auth:** extract the five sibling packages to their own repos, put extras behind build tags, add OAuth 2.1 middleware to the HTTP transport.

Steps 1–2 are weeks; step 3 is the 2026 MCP-parity roadmap from report 001 §8 with a concrete spec; step 4 is housekeeping that can trail. Every step shrinks the surface the single maintainer must review — which report 002 identified as the binding constraint.

## 8. What deliberately stays the same

Go single-binary (distribution is a strength), ADT-REST-native core (zero-footprint adoption), the `pkg/adt` client as the substrate, the 13 op-type taxonomy (good atoms — the ladder reuses them), the analysis suite as the moat, ZADT_VSP as *optional* superpowers, and the multi-system `config.SystemsConfig` model (it slots directly into the Resource URI scheme and future multi-tenant auth).

## 9. Evidence appendix (representative)

| Claim | Citation |
|---|---|
| Universal router: 26 fixed-order routes, first-match-wins | `internal/mcp/handlers_universal.go:18-42,72-113` |
| gCTS/revisions/i18n/lint unreachable from default mode | no `route*` for `handlers_gcts.go`, `handlers_revisions.go`, `handlers_i18n.go`, `handlers_codeanalysis.go` |
| Single-type getters superseded by GetSource | `tools_register.go:111-165`; `handlers_source.go:91` |
| Context stats appended into source body | `handlers_source.go:196-200` |
| Indented array-of-objects table output | `handlers_read.go:274-295` |
| Unrestricted runtime default vs safe preset | `internal/mcp/server.go:135` vs `pkg/adt/safety.go:76-82` |
| Op-type atoms and gates | `pkg/adt/safety.go:117-131,143-158,254-259,303-324` |
| Mutation chokepoint for tier + elicitation enforcement | `pkg/adt/mutation_gate.go:35-90` |
| Bespoke async system, reports only | `server.go:17,39`; `handlers_report.go:157-258` |
| Blocking ops that become Tasks | `devtools.go:159,628,1002-1203`; `gcts.go:211-267`; `git.go:59`; `transport.go:298,818` |
| Resource capability advertised, zero registered | `server.go:194`; no `AddResource` in tree |
| MCP HTTP endpoint unauthenticated | `server.go:237-239` |
| Workflow model for Prompts | `pkg/dsl/types.go:151-168`; `examples/workflows/*.yaml` |
| No elicitation anywhere today | grep `Elicit` across `*.go` = 0 |
