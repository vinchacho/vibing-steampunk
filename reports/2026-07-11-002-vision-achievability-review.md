# Vision Achievability Review — Is the Stated Nirvana Reachable?

**Date:** 2026-07-11
**Report ID:** 002
**Subject:** Evidence-based assessment of VISION.md's five-phase end-state ("AI as a senior developer that investigates, fixes, tests, and ships") against code reality, SAP platform limits, and delivery capacity — with a rescoped plan and next steps
**Related Documents:** [VISION.md](../VISION.md) · [ROADMAP.md](../ROADMAP.md) · [2026-07-11-001-improvement-plan-and-landscape.md](2026-07-11-001-improvement-plan-and-landscape.md) · [2025-12-21-001-tas-scripting-time-travel-vision.md](2025-12-21-001-tas-scripting-time-travel-vision.md) · [2025-12-21-002-test-extraction-isolated-replay.md](2025-12-21-002-test-extraction-isolated-replay.md) · [2026-06-15-001-issue-pr-triage-and-roadmap.md](2026-06-15-001-issue-pr-triage-and-roadmap.md)
**Method:** Five independent read-only investigations (one per vision pillar), each grounding claims in file:line evidence from the code and the project's own design reports. All verdicts below carry high confidence.

---

## 1. Verdict

**Not achievable as written — but a rescoped ~70% of it is, and the destination survives even though two of the four waypoints don't.**

Three different things went wrong, and they must not be conflated, because each demands a different response:

1. **Platform physics** kills two headline claims outright (the 0.3-second playground; true execution rewind). No amount of engineering effort inside this repo changes that; the project's own appendices already concede it.
2. **Unwired engineering** hollows out the one phase marked complete. The TAS-debugging layer's recording/checkpoint pipeline exists as tested data structures that no production code path ever invokes. This is days-to-weeks of wiring, not research.
3. **Capacity collapse** killed the calendar. Commit velocity fell ~99% from the March peak precisely when Phases 2–3 came due, and the vision phases were silently dropped from active planning rather than re-scoped.

The North Star itself — an agent that investigates, fixes, tests, and ships to transport release, under supervision — is *demonstrable with what is shipped today*. It is the four intermediate phases that overpromise, not the destination.

## 2. Scorecard

| VISION.md phase | Due | Shipped? | Achievable? | Failure mode |
|---|---|---|---|---|
| 1. TAS-style debugging | Q1 2026 | Early (2025-12-21, v2.15) — but ~60% real | **With rescoping** | Unwired engineering |
| 2. Test case extraction | Q2 2026 | Nothing | **With heavy rescoping** | Capacity + capture design gap |
| 3. Isolated playground (0.3 s) | Q3 2026 | Nothing | **Not on this platform** (as designed) | Platform physics |
| 4. Time-travel debugging | Q4 2026 | Data model only | **With rescoping** ("lite") | Platform physics + unwired |
| 5. AI swarm + North Star | 2027+ | Substrate partial | **Yes** — mostly the MCP host's job | Division-of-labor clarity |

## 3. Findings per pillar

### 3.1 Phase 1 — TAS-style debugging: shipped early, shallower than claimed

**Real:** `vsp lua` drives a genuine Lua engine (`cmd/vsp/lua.go`) with live debugger bindings — breakpoints (line/exception/statement/message/method), listen/attach/detach, step into/over/return/continue, stack, variable read/write — mapped to working ADT client methods (`pkg/adt/debugger.go:567–1135`). The variable-history data model (`pkg/adt/recorder.go`: delta compression, checkpoints, `GetVariablesAtStep`, `FindWhenChanged`) and persistence layer (`pkg/adt/history.go`: JSON recordings, index, compare) are implemented and unit-tested.

**Hollow:** ROADMAP.md marks Phase 5.2 (Variable History) and 5.3 (Checkpoint System) complete; the code says otherwise.

- `saveCheckpoint` is a stub: it stores a `_timestamp` and the literal note "Checkpoint saved - variable capture requires active debug session" — zero variables (`pkg/scripting/bindings.go:821-834`). Since `injectCheckpoint` skips `_`-prefixed keys, injecting a saved checkpoint injects nothing.
- `RecordFrame` is never called from any production path — only from tests. The Lua step functions call `DebuggerStep` but never record (`bindings.go:577-639`); the `isRecording` flag is set (`bindings.go:1094,1108`) but never read by production code (only by test assertions), so it drives no behavior. A recording driven through the shipped API completes with **0 frames**. Even the shipped example `examples/scripts/record-debug-session.lua` has its `getVariables` call commented out.
- `waitForBreakpoint` — the marquee loop in both VISION.md:50 and ROADMAP.md:71 — does not exist in the bindings. Only `listen` (initial attach) is implemented.
- Watchpoint "when X changes do Y": `luaSetWatchpoint` repurposes the external-breakpoint endpoint rather than the dedicated watchpoints API; there is no event loop to execute the "do Y", and no test.
- JS scripting never existed: `pkg/jseval` is a minimal evaluator built as an ABAP-transpilation target (`jseval.go:1-5`), with no debugger bindings.

**Platform ceiling (per the project's own reports):** ADT exposes no "snapshot all state" API; the only state-*write* primitive is `setVariableValue`; watchpoints are session-bound; debug events arrive by long-polling (2025-12-21-001:43-44,61; 2025-12-05-014:53-59,106-108). "Restore" can only ever mean re-POSTing recorded variable values into a live, paused session — not restoring stack, program counter, heap, or DB state.

### 3.2 Phases 2+3 — test extraction & playground: nothing built, and the 0.3 s claim is impossible

**Nothing shipped.** No `pkg/extraction/` (the ROADMAP-named `extractor.go` / `abap_generator.go` don't exist), no `ZCL_VSP_MOCK` ABAP class, zero `playground` references in Go code, ROADMAP Phases 6–7 entirely unchecked. (`cmd/vsp/value_extractor.go` is a CR-config-audit literal matcher — a name false-positive.)

**The capture design gap is structural, not just unimplemented.** The shipped `DBOperation` struct stores only Operation/Table/Rows/Duration (`recorder.go:27-34`) — no WHERE clause, no result-row payload — while the design (2025-12-21-002:70-76) requires both to synthesize a mock. `AddDBOperation`/`AddRFCCall` are invoked only from tests with hardcoded dummies. SQL trace can't fill the gap either: `SQLTraceEntry` (`pkg/adt/client.go:2343-2352`) carries statements and timings, not bind values or result sets. Today there is no implemented way to capture what a DB mock would need.

**The 0.3-second playground has no runtime to run in.** ABAP executes only inside the SAP kernel. The repo's compiler research points the *wrong direction* for a playground: `llvm2abap`/`wasmcomp`/`ts2abap` compile X→ABAP to run **on** SAP (wasmcomp's tests run the WASM via wazero to validate the *source* before transpiling — they never execute ABAP locally). Every test run, even fully mocked via ABAP Unit, remains an HTTP round trip plus activation. **The 100× iteration-speed claim dies; a ~10× claim (seconds, on-platform, via `CL_OSQL_TEST_ENVIRONMENT`/`CL_ABAP_TESTDOUBLE`) survives** — those frameworks are referenced in the project's reports but nothing generates or wires them yet.

### 3.3 Phase 4 — time travel: "lite" is achievable and half-built; true rewind is kernel-gated

The project's own report already scopes Q4 honestly as "Time Travel Lite — pseudo-rewind (state inspection only)" and defers full time-travel to "2027+ (if SAP provides kernel access)" (2025-12-21-001:690-703). The TPDA step types are forward-only (`debugger.go:822-828`); the project's reverse-engineered TPDA method catalog (2026-04-05-001 §11) contains no reverse-execution method; full state recording would require kernel modification — "impossible without SAP" (2025-12-21-001:299-331).

What "lite" needs mostly exists (recorder, history, `getStateAtStep`, `findWhenChanged`, `replayFromStep` Lua bindings) but is (a) not fed by live stepping (§3.1) and (b) **invisible to MCP** — no handler exposes recording/replay/history; the feature is CLI-Lua-only. An AI-driven debugger that can't reach the recordings defeats the premise.

### 3.4 Phase 5 + North Star — the swarm is the host's job; vsp's part is mostly shipped

The swarm (investigator/historian/theorist/fixer) is client-side orchestration — subagents, messaging, reasoning, and memory come free from MCP hosts (Claude Code et al.). The project knows this: "VSP is the execution layer, NOT the orchestration layer" (`plugin/skills/cba-enterprise/SKILL.md:67`). vsp's actual obligations and their status:

| Obligation | Status |
|---|---|
| Tool surface an agent can drive | ✅ universal `SAP()` tool → ~26 domain routers (`handlers_universal.go:17-113`) |
| Evidence access (traces, dumps, SQL, call graphs) | ✅ MCP-exposed, read-only |
| Safety gates for the fixer path | ✅ `pkg/adt/safety.go` (op/package gates, transport opt-in) |
| "Ships" primitive | ✅ to transport release (`transport.go:818-851`); import/approval beyond release is TMS/Cloud ALM territory — outside vsp by design |
| Recording/trace access for the "historian" | ❌ not MCP-exposed (§3.3) |
| "Learns from traces" memory | ❌ nothing; OPUS report defers feedback capture/pattern mining as "not actionable yet" (2026-01-19:431-436,935-948) |
| Workflow substrate | ⚠️ YAML engine is sequential-only with stub `transform`/`save` handlers (`pkg/dsl/workflow.go:130,479,487`) |

### 3.5 Timeline & capacity — the calendar was killed by attention, not physics

- Monthly commit cadence: **2025-12: 125 · 01: 55 · 02: 49 · 03: 194 · 04: 150 · 05: 4 · 06: 1 · 07: 7** — a ~99% collapse from the March peak, exactly at the Q2/Q3 boundary when Phases 2–3 were due.
- Phase 1 shipped a quarter *early* (all five Phase-5 commits dated 2025-12-21). The energy then went to different, real features (graph engine, transport analysis, health, auth, compilers) — v2.16→v2.39 contain no vision-phase work.
- ROADMAP.md froze at v2.15 (last touched 2025-12-22), 24 minor versions behind; its milestone line (v3.0 extraction Apr 2026, v3.2 playground Jun 2026, v4.0 time-travel Dec 2026) lapsed without comment — the version line never left 2.x.
- The vision was **silently replaced, not rescoped**: report 2026-07-11-001's roadmap (quality/DX/MCP parity) doesn't mention extraction, playground, or time-travel; upstream's own triage (2026-06-15-001) says the maintainer bottleneck is PR review throughput and its top-4 priorities are all bug fixes.

## 4. The honest nirvana (rescoped vision)

What survives contact with the platform, stated so every line is true:

1. **Scriptable forward debugging** with real event loops (`waitForBreakpoint`), functional watchpoints, and variable-history capture on every step.
2. **Time-Travel Lite:** navigate any recorded session — state at step N, find-when-X-changed, temporal queries — from the CLI *and* over MCP.
3. **Production-state replay:** capture a dump/session's variables, re-inject them into a dev session (`forceReplay`) and debug from there. This is the killer feature and it is *more* compelling than "0.3 seconds" because it is true.
4. **Recorded-execution test skeletons:** generate ABAP Unit classes from entry/exit frames (inputs → assertions), with DB mocks via SAP's own test-double frameworks where capture permits — runs in seconds, on-platform.
5. **Swarm-ready execution layer:** every capability above exposed over MCP with safety gates, so any MCP host can be the swarm.

Parked until the platform moves: true rewind and branch-fork (needs kernel access), sub-second off-platform iteration (needs an ABAP→X runtime that doesn't exist), self-healing production (governance, not code).

## 5. What to do — workstreams

Ordered by leverage per unit of effort. Estimates assume one experienced contributor; current cadence (§3.5) means W1–W3 are the realistic 2026 scope, W4 a stretch, W5 gated and likely 2027/community.

### W1 — Truth reconciliation (docs only, ~half a day) — do first
1. Rewrite VISION.md Phases 3–4 to the rescoped versions its own appendix argues for; delete the 0.3 s table; re-date phases against actual cadence; move parked items to the Wild Ideas section where they belong.
2. Fix ROADMAP.md: either un-check 5.2/5.3 sub-items that aren't real (recording pipeline, checkpoint capture) with a "wired ≠ written" note, or freeze it with a banner pointing to reports/2026-07-11-001 + this report. Don't let ✅ marks contradict the code — same "code wins, re-measure" rule CLAUDE.md now enforces for counts.
3. Cross-link this report from CLAUDE.md's Current Priorities.

### W2 — Wire the dangling Phase-1 pieces (~1–2 weeks) — highest leverage per line of code
1. Call `recorder.RecordFrame` from the Lua step functions (`luaStepOver/Into/Return/Continue`), honoring `isRecording`; capture via existing `DebuggerGetVariables`. Un-comment and fix the shipped recording example.
2. Replace the `saveCheckpoint` stub with real variable capture from the active session.
3. Implement `waitForBreakpoint` as a binding wrapping the existing `DebuggerCheckListener` long-poll.
4. Watchpoints: either implement the dedicated watchpoint API client method (per report 2025-12-05-014) or document the breakpoint-kind fallback's limits; add the missing test either way.
5. Tests: end-to-end record→`getStateAtStep` against the httptest mock; one integration test (tag `integration`) driving a live record-and-navigate cycle.
   *Exit criterion: a recording made through the shipped API contains N>0 frames and `getStateAtStep` answers from it.*

### W3 — MCP exposure of the recorder/history (~1 week, after W2)
New tools in the debugger domain (`internal/mcp/handlers_debugger.go` + `tools_register.go` + universal router): `DebugStartRecording`, `DebugStopRecording`, `DebugGetStateAtStep`, `DebugFindWhenChanged`, `DebugListRecordings`, `DebugReplayFromStep`. Safety: read paths always allowed; replay/injection behind the existing write gates. This single workstream converts "TAS debugging" from a CLI curiosity into the AI-host feature the vision describes — and it compounds with the Phase-6 MCP-parity roadmap (long-running Tasks fit debug sessions naturally).

### W4 — Time-Travel Lite completion (~2–3 weeks, stretch for Q4 2026)
Temporal queries over recordings (filter by variable/value/condition, first/last/count — the "Temporal SQL" wild idea, scoped to recordings, is actually cheap client-side); `CompareRecordings` surfaced as a diff tool; recording auto-start on listener attach (opt-in). All client-side Go — zero platform risk. This makes the original Phase-4 *calendar* commitment true in its rescoped form.

### W5 — Recorded-execution test skeletons (gated: 1-week spike first)
**Spike (decision gate):** at a breakpoint, can DB-read results realistically be captured? Options: (a) read the SELECT target variables post-statement via `DebuggerGetVariables` (fragile, per-variable); (b) extend ST05 usage if bind/result access exists anywhere; (c) punt on DB mocks entirely for v1.
**If (c):** still ship value — generate ABAP Unit skeletons from entry/exit frames only (method inputs → invocation → output assertions), with `CL_OSQL_TEST_ENVIRONMENT` scaffolding emitted as TODOs for the human. Extend `DBOperation` with `Where` + result rows regardless, so capture can improve without another schema break. ~4–6 weeks after the spike; candidate for community contribution given §3.5 capacity.

### W-kill — explicitly park (write it down, stop paying attention tax)
Off-platform playground (until an ABAP→X runtime exists — do not build); true rewind/branch-fork (until SAP exposes kernel-level record-replay); genetic/parallel-universe debugging (wild ideas stay wild); self-healing production (governance blocker, not technical).

## 6. Next steps (two-week horizon)

1. **Decide the vision's fate explicitly** — adopt §4 as the rewritten VISION.md or consciously retire the document. The current state (aspirational doc + frozen roadmap + contradicting code) is the worst of the three options. *(Owner: maintainer; 30-minute decision.)*
2. Execute **W1** the same day as the decision.
3. Open tracking issues for **W2 items 1–5** (they are independently mergeable, good-first-issue-adjacent, and the repo's contributor funnel is PR-rich per upstream triage — label them for community).
4. Start **W2.1** (RecordFrame wiring) — it unblocks everything downstream and proves the pipeline.
5. Fold **W3** into the existing Phase-6 MCP-parity plan in report 2026-07-11-001 §8 so recording tools ride the same release as Resources/Prompts work.
6. Schedule the **W5 spike** only after W2's exit criterion is met — capture feasibility is unknowable while recordings contain 0 frames.
7. Revisit this report when SAP's ABAP MCP Server GA materializes (announced Q2 2026, unverified): if SAP ships debugger access over MCP, W3's differentiation window narrows — move faster or reposition on the analysis suite.

## 7. Appendix — evidence index

| Claim | Evidence |
|---|---|
| Checkpoint is a stub | `pkg/scripting/bindings.go:821-834` |
| RecordFrame never called live | grep: defined/tested only; `bindings.go:577-639` steps don't record; `isRecording` set `:1094,1108`, read nowhere |
| waitForBreakpoint absent | VISION.md:50, ROADMAP.md:71 vs zero hits in `bindings.go` |
| Replay ≠ restore | `bindings.go:1449-1488` re-POSTs values via `DebuggerSetVariableValue` (`debugger.go:1116`); read-only failures swallowed |
| Forward-only stepping | `debugger.go:822-828`; TPDA catalog 2026-04-05-001 §11 has no reverse method |
| Kernel gate on true time-travel | 2025-12-21-001:43-44,61,299-331,690-703 |
| Mock capture structurally impossible today | `recorder.go:27-34` (no WHERE/rows; `Rows` is a count) vs design 2025-12-21-002:70-76; `client.go:2343-2352` (SQL trace carries no results); `AddDBOperation`/`AddRFCCall` called only from `recorder_test.go:318`/`:321` with dummy data |
| No extraction/playground code | no `pkg/extraction/`; grep `playground` in Go = 0; ROADMAP Phases 6–7 all unchecked |
| Transpilers point X→ABAP | `pkg/llvm2abap/README.md`, `pkg/wasmcomp/execute_test.go` (wazero runs WASM, not ABAP) |
| Recorder invisible to MCP | grep recording/replay/checkpoint in `internal/mcp` = 0; `handlers_debugger.go:21-31` |
| Execution-layer self-description | `plugin/skills/cba-enterprise/SKILL.md:56,67` |
| Learning deferred | 2026-01-19-OPUS-STRATEGIC-RECOMMENDATIONS.md:431-436,935-948 |
| Workflow engine partly hollow | `pkg/dsl/workflow.go:130,479,487` |
| Cadence collapse | git log by month: 125/55/49/194/150/4/1/7 (2025-12→2026-07) |
| Phase 1 shipped early | commits `19405b2`,`0e5c5c2`,`29e192d`,`3dd20cd`,`70fb43f` (2025-12-21) |
| Roadmap abandoned | ROADMAP.md:3,5 (Last Updated 2025-12-21; v2.15.0) vs v2.39.0 |
