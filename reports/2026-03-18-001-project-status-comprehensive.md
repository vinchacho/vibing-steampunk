# VSP Project Status Report — Comprehensive Review

**Date:** 2026-03-18
**Report ID:** 2026-03-18-001
**Subject:** Complete analysis of all issues, PRs, and strategic alignment
**Version:** v2.29.0-dev (post-decomposition)

---


## 1. Executive Summary

VSP (vibing-steampunk) is a Go-native MCP server for SAP ABAP Development Tools (ADT). As of March 18, 2026:

- **122 tools** across focused (81) and expert (122) modes
- **8 unique contributors** have merged PRs
- **13 open issues**, 4 bugs, 6 feature requests
- **12 open PRs** from 7 contributors
- **20 issues closed** in the last month
- **14 PRs merged** in the last month

Today's session delivered: strategic decomposition (Phase 1), one-tool mode (Phase 2), CLI DevOps surface (Phase 4), 4 community PRs merged, 5 bugs fixed, 6 issues closed.

---

## 2. Closed Issues (Resolved)

### #69 — License file missing
**What:** README said MIT but no LICENSE file existed.
**Resolution:** Added MIT LICENSE file. Trivial.

### #71 — CreatePackage safety check fails with empty package name
**What:** When creating a transportable package with `SAP_ALLOWED_PACKAGES` set, safety check read an empty string instead of the package being created.
**Root cause:** `checkPackageSafety` was checking `opts.PackageName` (parent) instead of `opts.Name` for package creation.
**Resolution:** Fixed in `crud.go`. Effort: 15 min.

### #70 — CreateTransport fails on S/4HANA 757
**What:** Wrong endpoint (`/cts/transports`) and content type for newer S/4HANA systems.
**Root cause:** Endpoint and XML format were for older systems. S/4HANA 757 requires `/cts/transportrequests` with `transportorganizer.v1` content type.
**Resolution:** Updated `transport.go` with correct endpoint, headers, and XML body format. Effort: 30 min.

### #54 — SAP_ALLOWED_PACKAGES blocks InstallZADTVSP
**What:** Bootstrap chicken-and-egg: install needs to create `$ZADT_VSP` but safety blocks it because it's not in the allowed list.
**Resolution:** Added `AllowPackageTemporarily()` method that temporarily adds the install target package. All install handlers use it with `defer` cleanup. Effort: 30 min.

### #52 — SyntaxCheck fails for long namespaced classes
**What:** `/source/main` suffix appended to already-long namespaced class URLs exceeds SAP URI limit.
**Resolution:** SyntaxCheck now uses bare object URL for the `checkObject` URI. Effort: 15 min.

### #33 — EditSource treats warnings as errors
**What:** Any syntax check result (including warnings) blocked saves.
**Resolution:** Merged PR #36 from kts982. EditSource now separates errors from warnings, adds `ignore_warnings` parameter.

### #58 — Local code behavior (wontfix)
### #57 — DebuggerGetVariables validation (resolved)
### #50 — Accidental PRs (housekeeping)
### #47 — OpenAI/GPT models with MCP (resolved — user config issue)
### #32 — 401 auto-retry (fixed in PR #35)
### #28 — macOS Apple Silicon build (resolved)
### #24 — DebuggerGetVariables schema (fixed in PR #25)
### #19 — Packages not found (fixed in PR #20)
### #18 — WriteSource namespaced objects (fixed)
### #17 — EditSource lock conflict (fixed)
### #15 — macOS M1 version lag (fixed)
### #13 — HTTP proxy support (fixed)
### #12 — Tool usage unclear (docs improved)

---

## 3. Open Issues — Bugs

### #55 — RunReport fails in APC context
**What:** RunReport's spool output retrieval times out because all standard SAP spool mechanisms (SUBMIT, COMMIT WORK AND WAIT, CALL FUNCTION...DESTINATION, RSTS_OPEN) are blocked inside APC WebSocket handler context.
**Impact:** RunReport via WebSocket is unreliable. Users get timeouts.
**Strategic alignment:** RunReport is a focused-mode tool. Reliability matters.
**Possible fix:** Wrapper report + cache table pattern. The ABAP side writes spool output to a Z-table, then the APC handler reads it. Requires ABAP-side changes to ZADT_VSP.
**Effort:** Medium (2-3 hours). Needs ABAP development + Go-side retry logic.
**Priority:** Medium. Workaround exists (use variants, use RunReportAsync).

### #56 — Unable to create new program
**What:** User reports "no such capability" when trying to create a program. Screenshots show the tool isn't visible.
**Impact:** User confusion. Likely a mode/configuration issue (focused mode doesn't expose CreateAndActivateProgram, only WriteSource with upsert).
**Possible fix:** Better error messages, documentation. WriteSource in focused mode handles create-if-not-exists via upsert.
**Effort:** Low (1 hour). Mostly docs/messaging.
**Priority:** Low. Not a code bug — user education.

### #43 — Missing commands
**What:** Originally about `deploy-handler` command not found. Updated to include questions about SICF activation and class naming discrepancies.
**Impact:** Confusion about installation workflow.
**Possible fix:** PR #67 fixed the naming issue. Remaining questions are documentation gaps.
**Effort:** Low. Close with documentation update.
**Priority:** Low.

### #26 — GetTransport fails
**What:** GetTransport returns "transport not found in response". User has `SAP_ENABLE_TRANSPORTS` set but system doesn't recognize it.
**Impact:** Transport feature unusable for this user.
**Possible fix:** May be related to #70 (S/4HANA endpoint differences). The fix for #70 may resolve this. Also check if `--enable-transports` flag vs env var parsing is correct.
**Effort:** Low-Medium. May already be fixed by #70.
**Priority:** Medium. Should verify after #70 fix.

---

## 4. Open Issues — Feature Requests

### #40 — i18n/Translation tools
**What:** 7 tools for managing ABAP translations across languages. GetObjectTextsInLanguage, GetDataElementLabels, GetMessageClassTexts, etc.
**Has PR:** #42 (874 additions, 6 tests)
**Strategic alignment:** Good. Translation is a common DevOps task. Extends the tool surface naturally.
**Effort:** Already implemented in PR #42. Review + merge.
**Priority:** Medium.

### #39 — gCTS tools
**What:** 10 tools for git-enabled CTS. Repository CRUD, clone, pull, commit, branch management.
**Has PR:** #41 (1,026 additions, 11 tests)
**Strategic alignment:** Good. gCTS is SAP's native Git integration. Complements our abapGit integration. Important for cloud/BTP customers.
**Effort:** Already implemented in PR #41. Review + merge.
**Priority:** Medium-High for cloud customers.

### #34 — GetTableContents pagination and schema
**What:** No pagination (offset) support. No way to get table schema without querying data.
**Has PR:** #37 (92 additions)
**Strategic alignment:** Good. Pagination is basic usability. Schema introspection helps LLMs understand data structures.
**Effort:** Already implemented in PR #37. Review + merge.
**Priority:** Medium.

### #30 — Cookie authentication docs
**What:** User wants more documentation on cookie auth — where to get cookies, what format.
**Strategic alignment:** Cookie auth is already implemented. Just needs docs.
**Effort:** Low (30 min). Add to README or docs site.
**Priority:** Low.

### #27 — More object types (AFF/NROB)
**What:** Request to support ABAP File Format (AFF) native objects like NROB (Number Range Object).
**Strategic alignment:** Good long-term. AFF is the future of ABAP object serialization. Aligns with abapGit compatibility.
**Effort:** Medium. Each object type needs URL mapping, XML parsing, and potentially different content types.
**Priority:** Low-Medium. Nice-to-have.

### #21 — Streaming HTTP support
**What:** "Sometime STDIO is not enough." Request for HTTP transport.
**Has PR:** #38 (mcp-go upgrade to v0.43.2 with streamable HTTP)
**Strategic alignment:** Critical for Docker deployment (#65) and remote/cloud usage. STDIO only works for local process spawning.
**Effort:** Already implemented in PR #38. Major dependency upgrade (37 files changed). Needs careful review.
**Priority:** High. Unblocks Docker and remote deployment.

---

## 5. Open Issues — Other

### #46 — Sync script: fix oisee references in markdown
**What:** Internal sync script improvement for fork maintenance.
**Effort:** Low. Script fix.
**Priority:** Low.

### #45 — Sync script: auto-resolve CLAUDE.md/README.md conflicts
**What:** Internal sync script improvement.
**Effort:** Low. Script fix.
**Priority:** Low.

### #2 — GUI debugger
**What:** Oldest open issue. Breakpoints set via vsp don't get hit in SAP GUI.
**Status:** Partially addressed with terminal ID feature (v2.21.0) and HTTP breakpoints. External debugger remains unreliable via REST → WebSocket (ZADT_VSP) is the recommended path.
**Effort:** High. Fundamental SAP GUI integration challenge.
**Priority:** Low. WebSocket debugger is the strategic direction.

---

## 6. Open PRs — Ready to Merge

### #37 — Table pagination + schema introspection
**Author:** kts982
**What:** Adds `offset` parameter to GetTableContents for pagination and `columns_only` flag for schema-only queries.
**Quality:** Clean, small (92 additions), live-tested. Closes #34.
**Merge recommendation:** ✅ Merge. Non-breaking, well-tested.

### #44 — Windows Quick Start docs
**Author:** mv0101
**What:** Adds Windows-specific setup instructions to README.
**Quality:** Docs-only (61 additions).
**Merge recommendation:** ✅ Merge. No risk.

### #53 — Clean Core API states
**Author:** andreasmuenster
**What:** Adds `GetAPIReleaseState` tool for clean core validation. Queries ADT API release states.
**Quality:** Small (44 additions), 4 files. Includes a build fix for GetDependencyZIP.
**Merge recommendation:** ✅ Merge after quick review. Useful for S/4HANA customers.

### #62 — Readonly mode
**Author:** marianfoo
**What:** Adds `--mode readonly` with ~46 read-only tools. Implies `--read-only` safety.
**Quality:** Clean concept (97 additions). But conflicts with our decomposed `tools_register.go` — needs adaptation.
**Merge recommendation:** ⚠️ Needs rebase. Adapt `readonlyTools` map into `tools_focused.go` pattern. Then merge.
**Effort to adapt:** 30 min.

---

## 7. Open PRs — Need Review

### #38 — mcp-go v0.43.2 with streamable HTTP
**Author:** danielheringers
**What:** Major dependency upgrade from v0.17.0 to v0.43.2. Migrates all handlers to new API. Adds `--transport http-streamable` flag.
**Quality:** Large (1,149 additions, 338 deletions, 37 files). Touches every handler file.
**Risk:** High — changes every handler's parameter extraction. Will conflict heavily with our Phase 2 routing changes.
**Strategic alignment:** Critical. Unblocks Docker (#65) and remote deployment. mcp-go is moving fast and we're 26 versions behind.
**Merge recommendation:** ⚠️ Needs careful review and rebase on our decomposed code. Consider as next major effort.
**Effort:** 2-4 hours to review, resolve conflicts, test.

### #42 — i18n/Translation tools
**Author:** Prolls
**What:** 7 new tools for ABAP translation management. New `pkg/adt/i18n.go`, `handlers_i18n.go`, 6 tests.
**Quality:** Well-structured (874 additions). Follows existing patterns. Closes #40.
**Strategic alignment:** Good. Common DevOps need.
**Merge recommendation:** ✅ Merge after review. May need tool registration updates for our new structure.
**Effort:** 1 hour to review and adapt registration.

### #41 — gCTS tools
**Author:** Prolls
**What:** 10 new tools for git-enabled CTS. New `pkg/adt/gcts.go`, `handlers_gcts.go`, 11 tests.
**Quality:** Well-structured (1,026 additions). Closes #39.
**Strategic alignment:** Good for cloud/BTP customers. Complements abapGit.
**Merge recommendation:** ✅ Merge after review. Same adaptation needed as #42.
**Effort:** 1 hour.

---

## 8. Open PRs — Draft/Blocked

### #66 — Integration test infrastructure overhaul
**Author:** marianfoo
**Status:** Draft. Part A of larger effort.
**What:** Overhauls integration test suite for Docker-based SAP Cloud Developer Trial. 23 new tests, helper functions, bug fixes.
**Strategic alignment:** Excellent. Automated testing is critical for quality with growing contributor base.
**Blockers:** None, but draft status suggests still in progress.
**Effort:** Review when author marks ready.

### #65 — Docker support
**Author:** marianfoo
**Status:** Draft. Blocked on HTTP transport (#38).
**What:** Dockerfile, GitHub Actions for GHCR, documentation.
**Strategic alignment:** High. Docker is the standard for cloud deployment.
**Blockers:** Needs HTTP streamable transport. STDIO doesn't work well in containers.

### #64 — Future plans
**Author:** marianfoo
**Status:** Draft. Planning document, not code.

### #63 — MkDocs documentation site
**Author:** marianfoo
**What:** Full documentation website with MkDocs Material theme. 15 doc pages, GitHub Actions auto-deploy.
**Strategic alignment:** Good. Professional docs attract enterprise users.
**Merge recommendation:** ⚠️ Review content accuracy. Large (2,562 additions).
**Effort:** 1-2 hours to review content.

---

## 9. Merged PRs (Recent) — Contributors

| PR | Author | What | Date |
|----|--------|------|------|
| #72 | oisee | Strategic decomposition + one-tool + CLI | 2026-03-18 |
| #68 | dominik-kropp | Fix ExportToFile for function modules | 2026-03-18 |
| #67 | AndreaBorgia-Abo | Fix class name reference | 2026-03-18 |
| #61 | marianfoo | Automated release workflow | 2026-03-13 |
| #60 | marianfoo | GetDependencyZIP function | 2026-03-12 |
| #59 | thm-ma | CLI source --parent/--include/--method | 2026-03-18 |
| #36 | kts982 | EditSource ignore_warnings | 2026-03-18 |
| #35 | kts982 | 401 auto-retry | 2026-03-13 |
| #25 | marianfoo | DebuggerGetVariables schema fix | 2026-03-12 |
| #20 | ingenium-it-engineering | Package $ name fix | 2026-02-04 |
| #14 | kts982 | Transport API fix + EditSource transport | 2026-02-01 |
| #6 | vitalratel | MoveObject + WebSocket refactor | 2026-01-07 |
| #4 | vitalratel | RunReport background jobs | 2026-01-07 |
| #3 | vitalratel | CLI mode + method breakpoints | 2026-01-06 |

**8 unique contributors:** oisee, marianfoo, kts982, dominik-kropp, AndreaBorgia-Abo, thm-ma, vitalratel, ingenium-it-engineering

---

## 10. Strategic Priorities

### Immediate (this week)
1. **Merge #37, #44, #53** — easy wins, no conflicts
2. **Adapt and merge #62** — readonly mode (30 min)
3. **Close #43** — documentation update
4. **Verify #26** — may be fixed by #70

### Short-term (next 2 weeks)
5. **Review and merge #38** — mcp-go upgrade. Critical path for Docker.
6. **Review and merge #42, #41** — i18n and gCTS tools from community
7. **Review #63** — documentation site
8. **Fix #55** — RunReport APC workaround

### Medium-term (next month)
9. **Phase 3** — WASM ABAP parser integration
10. **Merge #65** — Docker support (after #38)
11. **Merge #66** — Integration test infrastructure
12. **Address #27** — AFF object types

### Backlog
- #30 — Cookie auth docs
- #2 — GUI debugger improvements
- #45, #46 — Sync script enhancements
- Cross-system `vsp copy --from source -s target` command

---

## 11. Project Health

| Metric | Value | Trend |
|--------|-------|-------|
| Open issues | 13 | ↓ (was 19) |
| Open PRs | 12 | Stable |
| Contributors (active) | 8 | ↑ |
| Test count | 244 unit + 34 integration | Stable |
| Tool count | 122 (granular) / 1 (universal) | ↑ New mode |
| CLI commands | 12 | ↑ (was 5) |
| Code size | ~15K LOC Go | Stable (decomposed, not grown) |

The project is healthy. Community is active with quality contributions. The decomposition work keeps the codebase maintainable as it grows.

---

---
