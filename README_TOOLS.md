# vsp Tool Reference

Complete documentation for all 157 MCP tools available in vsp.

**Mode Legend:**
- **Focused** - Available in focused mode (103 whitelisted tools, plus the 3 always-on tools below)
- **Expert** - Only available in expert mode (157 tools total: 154 mode-gated + 3 always-on)
- **Always** - Registered in both focused and expert mode (`GetConnectionInfo`, `GetFeatures`, `GetAbapHelp`)

> **Default mode is `hyperfocused`**: a single universal `SAP` tool that routes to all operations below. Use `--mode focused` or `--mode expert` to expose the individual tools documented here.

---

## Unified Tools (2 tools) - Focused Mode

These tools replace 11 granular read/write operations with intelligent parameter-based routing:

| Tool | Description | Mode |
|------|-------------|------|
| `GetSource` | Unified read for any ABAP source. Parameters: `type` (PROG/CLAS/INTF/FUNC/FUGR/INCL/DDLS/VIEW/BDEF/SRVD/SRVB/MSAG), `name`, optional `parent` (for FUNC), optional `include` (for CLAS). | Focused |
| `WriteSource` | Unified write with auto-upsert. Parameters: `type` (PROG/CLAS/INTF/DDLS/BDEF/SRVD), `name`, `source`, `mode`, `options`. Supports create and update for classic ABAP and RAP types. | Focused |

**Benefits:** 70% token reduction, simplified tool selection, extensible for new types.

**RAP Support (NEW):** WriteSource now supports creating and updating CDS views (DDLS), behavior definitions (BDEF), and service definitions (SRVD).

**Impact-gated writes:** with `--impact-gate advise|block`, write results carry an `impact` blast-radius block (callers, packages, recent transports, risk tier, advice). In `block` mode a high-impact write is refused with an `impact-confirm-...` token; the 13 gated write tools — `WriteSource`, `EditSource`, `UpdateSource`, `DeleteObject`, `RenameObject`, `UpdateClassInclude`, `WriteProgram`, `WriteClass`, `DeployFromFile`, `ImportFromFile`, `DeployZip`, `InstallZADTVSP`, `InstallDummyTest` — accept an optional `confirm` parameter to retry the identical call with that token. The hyperfocused SAP tool forwards `params.confirm` for the same actions.

---

## Search & Grep Tools (5 tools)

| Tool | Description | Mode |
|------|-------------|------|
| `SearchObject` | Quick search for ABAP objects by name pattern | Focused |
| `GrepObjects` | Regex search across multiple objects (array of URLs) | Focused |
| `GrepPackages` | Regex search across packages with recursive subpackage support | Focused |
| `GrepObject` | Regex search in single object | Expert |
| `GrepPackage` | Regex search in single package | Expert |

**Grep Features:**
- Full regex support (Go regexp syntax)
- Case-sensitive or case-insensitive matching
- Context lines (like `grep -C`)
- Object type filtering
- Max results limit

---

## Read Operations (18 tools)

| Tool | Description | Mode |
|------|-------------|------|
| `GetProgram` | Get ABAP program source | Expert |
| `GetClass` | Get ABAP class source | Expert |
| `GetInterface` | Get ABAP interface source | Expert |
| `GetFunction` | Get function module source | Expert |
| `GetFunctionGroup` | Get function group structure | Focused |
| `GetInclude` | Get ABAP include source | Expert |
| `GetTable` | Get table structure definition | Focused |
| `GetTableContents` | Get table data (supports SQL filtering) | Focused |
| `GetStructure` | Get structure definition | Expert |
| `GetPackage` | Get package contents | Focused |
| `GetTransaction` | Get transaction details | Expert |
| `GetTypeInfo` | Get data type information | Expert |
| `GetCDSDependencies` | Get CDS view dependency tree | Focused |
| `GetCDSImpactAnalysis` | Get CDS view reverse dependencies (where-used / downstream consumers) | Focused |
| `GetCDSElementInfo` | Get metadata for all elements (fields) of a CDS view: names, types, annotations | Focused |
| `GetMessages` | Get all messages from an ABAP message class (SE91) | Focused |
| `GetAPIReleaseState` | Check API release state for S/4HANA Clean Core / ABAP Cloud compatibility | Focused |
| `RunQuery` | Execute freestyle SQL query | Focused |

---

## System Information (5 tools)

| Tool | Description | Mode |
|------|-------------|------|
| `GetSystemInfo` | Get SAP system information (SID, release, kernel, database) | Focused |
| `GetInstalledComponents` | List installed software components with versions | Focused |
| `GetConnectionInfo` | Get current MCP connection info: user, URL, client | Always |
| `GetFeatures` | Probe SAP system for available optional capabilities (abapGit, RAP/OData, AMDP, UI5/BSP, CTS) | Always |
| `GetAbapHelp` | Get ABAP keyword documentation (SAP Help Portal URL; real docs when ZADT_VSP is installed) | Always |

---

## Code Analysis (10 tools)

| Tool | Description | Mode |
|------|-------------|------|
| `GetCallGraph` | Get call hierarchy (callers/callees) for methods/functions | Focused |
| `GetObjectStructure` | Get object explorer tree structure | Focused |
| `GetCallersOf` | Get who calls this object (static call graph - up traversal) | Focused |
| `GetCalleesOf` | Get what this object calls (static call graph - down traversal) | Focused |
| `AnalyzeCallGraph` | Get statistics about call graph (nodes, edges, depth, types) | Focused |
| `CompareCallGraphs` | Compare static vs actual execution for test coverage analysis | Focused |
| `TraceExecution` | **COMPOSITE RCA TOOL**: Static graph + trace + comparison for root cause analysis | Focused |
| `CheckBoundaries` | Analyze package boundary violations (same-package, SAP-standard, whitelisted, cross-package Z* deps, dynamic calls) | Focused |
| `GraphStats` | Extract dependency statistics from ABAP source using the embedded parser (works offline) | Expert |
| `GetContext` | Analyze source dependencies and return compressed public API contracts of referenced objects | Focused |

---

## Development Tools (11 tools)

| Tool | Description | Mode |
|------|-------------|------|
| `SyntaxCheck` | Check source code for syntax errors | Focused |
| `Activate` | Activate an ABAP object | Focused |
| `ActivateMultiple` | Batch activate specific objects in one request (dependency-aware, like Eclipse) | Focused |
| `ActivatePackage` | Batch activate all inactive objects in package | Focused |
| `RunUnitTests` | Execute ABAP Unit tests | Focused |
| `RunATCCheck` | Run ATC code quality checks | Focused |
| `CompareSource` | Unified diff between any two ABAP objects | Focused |
| `CloneObject` | Copy PROG/CLAS/INTF to new name | Focused |
| `GetClassInfo` | Quick class metadata (methods, attrs, interfaces) | Focused |
| `CreateTable` | Create DDIC table from JSON definition | Focused |
| `CreatePackage` | Create local package ($...) | Focused |

---

## ATC (Code Quality) Tools (2 tools)

| Tool | Description | Mode |
|------|-------------|------|
| `RunATCCheck` | Run ATC check, returns findings with priority (1=Error, 2=Warning, 3=Info) | Focused |
| `GetATCCustomizing` | Get ATC system configuration | Expert |

**Example ATC Output:**
```json
{
  "summary": { "totalFindings": 3, "errors": 1, "warnings": 2 },
  "worklist": {
    "objects": [{
      "name": "ZCL_TEST",
      "findings": [{ "priority": 1, "checkTitle": "Syntax Check", "line": 42 }]
    }]
  }
}
```

---

## CRUD Operations (7 tools)

| Tool | Description | Mode |
|------|-------------|------|
| `LockObject` | Acquire edit lock | Focused |
| `UnlockObject` | Release edit lock | Focused |
| `CreateObject` | Create new object (program, class, interface, include, function group, function module, package, **DDLS, BDEF, SRVD, SRVB**) | Expert |
| `UpdateSource` | Write source code | Expert |
| `DeleteObject` | Delete an object | Expert |
| `MoveObject` | Move an object to a different package (via ZADT_VSP WebSocket, TR_TADIR_INTERFACE) | Focused |
| `RecoverFailedCreate` | Recover a zombie object left behind by a failed CreateObject (probe + compensating cleanup) | Expert |

**RAP Object Creation (NEW):** CreateObject now supports:
- `DDLS/DF` - CDS DDL Source (view definitions)
- `BDEF/BDO` - Behavior Definition
- `SRVD/SRV` - Service Definition
- `SRVB/SVB` - Service Binding (requires `service_definition`, optional `binding_version`, `binding_category`)

---

## Service Binding Operations (2 tools) - NEW

| Tool | Description | Mode |
|------|-------------|------|
| `PublishServiceBinding` | Publish a service binding to make it available as OData service | Expert |
| `UnpublishServiceBinding` | Unpublish a service binding | Expert |

**Parameters:**
- `service_name` (required) - Service binding name
- `service_version` (default: "0001")

---

## Class Include Operations (4 tools)

| Tool | Description | Mode |
|------|-------------|------|
| `GetClassInclude` | Get class include (definitions, implementations, macros, testclasses) | Expert |
| `CreateTestInclude` | Create test classes include | Expert |
| `UpdateClassInclude` | Update class include source | Expert |
| `GetClassComponents` | Get class structure: methods, attributes, events with visibility and properties | Expert |

---

## Workflow Tools (5 tools)

Composite operations that combine multiple ADT API calls:

| Tool | Description | Steps | Mode |
|------|-------------|-------|------|
| `EditSource` | **Surgical string replacement** (matches Edit tool pattern) | GetSource → FindReplace → SyntaxCheck → Lock → Update → Unlock → Activate | Focused |
| `WriteProgram` | Update program with activation | Lock → SyntaxCheck → Update → Unlock → Activate | Expert |
| `WriteClass` | Update class with activation | Lock → SyntaxCheck → Update → Unlock → Activate | Expert |
| `CreateAndActivateProgram` | Create new program | Create → UpdateSource → Activate | Expert |
| `CreateClassWithTests` | Create class with unit tests | Create → Lock → Update → CreateTestInclude → WriteTests → Unlock → Activate → RunUnitTests | Expert |

---

## File-Based Deployment Tools (6 tools)

Solves token limit problem for large files:

| Tool | Description | Mode |
|------|-------------|------|
| `ImportFromFile` | **File → SAP** - Smart deploy with auto create/update detection | Focused |
| `ExportToFile` | **SAP → File** - Save object source to local file | Focused |
| `DeployFromFile` | Legacy name for ImportFromFile | Expert |
| `SaveToFile` | Legacy name for ExportToFile | Expert |
| `RenameObject` | Rename object by creating copy | Expert |
| `DeployZip` | Deploy objects from an abapGit-format ZIP to a SAP package via ADT | Focused |

**Supported Extensions:**
- `.clas.abap` - Classes
- `.prog.abap` - Programs
- `.intf.abap` - Interfaces
- `.fugr.abap` - Function Groups
- `.func.abap` - Function Modules
- `.ddls.asddls` - CDS DDL Sources (ABAPGit format)
- `.bdef.asbdef` - Behavior Definitions (ABAPGit format)
- `.srvd.srvdsrv` - Service Definitions (ABAPGit format)

---

## Code Intelligence Tools (7 tools)

| Tool | Description | Mode |
|------|-------------|------|
| `FindDefinition` | Navigate to symbol definition | Focused |
| `FindReferences` | Find all references to symbol | Focused |
| `CodeCompletion` | Get code completion suggestions | Expert |
| `PrettyPrint` | Format ABAP source code | Focused |
| `GetPrettyPrinterSettings` | Get formatter settings | Expert |
| `SetPrettyPrinterSettings` | Update formatter settings | Expert |
| `GetTypeHierarchy` | Get type hierarchy (supertypes/subtypes) | Expert |

---

## Transport Tools (8 tools)

| Tool | Description | Mode |
|------|-------------|------|
| `CreateTransport` | Create transport request | Expert |
| `GetTransportInfo` | Get transport details | Expert |
| `ReleaseTransport` | Release transport | Expert |
| `GetUserTransports` | List user's transports | Expert |
| `GetInactiveObjects` | List inactive objects | Focused |
| `ListTransports` | List modifiable transport requests for a user (requires transport flags) | Focused |
| `GetTransport` | Get detailed transport information including objects and tasks | Focused |
| `DeleteTransport` | Delete a modifiable transport request | Expert |

---

## ExecuteABAP (Expert Mode)

Execute arbitrary ABAP code via unit test wrapper:

| Tool | Description | Mode |
|------|-------------|------|
| `ExecuteABAP` | Run ABAP code and capture output | Expert |

**Risk Levels:**
- `harmless` - Read-only, no external calls
- `dangerous` - Can write to DB, call external
- `critical` - Full system access

See [ExecuteABAP Implementation Report](reports/2025-12-05-004-execute-abap-implementation.md) for details.

---

## Runtime Errors / Short Dumps (2 tools) - RABAX

| Tool | Description | Mode |
|------|-------------|------|
| `ListDumps` | List runtime errors with filters (user, exception type, program, date range) | Focused |
| `GetDump` | Get full details of a specific dump including stack trace | Focused |

**Use Cases:**
- Monitor system health by checking recent dumps
- Debug production issues by examining dump details
- Track error patterns by exception type

---

## ABAP Profiler / Traces (2 tools) - ATRA

| Tool | Description | Mode |
|------|-------------|------|
| `ListTraces` | List ABAP runtime traces (profiler results) | Focused |
| `GetTrace` | Get trace analysis (hitlist, statements, dbAccesses) | Focused |

**Analysis Types:**
- `hitlist` - Hot spots by execution time
- `statements` - Statement-level trace
- `dbAccesses` - Database access analysis

---

## SQL Trace (2 tools) - ST05

| Tool | Description | Mode |
|------|-------------|------|
| `GetSQLTraceState` | Check if SQL trace is currently active | Focused |
| `ListSQLTraces` | List SQL trace files | Focused |

---

## ABAP Debugger Tools (10 tools)

Session-based debugging via WebSocket connection to ZADT_VSP. **Requires ZADT_VSP installed on the SAP system.** Can be disabled with `--disabled-groups D`.

| Tool | Description | Mode |
|------|-------------|------|
| `SetBreakpoint` | Set a breakpoint: 'line' (specific location), 'statement' (ABAP keyword), or 'exception' (exception class) | Focused |
| `GetBreakpoints` | Get all breakpoints registered in the current debug session | Focused |
| `DeleteBreakpoint` | Delete a breakpoint by ID | Focused |
| `DebuggerListen` | Start a debug listener that waits (blocking, long-poll) for a debuggee to hit a breakpoint | Focused |
| `DebuggerAttach` | Attach to a debuggee that has hit a breakpoint (use debuggee_id from DebuggerListen) | Focused |
| `DebuggerDetach` | Detach from the current debug session and release the debuggee | Focused |
| `DebuggerStep` | Perform a step operation in the debugger | Focused |
| `DebuggerGetStack` | Get the current call stack during a debug session | Focused |
| `DebuggerGetVariables` | Get variable values during a debug session ('@ROOT' for top-level variables) | Focused |
| `CallRFC` | Call a function module via WebSocket — useful for triggering code execution to hit breakpoints | Focused |

---

## AMDP / HANA Debugger Tools (7 tools) - Experimental

Debug AMDP (HANA SQLScript) procedures. **Experimental** — can be disabled with `--disabled-groups H`.

| Tool | Description | Mode |
|------|-------------|------|
| `AMDPDebuggerStart` | Start an AMDP debug session with a persistent background session manager | Focused |
| `AMDPDebuggerResume` | Get current AMDP debug session status | Focused |
| `AMDPDebuggerStop` | Stop the AMDP debug session and clean up the server-side session | Focused |
| `AMDPDebuggerStep` | Perform a step operation in the AMDP debugger | Focused |
| `AMDPGetVariables` | Get variable values during AMDP debugging (scalar, table, and array types) | Focused |
| `AMDPSetBreakpoint` | Set a breakpoint in AMDP (SQLScript) code by procedure name and line | Focused |
| `AMDPGetBreakpoints` | Get all breakpoints registered in the current AMDP debug session | Focused |

---

## UI5 / BSP Management Tools (7 tools)

Manage UI5/Fiori BSP applications. Can be disabled with `--disabled-groups 5` (or `U`).

| Tool | Description | Mode |
|------|-------------|------|
| `UI5ListApps` | List UI5/Fiori BSP applications (supports wildcard query) | Focused |
| `UI5GetApp` | Get details of a UI5/Fiori BSP application including file structure | Focused |
| `UI5GetFileContent` | Get content of a specific file within a BSP application | Focused |
| `UI5UploadFile` | Upload a file to a BSP application | Expert |
| `UI5DeleteFile` | Delete a file from a BSP application | Expert |
| `UI5CreateApp` | Create a new UI5/Fiori BSP application | Expert |
| `UI5DeleteApp` | Delete a UI5/Fiori BSP application | Expert |

---

## Git / abapGit Tools (2 tools) - NEW v2.16.0

Exports ABAP objects using abapGit's native serialization. **Requires abapGit installed on SAP system.**

| Tool | Description | Mode |
|------|-------------|------|
| `GitTypes` | Get list of 158 supported abapGit object types | Focused |
| `GitExport` | Export packages/objects as abapGit-compatible ZIP (base64) | Focused |

**GitExport Parameters:**
- `packages` - Comma-separated package names (e.g., "$ZRAY,$TMP")
- `objects` - JSON array of objects: `[{"type":"CLAS","name":"ZCL_TEST"}]`
- `include_subpackages` - Include subpackages (default: true)

**Returns:** Base64-encoded ZIP with abapGit file structure:
```
src/
├── zcl_example.clas.abap      # Class source
├── zcl_example.clas.xml       # Class metadata
├── zif_example.intf.abap      # Interface source
└── ...
```

**Tool Group:** Git tools can be disabled with `--disabled-groups G`

**SAP Requirements:**
- `ZCL_ABAPGIT_OBJECTS` - Core serialization class
- `ZCL_ABAPGIT_FACTORY` - TADIR access factory
- Install via [abapGit standalone](https://github.com/abapGit/abapGit) or S/4HANA Developer Edition

---

## gCTS Tools (10 tools)

Git-enabled CTS repository management (S/4HANA systems with gCTS configured).

| Tool | Description | Mode |
|------|-------------|------|
| `GctsListRepositories` | List all gCTS repositories (ID, name, URL, branch, status, role) | Focused |
| `GctsGetRepository` | Get details of a specific gCTS repository including configuration | Focused |
| `GctsCreateRepository` | Create a new gCTS repository | Expert |
| `GctsDeleteRepository` | Delete a gCTS repository | Expert |
| `GctsCloneRepository` | Clone a gCTS repository on the SAP system | Expert |
| `GctsPull` | Pull changes into a gCTS repository, optionally to a specific commit | Expert |
| `GctsCommit` | Create a commit in a gCTS repository | Expert |
| `GctsListBranches` | List branches in a gCTS repository | Focused |
| `GctsSwitchBranch` | Switch the active branch of a gCTS repository | Expert |
| `GctsGetHistory` | Get commit history of a gCTS repository | Focused |

---

## Install/Setup Tools (4 tools)

Deploy VSP components and dependencies to SAP systems via ADT.

| Tool | Description | Mode |
|------|-------------|------|
| `InstallZADTVSP` | Deploy ZADT_VSP WebSocket handler (6 ABAP objects) | Focused |
| `InstallAbapGit` | Deploy abapGit from embedded ZIP (standalone or dev edition) | Focused |
| `ListDependencies` | List available dependencies for installation | Focused |
| `InstallDummyTest` | Create a simple interface + class to verify the Install* workflow end-to-end | Focused |

**InstallZADTVSP Parameters:**
- `package` - Target package name (default: `$ZADT_VSP`)
- `skip_git_service` - Skip Git service if no abapGit (default: auto-detected)
- `check_only` - Only check prerequisites, don't deploy

**InstallAbapGit Parameters:**
- `edition` - `standalone` (single program) or `dev` (full packages)
- `package` - Target package (default: `$ABAPGIT` or `$ZGIT_DEV`)
- `check_only` - Only show deployment plan

**Architecture:**
```
embedded/
├── abap/           # ZADT_VSP source (raw ABAP, go:embed)
│   ├── zif_vsp_service.intf.abap
│   ├── zcl_vsp_*.clas.abap
│   └── embed.go
│
└── deps/           # Dependencies (abapGit ZIP format)
    ├── abapgit-standalone.zip  # Placeholder
    ├── abapgit-dev.zip         # Placeholder
    └── embed.go                # Unzip + deploy logic
```

**Tool Group:** Install tools can be disabled with `--disabled-groups I`

---

## Report Execution Tools (6 tools) - NEW v2.19.0

Execute ABAP reports with parameters and capture ALV output. Includes async pattern for long-running reports.

| Tool | Description | Mode |
|------|-------------|------|
| `RunReport` | Execute report with params/variant, capture ALV output | Focused |
| `RunReportAsync` | Start report in background, returns task_id | Focused |
| `GetAsyncResult` | Poll or wait for async task completion | Focused |
| `GetVariants` | List available variants for a report | Focused |
| `GetTextElements` | Get selection texts and text symbols | Focused |
| `SetTextElements` | Update selection texts and text symbols | Focused |

**Async Pattern:**
```
1. RunReportAsync(report="RFITEMGL", params={...})
   → {"task_id": "report_1736034567_1", "status": "started"}

2. GetAsyncResult(task_id="...", wait=true)
   → Blocks up to 60s, returns full result when complete
```

**Requires:** ZADT_VSP WebSocket handler deployed to SAP system.

**Tool Group:** Report tools can be disabled with `--disabled-groups R`

---

## Version History Tools (3 tools)

| Tool | Description | Mode |
|------|-------------|------|
| `GetRevisions` | List version history of an object (versions, dates, authors, transports) | Focused |
| `GetRevisionSource` | Get source code of a specific version of an object | Focused |
| `CompareVersions` | Compare two versions of an object with unified diff ('current' for active version) | Focused |

---

## Testing & Quality Tools (3 tools)

| Tool | Description | Mode |
|------|-------------|------|
| `GetCodeCoverage` | Run ABAP Unit tests with coverage enabled: line-level statement, branch, and procedure coverage | Focused |
| `GetCheckRunResults` | Get detailed results for a check run (messages, line numbers, severity, summary) | Focused |
| `AnalyzeABAPCode` | Static analysis of ABAP source for quality, performance, security, and robustness issues | Focused |

---

## Internationalization Tools (7 tools)

| Tool | Description | Mode |
|------|-------------|------|
| `GetObjectTextsInLanguage` | Get object source/content in a specific language (overrides session language per request) | Focused |
| `GetDataElementLabels` | Get data element labels (short/medium/long/heading) in a specific language | Focused |
| `GetMessageClassTexts` | Get all messages of a message class in a specific language | Focused |
| `WriteMessageClassTexts` | Update message class texts in a specific language (requires lock handle) | Expert |
| `WriteDataElementLabels` | Update data element labels in a specific language (requires lock handle) | Expert |
| `GetTextPool` | Get the text pool (text elements/symbols) of a program in a specific language | Focused |
| `CompareLanguages` | Compare object text content in two languages to find missing/outdated translations | Focused |

---

## Tool Count Summary

| Mode | Tools | Description |
|------|-------|-------------|
| **Hyperfocused** (default) | 1 | Single universal `SAP` tool routing to all operations |
| **Focused** | 106 | 103 whitelisted tools + 3 always-on system tools |
| **Expert** | 157 | All tools: 154 mode-gated + 3 always-on |

**Token Savings:**
- Hyperfocused mode: one tool definition instead of 157
- Focused mode: essential tools only — 106 choices instead of 157
