# vsp - Detailed Capabilities Reference

**Document:** Technical Capabilities | **Version:** 1.0 | **Date:** 2026-01-19

---

## Overview

**vsp** is a Go-native MCP (Model Context Protocol) server providing 112 active tools (124 total including legacy) for SAP ABAP development. It enables AI agents to execute full SDLC operations autonomously using stable ADT REST APIs.

```
┌──────────────────────────────────────────────────────────────────┐
│                          vsp MCP Server                          │
├──────────────────────────────────────────────────────────────────┤
│  112 Active Tools │ 244 Unit Tests │ 38 Integration Tests        │
├──────────────────────────────────────────────────────────────────┤
│  Single Go Binary │ Zero Dependencies │ 9 Platforms │ Apache 2.0 │
└──────────────────────────────────────────────────────────────────┘
```

---

## Tool Modes

vsp offers two operational modes to balance capability with complexity:

| Mode | Tools | Use Case |
|------|-------|----------|
| **Focused** (default) | 85 | Day-to-day development, reduced cognitive load |
| **Expert** | 124 | Full capabilities including debugging, AMDP, advanced features |

**Tool breakdown:**
- **112 active tools** - Recommended for new development
- **12 legacy tools** - Superseded by unified source operations, retained for backward compatibility
- **39 expert-only tools** - CreateObject, CreateTransport, ExecuteABAP, CodeCompletion, etc.

```bash
# Focused mode (default)
./vsp

# Expert mode
./vsp --mode expert
```

---

## 1. CRUD Operations (15 tools)

Complete object lifecycle management with locking and activation.

### Object Management

| Tool | Description | Key Parameters |
|------|-------------|----------------|
| **GetSource** | Retrieve source code | `object`, `method` (optional for method-level) |
| **WriteSource** | Update source code | `object`, `content`, `method` (optional) |
| **EditSource** | Partial source edit | `object`, `method`, `old_text`, `new_text` |
| **CreateObject** | Create new ABAP object | `type`, `name`, `package`, `description` |
| **DeleteObject** | Delete ABAP object | `object`, `transport` |
| **MoveObject** | Move object to package | `object`, `target_package`, `transport` |

### Locking & Activation

| Tool | Description | Key Parameters |
|------|-------------|----------------|
| **LockObject** | Acquire edit lock | `object` |
| **UnlockObject** | Release edit lock | `object` |
| **ActivateObject** | Activate single object | `object` |
| **ActivateMultiple** | Mass activation | `objects[]` |

### Class-Specific Operations

| Tool | Description | Key Parameters |
|------|-------------|----------------|
| **GetSource** (includes) | Get class includes | `object`, `include` (testclasses, locals_def, locals_imp, macros) |
| **WriteSource** (includes) | Write class includes | `object`, `include`, `content` |

**Method-Level Granularity** (95% token reduction):
```bash
# Get single method instead of entire class
GetSource object=ZCL_TRAVEL method=PROCESS_BOOKING

# Edit single method
EditSource object=ZCL_TRAVEL method=PROCESS_BOOKING old_text="..." new_text="..."
```

---

## 2. Code Search & Discovery (8 tools)

Powerful search capabilities across the entire SAP system.

| Tool | Description | Key Parameters |
|------|-------------|----------------|
| **GrepObjects** | Search code patterns across packages | `pattern`, `package`, `object_type` |
| **GrepPackages** | Search within package hierarchy | `pattern`, `package`, `recursive` |
| **SearchObject** | Find objects by name/type | `query`, `type`, `max_results` |
| **ListPackageContents** | List objects in package | `package`, `recursive` |
| **GetObjectStructure** | Object design & dependencies | `object` |
| **ListDependencies** | Analyze object relationships | `object`, `direction` (uses/used_by) |
| **GetCallGraph** | Trace execution paths | `object`, `method`, `depth` |
| **GetWhereUsed** | Find all references | `object` |

**Example: Find all SELECT in LOOP anti-patterns**
```bash
GrepObjects pattern="SELECT.*ENDLOOP" package="$TMP" object_type="CLAS"
```

---

## 3. Code Intelligence (8 tools)

IDE-quality code navigation and assistance.

| Tool | Description | Key Parameters |
|------|-------------|----------------|
| **SyntaxCheck** | Validate ABAP syntax | `object` |
| **FindDefinition** | Go to definition | `object`, `line`, `column` |
| **FindReferences** | Find all usages | `object`, `line`, `column` |
| **GetCompletion** | Code completion suggestions | `object`, `line`, `column`, `prefix` |
| **GetQuickFixes** | Suggested fixes for issues | `object`, `line` |
| **PrettyPrint** | Format ABAP code | `object` |
| **GetDocumentation** | Retrieve ABAP Doc | `object`, `method` |
| **GetSignature** | Method signature help | `object`, `method` |

---

## 4. Testing & Quality (5 tools)

Unit test execution and orchestration.

| Tool | Description | Key Parameters |
|------|-------------|----------------|
| **RunUnitTests** | Execute ABAP Unit tests | `object` or `package` |
| **RunUnitTestsParallel** | Parallel test execution | `objects[]`, `workers` |
| **GetTestCoverage** | Code coverage analysis | `object`, `test_class` |
| **RunATCCheck** | ATC quality checks | `object` or `package`, `variant` |
| **GetATCResults** | Retrieve ATC findings | `run_id` |

**Test Orchestration Features:**
- Parallel execution across multiple objects
- Test result aggregation
- Coverage reporting
- Evidence bundle generation for compliance

```lua
-- Lua example: Run tests with evidence
local results = adt.RunUnitTests("ZCL_TRAVEL_PROCESSOR")
if results.failures > 0 then
    print("FAILED: " .. results.summary)
else
    adt.ExportEvidence(results, "./evidence/")
end
```

---

## 5. Debugging (12 tools)

### External Debugger

Traditional ABAP debugging with breakpoint management.

| Tool | Description | Key Parameters |
|------|-------------|----------------|
| **DebuggerListen** | Start debugger listener | `terminal_id` (optional) |
| **DebuggerAttach** | Attach to debug session | `session_id` |
| **DebuggerDetach** | Detach from session | - |
| **SetBreakpoint** | Set external breakpoint | `object`, `line` |
| **DeleteBreakpoint** | Remove breakpoint | `breakpoint_id` |
| **ListBreakpoints** | List all breakpoints | - |
| **DebuggerStep** | Step into/over/out | `step_type` |
| **DebuggerGetVariables** | Inspect variables | `scope` |
| **DebuggerSetVariable** | Modify variable value | `name`, `value` |

**SAP GUI Breakpoint Sharing:**
```bash
# Use same terminal ID as SAP GUI for shared breakpoints
./vsp --terminal-id D0C586D015974B75BFB2A306A4A13AEA
```

### AMDP/HANA Debugger (Expert Mode)

SQLScript debugging via WebSocket connection.

| Tool | Description | Key Parameters |
|------|-------------|----------------|
| **AMDPDebuggerStart** | Start AMDP debug session | `class`, `method` |
| **AMDPDebuggerStep** | Step through SQLScript | `step_type` |
| **AMDPDebuggerGetVariables** | Inspect HANA variables | - |

---

## 6. Transport Management (5 tools)

Complete CTS/CTS+ lifecycle with safety controls.

| Tool | Description | Key Parameters |
|------|-------------|----------------|
| **CreateTransport** | Create transport request | `description`, `type` (workbench/customizing) |
| **AssignToTransport** | Assign object to transport | `object`, `transport` |
| **ReleaseTransport** | Release transport | `transport` |
| **GetTransportInfo** | Transport details & contents | `transport` |
| **ListTransports** | List transports by user/status | `user`, `status` |

**Safety Controls:**
- Transport operations can be disabled entirely (`--feature-transport off`)
- Package restrictions prevent assignments outside allowed namespaces
- Operation filtering can block specific transport operations

---

## 7. abapGit Integration (6 tools)

Git-based version control with 158 supported object types.

| Tool | Description | Key Parameters |
|------|-------------|----------------|
| **GitStatus** | Repository status | `package` |
| **GitPull** | Pull from remote | `package`, `branch` |
| **GitPush** | Push to remote | `package`, `branch`, `message` |
| **GitExport** | Export to file system | `package`, `directory` |
| **GitImport** | Import from file system | `directory`, `package`, `transport` |
| **ListGitRepos** | List linked repositories | - |

**Supported Object Types (158):**
- Core: CLAS, INTF, PROG, FUGR, FUNC, TABL, DTEL, DOMA, TTYP
- RAP: DDLS, DDLX, DCLS, BDEF, SRVD, SRVB
- UI5: BSP, WAPA, WDYN
- And 140+ more...

**RAP-Aware Ordering:**
```go
// Import respects RAP dependency order
pipeline := dsl.Import(client).
    FromDirectory("./src/").
    ToPackage("$ZRAY").
    RAPOrder().  // DDLS → BDEF → Classes → SRVD → SRVB
    Execute(ctx)
```

---

## 8. System Introspection (8 tools)

System analysis and monitoring.

| Tool | Description | Key Parameters |
|------|-------------|----------------|
| **GetSystemInfo** | System details (version, components) | - |
| **GetInstalledComponents** | List installed software | - |
| **GetDumps** | List runtime errors (RABAX) | `days`, `user` |
| **GetDump** | Dump details and stack trace | `dump_id` |
| **ListTraces** | ABAP profiler traces (ATRA) | `user`, `days` |
| **GetTrace** | Trace details | `trace_id` |
| **GetSQLTraceState** | ST05 trace status | - |
| **ListSQLTraces** | SQL trace entries | `transaction`, `user` |

---

## 9. RAP/OData Development (8 tools)

Full RAP (RESTful ABAP Programming) support.

| Tool | Description | Key Parameters |
|------|-------------|----------------|
| **CreateDDLS** | Create CDS view | `name`, `package`, `source` |
| **CreateBDEF** | Create behavior definition | `name`, `package`, `source` |
| **CreateSRVD** | Create service definition | `name`, `package`, `source` |
| **CreateSRVB** | Create service binding | `name`, `package`, `service_definition`, `binding_type` |
| **PublishService** | Publish OData service | `service_binding` |
| **UnpublishService** | Unpublish OData service | `service_binding` |
| **GetServiceMetadata** | OData $metadata | `service_binding` |
| **TestService** | Execute OData request | `service_binding`, `entity`, `operation` |

**RAP Pipeline Example:**
```go
pipeline := dsl.RAPPipeline(client, "./src/", "$ZTRAVEL", "ZTRAVEL_001")
result := pipeline.
    CreateDDLS().
    CreateBDEF().
    CreateServiceDefinition().
    CreateServiceBinding().
    PublishOData().
    RunTests().
    Execute(ctx)
```

---

## 10. UI5/BSP Management (4 tools)

Fiori and BSP application support.

| Tool | Description | Key Parameters |
|------|-------------|----------------|
| **ListBSPApplications** | List BSP apps | `package` |
| **GetBSPApplication** | BSP app details | `application` |
| **GetBSPResource** | Read BSP file content | `application`, `resource` |
| **ListUI5Apps** | List deployed UI5 apps | - |

---

## 11. Workflow & Orchestration

### Lua Scripting Engine

40+ ADT bindings for custom automation.

```lua
-- Example: Autonomous bug fix workflow
local class = "ZCL_INVOICE_PROCESSOR"

-- Find the issue
local source = adt.GetSource(class, "PROCESS_INVOICE")

-- AI analyzes and generates fix
local fix = ai.AnalyzeAndFix(source, "timeout on large datasets")

-- Apply fix
adt.EditSource(class, "PROCESS_INVOICE", source, fix)

-- Validate
local syntax = adt.SyntaxCheck(class)
if syntax.errors > 0 then
    error("Syntax errors: " .. syntax.message)
end

-- Test
local results = adt.RunUnitTests(class)
print("Tests: " .. results.passed .. "/" .. results.total)

-- Create transport
local transport = adt.CreateTransport("Fix invoice timeout - AI generated")
adt.AssignToTransport(class, transport)
```

### Go DSL (Fluent API)

Programmatic workflow construction.

```go
// Search and test workflow
objects, _ := dsl.Search(client).
    Query("ZCL_*").
    Classes().
    InPackage("$TMP").
    Execute(ctx)

summary, _ := dsl.Test(client).
    Objects(objects...).
    Parallel(4).
    Run(ctx)

// Batch export
result, _ := dsl.Export(client).
    Package("$ZRAY").
    ToDirectory("./backup/").
    Execute(ctx)
```

### YAML Pipelines

Declarative workflow definitions.

```yaml
name: autonomous_bug_fix
description: AI-driven bug fix workflow

steps:
  - name: find_affected_code
    tool: GrepObjects
    params:
      pattern: "{{issue_pattern}}"
      package: "{{target_package}}"

  - name: analyze_code
    tool: GetSource
    params:
      object: "{{found_object}}"
      method: "{{found_method}}"

  - name: apply_fix
    tool: EditSource
    params:
      object: "{{found_object}}"
      method: "{{found_method}}"
      new_text: "{{ai_generated_fix}}"

  - name: validate
    tool: SyntaxCheck
    params:
      object: "{{found_object}}"
    on_failure: abort

  - name: test
    tool: RunUnitTests
    params:
      object: "{{found_object}}"
    on_failure: notify

  - name: create_transport
    tool: CreateTransport
    params:
      description: "{{fix_description}}"
    condition: "{{tests_passed}}"
```

---

## 12. Safety & Governance

### Protection Mechanisms

| Control | Flag | Description |
|---------|------|-------------|
| **Read-Only Mode** | `--read-only` | Block all write operations |
| **Package Restrictions** | `--allowed-packages` | Whitelist packages (wildcard support) |
| **Operation Filtering** | `--allowed-ops` / `--disallowed-ops` | Whitelist/blacklist operation types |
| **Block Free SQL** | `--block-free-sql` | Prevent RunQuery execution |
| **Feature Detection** | `--feature-*` | Auto-detect/enable/disable capabilities |

### Configuration Examples

```bash
# Production environment (read-only)
./vsp --read-only --verbose

# Development with namespace enforcement
./vsp --allowed-packages "/CBA/*,$TMP"

# Restricted operations (no deletes, no transports)
./vsp --disallowed-ops "CDUA,TRSP"

# Feature control
./vsp --feature-transport off --feature-abapgit auto

# Combined safety profile
./vsp \
  --allowed-packages "/CBA/*" \
  --disallowed-ops "CDUA" \
  --block-free-sql \
  --feature-transport off
```

### Operation Type Codes

| Code | Operations |
|------|------------|
| R | Read operations (GetSource, Search, etc.) |
| S | Search operations (Grep, Find, etc.) |
| Q | Query operations (RunQuery) |
| C | Create operations |
| D | Delete operations |
| U | Update operations |
| A | Activate operations |
| T | Transport operations |
| X | Execute operations (tests, ATC) |

### Feature Detection (Safety Network)

Automatically detects system capabilities and disables unavailable tools:

| Feature | Flag | Auto-Detection |
|---------|------|----------------|
| abapGit | `--feature-abapgit` | Checks for abapGit installation |
| RAP | `--feature-rap` | Checks for RAP support (7.54+) |
| AMDP | `--feature-amdp` | Checks for HANA database |
| UI5 | `--feature-ui5` | Checks for UI5 repository |
| Transport | `--feature-transport` | Checks for CTS access |

---

## 13. Deployment Options

### Stdio Mode (Default)

For MCP clients (Claude Code, RooCode, Cline).

```json
{
  "mcpServers": {
    "vsp": {
      "command": "/path/to/vsp",
      "args": ["--allowed-packages", "/CBA/*"],
      "env": {
        "SAP_URL": "http://sap-dev:50000",
        "SAP_USER": "developer",
        "SAP_PASSWORD": "****",
        "SAP_CLIENT": "001"
      }
    }
  }
}
```

### HTTP Mode

For web services and firewalled environments.

```bash
./vsp --http-mode --port 8080
```

### Authentication Options

| Method | Configuration |
|--------|---------------|
| **Basic Auth** | `--user`, `--password` or `SAP_USER`, `SAP_PASSWORD` |
| **Cookie Auth** | `--cookie-file` or `--cookie-string` |
| **Environment** | `.env` file in working directory |

**Priority:** CLI flags > Environment variables > .env file > Defaults

---

## 14. Supported Platforms

| Platform | Architecture | Binary |
|----------|--------------|--------|
| Linux | amd64, arm64 | `vsp-linux-amd64`, `vsp-linux-arm64` |
| macOS | amd64, arm64 | `vsp-darwin-amd64`, `vsp-darwin-arm64` |
| Windows | amd64 | `vsp-windows-amd64.exe` |
| FreeBSD | amd64 | `vsp-freebsd-amd64` |
| OpenBSD | amd64 | `vsp-openbsd-amd64` |
| NetBSD | amd64 | `vsp-netbsd-amd64` |
| Solaris | amd64 | `vsp-solaris-amd64` |
| AIX | ppc64 | `vsp-aix-ppc64` |
| Plan9 | amd64 | `vsp-plan9-amd64` |

---

## 15. Tool Groups & Disabling

Selectively disable tool categories:

| Group | Flag | Tools Disabled |
|-------|------|----------------|
| UI5 | `--disabled-groups "5"` or `"U"` | UI5/BSP management tools |
| Tests | `--disabled-groups "T"` | Unit test execution tools |
| HANA | `--disabled-groups "H"` | AMDP/HANA debugging tools |
| Debug | `--disabled-groups "D"` | External debugger tools |
| CTS | `--disabled-groups "C"` | Transport management tools |

```bash
# Disable HANA debugging and transport tools
./vsp --disabled-groups "H,C"
```

---

## 16. Key Metrics Summary

| Metric | Value |
|--------|-------|
| **Total Tools** | 124 (112 active + 12 legacy) |
| **Focused Mode Tools** | 85 |
| **Expert-Only Tools** | 39 |
| **Unit Tests** | 244 |
| **Integration Tests** | 38 |
| **Supported Platforms** | 9 |
| **abapGit Object Types** | 158 |
| **Lua Bindings** | 40+ |
| **Binary Size** | ~15MB |
| **Dependencies** | Zero (single binary) |
| **API Stability** | ADT REST APIs (stable since SAP 7.40) |
| **License** | Apache 2.0 |
| **Current Version** | v2.21.0 |

---

## 17. Features Roadmap

### Already Implemented ✅

| Feature | Tools | Status |
|---------|-------|--------|
| **Transport Workflow** | CreateTransport, ReleaseTransport, DeleteTransport, ListTransports, GetTransport, GetTransportInfo, GetUserTransports | ✅ Done (7 tools) |
| **ATC Integration** | RunATCCheck, GetATCCustomizing | ✅ Done (2 tools) |
| **Refactor Rename** | RenameObject | ✅ Done |
| **abapGit Import/Export** | ImportFromFile, ExportToFile, DeployFromFile, GitExport, GitTypes | ✅ Done (5 tools) |
| **Graph Traversal Engine** | GetCallGraph, GetCallersOf, GetCalleesOf, AnalyzeCallGraph, CompareCallGraphs, TraceExecution | ✅ Done (6 tools) |
| **Report Execution** | RunReport, RunReportAsync, GetVariants, GetTextElements, SetTextElements | ✅ Done (5 tools) |
| **Code Intelligence** | FindDefinition, FindReferences, CodeCompletion, GetTypeHierarchy, PrettyPrint | ✅ Done (5 tools) |
| **Debugging** | 8 external debugger tools + 7 AMDP debugger tools | ✅ Done (15 tools) |

### Planned - AI-Powered Features (CRITICAL)

| Feature | Description | Effort | Priority | Status |
|---------|-------------|--------|----------|--------|
| **ReviewCode + AutoFixIssue** | AI code review (security, best practices) with auto-fix | 2w | CRITICAL | Designed |
| **GenerateCode** | AI code generation with multi-agent workflow | 2w | CRITICAL | Designed |
| **NaturalLanguageQuery** | Query SAP in plain English/Spanish | 1w | HIGH | Designed |
| **GenerateDocumentation** | Auto-docs from source (Markdown, Confluence, Mermaid diagrams) | 1w | MEDIUM | Designed |
| **SmartJoin** | Auto-discover SAP table relationships for JOINs | 3d | LOW | Designed |

### Planned - Mid Term (1-3 days each)

| Feature | Effort | Impact | Status |
|---------|--------|--------|--------|
| Basic DAP Adapter (VS Code attach) | 3d | High | Planned |
| ABAP Documentation Lookup | 2d | Medium | Planned |
| Code Coverage Reporting | 2d | Medium | Planned |
| Conditional Breakpoints | 1d | Medium | Planned |
| Fragment Mappings | 2d | Medium | Planned |
| Reentrance Tickets | 1d | Medium | Planned |
| abapGit Pull (from GitHub URL) | 3d | Medium | Planned |
| Watch Expressions (debug) | 1d | Low | Planned |

### Planned - Long Term (1-2 weeks each)

| Feature | Effort | Impact | Status |
|---------|--------|--------|--------|
| Test Intelligence (smart test selection) | 1w | High | Designed |
| Full DAP + Shared State | 2w | High | Planned |
| VS Code Extension (syntax, outline) | 2w | High | Planned |
| Revision History | 1w | Medium | Planned |
| Standard API Scraper | 1w | Medium | Designed |
| Multi-System Support (DEV→QA→PRD) | 1w | Medium | Planned |
| Advanced Refactoring (extract, inline, move) | 2w | Medium | Planned |
| Performance Profiler UI | 2w | Low | Planned |

---

## 18. Quick Start

### 1. Download

```bash
# macOS (Apple Silicon)
curl -L https://github.com/vinchacho/vibing-steampunk/releases/latest/download/vsp-darwin-arm64 -o vsp

# Linux (x64)
curl -L https://github.com/vinchacho/vibing-steampunk/releases/latest/download/vsp-linux-amd64 -o vsp

chmod +x vsp
```

### 2. Configure

```bash
# Create .env file
cat > .env << EOF
SAP_URL=http://sap-dev:50000
SAP_USER=developer
SAP_PASSWORD=your_password
SAP_CLIENT=001
SAP_LANGUAGE=EN
EOF
```

### 3. Test Connection

```bash
# Verify connectivity
./vsp --verbose 2>&1 | head -20
```

### 4. Run

```bash
# Start MCP server (stdio mode)
./vsp

# With safety controls
./vsp --allowed-packages "/CBA/*,$TMP" --read-only
```

---

## 19. Integration Examples

### Claude Code / Anthropic

```json
// ~/.config/claude/claude_desktop_config.json
{
  "mcpServers": {
    "vsp": {
      "command": "/usr/local/bin/vsp",
      "args": ["--mode", "expert"]
    }
  }
}
```

### RooCode / VS Code

```json
// .vscode/mcp.json
{
  "servers": {
    "vsp": {
      "command": "vsp",
      "args": ["--allowed-packages", "$TMP"]
    }
  }
}
```

### GitHub Actions

```yaml
- name: Run ABAP Tests
  run: |
    ./vsp --read-only << EOF
    RunUnitTests package=${{ env.PACKAGE }}
    EOF
```

---

## 20. Complete Tool Reference (124 Tools)

### Source Operations (3 tools)
| Tool | Description | Mode |
|------|-------------|------|
| `GetSource` | Get source code (supports method-level granularity) | Focused |
| `WriteSource` | Write/replace source code | Focused |
| `EditSource` | Partial source edit (find/replace) | Focused |

### Legacy Read Operations (9 tools) - Superseded
> **Superseded by:** `GetSource` - Use unified tool for new development. Legacy tools retained for backward compatibility.

| Tool | Description | Mode |
|------|-------------|------|
| `GetProgram` | Get program source | Expert |
| `GetClass` | Get class source | Expert |
| `GetInterface` | Get interface source | Expert |
| `GetFunction` | Get function module source | Expert |
| `GetFunctionGroup` | Get function group details | Focused |
| `GetInclude` | Get include source | Expert |
| `GetStructure` | Get DDIC structure | Expert |
| `GetTransaction` | Get transaction details | Expert |
| `GetTypeInfo` | Get data type information | Expert |

### Legacy Write Operations (3 tools) - Superseded
> **Superseded by:** `WriteSource` / `EditSource` - Use unified tools for new development.

| Tool | Description | Mode |
|------|-------------|------|
| `WriteProgram` | Create/update program | Expert |
| `WriteClass` | Create/update class | Expert |
| `UpdateSource` | Update source | Expert |

### CRUD Operations (10 tools)
| Tool | Description | Mode |
|------|-------------|------|
| `CreateObject` | Create any ABAP object | Expert |
| `CreatePackage` | Create local package | Focused |
| `CreateTable` | Create DDIC table from JSON | Focused |
| `CreateAndActivateProgram` | Create + activate in one step | Expert |
| `CreateClassWithTests` | Create class with test include | Expert |
| `CreateTestInclude` | Create test class include | Expert |
| `DeleteObject` | Delete ABAP object | Expert |
| `CloneObject` | Copy object to new name | Focused |
| `MoveObject` | Move object to different package | Focused |
| `RenameObject` | Rename object across codebase | Expert |

### Lock & Activation (4 tools)
| Tool | Description | Mode |
|------|-------------|------|
| `LockObject` | Acquire edit lock | Focused |
| `UnlockObject` | Release edit lock | Focused |
| `Activate` | Activate single object | Focused |
| `ActivatePackage` | Mass activate package objects | Focused |

### Search & Discovery (8 tools)
| Tool | Description | Mode |
|------|-------------|------|
| `SearchObject` | Find objects by name/type | Focused |
| `GrepObjects` | Search code patterns (multi-object) | Focused |
| `GrepPackages` | Search across packages (recursive) | Focused |
| `GrepObject` | Search single object (legacy) | Expert |
| `GrepPackage` | Search single package (legacy) | Expert |
| `ListDependencies` | Analyze object dependencies | Focused |
| `GetInactiveObjects` | List pending activations | Focused |
| `GetClassComponents` | List class methods/attributes | Expert |

### Code Intelligence (8 tools)
| Tool | Description | Mode |
|------|-------------|------|
| `SyntaxCheck` | Validate ABAP syntax | Focused |
| `FindDefinition` | Go to definition | Focused |
| `FindReferences` | Find all usages | Focused |
| `CodeCompletion` | Code completion suggestions | Expert |
| `GetTypeHierarchy` | Class inheritance tree | Expert |
| `PrettyPrint` | Format ABAP code | Focused |
| `GetPrettyPrinterSettings` | Get formatter settings | Expert |
| `SetPrettyPrinterSettings` | Set formatter settings | Expert |

### Call Graph Analysis (6 tools)
| Tool | Description | Mode |
|------|-------------|------|
| `GetCallGraph` | Get call hierarchy | Focused |
| `GetCallersOf` | Who calls this? (up) | Focused |
| `GetCalleesOf` | What does this call? (down) | Focused |
| `AnalyzeCallGraph` | Call graph statistics | Focused |
| `CompareCallGraphs` | Compare static vs runtime | Focused |
| `TraceExecution` | Composite RCA tool | Focused |

### Testing & Quality (4 tools)
| Tool | Description | Mode |
|------|-------------|------|
| `RunUnitTests` | Execute ABAP Unit tests | Focused |
| `RunATCCheck` | Run ATC quality checks | Focused |
| `GetATCCustomizing` | Get ATC configuration | Expert |
| `InstallDummyTest` | Test install workflow | Focused |

### External Debugger (9 tools)
| Tool | Description | Mode |
|------|-------------|------|
| `DebuggerListen` | Start debugger listener | Focused |
| `DebuggerAttach` | Attach to debug session | Focused |
| `DebuggerDetach` | Detach from session | Focused |
| `DebuggerStep` | Step into/over/out | Focused |
| `DebuggerGetStack` | Get call stack | Focused |
| `DebuggerGetVariables` | Inspect variables | Focused |
| `SetBreakpoint` | Set line breakpoint | Focused |
| `GetBreakpoints` | List active breakpoints | Focused |
| `DeleteBreakpoint` | Remove breakpoint | Focused |

### AMDP/HANA Debugger (7 tools)
| Tool | Description | Mode |
|------|-------------|------|
| `AMDPDebuggerStart` | Start AMDP debug session | Focused |
| `AMDPDebuggerStep` | Step through SQLScript | Focused |
| `AMDPDebuggerResume` | Resume execution | Focused |
| `AMDPDebuggerStop` | Stop debug session | Focused |
| `AMDPSetBreakpoint` | Set AMDP breakpoint | Focused |
| `AMDPGetBreakpoints` | List AMDP breakpoints | Focused |
| `AMDPGetVariables` | Inspect HANA variables | Focused |

### Transport Management (7 tools)
| Tool | Description | Mode |
|------|-------------|------|
| `ListTransports` | List transport requests | Focused |
| `GetTransport` | Get transport details | Focused |
| `GetTransportInfo` | Get transport metadata | Expert |
| `GetUserTransports` | List user's transports | Expert |
| `CreateTransport` | Create transport request | Expert |
| `ReleaseTransport` | Release transport | Expert |
| `DeleteTransport` | Delete transport | Expert |

### Data & Metadata (7 tools)
| Tool | Description | Mode |
|------|-------------|------|
| `GetTable` | Get table structure | Focused |
| `GetTableContents` | Read table data | Focused |
| `RunQuery` | Execute SQL query | Focused |
| `GetPackage` | Get package details | Focused |
| `GetMessages` | Get message class texts | Focused |
| `GetCDSDependencies` | CDS dependency tree | Focused |
| `GetClassInfo` | Quick class metadata | Focused |

### Class Includes (2 tools)
| Tool | Description | Mode |
|------|-------------|------|
| `GetClassInclude` | Get class include (testclasses, locals_def, etc.) | Expert |
| `UpdateClassInclude` | Update class include | Expert |

### System Introspection (6 tools)
| Tool | Description | Mode |
|------|-------------|------|
| `GetSystemInfo` | System ID, release, kernel | Focused |
| `GetInstalledComponents` | Installed software components | Focused |
| `GetConnectionInfo` | Connection details | Expert |
| `GetFeatures` | Feature detection (safety network) | Expert |
| `CompareSource` | Diff two objects | Focused |
| `CallRFC` | Call function module via RFC | Focused |

### Runtime Errors / Dumps (2 tools)
| Tool | Description | Mode |
|------|-------------|------|
| `ListDumps` | List runtime errors (RABAX) | Focused |
| `GetDump` | Get dump details + stack trace | Focused |

### ABAP Profiler / Traces (2 tools)
| Tool | Description | Mode |
|------|-------------|------|
| `ListTraces` | List ATRA trace files | Focused |
| `GetTrace` | Get trace analysis | Focused |

### SQL Trace / ST05 (2 tools)
| Tool | Description | Mode |
|------|-------------|------|
| `GetSQLTraceState` | Check if SQL trace active | Focused |
| `ListSQLTraces` | List SQL trace entries | Focused |

### Report Execution (5 tools)
| Tool | Description | Mode |
|------|-------------|------|
| `RunReport` | Execute report with params/variants | Focused |
| `RunReportAsync` | Background report execution | Focused |
| `GetAsyncResult` | Retrieve async task results | Focused |
| `GetVariants` | List report variants | Focused |
| `ExecuteABAP` | Execute arbitrary ABAP code | Expert |

### Text Elements (2 tools)
| Tool | Description | Mode |
|------|-------------|------|
| `GetTextElements` | Get program text elements | Focused |
| `SetTextElements` | Set program text elements | Focused |

### abapGit Integration (5 tools)
| Tool | Description | Mode |
|------|-------------|------|
| `GitTypes` | List 158 supported object types | Focused |
| `GitExport` | Export packages to abapGit ZIP | Focused |
| `ImportFromFile` | Import from file system | Focused |
| `ExportToFile` | Export to file system | Focused |
| `DeployFromFile` | Deploy from file (legacy) | Expert |

### File I/O (1 tool)
| Tool | Description | Mode |
|------|-------------|------|
| `SaveToFile` | Save content to file (legacy) | Expert |

### RAP/OData Service Binding (2 tools)
| Tool | Description | Mode |
|------|-------------|------|
| `PublishServiceBinding` | Publish OData service | Expert |
| `UnpublishServiceBinding` | Unpublish OData service | Expert |

### UI5/BSP Management (7 tools)
| Tool | Description | Mode |
|------|-------------|------|
| `UI5ListApps` | List UI5 applications | Focused |
| `UI5GetApp` | Get UI5 app details | Focused |
| `UI5GetFileContent` | Get file from UI5 app | Focused |
| `UI5CreateApp` | Create UI5 app | Expert |
| `UI5DeleteApp` | Delete UI5 app | Expert |
| `UI5UploadFile` | Upload file to UI5 app | Expert |
| `UI5DeleteFile` | Delete file from UI5 app | Expert |

### Installation Tools (3 tools)
| Tool | Description | Mode |
|------|-------------|------|
| `InstallZADTVSP` | Deploy ZADT_VSP WebSocket handler | Focused |
| `InstallAbapGit` | Deploy abapGit to SAP | Focused |
| `ListDependencies` | List available dependencies | Focused |

### Tool Count Summary

| Category | Count | Description |
|----------|-------|-------------|
| **Total Tools** | 124 | All implemented tools |
| **Active Tools** | 112 | Recommended for new development |
| **Legacy Tools** | 12 | Superseded, retained for backward compatibility |
| **Focused Mode** | 85 | Core tools for daily use (default) |
| **Expert Mode** | 124 | All tools including legacy |
| **Expert-Only** | 39 | Advanced tools not in Focused mode |

---

## 21. Resources

| Resource | Link |
|----------|------|
| **GitHub Repository** | https://github.com/vinchacho/vibing-steampunk |
| **Releases** | https://github.com/vinchacho/vibing-steampunk/releases |
| **Documentation** | `/docs` folder |
| **Reports** | `/reports` folder |
| **CLAUDE.md** | Project-specific AI instructions |

---

*vsp v2.21.0 - The execution layer for autonomous SAP development*
