# vsp - Solution On A Page (SOAP)

**Document:** Executive Summary | **Version:** 1.0 | **Date:** 2026-01-19

---

## The Problem

### Current State: Manual Copy-Paste Development

Today's AI-assisted SAP development is fundamentally broken:

```
Developer writes prompt → AI generates code → Developer copies to SAP GUI/Eclipse
→ Developer tests manually → Developer copies results back → Repeat
```

**Pain Points:**
- **No autonomous execution** - AI can suggest code but cannot deploy it
- **Context loss** - AI lacks enterprise-specific knowledge (standards, incidents, patterns)
- **Safety concerns** - No guardrails prevent AI from modifying production or wrong namespaces
- **Evidence gaps** - Manual process lacks audit trails for compliance
- **Waiting for SAP** - Phase 2 autonomous delivery "requires future SAP CLI support"

**The Gap:** AI assistants are powerful code generators, but they have **no hands** - they cannot execute operations in SAP systems.

---

## The Solution: vsp

**vsp** (vibing-steampunk) is a **Go-native MCP server** that gives AI agents hands to execute SAP development operations autonomously.

```
Developer defines intent → AI plans solution → vsp executes in SAP
→ vsp runs tests → vsp generates evidence → Developer reviews & approves
```

### What vsp Enables

| Capability | Description |
|------------|-------------|
| **Autonomous Execution** | Full CRUD, activate, lock/unlock via ADT REST APIs |
| **Testing & Evidence** | Run unit tests, ATC checks, generate compliance evidence |
| **Transport Management** | Create, release, import transports with safety controls |
| **Code Intelligence** | Syntax check, find definitions, references, completion |
| **Debugging** | External debugger + AMDP/HANA debugging |
| **Git Integration** | abapGit sync (158 object types) |

---

## Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                           DEVELOPER LAYER                                │
│  ┌──────────┐   ┌─────────────────┐   ┌─────────────────────────────┐   │
│  │   Jira   │──▶│    Engineer     │──▶│  VS Code + SAP ADT + AI     │   │
│  │ (intent) │   │ (review/approve)│   │  (Roo/Cline/Claude/Copilot) │   │
│  └──────────┘   └─────────────────┘   └──────────────┬──────────────┘   │
└──────────────────────────────────────────────────────┼──────────────────┘
                                                       │ MCP Protocol
┌──────────────────────────────────────────────────────┼──────────────────┐
│                           EXECUTION LAYER            ▼                   │
│  ┌────────────────────────────────────────────────────────────────────┐ │
│  │                        vsp MCP Server                              │ │
│  │  ┌─────────────┐  ┌─────────────┐  ┌────────────────────────────┐ │ │
│  │  │  124 Tools  │  │   Safety    │  │     Just-in-Time SKILLS    │ │ │
│  │  │ CRUD/Debug  │  │ /CBA/* only │  │  ┌────────────────────────┐│ │ │
│  │  │ Test/CTS    │  │ Read-only   │  │  │ QueryGuidelines        ││ │ │
│  │  │ abapGit     │  │ Op filtering│  │  │ ValidateStandards      ││ │ │
│  │  └─────────────┘  └─────────────┘  │  │ LearnFromIncidents     ││ │ │
│  │                                     │  └────────────────────────┘│ │ │
│  │                                     └────────────────────────────┘ │ │
│  └────────────────────────────────────────────────────────────────────┘ │
└──────────────────────────────────────────────────────┼──────────────────┘
                                                       │ ADT REST APIs
┌──────────────────────────────────────────────────────┼──────────────────┐
│                           SAP LANDSCAPE              ▼                   │
│  ┌────────────┐  ┌────────────┐  ┌────────────┐  ┌────────────────────┐ │
│  │ SAP System │  │  SAP ATC   │  │    CTS     │  │  Test Framework    │ │
│  │ DEV/QAS/PRD│  │  Quality   │  │ Transports │  │  Evidence Gen      │ │
│  └────────────┘  └────────────┘  └────────────┘  └────────────────────┘ │
└─────────────────────────────────────────────────────────────────────────┘
                                                       │
┌──────────────────────────────────────────────────────┼──────────────────┐
│                        GOVERNANCE LAYER              ▼                   │
│  ┌────────────────┐  ┌────────────────┐  ┌────────────────────────────┐ │
│  │    GitHub      │  │  Deployment    │  │   Evidence Store           │ │
│  │  PRs + Actions │  │   Pipeline     │  │   Fast Track Release       │ │
│  └────────────────┘  └────────────────┘  └────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## Key Differentiators

| Differentiator | vsp | Alternatives |
|----------------|-----|--------------|
| **Deployment** | Single Go binary, zero dependencies | Node.js + npm ecosystem |
| **Execution** | Full CRUD + Debug + Test + Transport | Read-only or limited |
| **Safety** | Namespace enforcement, op filtering, read-only mode | Minimal or none |
| **Orchestration** | Lua scripting, Go DSL, YAML pipelines | None |
| **Scale** | 158 object types (abapGit-compatible) | Limited object support |
| **Protocol** | MCP-native (Joule-compatible) | Varies |
| **Status** | Production-ready (244 tests) | Experimental |

---

## Safety & Governance

### Built-in Protection

```bash
# Namespace enforcement - only /CBA/* objects
vsp --allowed-packages "/CBA/*"

# Production read-only
vsp --read-only

# Operation filtering
vsp --allowed-ops "RSQ,CLAS,PROG"    # Whitelist
vsp --disallowed-ops "CDUA"          # Blacklist (block deletes)

# Block free SQL execution
vsp --block-free-sql
```

### Confidence-Based Escalation

| AI Confidence | Action |
|---------------|--------|
| > 95% | Auto-execute with evidence |
| 80-95% | Human review required |
| < 80% | Human takeover |

---

## Value Proposition

### For Developers
- **3-5x faster** development with AI execution
- **No copy-paste** - AI writes directly to SAP
- **Instant feedback** - syntax check, tests in seconds

### For Engineering Leaders
- **Phase 1.5 today** - autonomous execution without waiting for SAP
- **Evidence generation** - supports Fast Track Release
- **Audit trails** - complete change attribution

### For Enterprise
- **Zero dependencies** - single binary, no npm/Node.js
- **Safety by design** - namespace, operation, package restrictions
- **Open source** - Apache 2.0, no vendor lock-in

---

## Autonomous Workflow Example

**Bug Fix: Jira → PR in minutes, not hours**

```yaml
workflow: autonomous_bug_fix
steps:
  1. Ingest Jira issue          # Parse requirements
  2. GrepObjects                 # Find affected code
  3. GetSource                   # Analyze with AI
  4. EditSource                  # Apply AI-generated fix
  5. SyntaxCheck                 # Validate syntax
  6. RunUnitTests                # Execute tests
  7. CreateTransport             # Package changes
  8. CreatePR                    # Human review gate
     - Attach test evidence
     - Link to Jira issue
     - Include change summary
```

**Result:** Developer reviews evidence bundle, approves PR, changes deploy.

---

## Key Metrics

| Metric | Value |
|--------|-------|
| **Tools** | 124 total (112 active, 12 legacy) - 85 in focused mode |
| **Unit Tests** | 244 |
| **Integration Tests** | 34 |
| **Supported Platforms** | 9 (Linux, macOS, Windows, FreeBSD, etc.) |
| **Object Types** | 158 (abapGit-compatible) |
| **Lua Bindings** | 40+ |
| **API Stability** | ADT REST APIs (stable since SAP 7.40) |
| **Version** | v2.21.0 |
| **License** | Apache 2.0 (open source) |

---

## Integration Points

| System | Integration |
|--------|-------------|
| **AI Assistants** | RooCode, Cline, Claude, GitHub Copilot (MCP protocol) |
| **SAP Joule** | MCP-compatible - direct integration path |
| **GitHub** | abapGit sync, PRs, Actions, evidence attachment |
| **Jira/Confluence** | Issue ingestion, documentation updates |
| **Deployment Pipeline** | Evidence publication, transport linking |
| **Tricentis** | Hybrid testing (vsp unit + Tricentis E2E) |

---

## Implementation Roadmap

### Phase 1: Foundation (0-3 months)
- [x] vsp core (124 tools, safety controls)
- [ ] Namespace enforcement (/CBA/*)
- [ ] HTTP mode support
- [ ] 3 SKILLS integration (Guidelines, Standards, Incidents)

### Phase 2: Enterprise Integration (3-6 months)
- [ ] Deployment pipeline integration
- [ ] Jira/GitHub workflow automation
- [ ] ATC quality gates
- [ ] Evidence bundle generation

### Phase 3: Autonomous Delivery (6-12 months)
- [ ] Multi-agent orchestration (Coding, Testing, Security, Performance)
- [ ] Governance layer (risk scoring, confidence thresholds)
- [ ] Observability platform (audit trails, dashboards)

---

## Features Roadmap

### Already Implemented ✅

| Feature | Tools | Status |
|---------|-------|--------|
| **Transport Workflow** | CreateTransport, ReleaseTransport, DeleteTransport, ListTransports, GetTransport, GetTransportInfo, GetUserTransports | ✅ Done (7 tools) |
| **ATC Integration** | RunATCCheck, GetATCCustomizing | ✅ Done (2 tools) |
| **Refactor Rename** | RenameObject | ✅ Done |
| **abapGit Import/Export** | ImportFromFile, ExportToFile, DeployFromFile, GitExport, GitTypes | ✅ Done (5 tools) |
| **Graph Traversal Engine** | GetCallGraph, GetCallersOf, GetCalleesOf, AnalyzeCallGraph, CompareCallGraphs, TraceExecution | ✅ Done (6 tools) |
| **Report Execution** | RunReport, RunReportAsync, GetVariants, GetTextElements, SetTextElements | ✅ Done (5 tools) |

### Planned - AI-Powered Features (CRITICAL)

| Feature | Description | Effort | Priority | Status |
|---------|-------------|--------|----------|--------|
| **ReviewCode + AutoFixIssue** | AI code review (security, best practices) with auto-fix | 2w | CRITICAL | Designed |
| **GenerateCode** | AI code generation with multi-agent workflow | 2w | CRITICAL | Designed |
| **NaturalLanguageQuery** | Query SAP in plain English/Spanish | 1w | HIGH | Designed |
| **GenerateDocumentation** | Auto-docs from source (Markdown, Confluence, Mermaid diagrams) | 1w | MEDIUM | Designed |
| **SmartJoin** | Auto-discover SAP table relationships for JOINs | 3d | LOW | Designed |

### Planned - Mid Wins (1-3 days each)

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

### Planned - Far Wins (1-2 weeks each)

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

## Getting Started

### 1. Install
```bash
# Download single binary (no dependencies)
curl -L https://github.com/vinchacho/vibing-steampunk/releases/latest/download/vsp-darwin-arm64 -o vsp
chmod +x vsp
```

### 2. Configure
```bash
# Environment variables
export SAP_URL=http://sap-dev:50000
export SAP_USER=developer
export SAP_PASSWORD=****
export SAP_CLIENT=001
```

### 3. Run
```bash
# Start MCP server (stdio mode for AI tools)
./vsp

# Or with safety controls
./vsp --allowed-packages "/CBA/*" --read-only
```

### 4. Connect AI Tool
```json
// Claude Code / RooCode MCP configuration
{
  "mcpServers": {
    "vsp": {
      "command": "/path/to/vsp",
      "args": ["--allowed-packages", "/CBA/*"]
    }
  }
}
```

---

## Summary

**vsp transforms AI-assisted SAP development from "suggest and copy-paste" to "plan, execute, and review."**

| Before vsp | After vsp |
|------------|-----------|
| AI suggests code | AI executes code |
| Manual testing | Automated evidence |
| Copy-paste workflow | Autonomous workflow |
| Waiting for SAP CLI | Operational today |
| No safety controls | Enterprise-grade safety |

**The execution layer your AI agents need - operational today.**

---

## Contact & Resources

| Resource | Link |
|----------|------|
| **GitHub** | https://github.com/vinchacho/vibing-steampunk |
| **Documentation** | See `/docs` folder |
| **Reports** | See `/reports` folder |
| **Version** | v2.21.0 |
| **License** | Apache 2.0 |

---

*vsp - Giving AI agents the hands to build in SAP*
