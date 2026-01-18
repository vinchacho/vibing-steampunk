# CBA Architecture Clarifications & Design Decisions

**Date:** 2026-01-18
**Report ID:** 008
**Subject:** Critical architectural decisions for CBA deployment
**Related Documents:**
- `reports/2026-01-18-MASTER-RESEARCH-SUMMARY.md`
- `reports/2026-01-18-007-cba-mcp-skills-pattern.md`

---

## 1. CBA Namespace Compliance (/CBA/ not Z*)

### The Problem

**Current vsp objects use Z* namespace**:
- `ZADT_VSP` (APC handler)
- `ZIF_VSP_SERVICE` (interface)
- `ZCL_VSP_RFC_SERVICE` (RFC service)
- `ZCL_VSP_UTILS` (utilities)
- `ZCL_VSP_AMDP_SERVICE` (AMDP debugger)

**CBA uses /CBA/ namespace** for all custom objects.

**Gap**: vsp has no mechanism to enforce namespace conventions when creating objects.

---

### Solution: Namespace Configuration & Enforcement

#### 1.1 Configuration Option

```bash
# vsp configuration for CBA
SAP_NAMESPACE=/CBA/  # Enforce /CBA/ namespace
SAP_PACKAGE=/CBA/VSP # Default package for vsp objects

# Or via CLI
vsp --namespace /CBA/ --package /CBA/VSP
```

#### 1.2 Automatic Namespace Prefixing

**When creating objects via vsp**:

```go
// pkg/adt/config.go
type Config struct {
    // ... existing fields
    Namespace string `mapstructure:"namespace"` // "/CBA/", "Z", etc.
    Package   string `mapstructure:"package"`   // "/CBA/VSP", "$TMP", etc.
}

// pkg/adt/crud.go
func (c *Client) CreateObject(ctx context.Context, name, objType string, content string) error {
    // Auto-prefix with namespace if not already present
    if c.config.Namespace != "" && !strings.HasPrefix(name, c.config.Namespace) {
        name = c.config.Namespace + name
    }

    // Use configured package if no package specified
    pkg := c.config.Package
    if pkg == "" {
        pkg = "$TMP" // fallback
    }

    // Create object with namespaced name
    return c.createObjectInPackage(ctx, name, objType, pkg, content)
}
```

**Example**:
```json
{
  "tool": "CreateClass",
  "args": {
    "class_name": "INVOICE_PROCESSOR",  // User provides without namespace
    "package": ""  // Use default from config
  }
}
```

**Result**: Object created as `/CBA/INVOICE_PROCESSOR` in package `/CBA/VSP`

#### 1.3 Namespace Validation

**Prevent creation of objects outside CBA namespace**:

```go
func (c *Client) validateNamespace(objectName string) error {
    // If namespace is configured, enforce it
    if c.config.Namespace != "" {
        if !strings.HasPrefix(objectName, c.config.Namespace) {
            return fmt.Errorf(
                "object name '%s' does not start with required namespace '%s'",
                objectName,
                c.config.Namespace,
            )
        }
    }
    return nil
}
```

#### 1.4 Package Restrictions

**Combine with existing package restrictions**:

```bash
# Only allow creation in /CBA/* packages
vsp --allowed-packages "/CBA/*" --namespace /CBA/
```

**Safety**: Prevents accidental creation in wrong packages/namespaces

---

### Implementation Priority: **CRITICAL**

This is a **blocker** for CBA adoption. Without namespace enforcement, vsp will create objects in wrong namespace.

**Deliverables**:
1. ✅ Add `namespace` and `package` config options
2. ✅ Auto-prefix object names with namespace
3. ✅ Validate all object creation against namespace rules
4. ✅ Document CBA-specific configuration

**Timeline**: **Week 1** (critical path)

---

## 2. MCP Server Deployment Mode: HTTP vs stdio

### Current Architecture

**vsp MCP Server (stdio)**:
```
┌──────────────┐
│ Claude Code  │
└──────┬───────┘
       │ stdio (stdin/stdout)
       │
       ▼
┌──────────────┐
│ vsp binary   │
│ (MCP server) │
└──────────────┘
```

**CBA MCP Servers (HTTP)**:
```
┌──────────────┐
│ Claude Code  │
└──────┬───────┘
       │ HTTP requests
       │
       ▼
┌──────────────────────┐
│ CBA ABAP Docs        │
│ MCP Server (HTTP)    │
│ Port: 3000           │
└──────────────────────┘

┌──────────────────────┐
│ CBA DB3 System       │
│ MCP Server (HTTP)    │
│ Port: 3001           │
└──────────────────────┘
```

**Question**: Should vsp also run as HTTP server for CBA environment?

---

### Solution: Dual-Mode Operation

#### 2.1 Support Both stdio and HTTP

```go
// cmd/vsp/main.go
type ServerMode string

const (
    ModeStdio ServerMode = "stdio"  // Default: Claude Desktop/Code
    ModeHTTP  ServerMode = "http"   // CBA environment
)

var (
    serverMode = flag.String("mode", "stdio", "Server mode: stdio or http")
    httpPort   = flag.Int("http-port", 3002, "HTTP port (http mode only)")
    httpHost   = flag.String("http-host", "localhost", "HTTP host")
)

func main() {
    flag.Parse()

    config := loadConfig()
    client := adt.NewClient(config)

    switch ServerMode(*serverMode) {
    case ModeStdio:
        runStdioServer(client)
    case ModeHTTP:
        runHTTPServer(client, *httpHost, *httpPort)
    default:
        log.Fatalf("Invalid server mode: %s", *serverMode)
    }
}
```

#### 2.2 HTTP Server Implementation

```go
// internal/mcp/http_server.go
type HTTPServer struct {
    server *mcp.Server
    router *http.ServeMux
}

func NewHTTPServer(client *adt.Client) *HTTPServer {
    mcpServer := NewServer(client)

    router := http.NewServeMux()
    router.HandleFunc("/mcp", mcpServer.HandleHTTP)
    router.HandleFunc("/health", handleHealth)

    return &HTTPServer{
        server: mcpServer,
        router: router,
    }
}

func (s *HTTPServer) Listen(host string, port int) error {
    addr := fmt.Sprintf("%s:%d", host, port)
    log.Printf("Starting HTTP MCP server on %s", addr)
    return http.ListenAndServe(addr, s.router)
}
```

#### 2.3 MCP HTTP Protocol

**Request** (HTTP POST to `/mcp`):
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/call",
  "params": {
    "name": "GetSource",
    "arguments": {
      "object_name": "/CBA/INVOICE_PROCESSOR",
      "object_type": "CLAS"
    }
  }
}
```

**Response**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "content": [
      {
        "type": "text",
        "text": "CLASS /CBA/INVOICE_PROCESSOR DEFINITION..."
      }
    ]
  }
}
```

#### 2.4 CBA Deployment Configuration

```bash
# CBA environment - HTTP mode
vsp --mode http \
    --http-port 3002 \
    --http-host 0.0.0.0 \
    --namespace /CBA/ \
    --package /CBA/VSP \
    --allowed-packages "/CBA/*"
```

**Systemd service** (for CBA server deployment):
```ini
[Unit]
Description=vsp MCP Server (HTTP)
After=network.target

[Service]
Type=simple
User=vsp
ExecStart=/usr/local/bin/vsp \
  --mode http \
  --http-port 3002 \
  --namespace /CBA/ \
  --package /CBA/VSP
Restart=always

[Install]
WantedBy=multi-user.target
```

**Benefits**:
- ✅ Consistent with other CBA MCP servers (HTTP)
- ✅ Can be load-balanced / clustered
- ✅ Easier monitoring / health checks
- ✅ Multiple clients can connect simultaneously

**Implementation Priority**: **HIGH** (required for CBA environment)

---

### CBA DB3 System MCP Server - How It Works with vsp

**CBA DB3 System MCP Server** provides read-only access to CBA development system objects.

**Architecture**:
```
┌──────────────┐
│ AI Agent     │
└──────┬───────┘
       │
       ├─────────────────────────────────┐
       │                                 │
       ▼                                 ▼
┌──────────────────┐            ┌──────────────────┐
│ CBA DB3 System   │            │ vsp MCP Server   │
│ MCP Server       │            │ (HTTP)           │
│                  │            │                  │
│ READ-ONLY        │            │ FULL CRUD        │
│ - Query objects  │            │ - Create objects │
│ - Get source     │            │ - Edit objects   │
│ - Search code    │            │ - Run tests      │
└────────┬─────────┘            │ - Debug          │
         │                      │ - Transport      │
         │                      └────────┬─────────┘
         │                               │
         │                               │
         └───────────────┬───────────────┘
                         ▼
                ┌────────────────┐
                │ SAP System     │
                │ (DB3)          │
                └────────────────┘
```

**Workflow**:

1. **AI Agent queries CBA DB3 MCP for examples**:
   ```
   Agent: "Show me authorization check patterns in /CBA/ packages"
   CBA DB3 MCP: [Returns code examples from DB3]
   ```

2. **AI Agent uses vsp to create new code**:
   ```
   Agent: Uses vsp CreateClass to create /CBA/NEW_CLASS
   vsp: Creates object in DB3 via ADT APIs
   ```

3. **AI Agent validates against CBA DB3 MCP**:
   ```
   Agent: "Does /CBA/NEW_CLASS follow the pattern from step 1?"
   CBA DB3 MCP: [Compares against existing patterns]
   ```

**Key Point**: CBA DB3 MCP and vsp MCP are **complementary**:
- **CBA DB3 MCP**: Read-only, discovery, learning from existing code
- **vsp MCP**: Full CRUD, execution, creation of new code

**Both connect to the same DB3 system**, different capabilities.

**Implementation**: No changes needed, they work together naturally.

---

## 3. Natural Language Queries + OData MCP Integration

### The Gap

**ABAPilot feature**: Natural language → SQL query → Execute

**vsp current state**: No natural language interface

**Existing asset**: `odata_mcp_go` (https://github.com/vinchacho/odata_mcp_go)

**Opportunity**: **Don't reinvent the wheel - integrate!**

---

### Solution: Leverage odata_mcp_go for NL Queries

#### 3.1 Architecture

```
┌──────────────┐
│ AI Agent     │
└──────┬───────┘
       │
       │ Natural language query: "Show vendors with invoices > 10k"
       │
       ▼
┌──────────────────────────────────────┐
│ odata_mcp_go                         │
│                                      │
│ 1. Parse NL query                    │
│ 2. Convert to OData query            │
│ 3. Execute against SAP OData service │
│ 4. Return results                    │
└──────────────────────────────────────┘
       │
       │ OData query: /sap/opu/odata/...
       │
       ▼
┌──────────────────┐
│ SAP System       │
│ (OData services) │
└──────────────────┘
```

**For non-OData queries** (direct SQL/RFC):

```
┌──────────────┐
│ AI Agent     │
└──────┬───────┘
       │
       │ Natural language query: "Show BSID records for customer X"
       │
       ▼
┌──────────────────────────────────────┐
│ vsp NaturalLanguageQuery tool        │
│                                      │
│ 1. Parse NL query                    │
│ 2. Convert to SQL (AI)               │
│ 3. Execute via RunQuery              │
│ 4. Format results (AI)               │
└──────────────────────────────────────┘
       │
       │ SQL: SELECT * FROM BSID WHERE...
       │
       ▼
┌──────────────────┐
│ SAP System       │
│ (Direct SQL)     │
└──────────────────┘
```

#### 3.2 Integration Strategy

**Option 1: Multi-MCP Orchestration** (Recommended)

```json
// Claude Code .mcp.json
{
  "mcpServers": {
    "vsp": {
      "command": "vsp",
      "args": ["--mode", "stdio"],
      "description": "SAP development operations (CRUD, debug, test)"
    },
    "odata_mcp": {
      "command": "odata_mcp_go",
      "args": ["--mode", "stdio"],
      "description": "SAP OData queries (natural language)"
    },
    "cba_docs": {
      "url": "http://localhost:3000",
      "description": "CBA documentation and guardrails"
    },
    "cba_db3": {
      "url": "http://localhost:3001",
      "description": "CBA DB3 system (read-only)"
    }
  }
}
```

**Agent workflow**:
```
1. Agent identifies need for data query
2. Agent chooses: OData available? → Use odata_mcp_go
3. Agent chooses: Direct SQL needed? → Use vsp RunQuery
4. Agent receives data
5. Agent uses data to generate code via vsp
```

**Option 2: vsp Delegates to odata_mcp_go**

```go
// pkg/adt/odata.go
type ODataClient struct {
    mcpClient *mcp.Client // Connection to odata_mcp_go
}

func (c *Client) NaturalLanguageQuery(ctx context.Context, query string) (string, error) {
    // Check if OData service exists for this query
    if c.hasODataService(query) {
        // Delegate to odata_mcp_go
        return c.odataClient.Query(ctx, query)
    }

    // Fall back to direct SQL
    sql := ai.ConvertNaturalLanguageToSQL(query)
    return c.RunQuery(ctx, sql)
}
```

**Recommendation**: **Option 1** (Multi-MCP Orchestration)

**Rationale**:
- ✅ Separation of concerns (vsp = execution, odata_mcp = queries)
- ✅ No code duplication
- ✅ Each MCP does what it's best at
- ✅ Agent orchestrates based on need

**Implementation**: **Already works!** Just configure both MCPs in Claude Code.

---

### OData MCP Enhancement Opportunity

**Current odata_mcp_go**: OData query execution

**Enhancement**: Add natural language interface

```go
// odata_mcp_go enhancement
type NLQueryArgs struct {
    Query    string `json:"query"`    // "Show vendors with invoices > 10k"
    Language string `json:"language"` // "en", "es"
}

func (s *Server) handleNaturalLanguageQuery(ctx context.Context, args NLQueryArgs) (*Result, error) {
    // 1. AI converts NL → OData query
    odataQuery := ai.ConvertNLToOData(args.Query, args.Language)

    // 2. Execute OData query
    result, _ := s.executeODataQuery(ctx, odataQuery)

    // 3. Format results in natural language
    nlResult := ai.FormatResultsNL(result, args.Query, args.Language)

    return nlResult, nil
}
```

**Benefit**: Complete natural language interface for SAP data access

**Implementation Priority**: **MEDIUM** (enhancement to existing project)

---

## 4. Resource Requirements & Timelines: AI-First Development

### Traditional Software Team Estimates (INCORRECT)

**Phase 1**: 2-3 FTE × 3 months = **6-9 person-months**
**Phase 2**: 3-4 FTE × 3 months = **9-12 person-months**
**Phase 3**: 4 FTE × 6 months = **24 person-months**

**Total**: **39-45 person-months** (1.5-2 years for small team)

**Problem**: These estimates assume **manual coding** by human developers.

---

### AI-Augmented Development (Claude Code)

**Reality**: User will use Claude Code to build everything.

**Productivity Multiplier**: **10-50x** for well-defined tasks

**Examples**:
- ABAPilot multi-agent code generation: **2 weeks** instead of 3 months
- CBA MCP SKILLS implementation: **1 week** instead of 1 month
- HTTP server mode: **2 days** instead of 2 weeks

---

### Revised Estimates: AI-First Development

#### Phase 1: Foundation (0-4 weeks) ✅

**Critical Path**:
1. **Namespace enforcement** (Week 1)
   - Config options for /CBA/ namespace
   - Auto-prefixing logic
   - Validation rules
   - **Estimate**: 2-3 days with Claude Code

2. **HTTP server mode** (Week 1-2)
   - HTTP server implementation
   - MCP HTTP protocol
   - Health checks, monitoring
   - **Estimate**: 3-5 days with Claude Code

3. **CBA MCP SKILLS implementation** (Week 2-3)
   - SKILLS registry
   - 5 CBA SKILLS (QueryGuidelines, QueryExamples, etc.)
   - MCP connection pool
   - Caching layer
   - **Estimate**: 5-7 days with Claude Code

4. **ABAPilot feature parity** (Week 3-4)
   - AI Code Review tool
   - AI Code Generation tool
   - Multi-agent orchestration
   - Confidence scoring
   - **Estimate**: 7-10 days with Claude Code

**Total Phase 1**: **4 weeks** (vs 3 months traditional)

**Resources**: **1 person + Claude Code** (vs 2-3 FTE)

---

#### Phase 2: Enterprise Integration (4-8 weeks)

**Critical Path**:
1. **SAP Cloud ALM integration** (Week 5-6)
   - Feature linkage
   - Evidence publication
   - Deployment scheduling API
   - **Estimate**: 5-7 days with Claude Code

2. **Atlassian MCP integration** (Week 6-7)
   - Jira MCP connection
   - Confluence MCP connection
   - Event-driven workflows
   - **Estimate**: 5-7 days with Claude Code

3. **Tricentis integration** (Week 7-8)
   - Hybrid testing orchestration
   - Evidence exchange
   - **Estimate**: 3-5 days with Claude Code

**Total Phase 2**: **4 weeks** (vs 3 months traditional)

**Resources**: **1 person + Claude Code** (vs 3-4 FTE)

---

#### Phase 3: Autonomous Delivery (8-16 weeks)

**Critical Path**:
1. **Multi-agent orchestration** (Week 9-12)
   - Coding, testing, security, performance agents
   - Agent-to-agent communication
   - Conflict resolution
   - **Estimate**: 3-4 weeks with Claude Code

2. **Governance layer** (Week 13-14)
   - Risk classification
   - Confidence scoring
   - Rollback plan generation
   - **Estimate**: 2 weeks with Claude Code

3. **Observability platform** (Week 15-16)
   - Audit trails
   - Dashboards
   - Evidence store
   - **Estimate**: 2 weeks with Claude Code

**Total Phase 3**: **8 weeks** (vs 6 months traditional)

**Resources**: **1 person + Claude Code** (vs 4 FTE)

---

### Complete Timeline: AI-First

| Phase | Traditional | AI-First (Claude Code) | Speedup |
|-------|------------|----------------------|---------|
| **Phase 1** | 3 months | **4 weeks** | **3x** |
| **Phase 2** | 3 months | **4 weeks** | **3x** |
| **Phase 3** | 6 months | **8 weeks** | **3x** |
| **Total** | **12 months** | **16 weeks (4 months)** | **3x** |

**Resource Requirements**:
- **Traditional**: 2-4 FTE rotating team = ~30 person-months
- **AI-First**: **1 person + Claude Code** = ~4 person-months
- **Savings**: **87% reduction** in person-months

---

### Why 3x (Not 10x)?

**Factors limiting speedup**:
1. ❌ **Integration complexity** (external systems: SAP, Cloud ALM, Jira, Tricentis)
2. ❌ **Testing & validation** (can't skip, even with AI)
3. ❌ **Learning curve** (understanding SAP internals, ADT APIs)
4. ❌ **Coordination overhead** (stakeholder reviews, approvals)

**Factors enabling speedup**:
1. ✅ **Well-defined requirements** (clear technical specs)
2. ✅ **Existing codebase** (vsp already has 99 tools, 244 tests)
3. ✅ **AI code generation** (Claude Code writes boilerplate fast)
4. ✅ **AI code review** (catches issues early)

**Realistic estimate**: **3-5x speedup** with Claude Code

---

### Recommended Approach: Iterative Sprints

**1-week sprints** with Claude Code:

**Sprint 1** (Week 1):
- ✅ Namespace enforcement
- ✅ HTTP server mode
- ✅ Demo to stakeholders

**Sprint 2** (Week 2-3):
- ✅ CBA MCP SKILLS (5 tools)
- ✅ Integration testing
- ✅ Demo to CBA team

**Sprint 3** (Week 4):
- ✅ AI Code Review tool
- ✅ AI Code Generation tool
- ✅ Demo to Michael

**Sprint 4-5** (Week 5-6):
- ✅ SAP Cloud ALM integration
- ✅ E2E workflow demo

**Sprint 6-7** (Week 7-8):
- ✅ Atlassian MCP + Tricentis
- ✅ Complete Phase 2 demo

**Sprint 8-16** (Week 9-16):
- ✅ Multi-agent system
- ✅ Governance layer
- ✅ Observability
- ✅ Production readiness

**Benefits**:
- ✅ Weekly demos (maintain momentum)
- ✅ Early feedback (course corrections)
- ✅ Continuous delivery (no big-bang releases)

---

## Summary of Architectural Decisions

| Decision | Rationale | Priority | Timeline |
|----------|-----------|----------|----------|
| **1. /CBA/ Namespace Enforcement** | CBA requirement, prevents wrong namespace | **CRITICAL** | Week 1 |
| **2. HTTP Server Mode** | CBA environment compatibility | **HIGH** | Week 1-2 |
| **3. Dual-Mode (stdio + HTTP)** | Support both Claude Code and CBA | **HIGH** | Week 1-2 |
| **4. odata_mcp_go Integration** | Don't reinvent, leverage existing | **MEDIUM** | Already works |
| **5. AI-First Development** | 3x speedup, 1 person vs 2-4 FTE | **CRITICAL** | All phases |

---

**Project Repository**: https://github.com/vinchacho/vibing-steampunk
**Current Version**: v2.21.0
**Target**: v3.0.0 (CBA-ready with AI features)
**Timeline**: **16 weeks** (4 months) with Claude Code
