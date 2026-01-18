# CBA MCP Servers as Just-In-Time SKILLS Pattern

**Date:** 2026-01-18
**Report ID:** 007
**Subject:** Design pattern for CBA MCP servers as agent SKILLS instead of direct context injection
**Related Documents:**
- `reports/2026-01-18-004-agent-skills-opportunity-analysis.md`
- `docs/SAP MCP.pdf`

---

## Executive Summary

**Problem**: Direct MCP server connections overwhelm agents with unnecessary context, reducing effectiveness and increasing token costs.

**Solution**: **Just-in-time SKILLS pattern** - Agents query CBA MCP servers only when specific information is needed, not upfront loading of all context.

**Benefit**: Agents stay focused, context remains relevant, token usage optimized, and information retrieval is targeted.

---

## The Problem with Direct MCP Context Injection

### Anti-Pattern: Context Overload

**Traditional MCP approach** (what we want to avoid):

```
┌─────────────┐
│  AI Agent   │
└──────┬──────┘
       │
       │ Connect to all MCP servers upfront
       ├────────────────────────────────────┐
       │                                    │
       ▼                                    ▼
┌──────────────────┐              ┌──────────────────┐
│ CBA ABAP Docs    │              │ CBA DB3 System   │
│ MCP Server       │              │ MCP Server       │
│                  │              │                  │
│ • 10,000 pages   │              │ • 50,000 objects │
│ • All keywords   │              │ • All tables     │
│ • All guardrails │              │ • All code       │
│ • All cheat sheets│             │ • All data       │
└──────────────────┘              └──────────────────┘
       │                                    │
       └────────────────┬───────────────────┘
                        ▼
              ┌──────────────────┐
              │  Agent Context   │
              │  (OVERLOADED)    │
              │                  │
              │  200,000 tokens  │
              │  Can't focus     │
              │  Irrelevant data │
              └──────────────────┘
```

**Problems**:
1. ❌ **Context overload**: Agent receives 200k+ tokens of irrelevant information
2. ❌ **Loss of focus**: Agent can't distinguish signal from noise
3. ❌ **Wasted tokens**: Paying for context that's never used
4. ❌ **Slower inference**: More context = slower AI responses
5. ❌ **Reduced accuracy**: Needle-in-haystack problem (relevant info buried)

---

## The Solution: Just-In-Time SKILLS Pattern

### Design Pattern: Lazy Context Loading

**SKILLS approach** (what we want):

```
┌─────────────┐
│  AI Agent   │
│             │
│  SKILLS:    │
│  • QueryCBAGuidelines
│  • QueryCBAExamples
│  • QueryDB3Objects
│  • ValidateAgainstStandards
└──────┬──────┘
       │
       │ Only query when skill is invoked
       │ (just-in-time retrieval)
       │
       ▼
┌──────────────────────────────────────┐
│  Example: Agent generating code      │
│                                      │
│  1. Agent identifies: "Need auth check"
│  2. Agent invokes skill: QueryCBAGuidelines("authorization")
│  3. Skill queries CBA MCP: "authorization check patterns"
│  4. MCP returns: ONLY relevant guidelines (500 tokens)
│  5. Agent uses context to generate code
│  6. Context discarded after use
└──────────────────────────────────────┘

Result: Agent used 500 tokens instead of 200,000 tokens
```

**Benefits**:
1. ✅ **Focused context**: Agent only receives information it needs
2. ✅ **Token efficiency**: Pay only for what's used
3. ✅ **Faster inference**: Less context = faster responses
4. ✅ **Better accuracy**: Relevant information is clear and targeted
5. ✅ **Scalable**: Can add unlimited MCP servers without context explosion

---

## CBA MCP Servers: SKILLS Catalog

### SKILL 1: QueryCBAGuidelines

**Purpose**: Retrieve CBA coding standards and architectural guardrails

**When to invoke**: Before generating code, when validating code, when uncertain about CBA conventions

**Implementation**:

```go
// vsp SKILL (not direct MCP connection)
type QueryCBAGuidelinesArgs struct {
    Topic    string   `json:"topic"`     // "authorization", "error_handling", "naming"
    Context  string   `json:"context"`   // "writing new class", "reviewing code"
    Keywords []string `json:"keywords"`  // ["AUTHORITY-CHECK", "CX_*"]
}

type CBAGuidelineResult struct {
    Guideline   string   `json:"guideline"`    // The actual guideline text
    Examples    []string `json:"examples"`     // Code examples
    AntiPatterns []string `json:"anti_patterns"` // What NOT to do
    References  []string `json:"references"`   // Links to full docs
}

func (s *Server) handleQueryCBAGuidelines(ctx context.Context, args map[string]interface{}) (*mcp.ToolResult, error) {
    topic, _ := getString(args, "topic")
    context, _ := getString(args, "context")

    // Query CBA ABAP Docs MCP server (just-in-time)
    query := fmt.Sprintf("CBA coding standard for %s in context: %s", topic, context)
    result := cbaDocsServer.Query(query)

    // Return ONLY relevant guidelines (not entire docs)
    return mcp.NewToolResultJSON(result), nil
}
```

**Example invocation**:

```json
{
  "tool": "QueryCBAGuidelines",
  "args": {
    "topic": "authorization",
    "context": "writing method to check user permissions",
    "keywords": ["AUTHORITY-CHECK"]
  }
}
```

**Result** (500 tokens instead of 200,000):
```json
{
  "guideline": "Always use AUTHORITY-CHECK before accessing sensitive data. Check object S_TABU_DIS for table authorization.",
  "examples": [
    "AUTHORITY-CHECK OBJECT 'S_TABU_DIS' ID 'DICBERCLS' FIELD lv_auth_group."
  ],
  "anti_patterns": [
    "Do not hardcode user names for authorization bypass",
    "Do not skip authorization checks in development (even $TMP)"
  ],
  "references": [
    "CBA Security Guidelines: Section 4.2",
    "SAP Help: AUTHORITY-CHECK statement"
  ]
}
```

---

### SKILL 2: QueryCBAExamples

**Purpose**: Find real code examples from CBA codebase that match a pattern

**When to invoke**: When learning CBA-specific coding patterns, when implementing similar functionality

**Implementation**:

```go
type QueryCBAExamplesArgs struct {
    Pattern     string `json:"pattern"`      // "authorization check", "error handling"
    ObjectType  string `json:"object_type"`  // "CLAS", "PROG", "FUGR"
    PackagePattern string `json:"package_pattern"` // "ZFI_*", "ZSD_*"
}

type CBAExampleResult struct {
    ObjectName  string `json:"object_name"`
    SourceSnippet string `json:"source_snippet"` // Just the relevant part
    Explanation string `json:"explanation"`
    Rating      float64 `json:"rating"` // Quality score
}

func (s *Server) handleQueryCBAExamples(ctx context.Context, args map[string]interface{}) (*mcp.ToolResult, error) {
    pattern, _ := getString(args, "pattern")

    // Query CBA DB3 System MCP server (just-in-time)
    query := fmt.Sprintf("Find code examples matching pattern: %s", pattern)
    examples := cbaDB3Server.Query(query)

    // Return top 3 examples (not all 50,000 objects)
    topExamples := rankAndLimit(examples, 3)

    return mcp.NewToolResultJSON(topExamples), nil
}
```

**Example invocation**:

```json
{
  "tool": "QueryCBAExamples",
  "args": {
    "pattern": "authorization check for table access",
    "object_type": "CLAS",
    "package_pattern": "ZFI_*"
  }
}
```

**Result** (1,000 tokens instead of 50,000 objects):
```json
[
  {
    "object_name": "ZCL_FI_TABLE_AUTH",
    "source_snippet": "AUTHORITY-CHECK OBJECT 'S_TABU_DIS' ID 'DICBERCLS' FIELD lv_auth.",
    "explanation": "Standard pattern for table authorization in FI packages",
    "rating": 9.5
  },
  {
    "object_name": "ZCL_FI_DATA_ACCESS",
    "source_snippet": "IF sy-subrc <> 0. RAISE EXCEPTION TYPE zcx_authorization_failed. ENDIF.",
    "explanation": "CBA exception pattern for authorization failures",
    "rating": 9.2
  }
]
```

---

### SKILL 3: ValidateAgainstCBAStandards

**Purpose**: Validate generated code against CBA coding standards and guardrails

**When to invoke**: After code generation, before committing code, during code review

**Implementation**:

```go
type ValidateAgainstCBAStandardsArgs struct {
    SourceCode string `json:"source_code"`
    ObjectType string `json:"object_type"`
}

type ValidationResult struct {
    IsCompliant bool              `json:"is_compliant"`
    Violations  []StandardViolation `json:"violations"`
    Score       float64           `json:"score"` // 0-100
}

type StandardViolation struct {
    Standard    string `json:"standard"`    // "CBA-SEC-001: Authorization checks"
    Severity    string `json:"severity"`    // "CRITICAL", "WARNING"
    Line        int    `json:"line"`
    Message     string `json:"message"`
    Suggestion  string `json:"suggestion"`
}

func (s *Server) handleValidateAgainstCBAStandards(ctx context.Context, args map[string]interface{}) (*mcp.ToolResult, error) {
    sourceCode, _ := getString(args, "source_code")

    // Query CBA guardrails (just-in-time)
    guardrails := cbaDocsServer.Query("all coding standards and guardrails")

    // AI validates code against guardrails
    validation := ai.ValidateCode(sourceCode, guardrails)

    return mcp.NewToolResultJSON(validation), nil
}
```

**Example invocation**:

```json
{
  "tool": "ValidateAgainstCBAStandards",
  "args": {
    "source_code": "METHOD get_invoice_data. SELECT * FROM bsid INTO TABLE lt_data. ENDMETHOD.",
    "object_type": "CLAS"
  }
}
```

**Result**:
```json
{
  "is_compliant": false,
  "violations": [
    {
      "standard": "CBA-SEC-001: Authorization checks required",
      "severity": "CRITICAL",
      "line": 2,
      "message": "Missing authorization check for table BSID",
      "suggestion": "Add AUTHORITY-CHECK OBJECT 'S_TABU_DIS' before SELECT"
    },
    {
      "standard": "CBA-PERF-003: Avoid SELECT *",
      "severity": "WARNING",
      "line": 2,
      "message": "SELECT * is inefficient, specify fields explicitly",
      "suggestion": "SELECT bukrs, belnr, dmbtr FROM bsid"
    }
  ],
  "score": 45.0
}
```

---

### SKILL 4: QueryDB3Object

**Purpose**: Retrieve specific objects from CBA DB3 development system

**When to invoke**: When analyzing dependencies, when studying existing implementations, when validating assumptions

**Implementation**:

```go
type QueryDB3ObjectArgs struct {
    ObjectName string `json:"object_name"`
    ObjectType string `json:"object_type"`
    Include    string `json:"include"` // "source", "structure", "dependencies"
}

func (s *Server) handleQueryDB3Object(ctx context.Context, args map[string]interface{}) (*mcp.ToolResult, error) {
    objectName, _ := getString(args, "object_name")
    objectType, _ := getString(args, "object_type")

    // Query CBA DB3 System MCP (just-in-time)
    query := fmt.Sprintf("Get object %s type %s", objectName, objectType)
    object := cbaDB3Server.Query(query)

    return mcp.NewToolResultJSON(object), nil
}
```

**Example invocation**:

```json
{
  "tool": "QueryDB3Object",
  "args": {
    "object_name": "ZCL_FI_INVOICE_PROCESSOR",
    "object_type": "CLAS",
    "include": "structure"
  }
}
```

**Result** (2,000 tokens for ONE object instead of 50,000):
```json
{
  "object_name": "ZCL_FI_INVOICE_PROCESSOR",
  "object_type": "CLAS",
  "package": "$ZFI_INVOICES",
  "methods": [
    {
      "name": "PROCESS_INVOICE",
      "parameters": [
        {"name": "IV_INVOICE_ID", "type": "BELNR"}
      ],
      "exceptions": ["ZCX_INVOICE_ERROR"]
    }
  ],
  "dependencies": [
    "ZCL_FI_AUTH_CHECK",
    "ZCL_FI_DATA_ACCESS"
  ]
}
```

---

### SKILL 5: LearnFromCBAIncidents

**Purpose**: Query historical production incidents to avoid known anti-patterns

**When to invoke**: Before generating code, when reviewing code, when investigating similar issues

**Implementation**:

```go
type LearnFromCBAIncidentsArgs struct {
    Keyword     string `json:"keyword"`     // "unicode", "memory leak", "timeout"
    Severity    string `json:"severity"`    // "CRITICAL", "HIGH", "MEDIUM"
    MaxResults  int    `json:"max_results"` // Limit results to avoid overload
}

type IncidentLearning struct {
    IncidentID   string `json:"incident_id"`
    Description  string `json:"description"`
    RootCause    string `json:"root_cause"`
    AntiPattern  string `json:"anti_pattern"` // What code pattern caused it
    Prevention   string `json:"prevention"`   // How to avoid in future
    DateOccurred string `json:"date_occurred"`
}

func (s *Server) handleLearnFromCBAIncidents(ctx context.Context, args map[string]interface{}) (*mcp.ToolResult, error) {
    keyword, _ := getString(args, "keyword")
    maxResults, _ := getInt(args, "max_results")

    // Query CBA Knowledge MCP server (just-in-time)
    query := fmt.Sprintf("Production incidents related to: %s", keyword)
    incidents := cbaKnowledgeServer.Query(query)

    // Limit results
    limitedIncidents := incidents[:min(len(incidents), maxResults)]

    return mcp.NewToolResultJSON(limitedIncidents), nil
}
```

**Example invocation**:

```json
{
  "tool": "LearnFromCBAIncidents",
  "args": {
    "keyword": "SELECT in loop",
    "severity": "CRITICAL",
    "max_results": 3
  }
}
```

**Result** (500 tokens for 3 incidents instead of entire incident database):
```json
[
  {
    "incident_id": "INC-2024-1234",
    "description": "Production timeout in invoice processing",
    "root_cause": "SELECT statement inside LOOP processing 100k records",
    "anti_pattern": "LOOP AT lt_invoices. SELECT SINGLE * FROM bsid WHERE belnr = lv_id. ENDLOOP.",
    "prevention": "Use SELECT ... FOR ALL ENTRIES or JOIN instead of SELECT in LOOP",
    "date_occurred": "2024-11-15"
  }
]
```

---

## Workflow: Agent Using Just-In-Time SKILLS

### Example: Autonomous Bug Fix Agent

**Scenario**: Agent receives bug report: "Invoice processing times out for large datasets"

**Traditional approach (context overload)**:
```
1. Agent connects to all MCP servers
2. Agent loads 200k tokens of CBA context
3. Agent tries to find relevant information (needle in haystack)
4. Agent generates fix (possibly using irrelevant context)
```

**SKILLS approach (just-in-time)**:
```
1. Agent receives bug report: "Invoice processing times out"
   └─> Identifies keyword: "timeout"

2. Agent invokes SKILL: LearnFromCBAIncidents("timeout", "CRITICAL", 3)
   └─> Receives: 3 past incidents (500 tokens)
   └─> Learns: "SELECT in LOOP causes timeouts"

3. Agent gets source code: GetSource("ZCL_INVOICE_PROCESSOR")
   └─> Finds: LOOP with SELECT inside

4. Agent invokes SKILL: QueryCBAGuidelines("performance optimization")
   └─> Receives: "Use SELECT FOR ALL ENTRIES" (300 tokens)

5. Agent invokes SKILL: QueryCBAExamples("SELECT FOR ALL ENTRIES pattern")
   └─> Receives: 2 code examples from CBA codebase (800 tokens)

6. Agent generates fix using targeted context (1,600 tokens total vs 200k)

7. Agent invokes SKILL: ValidateAgainstCBAStandards(fixed_code)
   └─> Receives: Validation passed, score 95/100

8. Agent deploys fix via vsp tools
```

**Result**:
- ✅ **Token usage**: 1,600 tokens (99.2% reduction from 200k)
- ✅ **Relevance**: 100% of context was relevant
- ✅ **Speed**: Faster inference (less context)
- ✅ **Accuracy**: Higher quality fix (focused context)

---

## Implementation Architecture

### vsp SKILLS Registry

```go
// pkg/skills/registry.go
type SKILLRegistry struct {
    skills map[string]SKILL
    mcpServers map[string]MCPServer
}

type SKILL struct {
    Name        string
    Description string
    Handler     SKILLHandler
    MCPServer   string // Which MCP server to query
}

type SKILLHandler func(ctx context.Context, args map[string]interface{}) (*Result, error)

// Register CBA SKILLS
func RegisterCBASkills(registry *SKILLRegistry) {
    registry.Register(SKILL{
        Name: "QueryCBAGuidelines",
        Description: "Query CBA coding standards and guardrails",
        Handler: handleQueryCBAGuidelines,
        MCPServer: "cba-abap-documentation",
    })

    registry.Register(SKILL{
        Name: "QueryCBAExamples",
        Description: "Find code examples from CBA DB3 system",
        Handler: handleQueryCBAExamples,
        MCPServer: "cba-db3-system",
    })

    registry.Register(SKILL{
        Name: "ValidateAgainstCBAStandards",
        Description: "Validate code against CBA standards",
        Handler: handleValidateAgainstCBAStandards,
        MCPServer: "cba-abap-documentation",
    })

    registry.Register(SKILL{
        Name: "QueryDB3Object",
        Description: "Retrieve specific object from DB3",
        Handler: handleQueryDB3Object,
        MCPServer: "cba-db3-system",
    })

    registry.Register(SKILL{
        Name: "LearnFromCBAIncidents",
        Description: "Query historical production incidents",
        Handler: handleLearnFromCBAIncidents,
        MCPServer: "cba-knowledge-base",
    })
}
```

### MCP Server Connection Pool

```go
// pkg/skills/mcp_pool.go
type MCPServerPool struct {
    servers map[string]MCPServer
}

type MCPServer struct {
    Name     string
    URL      string
    Auth     AuthConfig
    Cache    *cache.Cache // Cache frequent queries
    RateLimit *ratelimit.Limiter
}

func (p *MCPServerPool) Query(serverName, query string) (interface{}, error) {
    server := p.servers[serverName]

    // Check cache first (avoid redundant queries)
    if cached := server.Cache.Get(query); cached != nil {
        return cached, nil
    }

    // Rate limit (avoid overwhelming MCP server)
    server.RateLimit.Wait()

    // Query MCP server
    result, err := server.Query(query)
    if err != nil {
        return nil, err
    }

    // Cache result
    server.Cache.Set(query, result, 1*time.Hour)

    return result, nil
}
```

---

## Benefits Summary

### Token Efficiency

| Approach | Tokens Used | Cost (Sonnet 4.5) | Inference Time |
|----------|-------------|-------------------|----------------|
| **Direct MCP** | 200,000 | $0.60 per request | ~5 seconds |
| **SKILLS (JIT)** | 1,600 | $0.005 per request | ~0.5 seconds |
| **Savings** | **99.2%** | **99.2%** | **90%** |

### Context Relevance

| Approach | Relevant Tokens | Irrelevant Tokens | Signal/Noise Ratio |
|----------|----------------|-------------------|-------------------|
| **Direct MCP** | 1,600 | 198,400 | 0.008 (0.8%) |
| **SKILLS (JIT)** | 1,600 | 0 | 1.0 (100%) |

### Scalability

**Direct MCP approach**:
```
3 MCP servers × 70k tokens each = 210k tokens upfront
Add 4th server → 280k tokens (context limit exceeded!)
```

**SKILLS approach**:
```
10 MCP servers × 0 tokens upfront = 0 tokens
Query only when needed → Each query 500-2000 tokens
Can add unlimited MCP servers without context explosion
```

---

## Conclusion

**CBA MCP servers should be SKILLS, not direct connections**.

**Benefits**:
1. ✅ **99.2% token reduction** (1.6k vs 200k)
2. ✅ **100% context relevance** (no irrelevant data)
3. ✅ **10x faster inference** (less context to process)
4. ✅ **Unlimited scalability** (add MCP servers without limit)
5. ✅ **Better accuracy** (focused, targeted information)

**Implementation**:
- 5 CBA SKILLS (QueryGuidelines, QueryExamples, Validate, QueryObject, LearnIncidents)
- MCP server connection pool (caching, rate limiting)
- SKILLS registry (extensible for future skills)

This pattern should be **the standard for all MCP integrations** in vsp, not just CBA.

---

**Project Repository**: https://github.com/vinchacho/vibing-steampunk
**Current Version**: v2.21.0
**Target**: v3.0.0 (with SKILLS framework)
