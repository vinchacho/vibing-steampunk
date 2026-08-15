package mcp

import (
	"context"
	"fmt"
	"io"
	"net/http"
	"regexp"
	"strings"
	"testing"

	"github.com/mark3labs/mcp-go/mcp"
	"github.com/vinchacho/vibing-steampunk/pkg/adt"
)

func TestValidateWriteSourceResult(t *testing.T) {
	tests := []struct {
		name    string
		result  *adt.WriteSourceResult
		wantErr string
	}{
		{name: "success", result: &adt.WriteSourceResult{Success: true}},
		{name: "nil result", wantErr: "no result"},
		{name: "diagnostic", result: &adt.WriteSourceResult{Message: "synthetic syntax failure"}, wantErr: "synthetic syntax failure"},
		{name: "missing diagnostic", result: &adt.WriteSourceResult{}, wantErr: "without a diagnostic"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			err := validateWriteSourceResult(tt.result)
			if tt.wantErr == "" {
				if err != nil {
					t.Fatalf("validateWriteSourceResult() error = %v", err)
				}
				return
			}
			if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
				t.Fatalf("validateWriteSourceResult() error = %v, want substring %q", err, tt.wantErr)
			}
		})
	}
}

// --- Impact-gate confirm round-trip through the MCP handler layer ---
//
// pkg/adt's routed mock (methodPathMock) is test-internal to that package,
// so a minimal equivalent lives here: an adt.HTTPDoer that routes on
// method + path substring and stamps a CSRF token on every response, which
// also satisfies the transport's /core/discovery token fetch.

type routedDoerRoute struct {
	method        string
	pathSubstring string
	status        int
	body          string
}

type routedDoer struct {
	routes []routedDoerRoute
}

func (d *routedDoer) Do(req *http.Request) (*http.Response, error) {
	h := http.Header{}
	h.Set("X-CSRF-Token", "test-token")
	for _, r := range d.routes {
		if r.method != "" && r.method != req.Method {
			continue
		}
		if r.pathSubstring == "" || strings.Contains(req.URL.Path, r.pathSubstring) {
			return &http.Response{
				StatusCode: r.status,
				Body:       io.NopCloser(strings.NewReader(r.body)),
				Header:     h,
			}, nil
		}
	}
	return &http.Response{
		StatusCode: http.StatusNotFound,
		Body:       io.NopCloser(strings.NewReader("not routed: " + req.Method + " " + req.URL.Path)),
		Header:     h,
	}, nil
}

const testCleanSyntaxXML = `<?xml version="1.0" encoding="UTF-8"?>
<chkrun:checkRunReports xmlns:chkrun="http://www.sap.com/adt/checkrun">
  <chkrun:checkReport chkrun:status="clean">
    <chkrun:checkMessageList/>
  </chkrun:checkReport>
</chkrun:checkRunReports>`

// testEmptyE071XML is an E071 lookup with zero rows so the impact
// transport-recency leg stops after the first RunQuery.
const testEmptyE071XML = `<?xml version="1.0" encoding="utf-8"?>
<dataPreview:tableData xmlns:dataPreview="http://www.sap.com/adt/dataPreview">
  <dataPreview:totalRows>0</dataPreview:totalRows>
</dataPreview:tableData>`

const testLocalLockXML = `<?xml version="1.0" encoding="UTF-8"?>
<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values><DATA>
    <LOCK_HANDLE>SYNTHETIC-HANDLE</LOCK_HANDLE>
  </DATA></asx:values>
</asx:abap>`

// testHighImpactUsageXML builds a usageReferences response with 30 distinct
// callers over two packages — enough to classify as risk "high".
func testHighImpactUsageXML() string {
	var b strings.Builder
	b.WriteString(`<?xml version="1.0" encoding="UTF-8"?>
<usageReferences:usageReferenceResult xmlns:usageReferences="http://www.sap.com/adt/ris/usageReferences" xmlns:adtcore="http://www.sap.com/adt/core">
  <usageReferences:referencedObjects>`)
	for i := 0; i < 30; i++ {
		pkg := "Z_PKG_A"
		if i%2 == 0 {
			pkg = "Z_PKG_B"
		}
		fmt.Fprintf(&b, `
    <usageReferences:referencedObject usageReferences:uri="/sap/bc/adt/oo/classes/zcl_demo_caller%03d" usageReferences:isResult="true">
      <adtcore:adtObject adtcore:uri="/sap/bc/adt/oo/classes/zcl_demo_caller%03d" adtcore:type="CLAS/OC" adtcore:name="ZCL_DEMO_CALLER%03d">
        <adtcore:packageRef adtcore:name="%s"/>
      </adtcore:adtObject>
    </usageReferences:referencedObject>`, i, i, i, pkg)
	}
	b.WriteString(`
  </usageReferences:referencedObjects>
</usageReferences:usageReferenceResult>`)
	return b.String()
}

// newImpactGateTestServer wires a Server to a mocked ADT backend where
// PROG ZTEST exists and has 30 callers, with the impact gate in block
// mode at threshold high. The full update flow (probe, impact legs,
// syntax check, lock, PUT, activation) is staged so a confirmed retry
// can succeed end-to-end.
func newImpactGateTestServer() *Server {
	doer := &routedDoer{routes: []routedDoerRoute{
		{method: "", pathSubstring: "discovery", status: http.StatusOK, body: ""},
		{method: http.MethodGet, pathSubstring: "/source/main", status: http.StatusOK, body: "REPORT ztest."},
		{method: http.MethodPost, pathSubstring: "usageReferences", status: http.StatusOK, body: testHighImpactUsageXML()},
		{method: http.MethodPost, pathSubstring: "datapreview/freestyle", status: http.StatusOK, body: testEmptyE071XML},
		{method: http.MethodPost, pathSubstring: "/checkruns", status: http.StatusOK, body: testCleanSyntaxXML},
		{method: http.MethodPost, pathSubstring: "/programs/programs/ZTEST", status: http.StatusOK, body: testLocalLockXML},
		{method: http.MethodPut, pathSubstring: "/source/main", status: http.StatusOK, body: ""},
		{method: http.MethodPost, pathSubstring: "/activation", status: http.StatusOK, body: ""},
	}}
	cfg := adt.NewConfig("https://sap.example.com:44300", "user", "pass")
	transport := adt.NewTransportWithClient(cfg, doer)
	client := adt.NewClientWithTransport(cfg, transport)
	client.Safety().ImpactGate = adt.ImpactGateBlock
	client.Safety().ImpactThreshold = adt.ImpactThresholdHigh
	return &Server{adtClient: client}
}

var testImpactTokenPattern = regexp.MustCompile(`impact-confirm-[0-9a-f]{32}`)

func resultText(t *testing.T, result *mcp.CallToolResult) string {
	t.Helper()
	if result == nil || len(result.Content) == 0 {
		t.Fatal("tool result has no content")
	}
	text, ok := result.Content[0].(mcp.TextContent)
	if !ok {
		t.Fatalf("Content[0] is %T, want mcp.TextContent", result.Content[0])
	}
	return text.Text
}

// TestHandleWriteSourceImpactConfirmRoundTrip is the MCP-layer contract for
// the confirm parameter: (a) under gate=block a high-risk WriteSource is
// refused with a tool error carrying an impact-confirm token; (b) the
// identical call with `confirm` set to that token succeeds; (c) reusing
// the consumed token re-blocks with a fresh token — the handler layer
// never caches or resurrects the confirmed ctx.
func TestHandleWriteSourceImpactConfirmRoundTrip(t *testing.T) {
	s := newImpactGateTestServer()
	args := map[string]any{
		"object_type": "PROG",
		"name":        "ZTEST",
		"source":      "REPORT ztest.",
		"mode":        "update",
	}

	// (a) blocked: tool error with a token in the text.
	result, err := s.handleWriteSource(context.Background(), newRequest(args))
	if err != nil {
		t.Fatalf("handleWriteSource() transport error = %v", err)
	}
	if !result.IsError {
		t.Fatalf("blocked write: IsError = false, text: %s", resultText(t, result))
	}
	blockedText := resultText(t, result)
	token := testImpactTokenPattern.FindString(blockedText)
	if token == "" {
		t.Fatalf("blocked write text carries no impact-confirm token: %s", blockedText)
	}

	// (b) identical call with confirm=token succeeds.
	args["confirm"] = token
	retry, err := s.handleWriteSource(context.Background(), newRequest(args))
	if err != nil {
		t.Fatalf("confirmed retry transport error = %v", err)
	}
	if retry.IsError {
		t.Fatalf("confirmed retry: IsError = true, text: %s", resultText(t, retry))
	}
	if text := resultText(t, retry); !strings.Contains(text, `"success": true`) {
		t.Fatalf("confirmed retry text lacks success marker: %s", text)
	}

	// (c) third call reusing the consumed token: re-blocked with a fresh token.
	reuse, err := s.handleWriteSource(context.Background(), newRequest(args))
	if err != nil {
		t.Fatalf("token-reuse call transport error = %v", err)
	}
	if !reuse.IsError {
		t.Fatalf("token reuse: IsError = false, text: %s", resultText(t, reuse))
	}
	if reissued := testImpactTokenPattern.FindString(resultText(t, reuse)); reissued == "" || reissued == token {
		t.Fatalf("token reuse must re-block with a fresh token, got %q (consumed %q)", reissued, token)
	}
}

// TestUniversalEditImpactConfirmRoundTrip proves the hyperfocused SAP tool
// forwards params.confirm into the gated handler: blocked without it,
// successful with it.
func TestUniversalEditImpactConfirmRoundTrip(t *testing.T) {
	s := newImpactGateTestServer()
	call := func(params map[string]any) *mcp.CallToolResult {
		t.Helper()
		result, err := s.handleUniversalTool(context.Background(), newRequest(map[string]any{
			"action": "edit",
			"target": "PROG ZTEST",
			"params": params,
		}))
		if err != nil {
			t.Fatalf("handleUniversalTool() transport error = %v", err)
		}
		return result
	}

	blocked := call(map[string]any{"source": "REPORT ztest.", "mode": "update"})
	if !blocked.IsError {
		t.Fatalf("blocked universal edit: IsError = false, text: %s", resultText(t, blocked))
	}
	token := testImpactTokenPattern.FindString(resultText(t, blocked))
	if token == "" {
		t.Fatalf("blocked universal edit carries no token: %s", resultText(t, blocked))
	}

	retry := call(map[string]any{"source": "REPORT ztest.", "mode": "update", "confirm": token})
	if retry.IsError {
		t.Fatalf("confirmed universal edit: IsError = true, text: %s", resultText(t, retry))
	}
}

// TestApplyImpactConfirm pins the helper contract: no/empty confirm leaves
// ctx untouched; a non-empty confirm wraps it.
func TestApplyImpactConfirm(t *testing.T) {
	ctx := context.Background()
	if got := applyImpactConfirm(ctx, newRequest(map[string]any{})); got != ctx {
		t.Error("missing confirm must return ctx unchanged")
	}
	if got := applyImpactConfirm(ctx, newRequest(map[string]any{"confirm": ""})); got != ctx {
		t.Error("empty confirm must return ctx unchanged")
	}
	if got := applyImpactConfirm(ctx, newRequest(map[string]any{"confirm": "impact-confirm-x"})); got == ctx {
		t.Error("non-empty confirm must wrap ctx")
	}
}
