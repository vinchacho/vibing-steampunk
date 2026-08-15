package adt

import (
	"context"
	"io"
	"net/http"
	"reflect"
	"strings"
	"testing"
	"time"
)

func containsAll(s string, subs ...string) bool {
	for _, sub := range subs {
		if !strings.Contains(s, sub) {
			return false
		}
	}
	return true
}

func TestClassifyImpactRisk(t *testing.T) {
	tests := []struct {
		name string
		s    ImpactSummary
		want string
	}{
		{"many callers", ImpactSummary{Available: true, Callers: 25}, "high"},
		{"cross-package with recent transport", ImpactSummary{Available: true, Callers: 6, CrossPackage: true,
			RecentTransports: []TransportTouch{{Transport: "TR-EXAMPLE"}}}, "high"},
		{"just under high", ImpactSummary{Available: true, Callers: 24}, "medium"},
		{"few callers", ImpactSummary{Available: true, Callers: 5}, "medium"},
		{"transport touch only", ImpactSummary{Available: true, Callers: 0,
			RecentTransports: []TransportTouch{{Transport: "TR-EXAMPLE"}}}, "medium"},
		{"quiet object", ImpactSummary{Available: true, Callers: 4}, "low"},
		{"cross-package, no transport", ImpactSummary{Available: true, Callers: 0, CrossPackage: true}, "low"},
		{"unavailable", ImpactSummary{Available: false}, "unknown"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := classifyImpactRisk(&tt.s); got != tt.want {
				t.Fatalf("classifyImpactRisk() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestImpactAdviceMentionsPackages(t *testing.T) {
	s := &ImpactSummary{Available: true, Callers: 30, CrossPackage: true,
		Packages: []string{"Z_BILLING", "Z_ORDERS"}, Risk: "high"}
	advice := impactAdvice(s)
	if advice == "" || !containsAll(advice, "30", "Z_BILLING") {
		t.Fatalf("advice %q must cite caller count and a package", advice)
	}
}

func TestImpactAdviceTruncatesPackageList(t *testing.T) {
	s := &ImpactSummary{Available: true, Callers: 30, CrossPackage: true,
		Packages: []string{"Z_PKG_A", "Z_PKG_B", "Z_PKG_C", "Z_PKG_D", "Z_PKG_E"}}
	advice := impactAdvice(s)
	if !containsAll(advice, "Z_PKG_A", "Z_PKG_B", "Z_PKG_C", "5 package(s)") {
		t.Fatalf("advice %q must name the first 3 packages and the total count", advice)
	}
	if strings.Contains(advice, "Z_PKG_D") || strings.Contains(advice, "Z_PKG_E") {
		t.Fatalf("advice %q must name at most 3 packages", advice)
	}
}

// TestDeriveWriteImpactIdentity pins the URL→identity mapping per ADT URL
// family. Two review-driven contracts live here: program includes keep their
// own identity (R3TR PROG <include> in E071 — the "/includes/" cut applies
// only to the class-include grammar), and CDS DDL sources map to DDLS so CDS
// edits get a transport-recency leg.
func TestDeriveWriteImpactIdentity(t *testing.T) {
	tests := []struct {
		name          string
		objectURL     string
		wantURL       string
		wantName      string
		wantTadirType string
	}{
		{
			name:          "class source main",
			objectURL:     "/sap/bc/adt/oo/classes/ZCL_DEMO/source/main",
			wantURL:       "/sap/bc/adt/oo/classes/ZCL_DEMO",
			wantName:      "ZCL_DEMO",
			wantTadirType: "CLAS",
		},
		{
			name:          "class testclasses include collapses to parent class",
			objectURL:     "/sap/bc/adt/oo/classes/ZCL_DEMO/includes/testclasses",
			wantURL:       "/sap/bc/adt/oo/classes/ZCL_DEMO",
			wantName:      "ZCL_DEMO",
			wantTadirType: "CLAS",
		},
		{
			name:          "program",
			objectURL:     "/sap/bc/adt/programs/programs/ZDEMO_REPORT/source/main",
			wantURL:       "/sap/bc/adt/programs/programs/ZDEMO_REPORT",
			wantName:      "ZDEMO_REPORT",
			wantTadirType: "PROG",
		},
		{
			name:          "program include keeps its own identity",
			objectURL:     "/sap/bc/adt/programs/includes/zinc/source/main",
			wantURL:       "/sap/bc/adt/programs/includes/zinc",
			wantName:      "ZINC",
			wantTadirType: "PROG",
		},
		{
			name:          "function group",
			objectURL:     "/sap/bc/adt/functions/groups/ZDEMO_FG",
			wantURL:       "/sap/bc/adt/functions/groups/ZDEMO_FG",
			wantName:      "ZDEMO_FG",
			wantTadirType: "FUGR",
		},
		{
			name:          "function module",
			objectURL:     "/sap/bc/adt/functions/groups/ZDEMO_FG/fmodules/ZDEMO_FM/source/main",
			wantURL:       "/sap/bc/adt/functions/groups/ZDEMO_FG/fmodules/ZDEMO_FM",
			wantName:      "ZDEMO_FM",
			wantTadirType: "FUGR",
		},
		{
			name:          "CDS DDL source",
			objectURL:     "/sap/bc/adt/ddic/ddl/sources/zdemo_cds/source/main",
			wantURL:       "/sap/bc/adt/ddic/ddl/sources/zdemo_cds",
			wantName:      "ZDEMO_CDS",
			wantTadirType: "DDLS",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotURL, gotName, gotType := deriveWriteImpactIdentity(tt.objectURL)
			if gotURL != tt.wantURL || gotName != tt.wantName || gotType != tt.wantTadirType {
				t.Fatalf("deriveWriteImpactIdentity(%q) = (%q, %q, %q), want (%q, %q, %q)",
					tt.objectURL, gotURL, gotName, gotType, tt.wantURL, tt.wantName, tt.wantTadirType)
			}
		})
	}
}

// impactUsageXML mirrors the ADT usageReferences response shape (see the
// fixture in cds_tools_test.go / parseUsageReferences), including an
// isResult="false" structural grouping row. It contains 6 rows:
//   - the object's own self-reference row (zcl_demo URI — where-used always
//     lists the queried object itself; excluded by URI, not name)
//   - 3 real callers across 2 packages, one in the object's own package
//     Z_PKG_A (Z_PKG_B rows come first to prove the summary sorts package
//     names)
//   - a duplicate of caller1 with a different-case URI (reached via another
//     include; must be deduped, case-insensitively)
//   - an isResult="false" DEVC/K package grouping node (structural, not a
//     usage; must be filtered)
//
// Expected Callers is therefore 3.
const impactUsageXML = `<?xml version="1.0" encoding="UTF-8"?>
<usageReferences:usageReferenceResult xmlns:usageReferences="http://www.sap.com/adt/ris/usageReferences" xmlns:adtcore="http://www.sap.com/adt/core">
  <usageReferences:referencedObjects>
    <usageReferences:referencedObject usageReferences:uri="/sap/bc/adt/oo/classes/zcl_demo" usageReferences:isResult="true">
      <adtcore:adtObject adtcore:uri="/sap/bc/adt/oo/classes/zcl_demo" adtcore:type="CLAS/OC" adtcore:name="ZCL_DEMO" adtcore:description="The object under edit itself">
        <adtcore:packageRef adtcore:name="Z_PKG_A"/>
      </adtcore:adtObject>
    </usageReferences:referencedObject>
    <usageReferences:referencedObject usageReferences:uri="/sap/bc/adt/oo/classes/zcl_demo_caller1" usageReferences:isResult="true">
      <adtcore:adtObject adtcore:uri="/sap/bc/adt/oo/classes/zcl_demo_caller1" adtcore:type="CLAS/OC" adtcore:name="ZCL_DEMO_CALLER1" adtcore:description="Caller outside own package">
        <adtcore:packageRef adtcore:name="Z_PKG_B"/>
      </adtcore:adtObject>
    </usageReferences:referencedObject>
    <usageReferences:referencedObject usageReferences:uri="/sap/bc/adt/programs/programs/zdemo_report" usageReferences:isResult="true">
      <adtcore:adtObject adtcore:uri="/sap/bc/adt/programs/programs/zdemo_report" adtcore:type="PROG/P" adtcore:name="ZDEMO_REPORT" adtcore:description="Caller outside own package">
        <adtcore:packageRef adtcore:name="Z_PKG_B"/>
      </adtcore:adtObject>
    </usageReferences:referencedObject>
    <usageReferences:referencedObject usageReferences:uri="/sap/bc/adt/oo/classes/ZCL_DEMO_CALLER1" usageReferences:isResult="true">
      <adtcore:adtObject adtcore:uri="/sap/bc/adt/oo/classes/ZCL_DEMO_CALLER1" adtcore:type="CLAS/OC" adtcore:name="ZCL_DEMO_CALLER1" adtcore:description="Same caller via another include; different-case URI">
        <adtcore:packageRef adtcore:name="Z_PKG_B"/>
      </adtcore:adtObject>
    </usageReferences:referencedObject>
    <usageReferences:referencedObject usageReferences:uri="/sap/bc/adt/oo/classes/zcl_demo_neighbor" usageReferences:isResult="true">
      <adtcore:adtObject adtcore:uri="/sap/bc/adt/oo/classes/zcl_demo_neighbor" adtcore:type="CLAS/OC" adtcore:name="ZCL_DEMO_NEIGHBOR" adtcore:description="Caller in the object's own package">
        <adtcore:packageRef adtcore:name="Z_PKG_A"/>
      </adtcore:adtObject>
    </usageReferences:referencedObject>
    <usageReferences:referencedObject usageReferences:uri="/sap/bc/adt/packages/z_pkg_b" usageReferences:isResult="false">
      <adtcore:adtObject adtcore:uri="/sap/bc/adt/packages/z_pkg_b" adtcore:type="DEVC/K" adtcore:name="Z_PKG_B" adtcore:description="Structural package grouping node, not a usage"/>
    </usageReferences:referencedObject>
  </usageReferences:referencedObjects>
</usageReferences:usageReferenceResult>`

// newImpactTestClient wires a Client to a scripted mockHTTPClient with the
// CSRF token pre-set so the where-used POST is the first (and only) request.
func newImpactTestClient(responses ...*http.Response) (*Client, *mockHTTPClient) {
	mock := &mockHTTPClient{responses: responses}
	cfg := NewConfig("https://sap.example.com:44300", "user", "pass")
	transport := NewTransportWithClient(cfg, mock)
	transport.setCSRFToken("synthetic-token")
	return NewClientWithTransport(cfg, transport), mock
}

//nolint:bodyclose // Transport.Request owns and closes every synthetic response body.
func TestComputeWriteImpactCountsCallersAcrossPackages(t *testing.T) {
	client, mock := newImpactTestClient(newMockResponse(http.StatusOK, impactUsageXML, nil))

	s := client.ComputeWriteImpact(context.Background(),
		"/sap/bc/adt/oo/classes/zcl_demo", "ZCL_DEMO", "CLAS", "Z_PKG_A")

	if s == nil {
		t.Fatal("ComputeWriteImpact() = nil, want summary")
	}
	if !s.Available {
		t.Fatalf("Available = false (%s), want true", s.Unavailable)
	}
	if s.Callers != 3 {
		t.Fatalf("Callers = %d, want 3 (self-reference, isResult=false grouping, and duplicate-URI rows must be excluded)", s.Callers)
	}
	if want := []string{"Z_PKG_A", "Z_PKG_B"}; !reflect.DeepEqual(s.Packages, want) {
		t.Fatalf("Packages = %v, want sorted %v", s.Packages, want)
	}
	if !s.CrossPackage {
		t.Fatal("CrossPackage = false, want true (callers exist outside Z_PKG_A)")
	}
	if s.Risk != riskLow {
		t.Fatalf("Risk = %q, want %q (3 callers, no transport touches)", s.Risk, riskLow)
	}
	if s.Advice == "" {
		t.Fatal("Advice is empty, want an agent-directed sentence")
	}
	// Request 1 is the where-used POST; request 2 is the transport-recency
	// leg's E071 attempt, which runs the mock out of responses (500) and
	// degrades to no touches — the refs leg is unaffected.
	if len(mock.requests) != 2 || !strings.Contains(mock.requests[0].URL.Path, "usageReferences") {
		t.Fatalf("requests = %d, want usageReferences then the E071 attempt", len(mock.requests))
	}
	if len(s.RecentTransports) != 0 {
		t.Fatalf("RecentTransports = %+v, want none (E071 lookup failed)", s.RecentTransports)
	}
}

//nolint:bodyclose // Transport.Request owns and closes every synthetic response body.
func TestComputeWriteImpactDegradesWhenLookupFails(t *testing.T) {
	client, _ := newImpactTestClient(newMockResponse(http.StatusInternalServerError, "boom", nil))

	s := client.ComputeWriteImpact(context.Background(),
		"/sap/bc/adt/oo/classes/zcl_demo", "ZCL_DEMO", "CLAS", "Z_PKG_A")

	if s == nil {
		t.Fatal("ComputeWriteImpact() = nil, want degraded summary")
	}
	if s.Available {
		t.Fatal("Available = true, want false on HTTP 500")
	}
	if s.Risk != riskUnknown {
		t.Fatalf("Risk = %q, want %q", s.Risk, riskUnknown)
	}
	if s.Unavailable == "" {
		t.Fatal("Unavailable is empty, want a reason")
	}
	if s.Advice == "" {
		t.Fatal("Advice is empty, want degraded-path guidance (verify callers manually)")
	}
	if s.Callers != 0 || s.CrossPackage || len(s.Packages) != 0 {
		t.Fatalf("degraded summary must stay empty, got %+v", s)
	}
}

// --- Transport-recency leg (E071→E070 via RunQuery) ---

// pinImpactNow pins the transport-recency clock to 2026-08-15 (cutoff for the
// 90-day window is therefore 2026-05-17) and restores it on cleanup.
func pinImpactNow(t *testing.T) {
	t.Helper()
	prev := impactNow
	impactNow = func() time.Time { return time.Date(2026, 8, 15, 12, 0, 0, 0, time.UTC) }
	t.Cleanup(func() { impactNow = prev })
}

// readRequestBody drains a recorded mock request body (the mock never
// consumes it, so it is still readable after the call).
func readRequestBody(t *testing.T, req *http.Request) string {
	t.Helper()
	if req.Body == nil {
		return ""
	}
	b, err := io.ReadAll(req.Body)
	if err != nil {
		t.Fatalf("reading recorded request body: %v", err)
	}
	return string(b)
}

// impactE071XML mirrors the ADT datapreview/freestyle response shape parsed
// by parseTableContents (per-column metadata + dataSet vectors). Two E071
// entries for ZCL_DEMO: one carried by task A4HK900101 and one attached
// directly to request A4HK900100.
const impactE071XML = `<?xml version="1.0" encoding="utf-8"?>
<dataPreview:tableData xmlns:dataPreview="http://www.sap.com/adt/dataPreview">
  <dataPreview:totalRows>2</dataPreview:totalRows>
  <dataPreview:columns>
    <dataPreview:metadata dataPreview:name="TRKORR" dataPreview:type="C" dataPreview:description="Request/Task" dataPreview:keyAttribute="false" dataPreview:length="20"/>
    <dataPreview:dataSet>
      <dataPreview:data>A4HK900101</dataPreview:data>
      <dataPreview:data>A4HK900100</dataPreview:data>
    </dataPreview:dataSet>
  </dataPreview:columns>
  <dataPreview:columns>
    <dataPreview:metadata dataPreview:name="PGMID" dataPreview:type="C" dataPreview:description="Program ID" dataPreview:keyAttribute="false" dataPreview:length="4"/>
    <dataPreview:dataSet>
      <dataPreview:data>R3TR</dataPreview:data>
      <dataPreview:data>R3TR</dataPreview:data>
    </dataPreview:dataSet>
  </dataPreview:columns>
  <dataPreview:columns>
    <dataPreview:metadata dataPreview:name="OBJECT" dataPreview:type="C" dataPreview:description="Object Type" dataPreview:keyAttribute="false" dataPreview:length="4"/>
    <dataPreview:dataSet>
      <dataPreview:data>CLAS</dataPreview:data>
      <dataPreview:data>CLAS</dataPreview:data>
    </dataPreview:dataSet>
  </dataPreview:columns>
  <dataPreview:columns>
    <dataPreview:metadata dataPreview:name="OBJ_NAME" dataPreview:type="C" dataPreview:description="Object Name" dataPreview:keyAttribute="false" dataPreview:length="120"/>
    <dataPreview:dataSet>
      <dataPreview:data>ZCL_DEMO</dataPreview:data>
      <dataPreview:data>ZCL_DEMO</dataPreview:data>
    </dataPreview:dataSet>
  </dataPreview:columns>
</dataPreview:tableData>`

// impactE070XML holds the headers for both E071 hits: the task row points at
// its parent request via STRKORR; the request row is its own header. Both
// dates fall inside the 90-day window relative to the pinned clock. The
// expected collapsed touch therefore carries the PARENT request's fields
// (status R, date 2026-08-03), not the task's (status D, date 2026-08-10).
const impactE070XML = `<?xml version="1.0" encoding="utf-8"?>
<dataPreview:tableData xmlns:dataPreview="http://www.sap.com/adt/dataPreview">
  <dataPreview:totalRows>2</dataPreview:totalRows>
  <dataPreview:columns>
    <dataPreview:metadata dataPreview:name="TRKORR" dataPreview:type="C" dataPreview:description="Request/Task" dataPreview:keyAttribute="false" dataPreview:length="20"/>
    <dataPreview:dataSet>
      <dataPreview:data>A4HK900101</dataPreview:data>
      <dataPreview:data>A4HK900100</dataPreview:data>
    </dataPreview:dataSet>
  </dataPreview:columns>
  <dataPreview:columns>
    <dataPreview:metadata dataPreview:name="STRKORR" dataPreview:type="C" dataPreview:description="Higher-Level Request" dataPreview:keyAttribute="false" dataPreview:length="20"/>
    <dataPreview:dataSet>
      <dataPreview:data>A4HK900100</dataPreview:data>
      <dataPreview:data></dataPreview:data>
    </dataPreview:dataSet>
  </dataPreview:columns>
  <dataPreview:columns>
    <dataPreview:metadata dataPreview:name="TRFUNCTION" dataPreview:type="C" dataPreview:description="Type" dataPreview:keyAttribute="false" dataPreview:length="1"/>
    <dataPreview:dataSet>
      <dataPreview:data>K</dataPreview:data>
      <dataPreview:data>K</dataPreview:data>
    </dataPreview:dataSet>
  </dataPreview:columns>
  <dataPreview:columns>
    <dataPreview:metadata dataPreview:name="TRSTATUS" dataPreview:type="C" dataPreview:description="Status" dataPreview:keyAttribute="false" dataPreview:length="1"/>
    <dataPreview:dataSet>
      <dataPreview:data>D</dataPreview:data>
      <dataPreview:data>R</dataPreview:data>
    </dataPreview:dataSet>
  </dataPreview:columns>
  <dataPreview:columns>
    <dataPreview:metadata dataPreview:name="AS4USER" dataPreview:type="C" dataPreview:description="Owner" dataPreview:keyAttribute="false" dataPreview:length="12"/>
    <dataPreview:dataSet>
      <dataPreview:data>TESTUSER</dataPreview:data>
      <dataPreview:data>TESTUSER</dataPreview:data>
    </dataPreview:dataSet>
  </dataPreview:columns>
  <dataPreview:columns>
    <dataPreview:metadata dataPreview:name="AS4DATE" dataPreview:type="D" dataPreview:description="Date" dataPreview:keyAttribute="false" dataPreview:length="8"/>
    <dataPreview:dataSet>
      <dataPreview:data>20260810</dataPreview:data>
      <dataPreview:data>20260803</dataPreview:data>
    </dataPreview:dataSet>
  </dataPreview:columns>
</dataPreview:tableData>`

// impactE071OldXML / impactE070OldXML: one request whose AS4DATE (2026-03-01)
// is older than the 90-day window relative to the pinned clock.
const impactE071OldXML = `<?xml version="1.0" encoding="utf-8"?>
<dataPreview:tableData xmlns:dataPreview="http://www.sap.com/adt/dataPreview">
  <dataPreview:totalRows>1</dataPreview:totalRows>
  <dataPreview:columns>
    <dataPreview:metadata dataPreview:name="TRKORR" dataPreview:type="C" dataPreview:description="Request/Task" dataPreview:keyAttribute="false" dataPreview:length="20"/>
    <dataPreview:dataSet>
      <dataPreview:data>A4HK900200</dataPreview:data>
    </dataPreview:dataSet>
  </dataPreview:columns>
  <dataPreview:columns>
    <dataPreview:metadata dataPreview:name="OBJ_NAME" dataPreview:type="C" dataPreview:description="Object Name" dataPreview:keyAttribute="false" dataPreview:length="120"/>
    <dataPreview:dataSet>
      <dataPreview:data>ZCL_DEMO</dataPreview:data>
    </dataPreview:dataSet>
  </dataPreview:columns>
</dataPreview:tableData>`

const impactE070OldXML = `<?xml version="1.0" encoding="utf-8"?>
<dataPreview:tableData xmlns:dataPreview="http://www.sap.com/adt/dataPreview">
  <dataPreview:totalRows>1</dataPreview:totalRows>
  <dataPreview:columns>
    <dataPreview:metadata dataPreview:name="TRKORR" dataPreview:type="C" dataPreview:description="Request/Task" dataPreview:keyAttribute="false" dataPreview:length="20"/>
    <dataPreview:dataSet>
      <dataPreview:data>A4HK900200</dataPreview:data>
    </dataPreview:dataSet>
  </dataPreview:columns>
  <dataPreview:columns>
    <dataPreview:metadata dataPreview:name="STRKORR" dataPreview:type="C" dataPreview:description="Higher-Level Request" dataPreview:keyAttribute="false" dataPreview:length="20"/>
    <dataPreview:dataSet>
      <dataPreview:data></dataPreview:data>
    </dataPreview:dataSet>
  </dataPreview:columns>
  <dataPreview:columns>
    <dataPreview:metadata dataPreview:name="TRFUNCTION" dataPreview:type="C" dataPreview:description="Type" dataPreview:keyAttribute="false" dataPreview:length="1"/>
    <dataPreview:dataSet>
      <dataPreview:data>K</dataPreview:data>
    </dataPreview:dataSet>
  </dataPreview:columns>
  <dataPreview:columns>
    <dataPreview:metadata dataPreview:name="TRSTATUS" dataPreview:type="C" dataPreview:description="Status" dataPreview:keyAttribute="false" dataPreview:length="1"/>
    <dataPreview:dataSet>
      <dataPreview:data>R</dataPreview:data>
    </dataPreview:dataSet>
  </dataPreview:columns>
  <dataPreview:columns>
    <dataPreview:metadata dataPreview:name="AS4USER" dataPreview:type="C" dataPreview:description="Owner" dataPreview:keyAttribute="false" dataPreview:length="12"/>
    <dataPreview:dataSet>
      <dataPreview:data>TESTUSER</dataPreview:data>
    </dataPreview:dataSet>
  </dataPreview:columns>
  <dataPreview:columns>
    <dataPreview:metadata dataPreview:name="AS4DATE" dataPreview:type="D" dataPreview:description="Date" dataPreview:keyAttribute="false" dataPreview:length="8"/>
    <dataPreview:dataSet>
      <dataPreview:data>20260301</dataPreview:data>
    </dataPreview:dataSet>
  </dataPreview:columns>
</dataPreview:tableData>`

//nolint:bodyclose // Transport.Request owns and closes every synthetic response body.
func TestRecentTransportTouchesCollapsesTasksToParent(t *testing.T) {
	pinImpactNow(t)
	client, mock := newImpactTestClient(
		newMockResponse(http.StatusOK, impactE071XML, nil),
		newMockResponse(http.StatusOK, impactE070XML, nil),
	)

	// Lowercase inputs prove the SQL uppercases type and name (mirrors
	// cli_extra.go's strings.ToUpper before interpolation).
	touches := client.recentTransportTouches(context.Background(), "clas", "zcl_demo")

	want := []TransportTouch{{
		Transport: "A4HK900100", Type: "K", Status: "R", Owner: "TESTUSER", Date: "2026-08-03",
	}}
	if !reflect.DeepEqual(touches, want) {
		t.Fatalf("touches = %+v, want task collapsed to parent request %+v", touches, want)
	}

	if len(mock.requests) != 2 {
		t.Fatalf("requests = %d, want 2 (E071 then E070 RunQuery)", len(mock.requests))
	}
	for i, req := range mock.requests {
		if !strings.Contains(req.URL.Path, "datapreview/freestyle") {
			t.Fatalf("request %d path = %s, want datapreview/freestyle", i, req.URL.Path)
		}
	}
	e071SQL := readRequestBody(t, mock.requests[0])
	if !containsAll(e071SQL, "FROM E071", "PGMID = 'R3TR'", "OBJECT = 'CLAS'", "OBJ_NAME = 'ZCL_DEMO'") {
		t.Fatalf("E071 SQL %q must filter on R3TR/CLAS/ZCL_DEMO", e071SQL)
	}
	e070SQL := readRequestBody(t, mock.requests[1])
	if !containsAll(e070SQL, "FROM E070", "'A4HK900100'", "'A4HK900101'") {
		t.Fatalf("E070 SQL %q must select headers for both E071 transports", e070SQL)
	}
}

//nolint:bodyclose // Transport.Request owns and closes every synthetic response body.
func TestRecentTransportTouchesExcludesOldTransports(t *testing.T) {
	pinImpactNow(t)
	client, mock := newImpactTestClient(
		newMockResponse(http.StatusOK, impactE071OldXML, nil),
		newMockResponse(http.StatusOK, impactE070OldXML, nil),
	)

	touches := client.recentTransportTouches(context.Background(), "CLAS", "ZCL_DEMO")

	if len(touches) != 0 {
		t.Fatalf("touches = %+v, want none (AS4DATE 20260301 is outside the 90-day window)", touches)
	}
	if len(mock.requests) != 2 {
		t.Fatalf("requests = %d, want 2 (the queries must still run; filtering is client-side)", len(mock.requests))
	}
}

//nolint:bodyclose // Transport.Request owns and closes every synthetic response body.
func TestRecentTransportTouchesSkipsWhenFreeSQLBlocked(t *testing.T) {
	pinImpactNow(t)
	client, mock := newImpactTestClient() // no responses: any request would 500
	client.Safety().BlockFreeSQL = true

	touches := client.recentTransportTouches(context.Background(), "CLAS", "ZCL_DEMO")

	if touches != nil {
		t.Fatalf("touches = %+v, want nil when free SQL is blocked", touches)
	}
	if len(mock.requests) != 0 {
		t.Fatalf("requests = %d, want 0 (BlockFreeSQL must skip before any HTTP call)", len(mock.requests))
	}
}

//nolint:bodyclose // Transport.Request owns and closes every synthetic response body.
func TestComputeWriteImpactIncludesTransportTouches(t *testing.T) {
	pinImpactNow(t)
	client, _ := newImpactTestClient(
		newMockResponse(http.StatusOK, impactUsageXML, nil),
		newMockResponse(http.StatusOK, impactE071XML, nil),
		newMockResponse(http.StatusOK, impactE070XML, nil),
	)

	s := client.ComputeWriteImpact(context.Background(),
		"/sap/bc/adt/oo/classes/zcl_demo", "ZCL_DEMO", "CLAS", "Z_PKG_A")

	if !s.Available {
		t.Fatalf("Available = false (%s), want true", s.Unavailable)
	}
	if len(s.RecentTransports) != 1 || s.RecentTransports[0].Transport != "A4HK900100" {
		t.Fatalf("RecentTransports = %+v, want the one collapsed parent request", s.RecentTransports)
	}
	if s.Risk != riskHigh {
		t.Fatalf("Risk = %q, want %q (cross-package spread + transport touch within 90 days)", s.Risk, riskHigh)
	}
	if !containsAll(s.Advice, "A4HK900100", "2026-08-03") {
		t.Fatalf("Advice %q must cite the recent transport and its date", s.Advice)
	}
}

//nolint:bodyclose // Transport.Request owns and closes every synthetic response body.
func TestComputeWriteImpactSurvivesTransportLookupFailure(t *testing.T) {
	pinImpactNow(t)
	client, mock := newImpactTestClient(
		newMockResponse(http.StatusOK, impactUsageXML, nil),
		newMockResponse(http.StatusInternalServerError, "boom", nil),
	)

	s := client.ComputeWriteImpact(context.Background(),
		"/sap/bc/adt/oo/classes/zcl_demo", "ZCL_DEMO", "CLAS", "Z_PKG_A")

	if !s.Available {
		t.Fatalf("Available = false (%s), want true — a failed E071 lookup degrades to refs-only", s.Unavailable)
	}
	if s.Callers != 3 {
		t.Fatalf("Callers = %d, want 3 from the where-used leg", s.Callers)
	}
	if len(s.RecentTransports) != 0 {
		t.Fatalf("RecentTransports = %+v, want none on query failure", s.RecentTransports)
	}
	if s.Risk != riskLow {
		t.Fatalf("Risk = %q, want %q (refs-only tiering without the transport signal)", s.Risk, riskLow)
	}
	if len(mock.requests) != 2 {
		t.Fatalf("requests = %d, want 2 (usageReferences + the failed E071 attempt)", len(mock.requests))
	}
}
