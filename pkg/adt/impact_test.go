package adt

import (
	"context"
	"net/http"
	"reflect"
	"strings"
	"testing"
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

// impactUsageXML mirrors the ADT usageReferences response shape (see the
// fixture in cds_tools_test.go / parseUsageReferences). It contains 4 rows:
// the object's own self-reference row (ZCL_DEMO — where-used always lists the
// queried object itself) plus 3 real callers across 2 packages, one of them
// in the object's own package Z_PKG_A. Z_PKG_B rows come first to prove the
// summary sorts package names.
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
    <usageReferences:referencedObject usageReferences:uri="/sap/bc/adt/oo/classes/zcl_demo_neighbor" usageReferences:isResult="true">
      <adtcore:adtObject adtcore:uri="/sap/bc/adt/oo/classes/zcl_demo_neighbor" adtcore:type="CLAS/OC" adtcore:name="ZCL_DEMO_NEIGHBOR" adtcore:description="Caller in the object's own package">
        <adtcore:packageRef adtcore:name="Z_PKG_A"/>
      </adtcore:adtObject>
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
		t.Fatalf("Callers = %d, want 3 (self-reference row must be excluded)", s.Callers)
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
	if len(mock.requests) != 1 || !strings.Contains(mock.requests[0].URL.Path, "usageReferences") {
		t.Fatalf("requests = %d (%v), want one usageReferences call", len(mock.requests), mock.requests)
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
	if s.Callers != 0 || s.CrossPackage || len(s.Packages) != 0 {
		t.Fatalf("degraded summary must stay empty, got %+v", s)
	}
}
