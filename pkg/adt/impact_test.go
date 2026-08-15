package adt

import (
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
		{"few callers", ImpactSummary{Available: true, Callers: 5}, "medium"},
		{"transport touch only", ImpactSummary{Available: true, Callers: 0,
			RecentTransports: []TransportTouch{{Transport: "TR-EXAMPLE"}}}, "medium"},
		{"quiet object", ImpactSummary{Available: true, Callers: 4}, "low"},
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
