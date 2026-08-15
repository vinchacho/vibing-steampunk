package adt

import (
	"fmt"
	"strings"
)

// TransportTouch is one recent transport that carried the object.
type TransportTouch struct {
	Transport string `json:"transport"`
	Type      string `json:"type"`
	Status    string `json:"status"`
	Owner     string `json:"owner"`
	Date      string `json:"date"`
}

// ImpactSummary is the blast-radius block attached to write results.
type ImpactSummary struct {
	Object           string           `json:"object"`
	Callers          int              `json:"callers"`
	Packages         []string         `json:"packages,omitempty"`
	CrossPackage     bool             `json:"cross_package"`
	RecentTransports []TransportTouch `json:"recent_transports,omitempty"`
	Risk             string           `json:"risk"`
	Advice           string           `json:"advice,omitempty"`
	Available        bool             `json:"available"`
	Unavailable      string           `json:"unavailable_reason,omitempty"`
	TransitiveDepth  int              `json:"transitive_depth,omitempty"` // reserved for Phase 2
}

const (
	impactHighCallers   = 25
	impactMediumCallers = 5
)

// Risk tier names. Task 4 compares these against configured thresholds —
// consts prevent typo bugs in tier lookups.
const (
	riskHigh    = "high"
	riskMedium  = "medium"
	riskLow     = "low"
	riskUnknown = "unknown"
)

func classifyImpactRisk(s *ImpactSummary) string {
	if !s.Available {
		return riskUnknown
	}
	touched := len(s.RecentTransports) > 0
	switch {
	case s.Callers >= impactHighCallers || (s.CrossPackage && touched):
		return riskHigh
	case s.Callers >= impactMediumCallers || touched:
		return riskMedium
	default:
		return riskLow
	}
}

// impactAdvice renders one agent-directed sentence for the summary's risk tier.
func impactAdvice(s *ImpactSummary) string {
	if !s.Available {
		return fmt.Sprintf("Impact unknown (%s); verify callers with FindReferences before changing %s.", s.Unavailable, s.Object)
	}
	spread := ""
	if n := len(s.Packages); n > 0 {
		named := s.Packages
		if len(named) > 3 {
			named = named[:3]
		}
		spread = fmt.Sprintf(" across %d package(s) (%s)", n, strings.Join(named, ", "))
	}
	recency := ""
	if len(s.RecentTransports) > 0 {
		t := s.RecentTransports[0]
		recency = fmt.Sprintf("; transport %s touched this object", t.Transport)
		if t.Date != "" {
			recency += " on " + t.Date
		}
	}
	testTarget := "the affected package"
	if len(s.Packages) > 0 {
		testTarget = s.Packages[0]
	}
	switch classifyImpactRisk(s) {
	case riskHigh:
		return fmt.Sprintf("High impact: %d callers%s%s — read 2-3 key callers before editing and run unit tests on %s after activation.", s.Callers, spread, recency, testTarget)
	case riskMedium:
		return fmt.Sprintf("Medium impact: %d callers%s%s — skim the main callers and run unit tests on %s after activation.", s.Callers, spread, recency, testTarget)
	default:
		return fmt.Sprintf("Low impact: %d callers%s%s — safe to proceed.", s.Callers, spread, recency)
	}
}
