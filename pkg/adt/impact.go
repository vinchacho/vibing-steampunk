package adt

import (
	"context"
	"fmt"
	"sort"
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

// ComputeWriteImpact builds the blast-radius summary for objectURL. It never
// returns an error: failures degrade to Available=false (design §Degradation
// ladder) so a broken where-used lookup can annotate but never fail a write.
// ownPackage is the object's package from the mutation context ("" if unknown).
func (c *Client) ComputeWriteImpact(ctx context.Context, objectURL, objectName, tadirType, ownPackage string) *ImpactSummary {
	s := &ImpactSummary{Object: objectName}
	// line=0, column=0 → FindReferences omits the #start fragment and queries
	// usages of the whole object rather than a symbol at a position.
	refs, err := c.FindReferences(ctx, objectURL, 0, 0)
	if err != nil {
		s.Unavailable = fmt.Sprintf("where-used lookup failed: %v", err)
		s.Risk = classifyImpactRisk(s)
		s.Advice = impactAdvice(s)
		return s
	}
	s.Available = true
	pkgs := map[string]bool{}
	seen := map[string]bool{}
	for _, r := range refs {
		if !r.IsResult {
			continue // structural grouping row (e.g. package node), not a usage — same filter as AnalyzeCDSImpact
		}
		// Exclude the queried object itself by URI, not name: name matching
		// would also drop a genuine same-named caller of a different type.
		if r.URI != "" && strings.EqualFold(r.URI, objectURL) {
			continue
		}
		// An object reached via multiple includes appears once per include;
		// count it once. Rows without a URI can't be correlated — count each.
		if uri := strings.ToLower(r.URI); uri != "" {
			if seen[uri] {
				continue
			}
			seen[uri] = true
		}
		s.Callers++
		if r.PackageName != "" {
			pkgs[r.PackageName] = true
			if ownPackage != "" && r.PackageName != ownPackage {
				s.CrossPackage = true
			}
		}
	}
	for p := range pkgs {
		s.Packages = append(s.Packages, p)
	}
	sort.Strings(s.Packages)
	if len(s.Packages) > 1 {
		// Callers spread over 2+ packages is cross-package by definition,
		// even when ownPackage is unknown ("").
		s.CrossPackage = true
	}
	s.RecentTransports = c.recentTransportTouches(ctx, tadirType, objectName)
	s.Risk = classifyImpactRisk(s)
	s.Advice = impactAdvice(s)
	return s
}

// recentTransportTouches returns transports that recently carried the object.
// Task 3 implements the E071→E070 lookup; until then summaries are refs-only.
func (c *Client) recentTransportTouches(_ context.Context, _, _ string) []TransportTouch {
	return nil
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
