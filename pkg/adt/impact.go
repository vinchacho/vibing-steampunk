package adt

import (
	"context"
	"fmt"
	"sort"
	"strings"
	"time"
)

// impactNow returns the current time. It is a package var so tests can pin
// the 90-day transport-recency window to a fixed date.
var impactNow = time.Now

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

// ImpactBlockedError is the refusal returned by checkMutation step 4 when
// the impact gate runs in block mode and the write's risk is at or above the
// configured threshold. It carries the blast-radius summary and a fresh
// single-use confirmation token; retrying the same call with the token
// (adt.WithImpactConfirm / the MCP `confirm` parameter) proceeds. Callers can
// detect it with errors.As.
type ImpactBlockedError struct {
	Summary *ImpactSummary
	Token   string
	Object  string
	Op      string
}

// Error renders the refusal per design §Confirmation flow: the headline, the
// blast-radius facts (callers/packages, most recent transport touch — or the
// degradation reason), and the exact retry.
func (e *ImpactBlockedError) Error() string {
	var b strings.Builder
	risk := riskUnknown
	if e.Summary != nil {
		risk = e.Summary.Risk
	}
	fmt.Fprintf(&b, "IMPACT GATE: refusing %s of %s (risk: %s).\n", e.Op, e.Object, risk)
	switch {
	case e.Summary == nil || !e.Summary.Available:
		reason := "impact analysis unavailable"
		if e.Summary != nil && e.Summary.Unavailable != "" {
			reason = e.Summary.Unavailable
		}
		fmt.Fprintf(&b, "Impact analysis unavailable: %s.\n", reason)
	default:
		s := e.Summary
		fmt.Fprintf(&b, "%d callers", s.Callers)
		if n := len(s.Packages); n > 0 {
			fmt.Fprintf(&b, " across %d package(s) (%s)", n, strings.Join(s.Packages, ", "))
		}
		if len(s.RecentTransports) > 0 {
			t := s.RecentTransports[0]
			b.WriteString("; ")
			if word := transportStatusWord(t.Status); word != "" {
				b.WriteString(word + " ")
			}
			fmt.Fprintf(&b, "transport %s touched this object", t.Transport)
			if t.Date != "" {
				b.WriteString(" on " + t.Date)
			}
		}
		b.WriteString(".\n")
	}
	fmt.Fprintf(&b, "To proceed, retry the same call with: confirm: %q\n", e.Token)
	b.WriteString("Token expires in 10 minutes and is valid only for this object and operation.")
	return b.String()
}

// transportStatusWord renders an E070 TRSTATUS value as the word used in the
// refusal's transport line (design §Confirmation flow: "released transport
// TR-EXAMPLE touched this object on 2026-08-03"). R is released; D and L are
// the two modifiable statuses ("open"); anything else passes through
// verbatim so an unexpected status is visible rather than mislabeled.
func transportStatusWord(status string) string {
	switch status {
	case "R":
		return "released"
	case "D", "L":
		return "open"
	default:
		return status
	}
}

// impactOpVerb renders an OperationType as the verb used in the block-mode
// refusal headline ("refusing update of ...").
func impactOpVerb(op OperationType) string {
	switch op {
	case OpUpdate:
		return "update"
	case OpDelete:
		return "delete"
	case OpCreate:
		return "create"
	case OpWorkflow:
		return "write"
	default:
		return "mutation"
	}
}

// impactGateActive reports whether blast-radius computation is enabled.
// Deliberately an allowlist of the two active modes — NOT `!= ImpactGateOff`
// — so an unrecognized value that slipped past config normalization stays
// inert instead of silently enabling network calls per write.
func impactGateActive(cfg *SafetyConfig) bool {
	return cfg.ImpactGate == ImpactGateAdvise || cfg.ImpactGate == ImpactGateBlock
}

// deriveWriteImpactIdentity maps an ADT object URL (possibly pointing at
// /source/main or a class include) to the whole-object identity used for
// blast-radius analysis: the normalized object URL, the object name, and the
// bare R3TR TADIR type ("CLAS", "PROG", ...; "" when the URL family is
// unknown). Class includes collapse to their parent class; program includes
// are standalone TADIR objects (E071 keys them as R3TR PROG <include>) and
// keep their own identity.
func deriveWriteImpactIdentity(objectURL string) (normalizedURL, name, tadirType string) {
	normalizedURL = normalizeObjectURLForPackageCheck(objectURL)
	name, err := objectNameFromURL(normalizedURL)
	if err != nil {
		name = ""
	}
	// extractTypeFromURI yields "CLAS/OC"-style workbench types; E071 keys on
	// the bare R3TR TADIR type ("CLAS").
	tadirType = extractTypeFromURI(normalizedURL)
	if idx := strings.Index(tadirType, "/"); idx >= 0 {
		tadirType = tadirType[:idx]
	}
	return normalizedURL, name, tadirType
}

// computeURLWriteImpact derives the whole-object identity from an ADT object
// URL (EditSource receives only a URL) and computes the write-impact summary
// for it. An unmappable URL degrades inside ComputeWriteImpact rather than
// erroring: the name may be empty and an unknown TADIR type simply skips the
// transport-recency leg.
func (c *Client) computeURLWriteImpact(ctx context.Context, objectURL string) *ImpactSummary {
	normalized, name, tadirType := deriveWriteImpactIdentity(objectURL)
	return c.ComputeWriteImpact(ctx, normalized, name, tadirType, "")
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

// impactTransportWindowDays is the recency window for transport touches.
const impactTransportWindowDays = 90

// recentTransportTouches returns the transport requests that carried the
// object within the last 90 days, via the canonical E071→E070 two-query
// pattern (mirrors `vsp graph co-change`, cmd/vsp/cli_extra.go). Tasks
// collapse to their parent request via STRKORR, deduped per request. Any
// failure — and BlockFreeSQL — degrades silently to nil so the summary stays
// refs-only (design §Degradation ladder, rung 2). E07x results are never
// cached: their whole point is to reflect the current transport state
// (cmd/vsp/audit_cache.go rule).
func (c *Client) recentTransportTouches(ctx context.Context, tadirType, objectName string) []TransportTouch {
	if c.config.Safety.BlockFreeSQL {
		return nil // free SQL blocked: skip before issuing any request
	}
	objType := strings.ToUpper(strings.TrimSpace(tadirType))
	objName := strings.ToUpper(strings.TrimSpace(objectName))
	if objType == "" || objName == "" || strings.Contains(objType+objName, "'") {
		return nil // nothing to query, or unquotable identifier — skip rather than emit broken SQL
	}

	// Step 1: transports containing this object (E071).
	e071Query := fmt.Sprintf(
		"SELECT TRKORR, PGMID, OBJECT, OBJ_NAME FROM E071 WHERE PGMID = 'R3TR' AND OBJECT = '%s' AND OBJ_NAME = '%s'",
		objType, objName)
	e071Result, err := c.RunQuery(ctx, e071Query, 200)
	if err != nil || e071Result == nil || len(e071Result.Rows) == 0 {
		return nil
	}
	trNums := make(map[string]bool)
	for _, row := range e071Result.Rows {
		tr := strings.TrimSpace(fmt.Sprintf("%v", row["TRKORR"]))
		if tr != "" {
			trNums[tr] = true
		}
	}
	if len(trNums) == 0 {
		return nil
	}

	// Step 2: resolve E070 headers (request/task hierarchy).
	trList := make([]string, 0, len(trNums))
	for tr := range trNums {
		trList = append(trList, "'"+tr+"'")
	}
	sort.Strings(trList) // deterministic SQL despite map iteration order
	e070Query := fmt.Sprintf(
		"SELECT TRKORR, STRKORR, TRFUNCTION, TRSTATUS, AS4USER, AS4DATE FROM E070 WHERE TRKORR IN (%s)",
		strings.Join(trList, ","))
	e070Result, err := c.RunQuery(ctx, e070Query, 500)
	if err != nil || e070Result == nil {
		return nil
	}

	type e070Header struct {
		trkorr, strkorr, trfunction, trstatus, as4user, as4date string
	}
	headers := make(map[string]e070Header)
	for _, row := range e070Result.Rows {
		h := e070Header{
			trkorr:     strings.TrimSpace(fmt.Sprintf("%v", row["TRKORR"])),
			strkorr:    strings.TrimSpace(fmt.Sprintf("%v", row["STRKORR"])),
			trfunction: strings.TrimSpace(fmt.Sprintf("%v", row["TRFUNCTION"])),
			trstatus:   strings.TrimSpace(fmt.Sprintf("%v", row["TRSTATUS"])),
			as4user:    strings.TrimSpace(fmt.Sprintf("%v", row["AS4USER"])),
			as4date:    strings.TrimSpace(fmt.Sprintf("%v", row["AS4DATE"])),
		}
		if h.trkorr != "" {
			headers[h.trkorr] = h
		}
	}

	// Collapse tasks to their parent request and dedupe per request (the
	// object-level analogue of aggregateChangelogEntries in
	// cmd/vsp/changelog.go). When the parent header is absent from the
	// result, fall back to the task's own header — same fallback as there.
	cutoff := impactNow().AddDate(0, 0, -impactTransportWindowDays).Format("20060102")
	keys := make([]string, 0, len(headers))
	for k := range headers {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	seen := make(map[string]bool)
	var touches []TransportTouch
	for _, k := range keys {
		h := headers[k]
		if h.strkorr != "" {
			if parent, ok := headers[h.strkorr]; ok {
				h = parent
			}
		}
		if seen[h.trkorr] {
			continue
		}
		seen[h.trkorr] = true
		// AS4DATE is YYYYMMDD, so the 90-day window is a string compare.
		if h.as4date == "" || h.as4date < cutoff {
			continue
		}
		touches = append(touches, TransportTouch{
			Transport: h.trkorr,
			Type:      h.trfunction,
			Status:    h.trstatus,
			Owner:     h.as4user,
			Date:      formatImpactDate(h.as4date),
		})
	}
	// Most recent first: the advice sentence cites touches[0].
	sort.Slice(touches, func(i, j int) bool {
		if touches[i].Date != touches[j].Date {
			return touches[i].Date > touches[j].Date
		}
		return touches[i].Transport < touches[j].Transport
	})
	return touches
}

// formatImpactDate renders an SAP DATS value (YYYYMMDD) as YYYY-MM-DD,
// passing through anything it cannot parse.
func formatImpactDate(dats string) string {
	if t, err := time.Parse("20060102", dats); err == nil {
		return t.Format("2006-01-02")
	}
	return dats
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
