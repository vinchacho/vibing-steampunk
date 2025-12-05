package adt

import (
	"context"
	"crypto/rand"
	"encoding/hex"
	"encoding/xml"
	"fmt"
	"net/http"
	"net/url"
	"strings"
	"sync"
	"time"
)

// --- Breakpoint Types ---

// BreakpointKind represents the type of breakpoint.
type BreakpointKind string

const (
	BreakpointKindLine      BreakpointKind = "line"
	BreakpointKindStatement BreakpointKind = "statement"
	BreakpointKindException BreakpointKind = "exception"
	BreakpointKindMessage   BreakpointKind = "message"
)

// BreakpointScope determines the lifetime of a breakpoint.
type BreakpointScope string

const (
	// BreakpointScopeExternal persists across sessions (external/static breakpoints)
	BreakpointScopeExternal BreakpointScope = "external"
	// BreakpointScopeDebugger is session-bound (only during debug session)
	BreakpointScopeDebugger BreakpointScope = "debugger"
)

// DebuggingMode determines how debugging is triggered.
type DebuggingMode string

const (
	// DebuggingModeUser debugs all processes of a specific user
	DebuggingModeUser DebuggingMode = "user"
	// DebuggingModeTerminal debugs processes from a specific terminal
	DebuggingModeTerminal DebuggingMode = "terminal"
)

// terminalID is a unique identifier for this vsp session.
// It's generated once per process and used for all breakpoint operations.
var (
	terminalID     string
	terminalIDOnce sync.Once
)

// getTerminalID returns a unique terminal ID for this vsp session.
// This is used by SAP to identify which IDE instance owns breakpoints
// and where to route debug events.
func getTerminalID() string {
	terminalIDOnce.Do(func() {
		// Generate a random 8-byte hex string prefixed with "vsp-"
		b := make([]byte, 8)
		if _, err := rand.Read(b); err != nil {
			// Fallback to a simple identifier if random fails
			terminalID = "vsp-default"
			return
		}
		terminalID = "vsp-" + hex.EncodeToString(b)
	})
	return terminalID
}

// Breakpoint represents an ABAP debugger breakpoint.
type Breakpoint struct {
	ID          string         `json:"id"`
	Kind        BreakpointKind `json:"kind"`
	Enabled     bool           `json:"enabled"`
	URI         string         `json:"uri,omitempty"`        // ADT URI for line breakpoints
	Line        int            `json:"line,omitempty"`       // Line number for line breakpoints
	Condition   string         `json:"condition,omitempty"`  // Optional condition expression
	Statement   string         `json:"statement,omitempty"`  // Statement type for statement breakpoints
	Exception   string         `json:"exception,omitempty"`  // Exception class for exception breakpoints
	MessageID   string         `json:"messageId,omitempty"`  // Message ID for message breakpoints
	MessageType string         `json:"messageType,omitempty"` // Message type (E, W, I, S, A)
	// Read-only fields returned by SAP
	ActualLine int    `json:"actualLine,omitempty"` // Actual line after adjustment
	IsActive   bool   `json:"isActive,omitempty"`   // Whether BP is currently active
	ObjectName string `json:"objectName,omitempty"` // Name of the object containing the BP
}

// BreakpointRequest contains parameters for creating breakpoints.
type BreakpointRequest struct {
	Scope           BreakpointScope `json:"scope"`
	DebuggingMode   DebuggingMode   `json:"debuggingMode"`
	TerminalID      string          `json:"terminalId,omitempty"`
	User            string          `json:"user,omitempty"`
	IdeID           string          `json:"ideId,omitempty"`           // IDE identifier (default: "vsp")
	ClientID        string          `json:"clientId,omitempty"`        // Client ID for breakpoints
	SystemDebugging bool            `json:"systemDebugging,omitempty"` // Enable system debugging
	Deactivated     bool            `json:"deactivated,omitempty"`     // Create breakpoints in deactivated state
	SyncScopeURI    string          `json:"syncScopeUri,omitempty"`    // Partial sync scope URI
	Breakpoints     []Breakpoint    `json:"breakpoints"`
}

// BreakpointResponse contains the result of breakpoint operations.
type BreakpointResponse struct {
	Breakpoints []Breakpoint `json:"breakpoints"`
}

// --- External Breakpoints API ---

// SetExternalBreakpoint creates an external (persistent) breakpoint.
// These breakpoints persist across sessions and trigger when the specified user runs code that hits them.
//
// For line breakpoints:
//   - objectURI: ADT URI of the object (e.g., "/sap/bc/adt/programs/programs/ZTEST")
//   - line: Source line number (1-based)
//
// For exception breakpoints:
//   - exceptionClass: Exception class name (e.g., "CX_SY_ZERODIVIDE")
//
// For statement breakpoints:
//   - statement: Statement type (e.g., "WRITE", "CALL FUNCTION")
func (c *Client) SetExternalBreakpoint(ctx context.Context, req *BreakpointRequest) (*BreakpointResponse, error) {
	if req.Scope == "" {
		req.Scope = BreakpointScopeExternal
	}
	if req.DebuggingMode == "" {
		req.DebuggingMode = DebuggingModeUser
	}

	body, err := buildBreakpointRequestXML(req)
	if err != nil {
		return nil, fmt.Errorf("building breakpoint request: %w", err)
	}

	// POST with XML body - all parameters are in the XML, no query params needed
	resp, err := c.transport.Request(ctx, "/sap/bc/adt/debugger/breakpoints", &RequestOptions{
		Method:      http.MethodPost,
		Body:        []byte(body),
		ContentType: "application/xml",
		Accept:      "application/xml",
	})
	if err != nil {
		return nil, fmt.Errorf("set external breakpoint failed: %w", err)
	}

	return parseBreakpointResponse(resp.Body)
}

// GetExternalBreakpoints retrieves all external breakpoints for a user.
// user is required for external breakpoints in user debugging mode.
func (c *Client) GetExternalBreakpoints(ctx context.Context, user string) (*BreakpointResponse, error) {
	query := url.Values{}
	query.Set("scope", string(BreakpointScopeExternal))
	query.Set("debuggingMode", string(DebuggingModeUser))
	// All four parameters required - terminalId identifies this vsp session
	query.Set("requestUser", user)
	query.Set("terminalId", getTerminalID())
	query.Set("ideId", "vsp")

	resp, err := c.transport.Request(ctx, "/sap/bc/adt/debugger/breakpoints", &RequestOptions{
		Method: http.MethodGet,
		Accept: "application/xml",
		Query:  query,
	})
	if err != nil {
		return nil, fmt.Errorf("get external breakpoints failed: %w", err)
	}

	// Handle empty response (no breakpoints)
	if len(resp.Body) == 0 {
		return &BreakpointResponse{}, nil
	}

	return parseBreakpointResponse(resp.Body)
}

// DeleteExternalBreakpoint removes an external breakpoint by ID.
// user is required for external breakpoints in user debugging mode.
func (c *Client) DeleteExternalBreakpoint(ctx context.Context, breakpointID string, user string) error {
	query := url.Values{}
	query.Set("scope", string(BreakpointScopeExternal))
	query.Set("debuggingMode", string(DebuggingModeUser))
	// All parameters required - terminalId identifies this vsp session
	query.Set("requestUser", user)
	query.Set("terminalId", getTerminalID())
	query.Set("ideId", "vsp")

	endpoint := fmt.Sprintf("/sap/bc/adt/debugger/breakpoints/%s", url.PathEscape(breakpointID))
	_, err := c.transport.Request(ctx, endpoint, &RequestOptions{
		Method: http.MethodDelete,
		Query:  query,
	})
	if err != nil {
		return fmt.Errorf("delete external breakpoint failed: %w", err)
	}

	return nil
}

// DeleteAllExternalBreakpoints removes all external breakpoints for a user.
func (c *Client) DeleteAllExternalBreakpoints(ctx context.Context, user string) error {
	// Get all breakpoints first
	bps, err := c.GetExternalBreakpoints(ctx, user)
	if err != nil {
		return fmt.Errorf("getting breakpoints for deletion: %w", err)
	}

	// Delete each one
	for _, bp := range bps.Breakpoints {
		if err := c.DeleteExternalBreakpoint(ctx, bp.ID, user); err != nil {
			return fmt.Errorf("deleting breakpoint %s: %w", bp.ID, err)
		}
	}

	return nil
}

// ValidateBreakpointCondition checks if a breakpoint condition expression is valid.
func (c *Client) ValidateBreakpointCondition(ctx context.Context, condition string) (bool, string, error) {
	body := fmt.Sprintf(`<?xml version="1.0" encoding="UTF-8"?>
<dbg:condition xmlns:dbg="http://www.sap.com/adt/debugger">%s</dbg:condition>`,
		xmlEscape(condition))

	resp, err := c.transport.Request(ctx, "/sap/bc/adt/debugger/breakpoints/conditions", &RequestOptions{
		Method:      http.MethodPost,
		Body:        []byte(body),
		ContentType: "application/xml",
		Accept:      "application/xml",
	})
	if err != nil {
		// Check if it's a validation error (400) vs other error
		return false, err.Error(), nil
	}

	// Parse validation result
	type validationResult struct {
		Valid   bool   `xml:"valid,attr"`
		Message string `xml:"message,attr"`
	}

	xmlStr := strings.ReplaceAll(string(resp.Body), "dbg:", "")
	var result validationResult
	if err := xml.Unmarshal([]byte(xmlStr), &result); err != nil {
		// If parsing fails but request succeeded, assume valid
		return true, "", nil
	}

	return result.Valid, result.Message, nil
}

// --- Helper functions ---

func buildBreakpointRequestXML(req *BreakpointRequest) (string, error) {
	// Set defaults
	ideID := req.IdeID
	if ideID == "" {
		ideID = "vsp"
	}
	// Use provided terminalId or generate session-unique one
	termID := req.TerminalID
	if termID == "" {
		termID = getTerminalID()
	}

	var bpElements []string

	for _, bp := range req.Breakpoints {
		switch bp.Kind {
		case BreakpointKindLine:
			// Line breakpoint: uses adtcore:uri attribute with fragment for line number
			uri := bp.URI
			if bp.Line > 0 {
				uri = fmt.Sprintf("%s#start=%d", bp.URI, bp.Line)
			}
			attrs := fmt.Sprintf(`kind="line" adtcore:uri="%s"`, xmlEscape(uri))
			if bp.Condition != "" {
				attrs += fmt.Sprintf(` condition="%s"`, xmlEscape(bp.Condition))
			}
			bpElements = append(bpElements, fmt.Sprintf(`<breakpoint %s/>`, attrs))

		case BreakpointKindException:
			bpElements = append(bpElements, fmt.Sprintf(`<breakpoint kind="exception" exceptionClass="%s"/>`,
				xmlEscape(bp.Exception)))

		case BreakpointKindStatement:
			bpElements = append(bpElements, fmt.Sprintf(`<breakpoint kind="statement" statement="%s"/>`,
				xmlEscape(bp.Statement)))

		case BreakpointKindMessage:
			bpElements = append(bpElements, fmt.Sprintf(`<breakpoint kind="message" msgId="%s" msgTy="%s"/>`,
				xmlEscape(bp.MessageID), xmlEscape(bp.MessageType)))
		}
	}

	// Build breakpoint content
	bpContent := ""
	if len(bpElements) > 0 {
		bpContent = "\n  " + strings.Join(bpElements, "\n  ") + "\n"
	}

	// Build optional attributes
	optionalAttrs := ""
	if req.SystemDebugging {
		optionalAttrs += ` systemDebugging="true"`
	}
	if req.Deactivated {
		optionalAttrs += ` deactivated="true"`
	}

	// XML format based on Simple Transformation TPDA_ADT_BREAKPOINTS_REQUEST
	// terminalId is always included - it identifies this vsp session to SAP
	return fmt.Sprintf(`<?xml version="1.0" encoding="UTF-8"?>
<dbg:breakpoints xmlns:dbg="http://www.sap.com/adt/debugger" xmlns:adtcore="http://www.sap.com/adt/core" debuggingMode="%s" scope="%s" requestUser="%s" terminalId="%s" ideId="%s"%s>%s</dbg:breakpoints>`,
		string(req.DebuggingMode), string(req.Scope), xmlEscape(req.User),
		xmlEscape(termID), xmlEscape(ideID),
		optionalAttrs, bpContent), nil
}

func parseBreakpointResponse(data []byte) (*BreakpointResponse, error) {
	// Strip namespace prefixes for easier parsing
	xmlStr := string(data)
	xmlStr = strings.ReplaceAll(xmlStr, "dbg:", "")
	xmlStr = strings.ReplaceAll(xmlStr, "adtcore:", "")

	// Response format: <breakpoints><breakpoint kind="..." id="..." uri="..."/></breakpoints>
	type xmlBreakpoint struct {
		ID             string `xml:"id,attr"`
		Kind           string `xml:"kind,attr"`
		Enabled        bool   `xml:"enabled,attr"`
		IsActive       bool   `xml:"isActive,attr"`
		URI            string `xml:"uri,attr"`          // adtcore:uri attribute
		Condition      string `xml:"condition,attr"`    // condition attribute
		ExceptionClass string `xml:"exceptionClass,attr"`
		Statement      string `xml:"statement,attr"`
		MsgID          string `xml:"msgId,attr"`
		MsgTy          string `xml:"msgTy,attr"`
		ErrorMessage   string `xml:"errorMessage,attr"` // Error case
		ObjectName     string `xml:"name,attr"`         // adtcore:name attribute
	}

	// Parse root <breakpoints> element directly
	type xmlBreakpoints struct {
		XMLName     xml.Name        `xml:"breakpoints"`
		Breakpoints []xmlBreakpoint `xml:"breakpoint"`
	}

	var resp xmlBreakpoints
	if err := xml.Unmarshal([]byte(xmlStr), &resp); err != nil {
		return nil, fmt.Errorf("parsing breakpoint response: %w", err)
	}

	result := &BreakpointResponse{}
	for _, bp := range resp.Breakpoints {
		// Skip breakpoints with error messages (couldn't be created)
		if bp.ErrorMessage != "" {
			continue
		}

		breakpoint := Breakpoint{
			ID:          bp.ID,
			Kind:        BreakpointKind(bp.Kind),
			Enabled:     bp.Enabled,
			IsActive:    bp.IsActive,
			URI:         bp.URI,
			Condition:   bp.Condition,
			Exception:   bp.ExceptionClass,
			Statement:   bp.Statement,
			MessageID:   bp.MsgID,
			MessageType: bp.MsgTy,
			ObjectName:  bp.ObjectName,
		}

		// Extract line number from URI fragment if present
		if strings.Contains(breakpoint.URI, "#start=") {
			parts := strings.SplitN(breakpoint.URI, "#start=", 2)
			if len(parts) == 2 {
				breakpoint.URI = parts[0]
				fmt.Sscanf(parts[1], "%d", &breakpoint.Line)
			}
		}

		result.Breakpoints = append(result.Breakpoints, breakpoint)
	}

	return result, nil
}

// xmlEscape escapes special XML characters
func xmlEscape(s string) string {
	s = strings.ReplaceAll(s, "&", "&amp;")
	s = strings.ReplaceAll(s, "<", "&lt;")
	s = strings.ReplaceAll(s, ">", "&gt;")
	s = strings.ReplaceAll(s, "\"", "&quot;")
	s = strings.ReplaceAll(s, "'", "&apos;")
	return s
}

// --- Convenience functions for creating breakpoints ---

// NewLineBreakpoint creates a line breakpoint request.
// objectURI: ADT URI (e.g., "/sap/bc/adt/programs/programs/ZTEST/source/main")
// line: Source line number (1-based)
func NewLineBreakpoint(objectURI string, line int) Breakpoint {
	return Breakpoint{
		Kind:    BreakpointKindLine,
		Enabled: true,
		URI:     objectURI,
		Line:    line,
	}
}

// NewExceptionBreakpoint creates an exception breakpoint request.
// exceptionClass: Exception class name (e.g., "CX_SY_ZERODIVIDE")
func NewExceptionBreakpoint(exceptionClass string) Breakpoint {
	return Breakpoint{
		Kind:      BreakpointKindException,
		Enabled:   true,
		Exception: exceptionClass,
	}
}

// NewStatementBreakpoint creates a statement breakpoint request.
// statement: Statement type (e.g., "WRITE", "CALL FUNCTION", "RAISE EXCEPTION")
func NewStatementBreakpoint(statement string) Breakpoint {
	return Breakpoint{
		Kind:      BreakpointKindStatement,
		Enabled:   true,
		Statement: statement,
	}
}

// NewMessageBreakpoint creates a message breakpoint request.
// messageID: Message ID (e.g., "001")
// messageType: Message type (E=Error, W=Warning, I=Info, S=Success, A=Abort)
func NewMessageBreakpoint(messageID string, messageType string) Breakpoint {
	return Breakpoint{
		Kind:        BreakpointKindMessage,
		Enabled:     true,
		MessageID:   messageID,
		MessageType: messageType,
	}
}

// --- Debug Listener Types ---

// DebuggeeKind represents the type of debuggee.
type DebuggeeKind string

const (
	DebuggeeKindDebuggee        DebuggeeKind = "debuggee"
	DebuggeeKindPostMortem      DebuggeeKind = "postmortem"
	DebuggeeKindPostMortemDialog DebuggeeKind = "postmortem_dialog"
)

// Debuggee represents a process that has hit a breakpoint and is waiting for debugging.
type Debuggee struct {
	ID            string       `json:"debuggeeId"`
	Kind          DebuggeeKind `json:"kind"`
	Client        int          `json:"client"`
	TerminalID    string       `json:"terminalId"`
	IdeID         string       `json:"ideId"`
	User          string       `json:"debuggeeUser"`
	Program       string       `json:"program"`
	Include       string       `json:"include"`
	Line          int          `json:"line"`
	RFCDest       string       `json:"rfcDest,omitempty"`
	AppServer     string       `json:"appServer,omitempty"`
	SystemID      string       `json:"systemId,omitempty"`
	SystemNumber  int          `json:"systemNumber,omitempty"`
	Timestamp     int64        `json:"timestamp,omitempty"`
	IsAttachable  bool         `json:"isAttachable"`
	IsSameServer  bool         `json:"isSameServer"`
	InstanceName  string       `json:"instanceName,omitempty"`
	// For post-mortem debugging (short dumps)
	DumpID     string `json:"dumpId,omitempty"`
	DumpDate   string `json:"dumpDate,omitempty"`
	DumpTime   string `json:"dumpTime,omitempty"`
	DumpHost   string `json:"dumpHost,omitempty"`
	DumpUser   string `json:"dumpUser,omitempty"`
	DumpClient string `json:"dumpClient,omitempty"`
	DumpURI    string `json:"dumpUri,omitempty"`
}

// ListenerConflict represents a conflict with another debug listener.
type ListenerConflict struct {
	ConflictText string `json:"conflictText"`
	IdeUser      string `json:"ideUser"`
}

// ListenResult represents the result of a debug listen operation.
type ListenResult struct {
	Debuggee *Debuggee         `json:"debuggee,omitempty"`
	Conflict *ListenerConflict `json:"conflict,omitempty"`
	TimedOut bool              `json:"timedOut"`
}

// ListenOptions configures the debug listener.
type ListenOptions struct {
	DebuggingMode         DebuggingMode `json:"debuggingMode"`
	User                  string        `json:"user,omitempty"`        // Required for user mode
	TerminalID            string        `json:"terminalId,omitempty"`  // Auto-generated if empty
	IdeID                 string        `json:"ideId,omitempty"`       // Default: "vsp"
	TimeoutSeconds        int           `json:"timeout,omitempty"`     // Default: 240
	CheckConflict         bool          `json:"checkConflict"`
	NotifyOnConflict      bool          `json:"notifyOnConflict"`
}

// --- Debug Listener API ---

// DebuggerListen starts a debug listener that waits for a debuggee to hit a breakpoint.
// This is a BLOCKING call that uses long-polling. It will return when:
// - A debuggee is caught (returns Debuggee info)
// - Timeout occurs (returns TimedOut=true)
// - A conflict is detected (returns Conflict info)
// - Context is cancelled
//
// Default timeout is 240 seconds. For longer waits, call this in a loop.
func (c *Client) DebuggerListen(ctx context.Context, opts *ListenOptions) (*ListenResult, error) {
	if opts == nil {
		opts = &ListenOptions{}
	}
	if opts.DebuggingMode == "" {
		opts.DebuggingMode = DebuggingModeUser
	}
	if opts.IdeID == "" {
		opts.IdeID = "vsp"
	}
	if opts.TerminalID == "" {
		opts.TerminalID = getTerminalID()
	}
	if opts.TimeoutSeconds == 0 {
		opts.TimeoutSeconds = 240
	}

	query := url.Values{}
	query.Set("debuggingMode", string(opts.DebuggingMode))
	query.Set("terminalId", opts.TerminalID)
	query.Set("ideId", opts.IdeID)
	query.Set("timeout", fmt.Sprintf("%d", opts.TimeoutSeconds))

	if opts.User != "" {
		query.Set("requestUser", opts.User)
	}
	if opts.CheckConflict {
		query.Set("checkConflict", "true")
	}
	if opts.NotifyOnConflict {
		query.Set("isNotifiedOnConflict", "true")
	}

	// Long-polling request - use extended timeout via context
	httpTimeout := time.Duration(opts.TimeoutSeconds+30) * time.Second
	listenCtx, cancel := context.WithTimeout(ctx, httpTimeout)
	defer cancel()

	resp, err := c.transport.Request(listenCtx, "/sap/bc/adt/debugger/listeners", &RequestOptions{
		Method: http.MethodPost,
		Accept: "application/xml",
		Query:  query,
	})
	if err != nil {
		// Check for conflict error
		if strings.Contains(err.Error(), "conflict") {
			return &ListenResult{
				Conflict: &ListenerConflict{
					ConflictText: err.Error(),
				},
			}, nil
		}
		return nil, fmt.Errorf("debugger listen failed: %w", err)
	}

	// Empty response = timeout
	if len(resp.Body) == 0 {
		return &ListenResult{TimedOut: true}, nil
	}

	// Parse debuggee response
	debuggee, err := parseDebuggeeResponse(resp.Body)
	if err != nil {
		return nil, fmt.Errorf("parsing debuggee response: %w", err)
	}

	return &ListenResult{Debuggee: debuggee}, nil
}

// DebuggerCheckListener checks if there are active debug listeners.
// Returns nil if no listeners are active.
func (c *Client) DebuggerCheckListener(ctx context.Context, opts *ListenOptions) (*ListenerConflict, error) {
	if opts == nil {
		opts = &ListenOptions{}
	}
	if opts.DebuggingMode == "" {
		opts.DebuggingMode = DebuggingModeUser
	}
	if opts.IdeID == "" {
		opts.IdeID = "vsp"
	}
	if opts.TerminalID == "" {
		opts.TerminalID = getTerminalID()
	}

	query := url.Values{}
	query.Set("debuggingMode", string(opts.DebuggingMode))
	query.Set("terminalId", opts.TerminalID)
	query.Set("ideId", opts.IdeID)
	query.Set("checkConflict", "true")

	if opts.User != "" {
		query.Set("requestUser", opts.User)
	}

	_, err := c.transport.Request(ctx, "/sap/bc/adt/debugger/listeners", &RequestOptions{
		Method: http.MethodGet,
		Accept: "application/xml",
		Query:  query,
	})
	if err != nil {
		// 404 = no listeners active
		if strings.Contains(err.Error(), "404") {
			return nil, nil
		}
		// Conflict detected
		if strings.Contains(err.Error(), "conflict") || strings.Contains(err.Error(), "409") {
			return &ListenerConflict{ConflictText: err.Error()}, nil
		}
		return nil, fmt.Errorf("check listener failed: %w", err)
	}

	return nil, nil
}

// DebuggerStopListener stops an active debug listener.
func (c *Client) DebuggerStopListener(ctx context.Context, opts *ListenOptions) error {
	if opts == nil {
		opts = &ListenOptions{}
	}
	if opts.DebuggingMode == "" {
		opts.DebuggingMode = DebuggingModeUser
	}
	if opts.IdeID == "" {
		opts.IdeID = "vsp"
	}
	if opts.TerminalID == "" {
		opts.TerminalID = getTerminalID()
	}

	query := url.Values{}
	query.Set("debuggingMode", string(opts.DebuggingMode))
	query.Set("terminalId", opts.TerminalID)
	query.Set("ideId", opts.IdeID)

	if opts.User != "" {
		query.Set("requestUser", opts.User)
	}

	_, err := c.transport.Request(ctx, "/sap/bc/adt/debugger/listeners", &RequestOptions{
		Method: http.MethodDelete,
		Query:  query,
	})
	if err != nil {
		return fmt.Errorf("stop listener failed: %w", err)
	}

	return nil
}

// parseDebuggeeResponse parses the XML response containing debuggee information.
func parseDebuggeeResponse(data []byte) (*Debuggee, error) {
	if len(data) == 0 {
		return nil, nil
	}

	// Strip namespace prefixes
	xmlStr := string(data)
	xmlStr = strings.ReplaceAll(xmlStr, "asx:", "")
	xmlStr = strings.ReplaceAll(xmlStr, "abap:", "")

	// The response is in ABAP XML format: <abap><values><DATA><STPDA_DEBUGGEE>...</STPDA_DEBUGGEE></DATA></values></abap>
	type stpdaDebuggee struct {
		Client              int    `xml:"CLIENT"`
		DebuggeeID          string `xml:"DEBUGGEE_ID"`
		TerminalID          string `xml:"TERMINAL_ID"`
		IdeID               string `xml:"IDE_ID"`
		DebuggeeUser        string `xml:"DEBUGGEE_USER"`
		ProgramCurrent      string `xml:"PRG_CURR"`
		IncludeCurrent      string `xml:"INCL_CURR"`
		LineCurrent         int    `xml:"LINE_CURR"`
		RFCDest             string `xml:"RFCDEST"`
		AppServer           string `xml:"APPLSERVER"`
		SystemID            string `xml:"SYSID"`
		SystemNumber        int    `xml:"SYSNR"`
		Timestamp           int64  `xml:"TSTMP"`
		DebuggeeKind        string `xml:"DBGEE_KIND"`
		IsAttachImpossible  string `xml:"IS_ATTACH_IMPOSSIBLE"`
		IsSameServer        string `xml:"IS_SAME_SERVER"`
		InstanceName        string `xml:"INSTANCE_NAME"`
		DumpID              string `xml:"DUMP_ID"`
		DumpDate            string `xml:"DUMP_DATE"`
		DumpTime            string `xml:"DUMP_TIME"`
		DumpHost            string `xml:"DUMP_HOST"`
		DumpUser            string `xml:"DUMP_UNAME"`
		DumpClient          string `xml:"DUMP_CLIENT"`
		DumpURI             string `xml:"DUMP_URI"`
	}

	type abapResponse struct {
		XMLName xml.Name `xml:"abap"`
		Values  struct {
			Data struct {
				Debuggee stpdaDebuggee `xml:"STPDA_DEBUGGEE"`
			} `xml:"DATA"`
		} `xml:"values"`
	}

	var resp abapResponse
	if err := xml.Unmarshal([]byte(xmlStr), &resp); err != nil {
		return nil, fmt.Errorf("parsing debuggee XML: %w", err)
	}

	d := resp.Values.Data.Debuggee
	if d.DebuggeeID == "" {
		return nil, nil // No debuggee
	}

	debuggee := &Debuggee{
		ID:           d.DebuggeeID,
		Client:       d.Client,
		TerminalID:   d.TerminalID,
		IdeID:        d.IdeID,
		User:         d.DebuggeeUser,
		Program:      d.ProgramCurrent,
		Include:      d.IncludeCurrent,
		Line:         d.LineCurrent,
		RFCDest:      d.RFCDest,
		AppServer:    d.AppServer,
		SystemID:     d.SystemID,
		SystemNumber: d.SystemNumber,
		Timestamp:    d.Timestamp,
		InstanceName: d.InstanceName,
		DumpID:       d.DumpID,
		DumpDate:     d.DumpDate,
		DumpTime:     d.DumpTime,
		DumpHost:     d.DumpHost,
		DumpUser:     d.DumpUser,
		DumpClient:   d.DumpClient,
		DumpURI:      d.DumpURI,
	}

	// Parse kind
	switch d.DebuggeeKind {
	case "POSTMORTEM":
		debuggee.Kind = DebuggeeKindPostMortem
	case "POSTMORTEM_DIALOG":
		debuggee.Kind = DebuggeeKindPostMortemDialog
	default:
		debuggee.Kind = DebuggeeKindDebuggee
	}

	// Parse boolean flags
	debuggee.IsAttachable = d.IsAttachImpossible != "X"
	debuggee.IsSameServer = d.IsSameServer == "X"

	return debuggee, nil
}
