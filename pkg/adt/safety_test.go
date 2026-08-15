package adt

import (
	"testing"
)

func TestSafetyConfig_IsOperationAllowed(t *testing.T) {
	tests := []struct {
		name     string
		config   SafetyConfig
		op       OperationType
		expected bool
	}{
		{
			name:     "ReadOnly blocks create",
			config:   SafetyConfig{ReadOnly: true},
			op:       OpCreate,
			expected: false,
		},
		{
			name:     "ReadOnly blocks update",
			config:   SafetyConfig{ReadOnly: true},
			op:       OpUpdate,
			expected: false,
		},
		{
			name:     "ReadOnly blocks delete",
			config:   SafetyConfig{ReadOnly: true},
			op:       OpDelete,
			expected: false,
		},
		{
			name:     "ReadOnly blocks activate",
			config:   SafetyConfig{ReadOnly: true},
			op:       OpActivate,
			expected: false,
		},
		{
			name:     "ReadOnly blocks workflow",
			config:   SafetyConfig{ReadOnly: true},
			op:       OpWorkflow,
			expected: false,
		},
		{
			name:     "ReadOnly allows read",
			config:   SafetyConfig{ReadOnly: true},
			op:       OpRead,
			expected: true,
		},
		{
			name:     "ReadOnly allows search",
			config:   SafetyConfig{ReadOnly: true},
			op:       OpSearch,
			expected: true,
		},
		{
			name:     "ReadOnly allows query",
			config:   SafetyConfig{ReadOnly: true},
			op:       OpQuery,
			expected: true,
		},
		{
			name:     "BlockFreeSQL blocks free SQL",
			config:   SafetyConfig{BlockFreeSQL: true},
			op:       OpFreeSQL,
			expected: false,
		},
		{
			name:     "BlockFreeSQL allows read",
			config:   SafetyConfig{BlockFreeSQL: true},
			op:       OpRead,
			expected: true,
		},
		{
			name:     "AllowedOps whitelist - allowed op",
			config:   SafetyConfig{AllowedOps: "RSQ"},
			op:       OpRead,
			expected: true,
		},
		{
			name:     "AllowedOps whitelist - blocked op",
			config:   SafetyConfig{AllowedOps: "RSQ"},
			op:       OpCreate,
			expected: false,
		},
		{
			name:     "DisallowedOps blacklist",
			config:   SafetyConfig{DisallowedOps: "CDUA"},
			op:       OpCreate,
			expected: false,
		},
		{
			name:     "DisallowedOps allows non-blacklisted",
			config:   SafetyConfig{DisallowedOps: "CDUA"},
			op:       OpRead,
			expected: true,
		},
		{
			name:     "DisallowedOps overrides AllowedOps",
			config:   SafetyConfig{AllowedOps: "RCDU", DisallowedOps: "CD"},
			op:       OpCreate,
			expected: false,
		},
		{
			name:     "DryRun allows all",
			config:   SafetyConfig{DryRun: true, ReadOnly: true},
			op:       OpCreate,
			expected: true,
		},
		{
			name:     "Unrestricted allows all",
			config:   UnrestrictedSafetyConfig(),
			op:       OpCreate,
			expected: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := tt.config.IsOperationAllowed(tt.op)
			if result != tt.expected {
				t.Errorf("IsOperationAllowed(%c) = %v, expected %v", tt.op, result, tt.expected)
			}
		})
	}
}

func TestSafetyConfig_CheckOperation(t *testing.T) {
	config := SafetyConfig{ReadOnly: true}

	// Should allow read
	err := config.CheckOperation(OpRead, "GetClass")
	if err != nil {
		t.Errorf("CheckOperation(OpRead) should not error, got: %v", err)
	}

	// Should block create
	err = config.CheckOperation(OpCreate, "CreateObject")
	if err == nil {
		t.Error("CheckOperation(OpCreate) should error in read-only mode")
	}
}

func TestSafetyConfig_IsPackageAllowed(t *testing.T) {
	tests := []struct {
		name     string
		config   SafetyConfig
		pkg      string
		expected bool
	}{
		{
			name:     "Empty AllowedPackages allows all",
			config:   SafetyConfig{},
			pkg:      "ZANY",
			expected: true,
		},
		{
			name:     "Exact match",
			config:   SafetyConfig{AllowedPackages: []string{"$TMP", "ZTEST"}},
			pkg:      "$TMP",
			expected: true,
		},
		{
			name:     "Not in whitelist",
			config:   SafetyConfig{AllowedPackages: []string{"$TMP", "ZTEST"}},
			pkg:      "ZPROD",
			expected: false,
		},
		{
			name:     "Wildcard match - Z*",
			config:   SafetyConfig{AllowedPackages: []string{"Z*"}},
			pkg:      "ZTEST",
			expected: true,
		},
		{
			name:     "Wildcard match - $*",
			config:   SafetyConfig{AllowedPackages: []string{"$*"}},
			pkg:      "$TMP",
			expected: true,
		},
		{
			name:     "Wildcard no match",
			config:   SafetyConfig{AllowedPackages: []string{"Z*"}},
			pkg:      "$TMP",
			expected: false,
		},
		{
			name:     "Case insensitive",
			config:   SafetyConfig{AllowedPackages: []string{"ztest"}},
			pkg:      "ZTEST",
			expected: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := tt.config.IsPackageAllowed(tt.pkg)
			if result != tt.expected {
				t.Errorf("IsPackageAllowed(%s) = %v, expected %v", tt.pkg, result, tt.expected)
			}
		})
	}
}

func TestSafetyConfig_CheckPackage(t *testing.T) {
	config := SafetyConfig{AllowedPackages: []string{"$TMP", "Z*"}}

	// Should allow $TMP
	err := config.CheckPackage("$TMP")
	if err != nil {
		t.Errorf("CheckPackage($TMP) should not error, got: %v", err)
	}

	// Should allow ZTEST (wildcard)
	err = config.CheckPackage("ZTEST")
	if err != nil {
		t.Errorf("CheckPackage(ZTEST) should not error, got: %v", err)
	}

	// Should block PROD
	err = config.CheckPackage("PROD")
	if err == nil {
		t.Error("CheckPackage(PROD) should error")
	}
}

func TestSafetyConfig_String(t *testing.T) {
	tests := []struct {
		name     string
		config   SafetyConfig
		contains []string
	}{
		{
			name:     "Unrestricted",
			config:   UnrestrictedSafetyConfig(),
			contains: []string{"UNRESTRICTED"},
		},
		{
			name:     "ReadOnly",
			config:   SafetyConfig{ReadOnly: true},
			contains: []string{"READ-ONLY"},
		},
		{
			name:     "BlockFreeSQL",
			config:   SafetyConfig{BlockFreeSQL: true},
			contains: []string{"NO-FREE-SQL"},
		},
		{
			name:     "DryRun",
			config:   SafetyConfig{DryRun: true},
			contains: []string{"DRY-RUN"},
		},
		{
			name:     "AllowedOps",
			config:   SafetyConfig{AllowedOps: "RSQ"},
			contains: []string{"AllowedOps=RSQ"},
		},
		{
			name:     "Multiple flags",
			config:   SafetyConfig{ReadOnly: true, BlockFreeSQL: true},
			contains: []string{"READ-ONLY", "NO-FREE-SQL"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := tt.config.String()
			for _, expected := range tt.contains {
				if !contains(result, expected) {
					t.Errorf("String() = %q, should contain %q", result, expected)
				}
			}
		})
	}
}

func TestDefaultSafetyConfig(t *testing.T) {
	config := DefaultSafetyConfig()

	if !config.ReadOnly {
		t.Error("DefaultSafetyConfig should be read-only")
	}

	if !config.BlockFreeSQL {
		t.Error("DefaultSafetyConfig should block free SQL")
	}

	if !config.IsOperationAllowed(OpRead) {
		t.Error("DefaultSafetyConfig should allow read operations")
	}

	if config.IsOperationAllowed(OpCreate) {
		t.Error("DefaultSafetyConfig should not allow create operations")
	}
}

func TestDevelopmentSafetyConfig(t *testing.T) {
	config := DevelopmentSafetyConfig()

	if config.ReadOnly {
		t.Error("DevelopmentSafetyConfig should not be read-only")
	}

	if !config.BlockFreeSQL {
		t.Error("DevelopmentSafetyConfig should block free SQL")
	}

	if !config.IsPackageAllowed("$TMP") {
		t.Error("DevelopmentSafetyConfig should allow $TMP")
	}

	if config.IsPackageAllowed("ZPROD") {
		t.Error("DevelopmentSafetyConfig should not allow ZPROD")
	}
}

// Helper function
func contains(s, substr string) bool {
	return len(s) >= len(substr) && (s == substr || len(s) > len(substr) && (s[:len(substr)] == substr || s[len(s)-len(substr):] == substr || containsInMiddle(s, substr)))
}

func containsInMiddle(s, substr string) bool {
	for i := 0; i <= len(s)-len(substr); i++ {
		if s[i:i+len(substr)] == substr {
			return true
		}
	}
	return false
}

func TestSafetyConfig_CheckTransportableEdit(t *testing.T) {
	tests := []struct {
		name        string
		config      SafetyConfig
		transport   string
		opName      string
		expectError bool
	}{
		{
			name:        "No transport - always allowed",
			config:      SafetyConfig{},
			transport:   "",
			opName:      "EditSource",
			expectError: false,
		},
		{
			name:        "Transport provided but not allowed - blocked",
			config:      SafetyConfig{AllowTransportableEdits: false},
			transport:   "DEVK900123",
			opName:      "EditSource",
			expectError: true,
		},
		{
			name:        "Transport provided and allowed - success",
			config:      SafetyConfig{AllowTransportableEdits: true},
			transport:   "DEVK900123",
			opName:      "EditSource",
			expectError: false,
		},
		{
			name:        "Transport allowed but not in whitelist - blocked",
			config:      SafetyConfig{AllowTransportableEdits: true, AllowedTransports: []string{"A4HK*"}},
			transport:   "DEVK900123",
			opName:      "WriteSource",
			expectError: true,
		},
		{
			name:        "Transport allowed and in whitelist - success",
			config:      SafetyConfig{AllowTransportableEdits: true, AllowedTransports: []string{"DEVK*"}},
			transport:   "DEVK900123",
			opName:      "WriteSource",
			expectError: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			err := tt.config.CheckTransportableEdit(tt.transport, tt.opName)
			if tt.expectError && err == nil {
				t.Error("Expected error but got nil")
			}
			if !tt.expectError && err != nil {
				t.Errorf("Expected no error but got: %v", err)
			}
		})
	}
}

func TestSafetyConfig_CheckTransportableEdit_ErrorMessage(t *testing.T) {
	config := SafetyConfig{AllowTransportableEdits: false}
	err := config.CheckTransportableEdit("DEVK900123", "EditSource")

	if err == nil {
		t.Fatal("Expected error but got nil")
	}

	errMsg := err.Error()

	// Check that error message contains helpful information
	if !contains(errMsg, "EditSource") {
		t.Error("Error message should contain operation name")
	}
	if !contains(errMsg, "DEVK900123") {
		t.Error("Error message should contain transport number")
	}
	if !contains(errMsg, "--allow-transportable-edits") {
		t.Error("Error message should mention CLI flag")
	}
	if !contains(errMsg, "SAP_ALLOW_TRANSPORTABLE_EDITS") {
		t.Error("Error message should mention environment variable")
	}
}

func TestIsUnrestricted(t *testing.T) {
	tests := []struct {
		name string
		cfg  SafetyConfig
		want bool
	}{
		{"unrestricted default", UnrestrictedSafetyConfig(), true},
		{"safe default", DefaultSafetyConfig(), false},
		{"development", DevelopmentSafetyConfig(), false},
		{"read-only", SafetyConfig{ReadOnly: true}, false},
		{"block free sql", SafetyConfig{BlockFreeSQL: true}, false},
		{"allowed ops", SafetyConfig{AllowedOps: "RSQ"}, false},
		{"disallowed ops", SafetyConfig{DisallowedOps: "CDUA"}, false},
		{"package allowlist", SafetyConfig{AllowedPackages: []string{"Z*"}}, false},
		{"transport opt-in alone still unrestricted", SafetyConfig{EnableTransports: true, AllowTransportableEdits: true}, true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := tt.cfg.IsUnrestricted(); got != tt.want {
				t.Errorf("IsUnrestricted() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestNormalizeImpactGate(t *testing.T) {
	tests := []struct {
		in      string
		want    string
		wantErr bool
	}{
		{"", ImpactGateOff, false}, // empty = default
		{"off", ImpactGateOff, false},
		{"advise", ImpactGateAdvise, false},
		{"block", ImpactGateBlock, false},
		{"Advise", ImpactGateAdvise, false}, // case-insensitive
		{" BLOCK ", ImpactGateBlock, false}, // trimmed
		{"banana", "", true},
		{"on", "", true},
	}
	for _, tt := range tests {
		got, err := NormalizeImpactGate(tt.in)
		if (err != nil) != tt.wantErr {
			t.Errorf("NormalizeImpactGate(%q) error = %v, wantErr %v", tt.in, err, tt.wantErr)
			continue
		}
		if got != tt.want {
			t.Errorf("NormalizeImpactGate(%q) = %q, want %q", tt.in, got, tt.want)
		}
	}
}

func TestNormalizeImpactGateErrorListsValidValues(t *testing.T) {
	_, err := NormalizeImpactGate("banana")
	if err == nil {
		t.Fatal("expected error for invalid impact gate")
	}
	for _, v := range []string{"off", "advise", "block"} {
		if !contains(err.Error(), v) {
			t.Errorf("error %q does not list valid value %q", err.Error(), v)
		}
	}
}

func TestNormalizeImpactThreshold(t *testing.T) {
	tests := []struct {
		in      string
		want    string
		wantErr bool
	}{
		{"", ImpactThresholdHigh, false}, // empty = default
		{"high", ImpactThresholdHigh, false},
		{"medium", ImpactThresholdMedium, false},
		{"High", ImpactThresholdHigh, false},       // case-insensitive
		{" Medium ", ImpactThresholdMedium, false}, // trimmed
		{"low", "", true},                          // only high/medium gate
		{"banana", "", true},
	}
	for _, tt := range tests {
		got, err := NormalizeImpactThreshold(tt.in)
		if (err != nil) != tt.wantErr {
			t.Errorf("NormalizeImpactThreshold(%q) error = %v, wantErr %v", tt.in, err, tt.wantErr)
			continue
		}
		if got != tt.want {
			t.Errorf("NormalizeImpactThreshold(%q) = %q, want %q", tt.in, got, tt.want)
		}
	}
}

func TestNormalizeImpactThresholdErrorListsValidValues(t *testing.T) {
	_, err := NormalizeImpactThreshold("low")
	if err == nil {
		t.Fatal("expected error for invalid impact threshold")
	}
	for _, v := range []string{"high", "medium"} {
		if !contains(err.Error(), v) {
			t.Errorf("error %q does not list valid value %q", err.Error(), v)
		}
	}
}

func TestSafetyDefaultsCarryImpactSettings(t *testing.T) {
	for name, cfg := range map[string]SafetyConfig{
		"unrestricted": UnrestrictedSafetyConfig(),
		"default":      DefaultSafetyConfig(),
	} {
		if cfg.ImpactGate != ImpactGateOff {
			t.Errorf("%s: ImpactGate = %q, want %q", name, cfg.ImpactGate, ImpactGateOff)
		}
		if cfg.ImpactThreshold != ImpactThresholdHigh {
			t.Errorf("%s: ImpactThreshold = %q, want %q", name, cfg.ImpactThreshold, ImpactThresholdHigh)
		}
	}
	// The impact gate is advisory config, not an op restriction: defaults must
	// not flip the unrestricted warning.
	u := UnrestrictedSafetyConfig()
	if !u.IsUnrestricted() {
		t.Error("impact defaults must not affect IsUnrestricted()")
	}
}
