package main

import (
	"reflect"
	"testing"

	"github.com/spf13/cobra"
	"github.com/vinchacho/vibing-steampunk/internal/mcp"
	"github.com/vinchacho/vibing-steampunk/pkg/adt"
	"github.com/vinchacho/vibing-steampunk/pkg/config"
)

func TestSafetyFlagsAreInheritedByCLISubcommands(t *testing.T) {
	for _, name := range []string{"allow-transportable-edits", "impact-gate", "impact-threshold"} {
		if searchCmd.InheritedFlags().Lookup(name) == nil {
			t.Errorf("--%s is not inherited by CLI subcommands", name)
		}
	}
}

func TestResolveCLISafetyPreservesProfileAndAppliesExplicitOverrides(t *testing.T) {
	oldCfg := cfg
	cfg = &mcp.Config{
		ReadOnly:                false,
		AllowedPackages:         []string{"ZFLAG*"},
		AllowedTransports:       []string{"REQ-FLAG*"},
		AllowTransportableEdits: true,
		ImpactGate:              "block",
		ImpactThreshold:         "high",
	}
	t.Cleanup(func() { cfg = oldCfg })

	cmd := &cobra.Command{Use: "test"}
	cmd.Flags().Bool("read-only", false, "")
	cmd.Flags().StringSlice("allowed-packages", nil, "")
	cmd.Flags().StringSlice("allowed-transports", nil, "")
	cmd.Flags().Bool("allow-transportable-edits", false, "")
	cmd.Flags().String("impact-gate", "off", "")
	cmd.Flags().String("impact-threshold", "high", "")
	if err := cmd.Flags().Set("allow-transportable-edits", "true"); err != nil {
		t.Fatal(err)
	}
	if err := cmd.Flags().Set("impact-gate", "block"); err != nil {
		t.Fatal(err)
	}

	profile := &config.SystemConfig{
		ReadOnly:          true,
		AllowedPackages:   []string{"ZPROFILE*"},
		AllowedTransports: []string{"REQ-PROFILE*"},
		ImpactGate:        "advise",
		ImpactThreshold:   "medium",
	}
	safety := resolveCLISafety(cmd, profile)

	if !safety.ReadOnly {
		t.Error("profile read_only setting was not preserved")
	}
	if !reflect.DeepEqual(safety.AllowedPackages, profile.AllowedPackages) {
		t.Fatalf("allowed packages = %v, want %v", safety.AllowedPackages, profile.AllowedPackages)
	}
	if !reflect.DeepEqual(safety.AllowedTransports, profile.AllowedTransports) {
		t.Fatalf("allowed transports = %v, want %v", safety.AllowedTransports, profile.AllowedTransports)
	}
	if !safety.AllowTransportableEdits {
		t.Error("explicit flag did not override the profile")
	}
	if safety.ImpactGate != adt.ImpactGateBlock {
		t.Errorf("impact gate = %q, want explicit flag %q to override profile", safety.ImpactGate, adt.ImpactGateBlock)
	}
	if safety.ImpactThreshold != adt.ImpactThresholdMedium {
		t.Errorf("impact threshold = %q, want profile value %q preserved", safety.ImpactThreshold, adt.ImpactThresholdMedium)
	}
}

func TestResolveCLISafetyNormalizesProfileImpactSettings(t *testing.T) {
	oldCfg := cfg
	cfg = &mcp.Config{ImpactGate: "off", ImpactThreshold: "high"}
	t.Cleanup(func() { cfg = oldCfg })

	cmd := &cobra.Command{Use: "test"}
	cmd.Flags().String("impact-gate", "off", "")
	cmd.Flags().String("impact-threshold", "high", "")

	// Mixed-case profile values are normalized.
	safety := resolveCLISafety(cmd, &config.SystemConfig{ImpactGate: "Advise", ImpactThreshold: "MEDIUM"})
	if safety.ImpactGate != adt.ImpactGateAdvise {
		t.Errorf("impact gate = %q, want normalized %q", safety.ImpactGate, adt.ImpactGateAdvise)
	}
	if safety.ImpactThreshold != adt.ImpactThresholdMedium {
		t.Errorf("impact threshold = %q, want normalized %q", safety.ImpactThreshold, adt.ImpactThresholdMedium)
	}

	// Invalid profile values fall back to the defaults instead of leaking
	// arbitrary strings into the gate comparison.
	safety = resolveCLISafety(cmd, &config.SystemConfig{ImpactGate: "banana", ImpactThreshold: "low"})
	if safety.ImpactGate != adt.ImpactGateOff {
		t.Errorf("impact gate = %q, want fallback %q for invalid profile value", safety.ImpactGate, adt.ImpactGateOff)
	}
	if safety.ImpactThreshold != adt.ImpactThresholdHigh {
		t.Errorf("impact threshold = %q, want fallback %q for invalid profile value", safety.ImpactThreshold, adt.ImpactThresholdHigh)
	}
}

func TestValidateImpactConfigNormalizesAndRejects(t *testing.T) {
	oldCfg := cfg
	t.Cleanup(func() { cfg = oldCfg })

	cfg = &mcp.Config{ImpactGate: "ADVISE", ImpactThreshold: " Medium "}
	if err := validateImpactConfig(); err != nil {
		t.Fatalf("valid values rejected: %v", err)
	}
	if cfg.ImpactGate != adt.ImpactGateAdvise || cfg.ImpactThreshold != adt.ImpactThresholdMedium {
		t.Errorf("normalized to gate=%q threshold=%q, want %q/%q",
			cfg.ImpactGate, cfg.ImpactThreshold, adt.ImpactGateAdvise, adt.ImpactThresholdMedium)
	}

	cfg = &mcp.Config{ImpactGate: "banana", ImpactThreshold: "high"}
	if err := validateImpactConfig(); err == nil {
		t.Error("invalid impact gate was accepted")
	}
	cfg = &mcp.Config{ImpactGate: "advise", ImpactThreshold: "low"}
	if err := validateImpactConfig(); err == nil {
		t.Error("invalid impact threshold was accepted")
	}
}

func TestResolveCLISafetyUsesEnvironmentOverride(t *testing.T) {
	oldCfg := cfg
	cfg = &mcp.Config{AllowTransportableEdits: true, ImpactGate: "advise", ImpactThreshold: "medium"}
	t.Cleanup(func() { cfg = oldCfg })
	t.Setenv("SAP_ALLOW_TRANSPORTABLE_EDITS", "true")
	t.Setenv("SAP_IMPACT_GATE", "advise")
	t.Setenv("SAP_IMPACT_THRESHOLD", "medium")

	cmd := &cobra.Command{Use: "test"}
	cmd.Flags().Bool("allow-transportable-edits", false, "")
	cmd.Flags().String("impact-gate", "off", "")
	cmd.Flags().String("impact-threshold", "high", "")
	safety := resolveCLISafety(cmd, &config.SystemConfig{})
	if !safety.AllowTransportableEdits {
		t.Error("SAP_ALLOW_TRANSPORTABLE_EDITS was not propagated")
	}
	if safety.ImpactGate != adt.ImpactGateAdvise {
		t.Errorf("impact gate = %q, want %q from SAP_IMPACT_GATE", safety.ImpactGate, adt.ImpactGateAdvise)
	}
	if safety.ImpactThreshold != adt.ImpactThresholdMedium {
		t.Errorf("impact threshold = %q, want %q from SAP_IMPACT_THRESHOLD", safety.ImpactThreshold, adt.ImpactThresholdMedium)
	}
}

func TestGetClientInstallsResolvedSafety(t *testing.T) {
	params := &systemParams{
		URL:      "https://example.invalid",
		User:     "user",
		Password: "password",
		Client:   "001",
		Language: "EN",
	}
	params.Safety.AllowTransportableEdits = true
	params.Safety.AllowedTransports = []string{"REQ-TEST*"}
	params.Safety.ImpactGate = "advise"
	params.Safety.ImpactThreshold = "medium"

	client, err := getClient(params)
	if err != nil {
		t.Fatal(err)
	}
	if !client.Safety().AllowTransportableEdits {
		t.Error("resolved safety was not installed on the ADT client")
	}
	if !reflect.DeepEqual(client.Safety().AllowedTransports, params.Safety.AllowedTransports) {
		t.Fatalf("allowed transports = %v, want %v", client.Safety().AllowedTransports, params.Safety.AllowedTransports)
	}
	if client.Safety().ImpactGate != adt.ImpactGateAdvise {
		t.Errorf("impact gate = %q, want %q installed on the ADT client", client.Safety().ImpactGate, adt.ImpactGateAdvise)
	}
	if client.Safety().ImpactThreshold != adt.ImpactThresholdMedium {
		t.Errorf("impact threshold = %q, want %q installed on the ADT client", client.Safety().ImpactThreshold, adt.ImpactThresholdMedium)
	}
}
