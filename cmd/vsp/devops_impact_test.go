package main

import (
	"context"
	"testing"

	"github.com/spf13/cobra"
	"github.com/vinchacho/vibing-steampunk/pkg/adt"
)

func TestFormatImpactLine(t *testing.T) {
	if got := formatImpactLine(nil); got != "" {
		t.Errorf("nil impact: got %q, want empty", got)
	}

	high := &adt.ImpactSummary{
		Risk:      "high",
		Callers:   47,
		Packages:  []string{"Z_PKG_A", "Z_PKG_B", "Z_PKG_C"},
		Available: true,
	}
	if got, want := formatImpactLine(high), "impact: high — 47 callers across 3 packages"; got != want {
		t.Errorf("formatImpactLine() = %q, want %q", got, want)
	}

	unavailable := &adt.ImpactSummary{
		Risk:        "unknown",
		Available:   false,
		Unavailable: "where-used lookup failed",
	}
	if got, want := formatImpactLine(unavailable), "impact: unknown — where-used unavailable (where-used lookup failed)"; got != want {
		t.Errorf("formatImpactLine() = %q, want %q", got, want)
	}
}

func TestConfirmImpactContext(t *testing.T) {
	cmd := &cobra.Command{}
	cmd.Flags().String("confirm-impact", "", "")

	if ctx := confirmImpactContext(cmd); ctx != context.Background() {
		t.Error("empty --confirm-impact must return the background ctx unchanged")
	}

	if err := cmd.Flags().Set("confirm-impact", "impact-confirm-x"); err != nil {
		t.Fatalf("setting flag: %v", err)
	}
	if ctx := confirmImpactContext(cmd); ctx == context.Background() {
		t.Error("set --confirm-impact must wrap the ctx with the token")
	}
}
