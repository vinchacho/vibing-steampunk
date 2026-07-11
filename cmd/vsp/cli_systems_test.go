package main

import (
	"os"
	"testing"

	"github.com/vinchacho/vibing-steampunk/pkg/config"
)

// systems init must write to a path the loader actually searches —
// regression test for init writing a dead .vsp-systems.json that
// LoadSystems never found.
func TestSystemsInitRoundtrip(t *testing.T) {
	oldWd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	if err := os.Chdir(t.TempDir()); err != nil {
		t.Fatal(err)
	}
	defer os.Chdir(oldWd)

	if err := systemsInitCmd.RunE(systemsInitCmd, nil); err != nil {
		t.Fatalf("systems init: %v", err)
	}

	cfg, path, err := config.LoadSystems()
	if err != nil {
		t.Fatalf("LoadSystems after init: %v", err)
	}
	if cfg == nil {
		t.Fatalf("LoadSystems found no config after init (searched %v)", config.ConfigPaths())
	}
	if want := config.ConfigPaths()[0]; path != want {
		t.Errorf("config found at %q, want %q", path, want)
	}
}
