package main

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/oisee/vibing-steampunk/pkg/config"
	"github.com/spf13/cobra"
)

func init() {
	rootCmd.AddCommand(configCmd)
	configCmd.AddCommand(configInitCmd)
	configCmd.AddCommand(configShowCmd)
}

var configCmd = &cobra.Command{
	Use:   "config",
	Short: "Manage vsp configuration files",
	Long: `Manage vsp configuration files for different usage modes.

vsp supports three configuration methods:

1. .env file (or SAP_* env vars) - Default system for MCP server mode
2. .vsp-systems.json - Multiple systems for CLI mode (vsp -s <system>)
3. .mcp.json - Claude Desktop MCP server configuration

Priority (highest to lowest):
  CLI flags > Environment variables > .env file > Defaults

Use 'vsp config init' to create example configuration files.
Use 'vsp config show' to display effective configuration.`,
}

var configInitCmd = &cobra.Command{
	Use:   "init",
	Short: "Create example configuration files",
	Long: `Create example configuration files with documentation.

Files created:
  .env.example           - Environment variables for default system
  .vsp-systems.example   - Multiple systems for CLI mode
  .mcp.json.example      - Claude Desktop configuration

These are created as .example files to avoid overwriting existing configs.
Copy and edit them to create your actual configuration.`,
	RunE: runConfigInit,
}

func runConfigInit(cmd *cobra.Command, args []string) error {
	files := map[string]string{
		".env.example":          envExample,
		".vsp-systems.example":  vspSystemsExample,
		".mcp.json.example":     mcpJsonExample,
	}

	created := 0
	for name, content := range files {
		if _, err := os.Stat(name); err == nil {
			fmt.Printf("  SKIP %s (already exists)\n", name)
			continue
		}

		if err := os.WriteFile(name, []byte(content), 0600); err != nil {
			return fmt.Errorf("failed to create %s: %w", name, err)
		}
		fmt.Printf("  CREATE %s\n", name)
		created++
	}

	fmt.Printf("\nCreated %d example files.\n", created)
	fmt.Println("\nNext steps:")
	fmt.Println("  1. Copy .env.example to .env and fill in your SAP credentials")
	fmt.Println("  2. Copy .vsp-systems.example to .vsp-systems.json for CLI mode")
	fmt.Println("  3. Copy .mcp.json.example to .mcp.json for Claude Desktop")
	fmt.Println("\nSee each file for detailed documentation.")

	return nil
}

var configShowCmd = &cobra.Command{
	Use:   "show",
	Short: "Show effective configuration",
	Long: `Display the effective configuration from all sources.

Shows:
  - Environment variables (SAP_*)
  - Systems from .vsp-systems.json
  - .mcp.json if present`,
	RunE: runConfigShow,
}

func runConfigShow(cmd *cobra.Command, args []string) error {
	fmt.Println("=== vsp Configuration ===")
	fmt.Println()

	// Environment variables
	fmt.Println("Environment Variables (SAP_*):")
	envVars := []string{"SAP_URL", "SAP_USER", "SAP_CLIENT", "SAP_LANGUAGE", "SAP_INSECURE", "SAP_MODE"}
	hasEnv := false
	for _, key := range envVars {
		if val := os.Getenv(key); val != "" {
			if key == "SAP_PASSWORD" {
				val = "***"
			}
			fmt.Printf("  %s=%s\n", key, val)
			hasEnv = true
		}
	}
	if os.Getenv("SAP_PASSWORD") != "" {
		fmt.Printf("  SAP_PASSWORD=***\n")
		hasEnv = true
	}
	if !hasEnv {
		fmt.Println("  (none set)")
	}

	// .env file
	fmt.Println("\n.env File:")
	if _, err := os.Stat(".env"); err == nil {
		fmt.Println("  Found: .env")
	} else {
		fmt.Println("  Not found")
	}

	// Systems config
	fmt.Println("\nSystems Config (.vsp-systems.json):")
	cfg, path, err := config.LoadSystems()
	if err != nil {
		fmt.Printf("  Error: %v\n", err)
	} else if cfg == nil {
		fmt.Println("  Not found")
	} else {
		fmt.Printf("  Found: %s\n", path)
		fmt.Printf("  Default: %s\n", cfg.Default)
		fmt.Println("  Systems:")
		for name, sys := range cfg.Systems {
			pwdStatus := "env"
			envKey := fmt.Sprintf("VSP_%s_PASSWORD", strings.ToUpper(name))
			if sys.Password != "" {
				pwdStatus = "inline"
			} else if os.Getenv(envKey) != "" {
				pwdStatus = "env ✓"
			} else {
				pwdStatus = "env ✗ (need " + envKey + ")"
			}
			marker := ""
			if name == cfg.Default {
				marker = " (default)"
			}
			fmt.Printf("    %s: %s [%s@%s] pwd:%s%s\n", name, sys.URL, sys.User, sys.Client, pwdStatus, marker)
		}
	}

	// MCP config
	fmt.Println("\nMCP Config (.mcp.json):")
	if mcpCfg, err := loadMCPConfig(); err == nil {
		fmt.Printf("  Found: .mcp.json\n")
		if vsp, ok := mcpCfg["mcpServers"].(map[string]interface{})["vsp"]; ok {
			if vspMap, ok := vsp.(map[string]interface{}); ok {
				if cmd, ok := vspMap["command"].(string); ok {
					fmt.Printf("  Command: %s\n", cmd)
				}
				if args, ok := vspMap["args"].([]interface{}); ok {
					argStrs := make([]string, len(args))
					for i, a := range args {
						argStrs[i] = fmt.Sprint(a)
					}
					// Mask password
					for i, a := range argStrs {
						if a == "--password" || a == "-p" {
							if i+1 < len(argStrs) {
								argStrs[i+1] = "***"
							}
						}
					}
					fmt.Printf("  Args: %s\n", strings.Join(argStrs, " "))
				}
			}
		} else {
			fmt.Println("  No 'vsp' server configured")
		}
	} else if os.IsNotExist(err) {
		fmt.Println("  Not found")
	} else {
		fmt.Printf("  Error: %v\n", err)
	}

	return nil
}

func loadMCPConfig() (map[string]interface{}, error) {
	data, err := os.ReadFile(".mcp.json")
	if err != nil {
		return nil, err
	}
	var cfg map[string]interface{}
	if err := json.Unmarshal(data, &cfg); err != nil {
		return nil, err
	}
	return cfg, nil
}

// Example configuration files

const envExample = `# vsp Environment Configuration
# Copy this file to .env and fill in your SAP credentials.
#
# This is the DEFAULT system used when running vsp without --system flag.
# For multiple systems, use .vsp-systems.json instead.
#
# Priority: CLI flags > Environment variables > .env > Defaults

# SAP Connection (required)
SAP_URL=http://your-sap-host:50000
SAP_USER=YOUR_USERNAME
SAP_PASSWORD=YOUR_PASSWORD

# SAP Options (optional)
SAP_CLIENT=001
SAP_LANGUAGE=EN
SAP_INSECURE=false

# Tool Mode (optional)
# focused = 19 essential tools (default)
# expert = all 45+ tools
SAP_MODE=focused

# Safety Options (optional)
# SAP_READ_ONLY=false
# SAP_BLOCK_FREE_SQL=false
# SAP_ALLOWED_PACKAGES=Z*,Y*
# SAP_ALLOWED_OPS=RSQ

# Feature Flags (optional, values: auto/on/off)
# SAP_FEATURE_ABAPGIT=auto
# SAP_FEATURE_RAP=auto
# SAP_FEATURE_AMDP=auto
# SAP_FEATURE_UI5=auto
# SAP_FEATURE_TRANSPORT=auto
`

var vspSystemsExample = func() string {
	cfg := config.SystemsConfig{
		Default: "dev",
		Systems: map[string]config.SystemConfig{
			"dev": {
				URL:      "http://dev-sap.example.com:50000",
				User:     "DEVELOPER",
				Client:   "001",
				Language: "EN",
			},
			"a4h": {
				URL:      "http://a4h.local:50000",
				User:     "ADMIN",
				Client:   "001",
				Insecure: true,
			},
			"prod": {
				URL:             "https://prod-sap.example.com:44300",
				User:            "READONLY",
				Client:          "100",
				ReadOnly:        true,
				AllowedPackages: []string{"Z*", "Y*"},
			},
		},
	}
	data, _ := json.MarshalIndent(cfg, "", "  ")

	return fmt.Sprintf(`// vsp Systems Configuration
// Copy this file to .vsp-systems.json and edit for your systems.
//
// Usage: vsp -s <system> <command>
// Example: vsp -s dev search "ZCL_*"
//
// Passwords are loaded from environment variables:
//   VSP_<SYSTEM>_PASSWORD (e.g., VSP_DEV_PASSWORD, VSP_A4H_PASSWORD)
//
// Config file locations (searched in order):
//   .vsp-systems.json        (current directory)
//   .vsp/systems.json        (current directory)
//   ~/.vsp-systems.json      (home directory)
//   ~/.vsp/systems.json      (home directory)

%s
`, string(data))
}()

var mcpJsonExample = func() string {
	// Get the executable path for the example
	execPath, _ := os.Executable()
	if execPath == "" {
		execPath = "/path/to/vsp"
	}
	execPath = filepath.Base(execPath) // Just use the binary name

	return fmt.Sprintf(`{
  "_comment": "vsp MCP Server Configuration for Claude Desktop",
  "_docs": [
    "Copy this file to .mcp.json in your project directory.",
    "Or add to ~/.config/claude/claude_desktop_config.json",
    "",
    "Password options:",
    "  1. Use 'env' block (recommended): SAP_PASSWORD loaded from environment",
    "  2. Use --password in args (not recommended): visible in config",
    "",
    "Multiple systems: Create separate server entries (vsp-dev, vsp-prod, etc.)"
  ],
  "mcpServers": {
    "vsp": {
      "command": "%s",
      "args": [
        "--url", "http://your-sap-host:50000",
        "--user", "YOUR_USERNAME",
        "--client", "001",
        "--mode", "focused"
      ],
      "env": {
        "SAP_PASSWORD": "YOUR_PASSWORD_HERE"
      }
    },
    "vsp-dev": {
      "command": "%s",
      "args": ["--url", "http://dev:50000", "--user", "DEV_USER"],
      "env": {"SAP_PASSWORD": "dev_password"}
    },
    "vsp-prod": {
      "command": "%s",
      "args": [
        "--url", "https://prod:44300",
        "--user", "PROD_USER",
        "--read-only",
        "--allowed-packages", "Z*,Y*"
      ],
      "env": {"SAP_PASSWORD": "prod_password"}
    }
  }
}
`, execPath, execPath, execPath)
}()
