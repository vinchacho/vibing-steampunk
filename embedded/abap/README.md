# Embedded ABAP Objects

This directory contains optional ABAP objects that can be deployed to SAP systems for enhanced vsp functionality.

## WebSocket Handler (ZADT_VSP)

The WebSocket handler enables stateful operations not available through standard ADT REST APIs, such as RFC/BAPI function calls.

### Objects

| File | Object | Description |
|------|--------|-------------|
| `zif_vsp_service.intf.abap` | Interface | Service contract for domain handlers |
| `zcl_vsp_rfc_service.clas.abap` | Class | RFC domain - function module calls |
| `zcl_vsp_debug_service.clas.abap` | Class | Debug domain - breakpoint management |
| `zcl_vsp_apc_handler.clas.abap` | Class | Main APC WebSocket handler |

### Deployment

#### Option 1: Using vsp WriteSource

```bash
# Create package first
vsp CreatePackage --name '$ZADT_VSP' --description 'VSP WebSocket Handler'

# Deploy interface
vsp WriteSource --object_type INTF --name ZIF_VSP_SERVICE \
    --package '$ZADT_VSP' --source "$(cat embedded/abap/zif_vsp_service.intf.abap)"

# Deploy RFC service
vsp WriteSource --object_type CLAS --name ZCL_VSP_RFC_SERVICE \
    --package '$ZADT_VSP' --source "$(cat embedded/abap/zcl_vsp_rfc_service.clas.abap)"

# Deploy debug service
vsp WriteSource --object_type CLAS --name ZCL_VSP_DEBUG_SERVICE \
    --package '$ZADT_VSP' --source "$(cat embedded/abap/zcl_vsp_debug_service.clas.abap)"

# Deploy handler
vsp WriteSource --object_type CLAS --name ZCL_VSP_APC_HANDLER \
    --package '$ZADT_VSP' --source "$(cat embedded/abap/zcl_vsp_apc_handler.clas.abap)"
```

#### Option 2: Using ImportFromFile

```bash
vsp ImportFromFile --file_path embedded/abap/zif_vsp_service.intf.abap --package_name '$ZADT_VSP'
vsp ImportFromFile --file_path embedded/abap/zcl_vsp_rfc_service.clas.abap --package_name '$ZADT_VSP'
vsp ImportFromFile --file_path embedded/abap/zcl_vsp_debug_service.clas.abap --package_name '$ZADT_VSP'
vsp ImportFromFile --file_path embedded/abap/zcl_vsp_apc_handler.clas.abap --package_name '$ZADT_VSP'
```

### Post-Deployment: Create APC Application

After deploying the ABAP objects, create the APC application manually:

1. **Transaction SAPC** - Create APC Application:
   - Application ID: `ZADT_VSP`
   - Description: `VSP WebSocket Handler`
   - Handler Class: `ZCL_VSP_APC_HANDLER`
   - State: Stateful

2. **Transaction SICF** - Activate ICF Service:
   - Path: `/sap/bc/apc/sap/zadt_vsp`
   - Activate the node

### Testing

```bash
# Test connection
wscat -c "ws://host:port/sap/bc/apc/sap/zadt_vsp?sap-client=001" \
      -H "Authorization: Basic $(echo -n user:pass | base64)"

# Or use the test script
go run test/websocket_test.go
```

### Usage

Once deployed, the WebSocket endpoint provides:

**RFC Domain** (`domain: "rfc"`):
- `call` - Execute any RFC/BAPI with parameters
- `search` - Search function modules by pattern
- `getMetadata` - Get function signatures

**Debug Domain** (`domain: "debug"`):

*Breakpoint Management:*
- `setBreakpoint` - Set session breakpoint (line, exception, statement)
- `getBreakpoints` - List session breakpoints
- `deleteBreakpoint` - Remove a breakpoint

*Full Debugging (TPDAPI Integration):*
- `listen` - Start debug listener, wait for debuggee (blocking, with timeout)
- `getDebuggees` - List waiting debuggees
- `attach` - Attach to a caught debuggee
- `step` - Step operations: `into`, `over`, `return`, `continue`
- `getStack` - Get current call stack
- `getVariables` - Get variable values (system scope)
- `detach` - Detach from debuggee

*Status:*
- `getStatus` - Get debug session status (attached, listener active, etc.)

### Debug Flow Example

```json
// 1. Start listener (blocking call)
{"id":"1","domain":"debug","action":"listen","params":{"timeout":60}}
// Response: {"status":"caught","debuggees":[{"id":"...","program":"ZTEST"}]}

// 2. Attach to debuggee
{"id":"2","domain":"debug","action":"attach","params":{"debuggeeId":"..."}}
// Response: {"attached":true,"program":"ZTEST","line":10}

// 3. Step through code
{"id":"3","domain":"debug","action":"step","params":{"type":"over"}}
// Response: {"stepped":"over","program":"ZTEST","line":11}

// 4. Get stack
{"id":"4","domain":"debug","action":"getStack"}
// Response: {"stack":[{"index":0,"program":"ZTEST","line":11,"active":true}]}

// 5. Get variables
{"id":"5","domain":"debug","action":"getVariables","params":{"scope":"system"}}
// Response: {"variables":[{"name":"SY-SUBRC","value":"0"}]}

// 6. Detach
{"id":"6","domain":"debug","action":"detach"}
// Response: {"detached":true}
```

See `reports/2025-12-18-002-websocket-rfc-handler.md` for full RFC documentation.

## Architecture

The WebSocket handler uses a domain-based service architecture:

```
WebSocket Client
      │
      ▼
ZCL_VSP_APC_HANDLER (router)
      │
      ├── ZCL_VSP_RFC_SERVICE (domain: "rfc")
      │
      └── ZCL_VSP_DEBUG_SERVICE (domain: "debug")
```

Each service implements `ZIF_VSP_SERVICE` interface for consistent message handling.
