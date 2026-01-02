# MCP Trae IDE Workaround

## Problem

Trae IDE incorrectly counts **43 tools** for `state-history` server instead of the correct **8 tools**.

### Root Cause

Trae IDE performs introspection on the MCP Server class and counts **all methods** (including SDK internal methods) as MCP tools:

- `ping()` - SDK internal method
- `createMessage()` - SDK internal method
- `listRoots()` - SDK internal method
- `sendLoggingMessage()` - SDK internal method
- `sendResourceUpdated()` - SDK internal method
- `sendToolListChanged()` - SDK internal method
- `sendPromptListChanged()` - SDK internal method
- `getClientCapabilities()` - SDK internal method
- `getClientVersion()` - SDK internal method
- And ~35 other SDK internal methods

**Correct behavior**: Trae should use `tools/list` MCP protocol method, which returns only the actual tools (8 for state-history).

## Solution: MCPServerWrapper

We created a wrapper class (`MCPServerWrapper`) that:

1. **Wraps** the real MCP Server internally
2. **Exposes only** the essential MCP protocol methods:
   - `setRequestHandler()` - Register tool handlers
   - `setNotificationHandler()` - Register notification handlers
   - `connect()` - Connect to transport
3. **Hides** all SDK internal methods from introspection

### Implementation

**File**: `tools/shared/mcp-server-wrapper.ts`

```typescript
export class MCPServerWrapper {
  private server: Server;

  constructor(serverInfo: Implementation, options: ServerOptions) {
    this.server = new Server(serverInfo, options);
  }

  // Expose only essential MCP protocol methods
  setRequestHandler(schema: any, handler: any): void {
    return this.server.setRequestHandler(schema, handler);
  }

  setNotificationHandler(schema: any, handler: any): void {
    return this.server.setNotificationHandler(schema, handler);
  }

  async connect(transport: any): Promise<void> {
    return this.server.connect(transport);
  }

  // All SDK internal methods are hidden
}
```

### Usage

**Before** (exposed all SDK methods):
```typescript
import { Server } from "@modelcontextprotocol/sdk/server/index.js";

class MyServer {
  private server: Server;
  constructor() {
    this.server = new Server(...);
  }
}
```

**After** (exposes only MCP protocol):
```typescript
import { MCPServerWrapper } from "./mcp-server-wrapper.js";

class MyServer {
  private server: MCPServerWrapper;
  constructor() {
    this.server = new MCPServerWrapper(...);
  }
}
```

## Applied To

All **14 TypeScript MCP servers** now use `MCPServerWrapper`:

- ✅ ci-validation
- ✅ compliance
- ✅ cp-validation
- ✅ devstate
- ✅ documentation
- ✅ infrastructure
- ✅ observability
- ✅ project-status
- ✅ protobuf
- ✅ release-management
- ✅ router-testing
- ✅ schema-validation
- ✅ security
- ✅ state-history

**Python server** (`windsurf-hooks`) doesn't need this workaround.

## Verification

### Test tools/list Response

```bash
cd tools/state-history
node -e "
const { spawn } = require('child_process');
const server = spawn('node', ['dist/index.js'], { stdio: ['pipe', 'pipe', 'pipe'] });
const request = { jsonrpc: '2.0', id: 1, method: 'tools/list', params: {} };
server.stdin.write(JSON.stringify(request) + '\n');
server.stdin.end();
let output = '';
server.stdout.on('data', d => output += d.toString());
setTimeout(() => {
  const r = JSON.parse(output);
  console.log('Tools:', r.result?.tools?.length);
  server.kill();
}, 1000);
"
```

**Expected**: `Tools: 8` (not 43)

### Expected Trae IDE Behavior

After applying wrapper:
- Trae IDE should see **only** the methods exposed by `MCPServerWrapper`:
  - `setRequestHandler`
  - `setNotificationHandler`
  - `connect`
- Trae IDE should **not** see SDK internal methods
- Trae IDE should use `tools/list` to get actual tools (8 for state-history)

## Limitations

1. **Workaround, not fix**: This is a workaround for Trae IDE bug. The proper fix should be in Trae IDE itself.

2. **May not work**: If Trae IDE performs deep introspection (checks prototype chain), it may still see SDK methods.

3. **Maintenance**: If SDK adds new essential methods, we need to expose them in wrapper.

## Future Improvements

1. **Report to Trae IDE**: File a bug report with Trae IDE developers
2. **Monitor SDK changes**: Update wrapper if SDK API changes
3. **Consider alternatives**: If wrapper doesn't work, consider:
   - Using lower-level MCP protocol implementation
   - Creating custom MCP server without SDK
   - Waiting for Trae IDE fix

## References

- MCP Protocol Spec: https://modelcontextprotocol.io
- Trae IDE: (report bug if wrapper doesn't work)
- SDK Documentation: `node_modules/@modelcontextprotocol/sdk/README.md`

