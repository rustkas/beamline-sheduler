# Trae IDE Bug Report Template

## Issue: Trae IDE Incorrectly Counts SDK Internal Methods as MCP Tools

### Description

Trae IDE performs static code analysis on MCP server implementations and incorrectly counts SDK internal methods as MCP tools.

### Expected Behavior

Trae IDE should:
1. Call `tools/list` JSON-RPC method on MCP server
2. Count only tools returned in the response
3. Ignore SDK internal methods

### Actual Behavior

Trae IDE:
1. Performs static code analysis on server source code
2. Finds `import { Server } from "@modelcontextprotocol/sdk/server/index.js"`
3. Analyzes Server class from `node_modules/@modelcontextprotocol/sdk/`
4. Counts ALL methods from Server class as MCP tools
5. Shows incorrect tool count (97 instead of 64)

### Example

**MCP Server Code:**
```typescript
import { Server } from "@modelcontextprotocol/sdk/server/index.js";

class MyServer {
  private server: Server;
  constructor() {
    this.server = new Server(...);
  }
}
```

**Server declares**: 8 tools via `tools/list`

**Trae IDE shows**: 43 tools (8 real + 35 SDK internal methods)

**SDK Internal Methods counted as tools:**
- `ping()`
- `createMessage()`
- `listRoots()`
- `sendLoggingMessage()`
- `sendResourceUpdated()`
- `sendToolListChanged()`
- `sendPromptListChanged()`
- `getClientCapabilities()`
- `getClientVersion()`
- And ~25 other internal methods

### Impact

- **Incorrect tool count**: Shows 97 tools instead of 64
- **Exceeds limit**: Trae has 40 tool limit, but shows 97
- **Confusing UX**: Users see tools that don't exist
- **Cannot use**: Cannot select tools due to limit

### Workaround Attempts

We tried several workarounds, but none work because Trae performs static analysis:

1. **Wrapper class**: Created wrapper that exposes only essential methods
   - ❌ Failed: Trae analyzes SDK class directly, not wrapper

2. **Proxy wrapper**: Used JavaScript Proxy to intercept property access
   - ❌ Failed: Proxy works at runtime, Trae analyzes at compile time

3. **Dynamic import**: Used `import()` to hide SDK from static analysis
   - ❌ Failed: Requires async initialization, breaks MCP protocol

### Correct Implementation

**Cursor IDE works correctly:**
- Uses `tools/list` protocol method
- Shows correct tool count (64 tools)
- No workaround needed

### Requested Fix

Trae IDE should:
1. **Use MCP protocol**: Call `tools/list` JSON-RPC method
2. **Count response**: Count only tools in `tools/list` response
3. **Ignore static analysis**: Don't analyze SDK class methods

### References

- MCP Protocol Spec: https://modelcontextprotocol.io
- MCP tools/list method: Returns actual tools, not SDK methods
- Cursor IDE: Works correctly (reference implementation)

### Environment

- Trae IDE: [version]
- MCP SDK: @modelcontextprotocol/sdk@0.5.0
- Node.js: v20.19.4

