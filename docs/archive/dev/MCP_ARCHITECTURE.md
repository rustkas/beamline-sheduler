# MCP Architecture Documentation

## Purpose

This document describes the architecture and file organization of MCP (Model Context Protocol) servers in the BeamLine project. It explains where MCP configurations, code, and documentation are located, and how to maintain them.

## Overview

The BeamLine project uses **15 MCP servers** that wrap **95 real tools** (scripts and commands) from the repository. All MCP servers follow the Cursor IDE MCP specification and use STDIO transport.

## File Organization

### 📋 MCP Configuration (Descriptions)

**Location**: `.cursor/mcp.json`

**Purpose**: This is the **ONLY** file where Cursor IDE reads MCP server configurations. It describes:
- Which MCP servers exist
- How to start each server (command, arguments)
- Environment variables for each server
- Working directory settings

**Structure**:
```json
{
  "mcpServers": {
    "server-id": {
      "type": "stdio",
      "command": "node",
      "args": ["${workspaceFolder}/tools/server-name/dist/index.js"],
      "cwd": "${workspaceFolder}",
      "env": {}
    }
  }
}
```

**Important**: 
- ✅ **ONLY** `.cursor/mcp.json` contains MCP server descriptions
- ❌ NOT in `package.json`
- ❌ NOT in `.cursor/rules/`
- ❌ NOT in `.cursor/settings.json`

**Maintenance**: When adding/removing/modifying MCP servers, update this file.

---

### 💻 MCP Code (Implementation)

**Location**: `tools/` directory

**Purpose**: Contains the actual implementation code for all MCP servers.

**Structure**:
```
tools/
  ├── shared/                    # Shared utilities (optional)
  │   └── script-runner.ts      # Script execution utility
  │
  ├── devstate/                  # MCP Server #1: DevState Management
  │   ├── src/
  │   │   ├── index.ts          # Main MCP server code
  │   │   └── script-runner.ts  # Local copy of script runner
  │   ├── dist/                  # Compiled output (after build)
  │   │   └── index.js          # Executable server
  │   ├── package.json          # Node.js dependencies
  │   └── tsconfig.json         # TypeScript configuration
  │
  ├── state-history/             # MCP Server #2: State & History
  ├── schema-validation/         # MCP Server #3: Schema Validation
  ├── protobuf/                  # MCP Server #4: Protobuf Tools
  ├── security/                  # MCP Server #5: Security & Secrets
  ├── compliance/                # MCP Server #6: Compliance
  ├── observability/            # MCP Server #7: Observability
  ├── infrastructure/            # MCP Server #8: Infrastructure
  ├── documentation/             # MCP Server #9: Documentation
  ├── release-management/        # MCP Server #10: Release Management
  ├── ci-validation/            # MCP Server #11: CI/CD Validation
  ├── cp-validation/            # MCP Server #12: CP Validation
  ├── router-testing/           # MCP Server #13: Router Testing
  ├── project-status/           # MCP Server #14: Project Status
  └── windsurf-hooks/           # MCP Server #15: Windsurf Hooks (Python)
      └── server.py             # Python MCP server
```

**Server Types**:

1. **TypeScript Servers** (14 servers):
   - Source: `tools/<server>/src/index.ts`
   - Compiled: `tools/<server>/dist/index.js` (after `npm run build`)
   - Dependencies: `tools/<server>/package.json`
   - Config: `tools/<server>/tsconfig.json`

2. **Python Server** (1 server):
   - Source: `tools/windsurf-hooks/server.py`
   - Runs directly (no compilation needed)

**Maintenance**: 
- Modify `src/index.ts` to change server behavior
- Run `npm run build` to compile TypeScript servers
- Update `package.json` to change dependencies

---

### 📚 MCP Documentation

**Locations**:

1. **`.cursor/MCP_README.md`** - Quick reference for MCP configuration
2. **`.cursor/MCP_STATUS.md`** - Implementation status and next steps
3. **`tools/README.md`** - Developer guide for building/running servers
4. **`mcp-inventory.json`** - Complete inventory of all MCP servers and tools
5. **`docs/archive/dev/MCP_ARCHITECTURE.md`** - This file (architecture documentation)

---

## MCP Server Structure

### TypeScript Server Template

Each TypeScript MCP server follows this structure:

```typescript
#!/usr/bin/env node
import { Server } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { CallToolRequestSchema, ListToolsRequestSchema } from "@modelcontextprotocol/sdk/types.js";
import { ScriptRunner } from "./script-runner.js";

class MyMCPServer {
  private server: Server;
  private runner: ScriptRunner;
  
  constructor() {
    this.server = new Server(
      { name: "my-mcp-server", version: "1.0.0" },
      { capabilities: { tools: {} } }
    );
    this.runner = new ScriptRunner(process.cwd());
    this.setupHandlers();
  }
  
  private setupHandlers() {
    // List tools
    this.server.setRequestHandler(ListToolsRequestSchema, async () => ({
      tools: [/* tool definitions */]
    }));
    
    // Handle tool calls
    this.server.setRequestHandler(CallToolRequestSchema, async (request) => {
      // Execute tools
    });
  }
  
  async run() {
    const transport = new StdioServerTransport();
    await this.server.connect(transport);
    console.error("My MCP Server running");
  }
}

new MyMCPServer().run().catch(console.error);
```

### Python Server Template

```python
#!/usr/bin/env python3
import json
import sys
import subprocess

def handle_mcp_request(request):
    # Handle MCP protocol
    pass

def main():
    for line in sys.stdin:
        request = json.loads(line.strip())
        response = handle_mcp_request(request)
        print(json.dumps(response))
        sys.stdout.flush()

if __name__ == "__main__":
    main()
```

---

## Key Concepts

### 1. MCP Configuration vs MCP Code

| Aspect | Configuration | Code |
|--------|--------------|------|
| **Location** | `.cursor/mcp.json` | `tools/<server>/` |
| **Purpose** | Describes servers | Implements servers |
| **Format** | JSON | TypeScript/Python |
| **When Changed** | Adding/removing servers | Modifying server behavior |
| **Cursor Reads** | ✅ Yes | ❌ No (runs via config) |

### 2. Tool vs Script

- **Tool**: MCP tool exposed to Cursor IDE (e.g., `devstate_up`)
- **Script**: Real script/command wrapped by tool (e.g., `make devstate-up`)

Every MCP tool wraps a real script from the repository.

### 3. Server Lifecycle

1. **Development**: Edit `tools/<server>/src/index.ts`
2. **Build**: Run `npm run build` → creates `dist/index.js`
3. **Configuration**: Cursor reads `.cursor/mcp.json`
4. **Runtime**: Cursor starts server via `node dist/index.js`
5. **Execution**: Server wraps scripts and returns results

---

## Maintenance Guide

### Adding a New MCP Server

1. **Create server code**:
   ```bash
   mkdir -p tools/my-new-server/src
   # Copy template from existing server
   cp tools/devstate/src/index.ts tools/my-new-server/src/index.ts
   cp tools/devstate/src/script-runner.ts tools/my-new-server/src/script-runner.ts
   cp tools/devstate/package.json tools/my-new-server/package.json
   cp tools/devstate/tsconfig.json tools/my-new-server/tsconfig.json
   ```

2. **Modify server code**:
   - Update `src/index.ts` with your tools
   - Update `package.json` name/description

3. **Build server**:
   ```bash
   cd tools/my-new-server
   npm install
   npm run build
   ```

4. **Add to configuration**:
   - Edit `.cursor/mcp.json`
   - Add entry for `my-new-server`

5. **Update documentation**:
   - Update `mcp-inventory.json`
   - Update this file if needed

### Modifying an Existing Server

1. **Edit source code**: `tools/<server>/src/index.ts`
2. **Rebuild**: `cd tools/<server> && npm run build`
3. **Restart Cursor IDE** to reload

### Removing a Server

1. **Remove from config**: Delete entry in `.cursor/mcp.json`
2. **Remove code**: `rm -rf tools/<server>`
3. **Update docs**: Update `mcp-inventory.json`

### Debugging a Server

1. **Check build**: Verify `dist/index.js` exists
2. **Test manually**: `node tools/<server>/dist/index.js`
3. **Check logs**: Cursor IDE MCP panel shows server logs
4. **Verify script paths**: Ensure wrapped scripts exist

---

## File Reference

### Configuration Files

| File | Purpose | Location |
|------|---------|----------|
| `.cursor/mcp.json` | MCP server configurations | Project root |
| `mcp-inventory.json` | Complete inventory of servers/tools | Project root |

### Code Files

| Pattern | Purpose | Example |
|---------|---------|---------|
| `tools/<server>/src/index.ts` | Server implementation | `tools/devstate/src/index.ts` |
| `tools/<server>/src/script-runner.ts` | Script execution utility | `tools/devstate/src/script-runner.ts` |
| `tools/<server>/dist/index.js` | Compiled server (after build) | `tools/devstate/dist/index.js` |
| `tools/<server>/package.json` | Node.js dependencies | `tools/devstate/package.json` |
| `tools/<server>/tsconfig.json` | TypeScript config | `tools/devstate/tsconfig.json` |
| `tools/windsurf-hooks/server.py` | Python server | `tools/windsurf-hooks/server.py` |

### Documentation Files

| File | Purpose | Location |
|------|---------|----------|
| `docs/archive/dev/MCP_ARCHITECTURE.md` | Architecture documentation | `docs/archive/dev/` |
| `.cursor/MCP_README.md` | Quick reference | `.cursor/` |
| `.cursor/MCP_STATUS.md` | Implementation status | `.cursor/` |
| `tools/README.md` | Developer guide | `tools/` |

---

## Quick Reference

### Where to Find Things

**Q: Where are MCP server descriptions?**
→ `.cursor/mcp.json` (ONLY this file)

**Q: Where is MCP server code?**
→ `tools/<server-name>/src/index.ts`

**Q: Where are compiled servers?**
→ `tools/<server-name>/dist/index.js` (after build)

**Q: How do I add a new tool to a server?**
→ Edit `tools/<server>/src/index.ts`, rebuild, restart Cursor

**Q: How do I add a new server?**
→ Create `tools/<new-server>/`, add entry to `.cursor/mcp.json`

**Q: Where is the complete inventory?**
→ `mcp-inventory.json` (root directory)

---

## Best Practices

1. **Always update `.cursor/mcp.json`** when adding/removing servers
2. **Rebuild servers** after modifying TypeScript code
3. **Test manually** before committing changes
4. **Keep `mcp-inventory.json`** in sync with actual servers
5. **Document changes** in relevant documentation files
6. **Use `script-runner.ts`** for consistent script execution
7. **Follow naming conventions**: kebab-case for server IDs

---

## Troubleshooting

### Server Not Loading

1. Check `.cursor/mcp.json` syntax (valid JSON)
2. Verify `dist/index.js` exists (run `npm run build`)
3. Check Cursor IDE MCP panel for errors
4. Verify script paths in server code

### Tool Not Working

1. Check that wrapped script exists
2. Verify script permissions (`chmod +x`)
3. Check script output/errors in tool response
4. Test script manually first

### Build Errors

1. Run `npm install` in server directory
2. Check TypeScript errors: `npx tsc --noEmit`
3. Verify `tsconfig.json` is correct
4. Check Node.js version compatibility

---

## Summary

- **MCP Descriptions**: `.cursor/mcp.json` (ONLY file)
- **MCP Code**: `tools/<server>/src/index.ts`
- **Compiled Code**: `tools/<server>/dist/index.js` (after build)
- **Documentation**: `docs/archive/dev/MCP_ARCHITECTURE.md` and others
- **Inventory**: `mcp-inventory.json`

All MCP servers wrap real scripts from the repository. No tools are invented - every tool corresponds to an actual script or command.

