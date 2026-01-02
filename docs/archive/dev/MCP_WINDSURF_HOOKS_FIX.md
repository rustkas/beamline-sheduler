# Windsurf Hooks MCP Fix

## Problem

Trae IDE showed error: `MCP error -32601: Unknown method: initialize`

### Root Cause

`tools/windsurf-hooks/server.py` was registered as MCP server, but:
- ❌ Does NOT implement MCP protocol
- ❌ Does NOT handle `initialize` method (required by MCP)
- ❌ Does NOT handle JSON-RPC properly
- ✅ Is a Windsurf IDE hook script, not an MCP server

**Trae IDE** (as MCP client) sends:
1. `initialize` - Required by MCP protocol
2. `tools/list` - Get available tools
3. `tools/call` - Call specific tool

**Old `server.py`** only handled `tools/list` and `tools/call`, but NOT `initialize` → Error.

## Solution

### Step 1: Removed from MCP Configuration

Removed `windsurf-hooks` from `.cursor/mcp.json`:
- It's not an MCP server
- It's a Windsurf IDE hook script
- Windsurf uses it via `.windsurf/hooks.json`, not via MCP

### Step 2: Created Full MCP Server

Created new **`tools/windsurf-hooks-mcp/`** - proper MCP server:

**Features:**
- ✅ Implements full MCP protocol (`initialize`, `tools/list`, `tools/call`)
- ✅ Uses `@modelcontextprotocol/sdk` (like other TypeScript servers)
- ✅ Provides 5 tools for managing Windsurf hooks

**Tools:**
1. `windsurf_recommend_model` - Recommend AI model based on changed files
2. `windsurf_block_read` - Block read access outside allowlist
3. `list_windsurf_hooks` - List hooks from `.windsurf/hooks.json`
4. `run_check_cp1_contracts` - Run CP1 contracts check
5. `run_check_cp2_behavior` - Run CP2 behavior check

## Files

### Removed
- ❌ `windsurf-hooks` entry from `.cursor/mcp.json`

### Created
- ✅ `tools/windsurf-hooks-mcp/` - New MCP server directory
- ✅ `tools/windsurf-hooks-mcp/package.json` - Node.js project config
- ✅ `tools/windsurf-hooks-mcp/tsconfig.json` - TypeScript config
- ✅ `tools/windsurf-hooks-mcp/src/index.ts` - MCP server implementation
- ✅ `tools/windsurf-hooks-mcp/src/script-runner.ts` - Script execution utility
- ✅ `tools/windsurf-hooks-mcp/src/mcp-server-wrapper.ts` - Server wrapper
- ✅ `scripts/windsurf_hooks/list_hooks.sh` - Helper script

### Kept (for Windsurf IDE)
- ✅ `tools/windsurf-hooks/server.py` - Still used by Windsurf IDE via `.windsurf/hooks.json`

## Usage

### In Trae IDE / Cursor IDE

**New MCP server** (`windsurf-hooks-mcp`) provides tools for:
- Managing Windsurf hooks
- Running CP validation checks
- Listing hook configuration

**Old script** (`windsurf-hooks/server.py`) continues to work for Windsurf IDE hooks.

### Configuration

**`.cursor/mcp.json`** (for Cursor/Trae):
```json
{
  "mcpServers": {
    "windsurf-hooks-mcp": {
      "type": "stdio",
      "command": "node",
      "args": ["${workspaceFolder}/tools/windsurf-hooks-mcp/dist/index.js"],
      "cwd": "${workspaceFolder}"
    }
  }
}
```

**`.windsurf/hooks.json`** (for Windsurf IDE):
```json
{
  "hooks": {
    "pre_write_code": [
      {
        "command": "bash -lc './scripts/check_cp1_contracts.sh'",
        "show_output": true,
        "error_action": "block_on_nonzero_exit"
      }
    ],
    "post_write_code": [
      {
        "command": "python3 scripts/windsurf_hooks/recommend_model.py",
        "show_output": true,
        "error_action": "continue_on_nonzero_exit"
      }
    ]
  }
}
```

## Build

```bash
cd tools/windsurf-hooks-mcp
npm install
npm run build
```

## Verification

After fix:
- ✅ No more `Unknown method: initialize` error
- ✅ Trae IDE can connect to `windsurf-hooks-mcp` server
- ✅ All 5 tools available via MCP
- ✅ Windsurf IDE hooks continue to work independently

## Summary

**Before:**
- `windsurf-hooks/server.py` registered as MCP → Error `Unknown method: initialize`
- Not a real MCP server

**After:**
- `windsurf-hooks/server.py` removed from MCP config (still works for Windsurf)
- `windsurf-hooks-mcp` created as proper MCP server
- Full MCP protocol support
- No more errors

