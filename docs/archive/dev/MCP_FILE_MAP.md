# MCP File Map

Quick reference: where to find what.

## Configuration Files (MCP Descriptions)

| File | Purpose | When to Edit |
|------|---------|--------------|
| `.cursor/mcp.json` | **ONLY** file with MCP server configs | Adding/removing servers |
| `mcp-inventory.json` | Complete inventory of servers/tools | Adding/removing tools/servers |

## Code Files (MCP Implementation)

### TypeScript Servers (14 servers)

| Pattern | Example | Purpose |
|---------|---------|---------|
| `tools/<server>/src/index.ts` | `tools/devstate/src/index.ts` | Server implementation |
| `tools/<server>/src/script-runner.ts` | `tools/devstate/src/script-runner.ts` | Script execution utility |
| `tools/<server>/dist/index.js` | `tools/devstate/dist/index.js` | Compiled server (after build) |
| `tools/<server>/package.json` | `tools/devstate/package.json` | Node.js dependencies |
| `tools/<server>/tsconfig.json` | `tools/devstate/tsconfig.json` | TypeScript config |

### Python Server (1 server)

| File | Purpose |
|------|---------|
| `tools/windsurf-hooks/server.py` | Python MCP server |

## Documentation Files

| File | Purpose |
|------|---------|
| `docs/archive/dev/MCP_ARCHITECTURE.md` | Architecture documentation |
| `docs/archive/dev/MCP_MAINTENANCE.md` | Maintenance guide |
| `docs/archive/dev/MCP_FILE_MAP.md` | This file |
| `.cursor/MCP_README.md` | Quick reference |
| `.cursor/MCP_STATUS.md` | Implementation status |
| `tools/README.md` | Developer guide |

## Quick Lookup

**Q: Where do I add a new MCP server?**
→ 1. Create `tools/<server>/src/index.ts`
   2. Add entry to `.cursor/mcp.json`

**Q: Where do I add a new tool?**
→ Edit `tools/<server>/src/index.ts`

**Q: Where is the server config?**
→ `.cursor/mcp.json` (ONLY this file)

**Q: Where is the server code?**
→ `tools/<server>/src/index.ts`

**Q: Where is compiled code?**
→ `tools/<server>/dist/index.js` (after build)

**Q: Where is the complete inventory?**
→ `mcp-inventory.json`
