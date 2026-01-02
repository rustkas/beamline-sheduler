# MCP Documentation Index

Complete documentation for MCP (Model Context Protocol) servers in the BeamLine project.

## 📚 Documentation Files

### Core Documentation

1. **[MCP_SERVERS_CATALOG.md](./MCP_SERVERS_CATALOG.md)** - Complete catalog of all MCP servers ⭐
   - Full list of all 15 servers
   - Purpose and capabilities of each server
   - All 64 tools with descriptions
   - **Start here** to see what's available

2. **[MCP_ARCHITECTURE.md](./MCP_ARCHITECTURE.md)** - Architecture and file organization
   - File locations and purposes
   - Server structure and templates
   - Key concepts and lifecycle
   - **Read this** to understand the system structure

3. **[MCP_MAINTENANCE.md](./MCP_MAINTENANCE.md)** - Maintenance guide
   - Common tasks (add tool, add server, modify, remove)
   - Build commands
   - Testing and debugging
   - **Use this** for day-to-day maintenance

4. **[MCP_FILE_MAP.md](./MCP_FILE_MAP.md)** - Quick file reference
   - Where to find what
   - Quick lookup table
   - **Reference this** when you need to find a file

## 🗺️ Quick Navigation

### I want to...

**See all available MCP servers and tools**
→ Read [MCP_SERVERS_CATALOG.md](./MCP_SERVERS_CATALOG.md) ⭐

**Understand what a specific server does**
→ See [MCP_SERVERS_CATALOG.md](./MCP_SERVERS_CATALOG.md) → Find server by ID

**Understand the architecture**
→ Read [MCP_ARCHITECTURE.md](./MCP_ARCHITECTURE.md)

**Add a new tool**
→ See [MCP_MAINTENANCE.md](./MCP_MAINTENANCE.md) → "Add a New Tool"

**Add a new server**
→ See [MCP_MAINTENANCE.md](./MCP_MAINTENANCE.md) → "Add a New MCP Server"

**Find where configs are**
→ See [MCP_FILE_MAP.md](./MCP_FILE_MAP.md) → "Configuration Files"

**Find where code is**
→ See [MCP_FILE_MAP.md](./MCP_FILE_MAP.md) → "Code Files"

**Debug a server**
→ See [MCP_MAINTENANCE.md](./MCP_MAINTENANCE.md) → "Debugging"

**Run and smoke C-Gateway (CP1 quick summary)**
→ See [GATEWAY_SSE_AND_EXTENSIONS.md](./GATEWAY_SSE_AND_EXTENSIONS.md)

## 📋 Key Facts

### MCP Descriptions (Configurations)
- **Location**: `.cursor/mcp.json` (ONLY this file)
- **Purpose**: Cursor IDE reads server configs from here
- **Format**: JSON

### MCP Code (Implementation)
- **Location**: `tools/<server>/src/index.ts`
- **Compiled**: `tools/<server>/dist/index.js` (after build)
- **Format**: TypeScript (14 servers) or Python (1 server)

### Complete Inventory
- **Location**: `mcp-inventory.json` (project root)
- **Purpose**: Complete list of all 15 servers and 64 tools

## 🔗 Related Files

### In Project Root
- `.cursor/mcp.json` - MCP server configurations
- `mcp-inventory.json` - Complete inventory

### In `.cursor/`
- `.cursor/MCP_README.md` - Quick reference
- `.cursor/MCP_STATUS.md` - Implementation status

### In `tools/`
- `tools/README.md` - Developer guide for building/running

### Gateway
- [GATEWAY_SSE_AND_EXTENSIONS.md](./GATEWAY_SSE_AND_EXTENSIONS.md) - C-Gateway realtime (SSE) and Extensions API (build/run, smoke)

## 📊 Statistics

- **Total Servers**: 15
- **Total Tools**: 95
- **TypeScript Servers**: 14
- **Python Servers**: 1
- **Configuration Files**: 1 (`.cursor/mcp.json`)

## 🎯 Getting Started

1. **Read architecture**: [MCP_ARCHITECTURE.md](./MCP_ARCHITECTURE.md)
2. **Check inventory**: `mcp-inventory.json`
3. **Review config**: `.cursor/mcp.json`
4. **Build servers**: See [MCP_MAINTENANCE.md](./MCP_MAINTENANCE.md) → "Build Commands"

## 💡 Remember

- **MCP Descriptions** = `.cursor/mcp.json` (ONLY file)
- **MCP Code** = `tools/<server>/src/index.ts`
- **All tools wrap real scripts** from the repository
- **Rebuild after changes** to TypeScript code
- **Restart Cursor IDE** to reload servers

