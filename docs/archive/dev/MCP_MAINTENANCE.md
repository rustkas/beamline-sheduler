# MCP Maintenance Guide

## Quick Reference

### File Locations

| What | Where | Purpose |
|------|-------|---------|
| **MCP Config** | `.cursor/mcp.json` | Cursor IDE reads MCP server configs from HERE ONLY |
| **MCP Code** | `tools/<server>/src/index.ts` | Server implementation code |
| **Compiled Code** | `tools/<server>/dist/index.js` | Executable server (after build) |
| **Inventory** | `mcp-inventory.json` | Complete list of all servers and tools |
| **Architecture Docs** | `docs/archive/dev/MCP_ARCHITECTURE.md` | Architecture documentation |

---

## Common Tasks

### 1. Add a New Tool to Existing Server

**Example**: Add `check_proto_version` tool to `protobuf` server

1. **Edit server code**:
   ```bash
   vim tools/protobuf/src/index.ts
   ```

2. **Add tool definition** in `ListToolsRequestSchema` handler:
   ```typescript
   {
     name: "check_proto_version",
     description: "Check protobuf version",
     inputSchema: { type: "object", properties: {} }
   }
   ```

3. **Add tool handler** in `CallToolRequestSchema` handler:
   ```typescript
   case "check_proto_version":
     result = await this.runner.runScript("scripts/check_proto_version.sh");
     break;
   ```

4. **Rebuild**:
   ```bash
   cd tools/protobuf
   npm run build
   ```

5. **Restart Cursor IDE**

---

### 2. Add a New MCP Server

**Example**: Create `database` server with 3 tools

1. **Create directory structure**:
   ```bash
   mkdir -p tools/database/src tools/database/dist
   ```

2. **Copy template files**:
   ```bash
   cp tools/devstate/package.json tools/database/
   cp tools/devstate/tsconfig.json tools/database/
   cp tools/devstate/src/script-runner.ts tools/database/src/
   ```

3. **Create server code**:
   ```bash
   cp tools/devstate/src/index.ts tools/database/src/index.ts
   # Edit tools/database/src/index.ts
   ```

4. **Update package.json**:
   ```json
   {
     "name": "@beamline/mcp-database",
     "description": "MCP server for database operations"
   }
   ```

5. **Build**:
   ```bash
   cd tools/database
   npm install
   npm run build
   ```

6. **Add to `.cursor/mcp.json`**:
   ```json
   {
     "mcpServers": {
       "database": {
         "type": "stdio",
         "command": "node",
         "args": ["${workspaceFolder}/tools/database/dist/index.js"],
         "cwd": "${workspaceFolder}"
       }
     }
   }
   ```

7. **Update `mcp-inventory.json`** with new server details

8. **Restart Cursor IDE**

---

### 3. Modify Existing Tool

**Example**: Change `devstate_health` to accept timeout parameter

1. **Edit server code**:
   ```bash
   vim tools/devstate/src/index.ts
   ```

2. **Update tool schema**:
   ```typescript
   {
     name: "devstate_health",
     description: "Check DevState health endpoint",
     inputSchema: {
       type: "object",
       properties: {
         url: { type: "string", default: "http://localhost:3180" },
         timeout: { type: "number", default: 5 }  // NEW
       }
     }
   }
   ```

3. **Update tool handler**:
   ```typescript
   case "devstate_health":
     const timeout = args?.timeout || 5;
     result = await this.runCurlHealth(args?.url || "http://localhost:3180", timeout);
     break;
   ```

4. **Rebuild and restart**:
   ```bash
   cd tools/devstate && npm run build
   # Restart Cursor IDE
   ```

---

### 4. Remove a Tool

**Example**: Remove `update_api_registry` from `protobuf` server

1. **Edit server code**:
   ```bash
   vim tools/protobuf/src/index.ts
   ```

2. **Remove from tool list** (in `ListToolsRequestSchema`)

3. **Remove handler** (in `CallToolRequestSchema`)

4. **Rebuild**:
   ```bash
   cd tools/protobuf && npm run build
   ```

5. **Update `mcp-inventory.json`** (remove tool entry)

6. **Restart Cursor IDE**

---

### 5. Remove a Server

**Example**: Remove `project-status` server

1. **Remove from `.cursor/mcp.json`**:
   ```bash
   vim .cursor/mcp.json
   # Delete "project-status" entry
   ```

2. **Remove code**:
   ```bash
   rm -rf tools/project-status
   ```

3. **Update `mcp-inventory.json`** (remove server entry)

4. **Restart Cursor IDE**

---

### 6. Fix Broken Script Path

**Example**: Script moved from `scripts/check.sh` to `scripts/validation/check.sh`

1. **Find all references**:
   ```bash
   grep -r "scripts/check.sh" tools/
   ```

2. **Update server code**:
   ```bash
   vim tools/<server>/src/index.ts
   # Change script path
   ```

3. **Rebuild**:
   ```bash
   cd tools/<server> && npm run build
   ```

---

## Build Commands

### Build Single Server

```bash
cd tools/devstate
npm install
npm run build
```

### Build All Servers

```bash
for dir in tools/*/; do
  if [ -f "$dir/package.json" ]; then
    echo "Building $(basename $dir)..."
    cd "$dir"
    npm install --silent && npm run build
    cd ../..
  fi
done
```

### Verify Builds

```bash
# Count compiled servers
find tools -name "dist/index.js" | wc -l
# Should output: 14 (all TypeScript servers)

# Check for build errors
find tools -name "dist/index.js" -exec node {} \; 2>&1 | grep -i error
```

---

## Testing

### Test Server Manually

```bash
# Start server
node tools/devstate/dist/index.js

# Send MCP request (in another terminal)
echo '{"jsonrpc":"2.0","id":1,"method":"tools/list"}' | node tools/devstate/dist/index.js
```

### Test Tool Execution

```bash
# Test script directly
bash scripts/validate_state.sh

# Test via MCP (requires Cursor IDE or MCP client)
```

---

## Debugging

### Server Not Starting

1. **Check build**:
   ```bash
   ls -la tools/devstate/dist/index.js
   ```

2. **Test manually**:
   ```bash
   node tools/devstate/dist/index.js
   ```

3. **Check Cursor logs**: Cursor IDE → MCP Panel → Server logs

### Tool Returns Error

1. **Test script directly**:
   ```bash
   bash scripts/validate_state.sh
   ```

2. **Check script permissions**:
   ```bash
   ls -la scripts/validate_state.sh
   chmod +x scripts/validate_state.sh  # if needed
   ```

3. **Check script path in server code**:
   ```bash
   grep "validate_state" tools/state-history/src/index.ts
   ```

### TypeScript Compilation Errors

1. **Check TypeScript version**:
   ```bash
   cd tools/devstate
   npx tsc --version
   ```

2. **Check for errors**:
   ```bash
   npx tsc --noEmit
   ```

3. **Check dependencies**:
   ```bash
   npm install
   ```

---

## File Checklist

When modifying MCP servers, update these files:

- [ ] `.cursor/mcp.json` - Server configuration
- [ ] `tools/<server>/src/index.ts` - Server code
- [ ] `tools/<server>/package.json` - Dependencies (if changed)
- [ ] `mcp-inventory.json` - Inventory (if tools/servers changed)
- [ ] `docs/archive/dev/MCP_ARCHITECTURE.md` - Architecture docs (if structure changed)
- [ ] `docs/archive/dev/MCP_MAINTENANCE.md` - This file (if procedures changed)

---

## Common Patterns

### Pattern 1: Simple Script Wrapper

```typescript
case "my_tool":
  result = await this.runner.runScript("scripts/my_script.sh");
  break;
```

### Pattern 2: Script with Arguments

```typescript
case "my_tool":
  const arg1 = args?.param1 || "default";
  result = await this.runner.runScript("scripts/my_script.sh", {}, arg1);
  break;
```

### Pattern 3: Script with Environment Variables

```typescript
case "my_tool":
  result = await this.runner.runScript("scripts/my_script.sh", {
    MY_VAR: args?.value || "default"
  });
  break;
```

### Pattern 4: Python Script

```typescript
case "my_tool":
  const pyArgs = ["--arg", args?.value];
  result = await this.runner.runPythonScript("scripts/my_script.py", pyArgs);
  break;
```

### Pattern 5: Make Command

```typescript
case "my_tool":
  result = await this.runner.runMakeCommand("my-target");
  break;
```

---

## Best Practices

1. ✅ **Always rebuild** after modifying TypeScript code
2. ✅ **Test scripts manually** before wrapping in MCP
3. ✅ **Keep `mcp-inventory.json`** in sync with actual code
4. ✅ **Use `script-runner.ts`** for consistent execution
5. ✅ **Handle errors gracefully** in tool handlers
6. ✅ **Document tool parameters** in input schemas
7. ✅ **Follow naming conventions**: kebab-case for server IDs, snake_case for tools
8. ✅ **Update documentation** when adding/removing tools

---

## Quick Commands Reference

```bash
# Build all servers
for d in tools/*/; do [ -f "$d/package.json" ] && (cd "$d" && npm run build); done

# Find all MCP servers
ls -d tools/*/ | grep -v node_modules

# Count tools in inventory
jq '.mcp_servers | length' mcp-inventory.json

# List all tool names
jq -r '.mcp_servers[].tools[].name' mcp-inventory.json

# Check server configs
jq '.mcpServers | keys' .cursor/mcp.json
```

---

## Support

For questions or issues:
1. Check `docs/archive/dev/MCP_ARCHITECTURE.md` for architecture details
2. Review `mcp-inventory.json` for complete tool list
3. Check Cursor IDE MCP panel for server logs
4. Test scripts manually before debugging MCP wrappers

