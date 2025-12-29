# BeamLine MCP Servers

This directory contains all MCP (Model Context Protocol) servers for the BeamLine project.

## Structure

```
tools/
  ├── shared/                    # Shared utilities (script-runner.ts)
  ├── devstate/                  # DevState Management (8 tools)
  ├── state-history/             # State & History Validation (9 tools)
  ├── schema-validation/         # Schema Validation (1 tool)
  ├── protobuf/                  # Protobuf Tools (7 tools)
  ├── security/                  # Security & Secrets (3 tools)
  ├── compliance/                # Compliance Validation (2 tools)
  ├── observability/             # Observability Validation (4 tools)
  ├── infrastructure/            # Infrastructure Management (9 tools)
  ├── documentation/             # Documentation Tools (1 tool)
  ├── release-management/        # Release Management (2 tools)
  ├── ci-validation/             # CI/CD Validation (2 tools)
  ├── cp-validation/             # Checkpoint Validation (4 tools)
  ├── router-testing/            # Router Testing (10 tools)
  ├── project-status/            # Project Status (1 tool)
  └── windsurf-hooks/            # Windsurf Hooks (Python, 2 tools)
```

## Building Servers

Each server has its own `package.json` and can be built independently:

```bash
# Build a specific server
cd tools/devstate
npm install
npm run build

# Build all servers
for dir in tools/*/; do
  if [ -f "$dir/package.json" ]; then
    echo "Building $dir"
    cd "$dir"
    npm install && npm run build
    cd ../..
  fi
done
```

## Running Servers

Servers are configured in `.cursor/mcp.json` and run automatically when Cursor IDE starts.

To test a server manually:

```bash
# Node.js servers
node tools/devstate/dist/index.js

# Python servers
python3 tools/windsurf-hooks/server.py
```

## Server Details

### Node.js Servers (TypeScript)

All Node.js servers follow the same structure:
- `src/index.ts` - Main server code
- `src/script-runner.ts` - Script execution utility
- `package.json` - Dependencies and scripts
- `tsconfig.json` - TypeScript configuration
- `dist/index.js` - Compiled output (after build)

### Python Servers

- `windsurf-hooks/server.py` - Windsurf hooks MCP server

## Configuration

All servers are configured in `.cursor/mcp.json`. See that file for server-specific settings.

## Development

1. Make changes to `src/index.ts` in the relevant server
2. Run `npm run build` to compile TypeScript
3. Restart Cursor IDE to reload MCP servers

## Notes

- All servers use STDIO transport (required by Cursor)
- Scripts are executed from the project root directory
- Environment variables can be set per-server in `.cursor/mcp.json`
- Exit codes from scripts are preserved in tool responses

