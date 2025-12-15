#!/bin/bash
# Apply MCPServerWrapper to all TypeScript MCP servers

WRAPPER_FILE="tools/shared/mcp-server-wrapper.ts"

# Copy wrapper to each server's src directory
for dir in tools/*/; do
  if [ -f "$dir/src/index.ts" ] && [ "$dir" != "tools/windsurf-hooks/" ]; then
    SERVER=$(basename "$dir")
    echo "Processing $SERVER..."
    
    # Copy wrapper file
    cp "$WRAPPER_FILE" "$dir/src/mcp-server-wrapper.ts"
    
    # Update imports and Server usage
    sed -i 's|import { Server } from "@modelcontextprotocol/sdk/server/index.js";|import { MCPServerWrapper } from "./mcp-server-wrapper.js";|g' "$dir/src/index.ts"
    sed -i 's|private server: Server;|private server: MCPServerWrapper;|g' "$dir/src/index.ts"
    sed -i 's|new Server(|new MCPServerWrapper(|g' "$dir/src/index.ts"
  fi
done

echo "✅ Wrapper applied to all servers"
