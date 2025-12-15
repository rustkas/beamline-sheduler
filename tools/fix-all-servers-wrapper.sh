#!/bin/bash
# Fix all servers to use MCPServerWrapper

for dir in tools/*/; do
  if [ -f "$dir/src/index.ts" ] && [ "$dir" != "tools/windsurf-hooks/" ]; then
    SERVER=$(basename "$dir")
    echo "Fixing $SERVER..."
    
    # Replace Server import with MCPServerWrapper
    sed -i 's|import { Server } from "@modelcontextprotocol/sdk/server/index.js";|import { MCPServerWrapper } from "./mcp-server-wrapper.js";|g' "$dir/src/index.ts"
    
    # Replace Server type with MCPServerWrapper
    sed -i 's|private server: Server;|private server: MCPServerWrapper;|g' "$dir/src/index.ts"
    sed -i 's|private server: Server|private server: MCPServerWrapper|g' "$dir/src/index.ts"
    
    # Replace new Server( with new MCPServerWrapper(
    sed -i 's|new Server(|new MCPServerWrapper(|g' "$dir/src/index.ts"
  fi
done

echo "✅ All servers updated"
