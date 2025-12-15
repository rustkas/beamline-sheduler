#!/bin/bash
# Fix types in all TypeScript servers

for dir in tools/*/; do
  if [ -f "$dir/src/index.ts" ]; then
    SERVER_NAME=$(basename "$dir")
    echo "Fixing types in $SERVER_NAME..."
    
    # Add CallToolResult import if not present
    if ! grep -q "type CallToolResult" "$dir/src/index.ts"; then
      sed -i '/from "@modelcontextprotocol\/sdk\/types.js";/a\import type { CallToolResult } from "@modelcontextprotocol/sdk/types.js";' "$dir/src/index.ts" 2>/dev/null || \
      sed -i 's|import {|import type { CallToolResult } from "@modelcontextprotocol/sdk/types.js";\nimport {|' "$dir/src/index.ts" 2>/dev/null || true
    fi
    
    # Replace ToolResult interface with CallToolResult type
    sed -i 's/interface ToolResult {[^}]*}/type ToolResult = CallToolResult;/' "$dir/src/index.ts" 2>/dev/null || true
    
    # Fix args?.url type assertions
    sed -i 's/args?.url ||/(args?.url as string | undefined) ||/g' "$dir/src/index.ts" 2>/dev/null || true
    sed -i 's/args?.url as string/(args?.url as string | undefined)/g' "$dir/src/index.ts" 2>/dev/null || true
  fi
done
echo "✅ Types fixed"
