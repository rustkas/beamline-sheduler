#!/bin/bash
# Fix imports in all TypeScript servers

for dir in tools/*/; do
  if [ -f "$dir/src/index.ts" ]; then
    echo "Fixing imports in $(basename $dir)..."
    # Fix Server import
    sed -i 's|@modelcontextprotocol/sdk/server/mcp.js|@modelcontextprotocol/sdk/server/index.js|g' "$dir/src/index.ts"
    # Fix script-runner import if exists
    sed -i 's|from "../../shared/script-runner.js"|from "./script-runner.js"|g' "$dir/src/index.ts"
    # Fix script-runner import if exists (alternative path)
    sed -i 's|from "\.\.\/\.\.\/shared\/script-runner\.js"|from "./script-runner.js"|g' "$dir/src/index.ts"
  fi
done
echo "✅ All imports fixed"
