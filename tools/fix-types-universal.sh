#!/bin/bash
# Universal type fixer for all MCP servers

fix_server() {
  local dir="$1"
  local file="$dir/src/index.ts"
  
  if [ ! -f "$file" ]; then
    return 0
  fi
  
  # Add CallToolResult import if not present
  if ! grep -q "CallToolResult" "$file"; then
    sed -i '/from "@modelcontextprotocol\/sdk\/types.js";/a\import type { CallToolResult } from "@modelcontextprotocol/sdk/types.js";' "$file"
  fi
  
  # Replace interface ToolResult with type alias
  if grep -q "interface ToolResult" "$file"; then
    sed -i '/^interface ToolResult {/,/^}$/c\type ToolResult = CallToolResult;' "$file"
  fi
  
  # Fix common type issues in args
  # Fix args?.secret, args?.cp, args?.version, etc.
  sed -i 's/args?.secret/args?.secret as string | undefined/g' "$file"
  sed -i 's/args?.cp/args?.cp as string | undefined/g' "$file"
  sed -i 's/args?.version/args?.version as string | undefined/g' "$file"
  sed -i 's/args?.tag_expr/args?.tag_expr as string | undefined/g' "$file"
  sed -i 's/args?.log_file/args?.log_file as string | undefined/g' "$file"
  sed -i 's/args?.target/args?.target as string | undefined/g' "$file"
  sed -i 's/args?.profile/args?.profile as string | undefined/g' "$file"
  sed -i 's/args?.mode/args?.mode as string | undefined/g' "$file"
  sed -i 's/args?.scenario/args?.scenario as string | undefined/g' "$file"
  sed -i 's/args?.directory/args?.directory as string | undefined/g' "$file"
  sed -i 's/args?.step/args?.step as string | undefined/g' "$file"
  sed -i 's/args?.output/args?.output as string | undefined/g' "$file"
  sed -i 's/args?.compare/args?.compare as string | undefined/g' "$file"
  
  # Fix arrays
  sed -i 's/args?.files || \[\]/(args?.files as string[] | undefined) || []/g' "$file"
  sed -i 's/args?.paths || \[\]/(args?.paths as string[] | undefined) || []/g' "$file"
  sed -i 's/args?.suites || \[\]/(args?.suites as string[] | undefined) || []/g' "$file"
  
  # Fix boolean
  sed -i 's/args?.verbose/args?.verbose as boolean | undefined/g' "$file"
  sed -i 's/args?.rebuild/args?.rebuild as boolean | undefined/g' "$file"
  sed -i 's/args?.backup/args?.backup as boolean | undefined/g' "$file"
  sed -i 's/args?.collect/args?.collect as boolean | undefined/g' "$file"
  sed -i 's/args?.baseline/args?.baseline as boolean | undefined/g' "$file"
}

for dir in tools/*/; do
  if [ -f "$dir/package.json" ] && [ "$dir" != "tools/windsurf-hooks/" ]; then
    fix_server "$dir"
  fi
done

echo "✅ Types fixed in all servers"
