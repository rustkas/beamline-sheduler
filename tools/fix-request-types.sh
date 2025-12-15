#!/bin/bash
# Fix request parameter types in all servers

for dir in tools/*/; do
  if [ -f "$dir/src/index.ts" ] && [ "$dir" != "tools/windsurf-hooks/" ]; then
    # Fix request parameter type
    sed -i 's|async (request) =>|async (request: any) =>|g' "$dir/src/index.ts"
  fi
done

echo "✅ Request types fixed"
