#!/bin/bash
# Build all MCP servers

set -e

SUCCESS=0
FAILED=0
FAILED_SERVERS=()

echo "=========================================="
echo "Building All MCP Servers"
echo "=========================================="
echo ""

for dir in tools/*/; do
  if [ -f "$dir/package.json" ] && [ "$dir" != "tools/windsurf-hooks/" ]; then
    SERVER_NAME=$(basename "$dir")
    echo "[$SERVER_NAME] Installing dependencies..."
    cd "$dir"
    if npm install --silent > /dev/null 2>&1; then
      echo "[$SERVER_NAME] Building..."
      if npm run build > /tmp/build_${SERVER_NAME}.log 2>&1; then
        if [ -f "dist/index.js" ]; then
          echo "[$SERVER_NAME] ✅ Built successfully"
          ((SUCCESS++))
        else
          echo "[$SERVER_NAME] ❌ Build failed: dist/index.js not found"
          ((FAILED++))
          FAILED_SERVERS+=("$SERVER_NAME")
        fi
      else
        echo "[$SERVER_NAME] ❌ Build failed (check /tmp/build_${SERVER_NAME}.log)"
        ((FAILED++))
        FAILED_SERVERS+=("$SERVER_NAME")
      fi
    else
      echo "[$SERVER_NAME] ❌ npm install failed"
      ((FAILED++))
      FAILED_SERVERS+=("$SERVER_NAME")
    fi
    cd ../..
  fi
done

echo ""
echo "=========================================="
echo "Build Summary"
echo "=========================================="
echo "✅ Success: $SUCCESS"
echo "❌ Failed: $FAILED"

if [ $FAILED -gt 0 ]; then
  echo ""
  echo "Failed servers:"
  for server in "${FAILED_SERVERS[@]}"; do
    echo "  - $server"
  done
  exit 1
fi

echo ""
echo "✅ All servers built successfully!"
