# Building All MCP Servers

## Quick Build

```bash
# Build all TypeScript servers
for dir in tools/*/; do
  if [ -f "$dir/package.json" ] && [ "$dir" != "tools/windsurf-hooks/" ]; then
    echo "Building $(basename $dir)..."
    cd "$dir"
    npm install --silent && npm run build
    cd ../..
  fi
done
```

## Individual Build

```bash
cd tools/devstate
npm install
npm run build
```

## Verification

After building, verify that `dist/index.js` exists:

```bash
find tools -name "dist/index.js" | wc -l
# Should output: 14 (all Node.js servers)
```

## Python Server

The `windsurf-hooks` server is Python and doesn't need compilation:

```bash
chmod +x tools/windsurf-hooks/server.py
```

## Next Steps

1. Build all servers: `bash tools/BUILD.md` (or run commands manually)
2. Restart Cursor IDE
3. MCP servers will be available in Cursor
