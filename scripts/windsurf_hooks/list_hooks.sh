#!/usr/bin/env bash
# List Windsurf hooks from .windsurf/hooks.json

if [ -f ".windsurf/hooks.json" ]; then
  cat .windsurf/hooks.json | python3 -m json.tool 2>/dev/null || cat .windsurf/hooks.json
else
  echo "Error: .windsurf/hooks.json not found"
  exit 1
fi
