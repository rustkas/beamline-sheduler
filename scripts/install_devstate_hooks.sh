#!/usr/bin/env bash
set -euo pipefail

PROJECT_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
cd "$PROJECT_ROOT"

HOOKS_DIR=".git/hooks"
SRC_PRE_COMMIT="scripts/hooks/pre-commit"
SRC_PRE_PUSH="scripts/hooks/pre-push"

USE_LINK=false
FORCE=false
BACKUP=false

for arg in "$@"; do
  case "$arg" in
    --link) USE_LINK=true ;;
    --force) FORCE=true ;;
    --backup) BACKUP=true ;;
  esac
done

if ! git rev-parse --git-dir >/dev/null 2>&1; then
  echo "[FAIL] Not a git repository" >&2
  exit 1
fi

if [ ! -f "$SRC_PRE_COMMIT" ] || [ ! -f "$SRC_PRE_PUSH" ]; then
  echo "[FAIL] Hook sources missing in scripts/hooks" >&2
  exit 1
fi

if [ ! -d ".trae" ] || [ ! -f ".trae/state.json" ] || [ ! -f ".trae/history.json" ]; then
  echo "[WARN] DevState files not found (.trae/state.json, .trae/history.json)"
fi

mkdir -p "$HOOKS_DIR"

install_hook() {
  name="$1"
  src="$2"
  dst="$HOOKS_DIR/$1"
  if [ -f "$dst" ] || [ -L "$dst" ]; then
    if [ "$BACKUP" = true ]; then
      ts="$(date -u +%Y%m%dT%H%M%SZ)"
      cp -f "$dst" "$dst.bak.$ts" || true
      echo "[INFO] Backed up $name to $name.bak.$ts"
    fi
    if [ "$FORCE" = true ]; then
      rm -f "$dst"
    else
      echo "[WARN] $name already exists; use --force to overwrite"
    fi
  fi
  if [ "$USE_LINK" = true ]; then
    rel="$(realpath --relative-to="$HOOKS_DIR" "$src" 2>/dev/null || true)"
    if [ -z "$rel" ]; then
      # Fallback if realpath --relative-to is unavailable
      rel="$(python3 - <<'PY'
import os,sys
hooks=os.environ.get('HOOKS_DIR')
src=os.environ.get('SRC_PATH')
print(os.path.relpath(src, hooks))
PY
      )"
    fi
    SRC_PATH="$src" HOOKS_DIR="$HOOKS_DIR" ln -sf "$rel" "$dst"
  else
    cp "$src" "$dst"
  fi
  chmod +x "$dst"
  echo "[OK] Installed $name"
}

install_hook pre-commit "$SRC_PRE_COMMIT"
install_hook pre-push "$SRC_PRE_PUSH"

echo "[OK] DevState hooks installation complete"
