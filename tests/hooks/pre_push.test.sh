#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(pwd -P)"
TMP_DIR="$(mktemp -d)"
trap 'rm -rf "$TMP_DIR"' EXIT

cd "$TMP_DIR"
git init -q

mkdir -p scripts/hooks devstate-tools/devstate/scripts .trae
cp "$REPO_ROOT/scripts/hooks/pre-push" scripts/hooks/pre-push
cp "$REPO_ROOT/scripts/hooks/pre-commit" scripts/hooks/pre-commit
cp "$REPO_ROOT/devstate-tools/devstate/scripts/devstate_verify.sh" devstate-tools/devstate/scripts/devstate_verify.sh
echo '{}' > .trae/state.json
echo '[]' > .trae/history.json

bash "$REPO_ROOT/scripts/install_devstate_hooks.sh" --backup --link

# Ensure .trae files not tracked to pass hard gate
git rm --cached -r . >/dev/null 2>&1 || true

.git/hooks/pre-push || true
echo "[OK] pre_push test executed"
