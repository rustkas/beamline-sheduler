#!/usr/bin/env bash
set -euo pipefail

TMP_DIR="$(mktemp -d)"
trap 'rm -rf "$TMP_DIR"' EXIT

cd "$TMP_DIR"
git init -q
mkdir -p scripts/hooks .trae
cp -r "${OLDPWD}/scripts/hooks"/* scripts/hooks/
cp "${OLDPWD}/.trae/state.json" .trae/state.json || echo '{}' > .trae/state.json
cp "${OLDPWD}/.trae/history.json" .trae/history.json || echo '[]' > .trae/history.json
cp "${OLDPWD}/scripts/install_devstate_hooks.sh" scripts/install_devstate_hooks.sh

bash scripts/install_devstate_hooks.sh --backup

test -x .git/hooks/pre-commit
test -x .git/hooks/pre-push
echo "[OK] install_hooks test passed"
