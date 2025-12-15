#!/usr/bin/env bash
set -euo pipefail

ROOT="$(pwd -P)"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

cd "$TMP"
git init -q
mkdir -p .trae
cp "$ROOT/.trae/state.json" .trae/state.json || echo '{"artifact_checksums":[]}' > .trae/state.json
cp "$ROOT/.trae/history.json" .trae/history.json || echo '[]' > .trae/history.json

# Create a dummy artifact and put wrong checksum into state
echo "hello" > artifact.txt
python3 - << 'PY'
import json, hashlib
state=json.load(open('.trae/state.json'))
state.setdefault('artifact_checksums',[]).append({"path":"artifact.txt","hash":"deadbeef"})
json.dump(state, open('.trae/state.json','w'), indent=2)
PY

bash "$ROOT/scripts/devstate_auto_repair.sh" --repair=drift --non-interactive --dry-run

test -f reports/devstate-auto-repair/state.diff
echo "[OK] auto_repair_drift test passed"
