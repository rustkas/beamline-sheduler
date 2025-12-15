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

echo "hello" > artifact.txt
python3 - << 'PY'
import json
state=json.load(open('.trae/state.json'))
state.setdefault('artifact_checksums',[]).append({"path":"artifact.txt","hash":"deadbeef"})
json.dump(state, open('.trae/state.json','w'), indent=2)
PY

DEVSTATE_URL=http://localhost:3180 BEAMLINE_HMAC_SECRET=dev-secret-not-for-prod \
bash "$ROOT/scripts/devstate_auto_repair.sh" --repair=all --non-interactive --api-url=http://localhost:3180 || true

test -f reports/devstate-auto-repair/run.log
echo "[OK] auto_repair_api_import test executed"
