#!/usr/bin/env bash
set -euo pipefail

ROOT="$(pwd -P)"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

cd "$TMP"
git init -q
mkdir -p .trae
echo '[]' > .trae/history.json
echo '{}' > .trae/state.json

python3 - << 'PY'
import json
hist=[]
hist.append({"ts":"2025-01-01T00:00:00Z","actor":"dev","action":"init","state_checksum":"abc"})
hist.append({"ts":"2025-01-01T01:00:00Z","actor":"dev","action":"update","state_checksum":"def"})
open('.trae/history.json','w').write(json.dumps(hist))
PY

BEAMLINE_HMAC_SECRET=dev-secret-not-for-prod bash "$ROOT/scripts/devstate_auto_repair.sh" --repair=hmac --non-interactive --dry-run

test -f reports/devstate-auto-repair/history.diff
echo "[OK] auto_repair_hmac test passed"
