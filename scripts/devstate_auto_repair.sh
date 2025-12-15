#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage: devstate_auto_repair.sh [options]
  --repair {drift|hmac|schema|all}
  --dry-run
  --interactive | --non-interactive
  --api-url <URL>
  --limit <N>
  --backup-dir <DIR>
  --force
EOF
}

REPAIR="all"
DRY_RUN=false
INTERACTIVE=true
API_URL="${DEVSTATE_URL:-http://localhost:3180}"
LIMIT="0"
BACKUP_DIR=""
FORCE=false

for arg in "$@"; do
  case "$arg" in
    --repair=*) REPAIR="${arg#*=}" ;;
    --dry-run) DRY_RUN=true ;;
    --interactive) INTERACTIVE=true ;;
    --non-interactive) INTERACTIVE=false ;;
    --api-url=*) API_URL="${arg#*=}" ;;
    --limit=*) LIMIT="${arg#*=}" ;;
    --backup-dir=*) BACKUP_DIR="${arg#*=}" ;;
    --force) FORCE=true ;;
    -h|--help) usage; exit 0 ;;
    *) ;; 
  esac
done

PROJECT_ROOT="$(pwd -P)"
cd "$PROJECT_ROOT"

mkdir -p reports/devstate-auto-repair
REPORT_DIR="reports/devstate-auto-repair"

if [ ! -f ".trae/state.json" ] || [ ! -f ".trae/history.json" ]; then
  echo "[FAIL] Missing .trae/state.json or .trae/history.json" | tee -a "$REPORT_DIR/run.log" >&2
  exit 1
fi

SECRET="${BEAMLINE_HMAC_SECRET:-dev-secret-not-for-prod}"

TS="$(date -u +%Y%m%d_%H%M%S)"
if [ -z "$BACKUP_DIR" ]; then
  BACKUP_DIR="/tmp/devstate-backups-$TS"
fi
mkdir -p "$BACKUP_DIR"
cp -a .trae/state.json "$BACKUP_DIR/state.json"
cp -a .trae/history.json "$BACKUP_DIR/history.json"
echo "[OK] Backup created at $BACKUP_DIR" | tee -a "$REPORT_DIR/run.log"

TMP_DIR="$(mktemp -d)"
trap 'rm -rf "$TMP_DIR"' EXIT
cp -a .trae/state.json "$TMP_DIR/state.json"
cp -a .trae/history.json "$TMP_DIR/history.json"

repair_drift() {
  python3 - "$TMP_DIR/state.json" << 'PY'
import sys, json, hashlib, os
path=sys.argv[1]
state=json.load(open(path))
changed=False
for a in state.get('artifact_checksums', []):
    p=a.get('path','')
    if not p:
        continue
    if not os.path.isfile(p):
        continue
    h=hashlib.sha256(open(p,'rb').read()).hexdigest()
    if a.get('hash')!=h:
        a['hash']=h
        changed=True
if changed:
    tmp=path+".updated"
    json.dump(state, open(tmp,'w'), ensure_ascii=False, indent=2)
    print(tmp)
else:
    print("")
PY
}

repair_hmac() {
  python3 - "$TMP_DIR/history.json" "$SECRET" << 'PY'
import sys, json, hmac, hashlib
hpath=sys.argv[1]
secret=sys.argv[2]
hist=json.load(open(hpath))
entries = hist if isinstance(hist,list) else hist.get('entries', [])
if not isinstance(entries,list):
    print("")
    sys.exit(0)
prev_h=None
for i,e in enumerate(entries):
    ts=e.get('ts','')
    actor=e.get('actor','')
    action=e.get('action','')
    state_checksum=e.get('state_checksum','')
    hprev = '' if i==0 else prev_h or ''
    data=f"{ts}{actor}{action}{state_checksum}{hprev}"
    digest=hmac.new(secret.encode(), data.encode(), hashlib.sha256).hexdigest()
    e['hmac_prev']=hprev
    e['hmac']=digest
    prev_h=digest
out=hpath+".updated"
if isinstance(hist,list):
    json.dump(entries, open(out,'w'), ensure_ascii=False, indent=2)
else:
    hist['entries']=entries
    json.dump(hist, open(out,'w'), ensure_ascii=False, indent=2)
print(out)
PY
}

UPDATED_STATE=""
UPDATED_HISTORY=""

if [ "$REPAIR" = "drift" ] || [ "$REPAIR" = "all" ]; then
  UPDATED_STATE="$(repair_drift || true)"
fi
if [ "$REPAIR" = "hmac" ] || [ "$REPAIR" = "all" ]; then
  UPDATED_HISTORY="$(repair_hmac || true)"
fi

apply_file() {
  src="$1"; dst="$2"; name="$3"
  if [ -n "$src" ] && [ -f "$src" ]; then
    if [ "$DRY_RUN" = true ]; then
      echo "[DIFF] $name" | tee -a "$REPORT_DIR/run.log"
      diff -u "$dst" "$src" | tee -a "$REPORT_DIR/${name}.diff" || true
    else
      if [ "$INTERACTIVE" = true ]; then
        echo "[DIFF] $name" | tee -a "$REPORT_DIR/run.log"
        diff -u "$dst" "$src" | tee -a "$REPORT_DIR/${name}.diff" || true
        read -r -p "Apply $name changes? [y/N] " ans || ans="n"
        case "$ans" in
          y|Y) ;;
          *) echo "[SKIPPED] $name" | tee -a "$REPORT_DIR/run.log"; return 0 ;;
        esac
      fi
      mv "$src" "$dst"
      echo "[APPLIED] $name" | tee -a "$REPORT_DIR/run.log"
    fi
  fi
}

API_AVAILABLE=false
if curl -sf --connect-timeout 2 --max-time 5 "$API_URL/health" >/dev/null 2>&1 || \
   curl -sf --connect-timeout 2 --max-time 5 "$API_URL/v1/devstate/verify?limit=$LIMIT" >/dev/null 2>&1; then
  API_AVAILABLE=true
fi

if [ "$DRY_RUN" = true ]; then
  apply_file "$UPDATED_STATE" .trae/state.json state
  apply_file "$UPDATED_HISTORY" .trae/history.json history
else
  if [ "$API_AVAILABLE" = true ]; then
    TMP_PAYLOAD="$TMP_DIR/import.json"
    python3 - "$TMP_DIR" << 'PY'
import json,sys,os
tmp=sys.argv[1]
def load(path):
    try:
        return json.load(open(path))
    except Exception:
        return None
state=load(os.path.join(tmp,'state.json.updated')) or load('.trae/state.json')
history=load(os.path.join(tmp,'history.json.updated')) or load('.trae/history.json')
payload={'state':state,'history':history}
json.dump(payload, open(os.path.join(tmp,'import.json'),'w'), ensure_ascii=False)
print(os.path.join(tmp,'import.json'))
PY
    if [ -f "$TMP_PAYLOAD" ]; then
      if [ "$INTERACTIVE" = true ]; then
        echo "[INFO] Showing state diff (if any)"
        [ -n "$UPDATED_STATE" ] && diff -u .trae/state.json "$UPDATED_STATE" | tee -a "$REPORT_DIR/state.diff" || true
        echo "[INFO] Showing history diff (if any)"
        [ -n "$UPDATED_HISTORY" ] && diff -u .trae/history.json "$UPDATED_HISTORY" | tee -a "$REPORT_DIR/history.diff" || true
        read -r -p "Apply via DevState API import? [y/N] " ans || ans="n"
        case "$ans" in y|Y) ;; *) echo "[SKIPPED] API import"; TMP_PAYLOAD="" ;; esac
      fi
      if [ -n "$TMP_PAYLOAD" ]; then
        if curl -sf --connect-timeout 3 --max-time 10 -X POST -H 'Content-Type: application/json' \
          --data-binary @"$TMP_PAYLOAD" "$API_URL/v1/devstate/import" >/dev/null 2>&1; then
          echo "[APPLIED] via DevState API import" | tee -a "$REPORT_DIR/run.log"
        else
          echo "[WARN] DevState API import failed, falling back to local apply" | tee -a "$REPORT_DIR/run.log"
          apply_file "$UPDATED_STATE" .trae/state.json state
          apply_file "$UPDATED_HISTORY" .trae/history.json history
        fi
      else
        apply_file "$UPDATED_STATE" .trae/state.json state
        apply_file "$UPDATED_HISTORY" .trae/history.json history
      fi
    else
      apply_file "$UPDATED_STATE" .trae/state.json state
      apply_file "$UPDATED_HISTORY" .trae/history.json history
    fi
  else
    apply_file "$UPDATED_STATE" .trae/state.json state
    apply_file "$UPDATED_HISTORY" .trae/history.json history
  fi
fi

EXIT_CODE=0
if [ -f "devstate-tools/devstate/scripts/devstate_verify.sh" ]; then
  if ! bash devstate-tools/devstate/scripts/devstate_verify.sh; then
    EXIT_CODE=1
  fi
fi
if [ -f "scripts/validate_state.sh" ]; then
  if ! bash scripts/validate_state.sh; then
    EXIT_CODE=1
  fi
fi

exit "$EXIT_CODE"
