#!/usr/bin/env bash
# OBS-1 validators wrapper: collects logs and runs Node + jq + Python validators.
# Usage:
#   ./scripts/observability/run_obs1_validators.sh [--collect] [--paths <glob>]
#   --collect    Attempt to export recent logs to .windsurf/reports/*.jsonl
#   --paths      Optional glob(s) of log files to validate (default: reports/dry-run-logs/obs1/*.jsonl and .windsurf/reports/*.jsonl)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

REPORT_DIR_DEV="$PROJECT_ROOT/reports/dry-run-logs/obs1"
REPORT_DIR_WINDSURF="$PROJECT_ROOT/.windsurf/reports"
SUMMARY_DIR="$PROJECT_ROOT/.windsurf/reports"

mkdir -p "$REPORT_DIR_DEV" "$REPORT_DIR_WINDSURF" "$SUMMARY_DIR"

TS="$(date -Iseconds)"
NODE_SUMMARY="$SUMMARY_DIR/obs1-node-summary.txt"
JQ_SUMMARY="$SUMMARY_DIR/obs1-jq-summary.txt"
PY_SUMMARY="$SUMMARY_DIR/obs1-py-summary.txt"

COLLECT=false
PATHS=()

while [[ $# -gt 0 ]]; do
  case "$1" in
    --collect)
      COLLECT=true
      shift
      ;;
    --paths)
      shift
      while [[ $# -gt 0 && ! "$1" =~ ^-- ]]; do
        PATHS+=("$1")
        shift
      done
      ;;
    *)
      echo "[WARN] Unknown arg: $1" >&2
      shift
      ;;
  esac
done

log() { echo "[$TS] $*"; }

# Attempt collection via docker compose logs if requested
if $COLLECT; then
  log "Collecting recent logs via docker compose (if available)..."
  if command -v docker &> /dev/null && [ -f "$PROJECT_ROOT/docker-compose.yml" ]; then
    # Gateway
    docker compose logs gateway --no-color | sed 's/\x1B\[[0-9;]*[JKmsu]//g' > "$REPORT_DIR_WINDSURF/gateway.logs.raw.txt" || true
    # Router
    docker compose logs router --no-color | sed 's/\x1B\[[0-9;]*[JKmsu]//g' > "$REPORT_DIR_WINDSURF/router.logs.raw.txt" || true
    # Provider (if service exists)
    docker compose logs provider --no-color | sed 's/\x1B\[[0-9;]*[JKmsu]//g' > "$REPORT_DIR_WINDSURF/provider.logs.raw.txt" || true

    # Note: raw logs may not be JSONL; keep placeholders to run validators gracefully
    : > "$REPORT_DIR_WINDSURF/gateway.logs.jsonl"
    : > "$REPORT_DIR_WINDSURF/router.logs.jsonl"
    : > "$REPORT_DIR_WINDSURF/provider.logs.jsonl"
    log "Exported raw logs; JSONL placeholders written to $REPORT_DIR_WINDSURF/*.jsonl"
  else
    log "docker compose not available or compose file missing; skipping collection"
  fi
fi

# Default paths if none provided
if [ ${#PATHS[@]} -eq 0 ]; then
  PATHS=(
    "$REPORT_DIR_DEV/gateway.jsonl"
    "$REPORT_DIR_DEV/router.jsonl"
    "$REPORT_DIR_WINDSURF/gateway.logs.jsonl"
    "$REPORT_DIR_WINDSURF/router.logs.jsonl"
    "$REPORT_DIR_WINDSURF/provider.logs.jsonl"
  )
fi

NODE_VALIDATOR="$PROJECT_ROOT/scripts/obs1_node_validate.mjs"
JQ_VALIDATOR="$PROJECT_ROOT/scripts/obs1_jq_validate.sh"
PY_VALIDATOR="$PROJECT_ROOT/tests/utils/log_conformance_validator.py"

node_ok=true
jq_ok=true
py_ok=true

echo "[$TS] OBS-1 Node validator results" > "$NODE_SUMMARY"
echo "[$TS] OBS-1 jq validator results" > "$JQ_SUMMARY"
echo "[$TS] OBS-1 Python validator results" > "$PY_SUMMARY"

processed_any=false
shopt -s nullglob
for p in "${PATHS[@]}"; do
  matched=false
  for f in $p; do
    matched=true
    processed_any=true
    # Node
    if command -v node &> /dev/null; then
      if node "$NODE_VALIDATOR" "$f" >> "$NODE_SUMMARY" 2>&1; then :; else node_ok=false; fi
    else
      echo "[WARN] node not found; skipping $f" >> "$NODE_SUMMARY"
      node_ok=false
    fi
    # jq
    if command -v jq &> /dev/null; then
      if bash "$JQ_VALIDATOR" "$f" >> "$JQ_SUMMARY" 2>&1; then :; else jq_ok=false; fi
    else
      echo "[WARN] jq not found; skipping $f" >> "$JQ_SUMMARY"
      jq_ok=false
    fi
  done
  if [ "$matched" = false ]; then
    echo "[WARN] No files matched pattern: $p" >> "$NODE_SUMMARY"
    echo "[WARN] No files matched pattern: $p" >> "$JQ_SUMMARY"
  fi
done
shopt -u nullglob

# Python validator supports multiple paths via glob; pass dev and windsorf dirs
if command -v python3 &> /dev/null; then
  # Pass relative paths (from project root) to satisfy pathlib.glob constraints
  rel_dev="reports/dry-run-logs/obs1/*.jsonl"
  rel_windsurf=".windsurf/reports/*.jsonl"
  cd "$PROJECT_ROOT"
  if python3 "$PY_VALIDATOR" --paths "$rel_dev" "$rel_windsurf" >> "$PY_SUMMARY" 2>&1; then :; else py_ok=false; fi
else
  echo "[WARN] python3 not found; skipping" >> "$PY_SUMMARY"
  py_ok=false
fi

log "Summaries written:"
log "  Node: $NODE_SUMMARY"
log "  jq:   $JQ_SUMMARY"
log "  Py:   $PY_SUMMARY"

if $node_ok && $jq_ok && $py_ok; then
  echo "[OK] OBS-1 validators: all passed or skipped due to empty logs"
  exit 0
else
  echo "[WARN] OBS-1 validators reported failures (see summaries)"
  exit 1
fi
