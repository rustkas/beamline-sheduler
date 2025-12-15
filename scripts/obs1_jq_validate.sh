#!/usr/bin/env bash
# OBS-1 JSON log conformance validator (jq-based)
# Usage: ./scripts/obs1_jq_validate.sh <log_file>
# Exits non-zero if validation fails.

set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: $0 <log_file>" >&2
  exit 2
fi

LOG_FILE="$1"
if [ ! -f "$LOG_FILE" ]; then
  echo "[ERROR] File not found: $LOG_FILE" >&2
  exit 2
fi

# Required keys and simple schema checks
REQUIRED_KEYS='["timestamp","level","msg","trace_id","tenant_id"]'

TOTAL=0
OK=0
FAIL=0

while IFS= read -r line || [ -n "$line" ]; do
  TOTAL=$((TOTAL+1))
  # Skip empty lines
  if [ -z "$line" ]; then
    continue
  fi

  # Validate JSON and required keys
  if ! echo "$line" | jq -e . >/dev/null 2>&1; then
    echo "[FAIL] Line $TOTAL: not valid JSON"
    FAIL=$((FAIL+1))
    continue
  fi

  MISSING=$(echo "$line" | jq --argjson req "$REQUIRED_KEYS" -r '
    . as $o | [ $req[] as $k | select( ($o | has($k)) | not ) ] | join(",")
  ')

  if [ -n "$MISSING" ]; then
    echo "[FAIL] Line $TOTAL: missing keys: $MISSING"
    FAIL=$((FAIL+1))
    continue
  fi

  # Basic type checks
  if ! echo "$line" | jq -e '(
      ( .timestamp | type == "string" ) and
      ( .level | type == "string" ) and
      ( .msg | type == "string" ) and
      ( .trace_id | type == "string" ) and
      ( .tenant_id | type == "string" )
    )' >/dev/null; then
    echo "[FAIL] Line $TOTAL: type mismatch for required keys"
    FAIL=$((FAIL+1))
    continue
  fi

  # Secrets/PII masking check: if present, must be masked
  if echo "$line" | jq -e '
    [ .api_key?, .token?, .secret?, .password?, .access_token?, .refresh_token?, .authorization? ]
    | map(select(. != null))
    | any(. != "[REDACTED]" and . != "[MASKED]")
  ' >/dev/null; then
    echo "[FAIL] Line $TOTAL: sensitive field present and not masked"
    FAIL=$((FAIL+1))
    continue
  fi

  OK=$((OK+1))

done < "$LOG_FILE"

if [ $FAIL -eq 0 ]; then
  echo "[OK] OBS-1 conformance passed ($OK/$TOTAL)"
  exit 0
else
  echo "[FAIL] OBS-1 conformance failed ($FAIL failures out of $TOTAL lines)"
  exit 1
fi
