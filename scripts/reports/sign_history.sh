#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR=$(cd "$(dirname "$0")/../../" && pwd)
HISTORY_PATH="$ROOT_DIR/.trae/history.json"

if [[ -z "${BEAMLINE_HMAC_SECRET:-}" ]]; then
  echo "[FAIL] BEAMLINE_HMAC_SECRET is not set"
  exit 1
fi

if [[ ! -f "$HISTORY_PATH" ]]; then
  echo "[FAIL] $HISTORY_PATH not found"
  exit 1
fi

tmp_file=$(mktemp)
cp "$HISTORY_PATH" "$tmp_file"

len=$(jq 'length' "$tmp_file")
if [[ "$len" -eq 0 ]]; then
  echo "[FAIL] history.json is empty"
  rm -f "$tmp_file"
  exit 1
fi

last_index=$((len - 1))

ts=$(jq -r ".[$last_index].ts" "$tmp_file")
actor=$(jq -r ".[$last_index].actor" "$tmp_file")
action=$(jq -r ".[$last_index].action" "$tmp_file")
state_checksum=$(jq -r ".[$last_index].state_checksum" "$tmp_file")
hmac_prev=$(jq -r ".[$last_index].hmac_prev // \"\"" "$tmp_file")
stored_hmac=$(jq -r ".[$last_index].hmac // \"\"" "$tmp_file")

if [[ -z "$ts" || -z "$actor" || -z "$action" || -z "$state_checksum" ]]; then
  echo "[FAIL] Missing required fields in latest history entry"
  rm -f "$tmp_file"
  exit 1
fi

message="${ts}${actor}${action}${state_checksum}${hmac_prev}"
computed_hmac=$(printf "%s" "$message" | openssl dgst -sha256 -hmac "$BEAMLINE_HMAC_SECRET" | awk '{print $2}')

if [[ -z "$computed_hmac" ]]; then
  echo "[FAIL] Failed to compute HMAC"
  rm -f "$tmp_file"
  exit 1
fi

if [[ -n "$stored_hmac" ]]; then
  echo "[WARN] Existing hmac present; overwriting"
fi

jq ".[$last_index].hmac = \"$computed_hmac\"" "$tmp_file" > "$HISTORY_PATH"
rm -f "$tmp_file"

echo "[OK] Signed latest history entry at index $last_index"
echo "HMAC: $computed_hmac"
