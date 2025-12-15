#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR=$(cd "$(dirname "$0")/../../" && pwd)
STATE_PATH="$ROOT_DIR/.trae/state.json"

if [[ ! -f "$STATE_PATH" ]]; then
  echo "[FAIL] $STATE_PATH not found"
  exit 1
fi

# Check if jq is available
if ! command -v jq &> /dev/null; then
  echo "[FAIL] jq is required but not installed"
  exit 1
fi

# Get list of files from command line arguments or read from artifact_checksums
if [[ $# -gt 0 ]]; then
  FILES=("$@")
else
  # Extract all paths from artifact_checksums
  mapfile -t FILES < <(jq -r '.artifact_checksums[].path' "$STATE_PATH")
fi

echo "[INFO] Recomputing checksums for ${#FILES[@]} file(s)..."

tmp_state=$(mktemp)
cp "$STATE_PATH" "$tmp_state"

updated_count=0
failed_count=0

for file_path in "${FILES[@]}"; do
  full_path="$ROOT_DIR/$file_path"
  
  if [[ ! -f "$full_path" ]]; then
    echo "[WARN] File not found: $file_path (skipping)"
    ((failed_count++))
    continue
  fi
  
  # Compute SHA256
  if [[ "$OSTYPE" == "darwin"* ]]; then
    new_hash=$(shasum -a 256 "$full_path" | awk '{print $1}')
  else
    new_hash=$(sha256sum "$full_path" | awk '{print $1}')
  fi
  
  # Update in state.json using temporary file
  tmp_new="${tmp_state}.new"
  jq --arg path "$file_path" --arg hash "$new_hash" \
    '(.artifact_checksums[] | select(.path == $path) | .hash) = $hash' \
    "$tmp_state" > "$tmp_new" || {
    echo "[FAIL] jq update failed for $file_path"
    rm -f "$tmp_new"
    ((failed_count++))
    continue
  }
  # Persist the update back to tmp_state for subsequent iterations
  mv "$tmp_new" "$tmp_state"
  
  echo "[OK] Updated $file_path"
  echo "     Hash: $new_hash"
  ((updated_count++))
done

# Move updated state back
mv "$tmp_state" "$STATE_PATH"

echo ""
echo "[SUMMARY]"
echo "  Updated: $updated_count"
echo "  Failed:  $failed_count"
echo "  Total:   ${#FILES[@]}"

if [[ $failed_count -gt 0 ]]; then
  echo ""
  echo "[WARN] Some files could not be updated"
  exit 1
fi

echo ""
echo "[OK] All checksums updated successfully"
