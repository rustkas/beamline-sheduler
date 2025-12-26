#!/usr/bin/env bash
set -euo pipefail

# Fast secret leak detector for Beamline umbrella repo.
# Default: scan staged files only.
#
# Goals:
# - High signal on real leaks (private keys, GitHub tokens, AWS keys, OpenAI sk_live_).
# - Avoid false positives for:
#   - documentation about env vars / secrets
#   - pattern registries / regex strings
#   - sample/fixture data
#   - default dev placeholder secrets (beamline-secret-key-v1, dev-secret-not-for-prod, etc.)

ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"

MODE="staged"
if [[ "${1:-}" == "--all" ]]; then
  MODE="all"
elif [[ "${1:-}" == "--staged" || "${1:-}" == "" ]]; then
  MODE="staged"
else
  echo "Usage: $0 [--staged|--all]" >&2
  exit 2
fi

echo "=========================================="
echo "Secret Leak Detection"
echo "=========================================="
echo "[INFO] Repo root: ${ROOT}"
echo "[INFO] Mode: ${MODE}"
echo

cd "$ROOT"

if ! command -v rg >/dev/null 2>&1; then
  echo "[FAIL] ripgrep (rg) is required" >&2
  exit 1
fi

# -------- file list --------

get_files_staged() {
  git diff --cached --name-only --diff-filter=ACMR || true
}

get_files_all() {
  git ls-files || true
}

files="$(
  if [[ "$MODE" == "all" ]]; then
    get_files_all
  else
    get_files_staged
  fi
)"

# Nothing to scan
if [[ -z "${files//$'\n'/}" ]]; then
  echo "[OK] No files to scan"
  exit 0
fi

# -------- exclusions --------
# These paths are intentionally noisy (fixtures, samples, deprecated, vendored).
# We still scan them for *high confidence* leaks.
is_noisy_path() {
  local f="$1"
  case "$f" in
    */node_modules/*|*/deps/*|*/_build/*|*/.git/*) return 0 ;;
    deprecated-solutions/*|fixtures/*|tests/fixtures/*|data/*.sample.json|docs/*|reports/*) return 0 ;;
    *) return 1 ;;
  esac
}

# -------- patterns --------
# High confidence patterns (always checked).
HIGH_PATTERNS=(
  '-----BEGIN (RSA|DSA|EC|OPENSSH) PRIVATE KEY-----'
  'ghp_[A-Za-z0-9]{36,}'
  'AKIA[0-9A-Z]{16}'
  'ASIA[0-9A-Z]{16}'
  'sk_live_[A-Za-z0-9]{32,}'
)

# Medium confidence patterns (checked only on non-noisy paths).
# NOTE: keep these strict to avoid false positives.
MED_PATTERNS=(
  # env assignment with a long-ish value (looks like a real secret, not a regex)
  '(^|[^A-Za-z0-9_])BEAMLINE_HMAC_SECRET\s*[:=]\s*["'"'"']?[^"'"'"'\s]{16,}["'"'"']?'
  # common "secret = value" style (exclude placeholders in filter below)
  '(^|[^A-Za-z0-9_])secret\s*[:=]\s*["'"'"']?[^"'"'"'\s]{12,}["'"'"']?'
  '(^|[^A-Za-z0-9_])token\s*[:=]\s*["'"'"']?[A-Za-z0-9_\-\.]{20,}["'"'"']?'
  '(^|[^A-Za-z0-9_])api[_-]?key\s*[:=]\s*["'"'"']?[^"'"'"'\s]{16,}["'"'"']?'
)

# Lines containing any of these are treated as "documentation/pattern" context and ignored
# for MED patterns (high patterns still apply).
IGNORE_LINE_RE='from_secret|secretKeyRef|env_variable|pattern|regex|\[a-zA-Z0-9_\-\]\{|\(\?|\^\(|\$\)|\.\*|\{16,|\{32,|masked|example|placeholder|REPLACE_WITH_|YOUR_|XXX|TODO|FIXME'

# Values that are explicitly allowed as dev placeholders (should not fail the hook).
ALLOW_VALUE_RE='beamline-secret-key-v1|dev-secret-not-for-prod|default-dev-secret-do-not-use-in-production|REPLACE_WITH_PRODUCTION_SECRET'

fail=0

scan_file_for_patterns() {
  local f="$1"
  local noisy=0
  if is_noisy_path "$f"; then noisy=1; fi

  # Build rg command with file scoping; use -- for safety.
  # High patterns: always
  for p in "${HIGH_PATTERNS[@]}"; do
    if rg -n --no-heading --fixed-strings -e "" >/dev/null 2>&1; then :; fi
    if rg -n --no-heading -e "$p" -- "$f" >/dev/null 2>&1; then
      echo "[FAIL] Potential secret leak in ${f}"
      rg -n --no-heading -e "$p" -- "$f" | head -n 5 | sed 's/^/  /'
      echo
      fail=1
      return
    fi
  done

  # Medium patterns: skip noisy paths
  if [[ "$noisy" -eq 1 ]]; then
    return
  fi

  # For MED patterns, we filter out known-safe contexts and placeholders.
  local matches
  for p in "${MED_PATTERNS[@]}"; do
    # Collect matches
    matches="$(rg -n --no-heading -e "$p" -- "$f" || true)"
    if [[ -z "$matches" ]]; then
      continue
    fi

    # Filter out obvious safe lines (docs/patterns/placeholders)
    matches="$(printf '%s\n' "$matches" | rg -v -e "$IGNORE_LINE_RE" || true)"
    if [[ -z "$matches" ]]; then
      continue
    fi

    # Filter out allowed placeholder values if present on the line
    matches="$(printf '%s\n' "$matches" | rg -v -e "$ALLOW_VALUE_RE" || true)"
    if [[ -z "$matches" ]]; then
      continue
    fi

    echo "[FAIL] Potential secret leak in ${f}"
    printf '%s\n' "$matches" | head -n 5 | sed 's/^/  /'
    echo
    fail=1
    return
  done
}

while IFS= read -r f; do
  [[ -z "$f" ]] && continue
  # Only scan regular files tracked in index (submodule entries are directories in status).
  if [[ -f "$f" ]]; then
    scan_file_for_patterns "$f"
  fi
done <<< "$files"

if [[ "$fail" -ne 0 ]]; then
  echo "[FAIL] Potential secret leaks detected."
  exit 1
fi

echo "[OK] No secret leaks detected."
