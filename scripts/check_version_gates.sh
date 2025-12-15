#!/bin/bash
# Check version gates before release
# Usage: bash scripts/check_version_gates.sh [version]

set -euo pipefail

VERSION="${1:-}"
if [ -n "$VERSION" ]; then
    VERSION_NUMBER="${VERSION#v}"
else
    # Try to extract version from state.json
    if [ -f ".trae/state.json" ] && command -v jq >/dev/null 2>&1; then
        VERSION_NUMBER=$(jq -r '.version // "unknown"' .trae/state.json 2>/dev/null || echo "unknown")
    else
        VERSION_NUMBER="unknown"
    fi
fi

ERRORS=0
WARNINGS=0
VERSION_MISMATCH=false

echo "[INFO] Checking version gates..."

# Gate 1: Schema version consistency
echo "[GATE 1] Schema version consistency..."
if bash scripts/check_schema_changes.sh >/dev/null 2>&1; then
    echo "  [PASS] Schema versions are consistent"
else
    echo "  [FAIL] Schema versions are inconsistent"
    echo "         Run: bash scripts/check_schema_changes.sh"
    ERRORS=$((ERRORS + 1))
    VERSION_MISMATCH=true
fi

# Gate 2: State validation
echo "[GATE 2] State validation..."
if bash scripts/validate_state.sh >/dev/null 2>&1; then
    echo "  [PASS] State validation passed"
else
    echo "  [FAIL] State validation failed"
    echo "         Run: bash scripts/validate_state.sh"
    ERRORS=$((ERRORS + 1))
fi

# Gate 3: HMAC chain integrity
echo "[GATE 3] HMAC chain integrity..."
if python3 scripts/verify_hmac_chain.py >/dev/null 2>&1; then
    echo "  [PASS] HMAC chain is valid"
else
    echo "  [FAIL] HMAC chain verification failed"
    echo "         Run: python3 scripts/verify_hmac_chain.py"
    ERRORS=$((ERRORS + 1))
fi

# Gate 4: Documentation (HMAC masking)
echo "[GATE 4] Documentation (HMAC masking)..."
if bash scripts/check_hmac_masking.sh docs/ >/dev/null 2>&1; then
    echo "  [PASS] HMAC masking check passed"
else
    echo "  [WARN] HMAC masking check failed (non-blocking)"
    echo "         Run: bash scripts/check_hmac_masking.sh docs/"
    WARNINGS=$((WARNINGS + 1))
fi

# Gate 5: Version in CHANGELOG
if [ "$VERSION_NUMBER" != "unknown" ]; then
    echo "[GATE 5] Version in CHANGELOG.md..."
    if grep -q "\[$VERSION_NUMBER\]" CHANGELOG.md 2>/dev/null; then
        echo "  [PASS] Version $VERSION_NUMBER found in CHANGELOG.md"
    else
        echo "  [WARN] Version $VERSION_NUMBER not found in CHANGELOG.md"
        echo "         Add entry to CHANGELOG.md before release"
        WARNINGS=$((WARNINGS + 1))
    fi
fi

# Gate 6: no_drift flag
echo "[GATE 6] no_drift flag..."
if [ -f ".trae/state.json" ] && command -v jq >/dev/null 2>&1; then
    NO_DRIFT=$(jq -r '.no_drift // false' .trae/state.json 2>/dev/null || echo "false")
    if [ "$NO_DRIFT" = "true" ]; then
        echo "  [PASS] no_drift flag is set"
    else
        echo "  [FAIL] no_drift flag is not set"
        echo "         Set no_drift: true in .trae/state.json"
        ERRORS=$((ERRORS + 1))
    fi
else
    echo "  [WARN] Could not check no_drift flag (jq not available)"
    WARNINGS=$((WARNINGS + 1))
fi

# Gate 7: Artifact checksums
echo "[GATE 7] Artifact checksums..."
if [ -f ".trae/state.json" ] && command -v jq >/dev/null 2>&1; then
    CHECKSUM_COUNT=$(jq '[.artifact_checksums[]?] | length' .trae/state.json 2>/dev/null || echo "0")
    if [ "$CHECKSUM_COUNT" -gt 0 ]; then
        echo "  [PASS] Found $CHECKSUM_COUNT artifact checksums"
    else
        echo "  [WARN] No artifact checksums found"
        WARNINGS=$((WARNINGS + 1))
    fi
else
    echo "  [WARN] Could not check artifact checksums (jq not available)"
    WARNINGS=$((WARNINGS + 1))
fi

# Summary
echo ""
echo "[SUMMARY] Version gates check complete"
echo "  Errors: $ERRORS"
echo "  Warnings: $WARNINGS"
echo ""

if [ $ERRORS -gt 0 ]; then
    echo "  [BLOCKED] Release is blocked due to $ERRORS error(s)"
    # Return specific exit codes for version errors
    if [ "${VERSION_MISMATCH:-false}" = "true" ]; then
        exit 102  # Version inconsistency
    else
        exit 101  # Invalid SemVer or other version error
    fi
elif [ $WARNINGS -gt 0 ]; then
    echo "  [WARNING] Release has $WARNINGS warning(s) but is not blocked"
    exit 0
else
    echo "  [PASS] All gates passed"
    exit 0
fi

