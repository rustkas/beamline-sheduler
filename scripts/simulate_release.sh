#!/bin/bash
# Simulate release process without creating actual release
# Usage: bash scripts/simulate_release.sh v1.0.0

set -euo pipefail

VERSION="${1:-}"
if [ -z "$VERSION" ]; then
    echo "Usage: $0 <version>"
    echo "Example: $0 v1.0.0"
    exit 1
fi

# Remove 'v' prefix if present
VERSION_NUMBER="${VERSION#v}"

REPORT_DIR="reports"
REPORT_FILE="$REPORT_DIR/release_simulation_${VERSION}.json"

mkdir -p "$REPORT_DIR"

echo "[INFO] Simulating release $VERSION..."

# Initialize report
cat > "$REPORT_FILE" <<EOF
{
  "version": "$VERSION",
  "simulation_date": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "checks": {
    "schema_consistency": { "status": "pending", "errors": [] },
    "state_validation": { "status": "pending", "errors": [] },
    "hmac_chain": { "status": "pending", "errors": [] },
    "documentation": { "status": "pending", "errors": [] },
    "version_gates": { "status": "pending", "errors": [] }
  },
  "summary": {
    "passed": 0,
    "failed": 0,
    "warnings": 0
  }
}
EOF

# Function to update report
update_report() {
    local check_name=$1
    local status=$2
    local error_msg="${3:-}"
    
    if command -v jq >/dev/null 2>&1; then
        if [ -n "$error_msg" ]; then
            jq ".checks.$check_name.status = \"$status\" | .checks.$check_name.errors += [\"$error_msg\"]" "$REPORT_FILE" > "${REPORT_FILE}.tmp" && mv "${REPORT_FILE}.tmp" "$REPORT_FILE"
        else
            jq ".checks.$check_name.status = \"$status\"" "$REPORT_FILE" > "${REPORT_FILE}.tmp" && mv "${REPORT_FILE}.tmp" "$REPORT_FILE"
        fi
    fi
}

# Check 1: Schema consistency
echo "[CHECK] Schema version consistency..."
if bash scripts/check_schema_changes.sh >/dev/null 2>&1; then
    echo "  [OK] Schema versions are consistent"
    update_report "schema_consistency" "passed"
else
    ERROR_MSG="Schema versions are inconsistent. Run: bash scripts/check_schema_changes.sh"
    echo "  [FAIL] $ERROR_MSG"
    update_report "schema_consistency" "failed" "$ERROR_MSG"
fi

# Check 2: State validation
echo "[CHECK] State validation..."
if bash scripts/validate_state.sh >/dev/null 2>&1; then
    echo "  [OK] State validation passed"
    update_report "state_validation" "passed"
else
    ERROR_MSG="State validation failed. Run: bash scripts/validate_state.sh"
    echo "  [FAIL] $ERROR_MSG"
    update_report "state_validation" "failed" "$ERROR_MSG"
fi

# Check 3: HMAC chain
echo "[CHECK] HMAC chain integrity..."
if python3 scripts/verify_hmac_chain.py >/dev/null 2>&1; then
    echo "  [OK] HMAC chain is valid"
    update_report "hmac_chain" "passed"
else
    ERROR_MSG="HMAC chain verification failed. Run: python3 scripts/verify_hmac_chain.py"
    echo "  [FAIL] $ERROR_MSG"
    update_report "hmac_chain" "failed" "$ERROR_MSG"
fi

# Check 4: Documentation
echo "[CHECK] Documentation (HMAC masking)..."
if bash scripts/check_hmac_masking.sh docs/ >/dev/null 2>&1; then
    echo "  [OK] Documentation checks passed"
    update_report "documentation" "passed"
else
    ERROR_MSG="HMAC masking check failed. Run: bash scripts/check_hmac_masking.sh docs/"
    echo "  [WARN] $ERROR_MSG"
    update_report "documentation" "warning" "$ERROR_MSG"
fi

# Check 5: Version gates
echo "[CHECK] Version gates..."
GATE_ERRORS=0

# Check if version is in CHANGELOG
if grep -q "\[$VERSION_NUMBER\]" CHANGELOG.md 2>/dev/null; then
    echo "  [OK] Version found in CHANGELOG.md"
else
    echo "  [WARN] Version not found in CHANGELOG.md"
    update_report "version_gates" "warning" "Version $VERSION_NUMBER not found in CHANGELOG.md"
    GATE_ERRORS=$((GATE_ERRORS + 1))
fi

# Check if manifest has correct version
if [ -f ".trae/manifest.json" ]; then
    MANIFEST_VERSION=$(jq -r '.version // "unknown"' .trae/manifest.json 2>/dev/null || echo "unknown")
    if [ "$MANIFEST_VERSION" != "unknown" ]; then
        echo "  [OK] Manifest version: $MANIFEST_VERSION"
    else
        echo "  [WARN] Could not read manifest version"
    fi
fi

if [ $GATE_ERRORS -eq 0 ]; then
    update_report "version_gates" "passed"
else
    update_report "version_gates" "warning" "$GATE_ERRORS warning(s)"
fi

# Calculate summary
if command -v jq >/dev/null 2>&1; then
    PASSED=$(jq '[.checks[] | select(.status == "passed")] | length' "$REPORT_FILE")
    FAILED=$(jq '[.checks[] | select(.status == "failed")] | length' "$REPORT_FILE")
    WARNINGS=$(jq '[.checks[] | select(.status == "warning")] | length' "$REPORT_FILE")
    
    jq ".summary.passed = $PASSED | .summary.failed = $FAILED | .summary.warnings = $WARNINGS" "$REPORT_FILE" > "${REPORT_FILE}.tmp" && mv "${REPORT_FILE}.tmp" "$REPORT_FILE"
fi

echo ""
echo "[SUMMARY] Release simulation complete"
echo "  Report: $REPORT_FILE"
echo ""
echo "Review the report and fix any issues before creating actual release."

if command -v jq >/dev/null 2>&1; then
    echo ""
    echo "Quick summary:"
    jq -r '.summary | "  Passed: \(.passed), Failed: \(.failed), Warnings: \(.warnings)"' "$REPORT_FILE"
fi

