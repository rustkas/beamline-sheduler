#!/bin/bash
# Sanity-check: HMAC masking verification in documentation
# Checks that all HMAC values in example outputs are truncated to format .{16}...

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

cd "$PROJECT_ROOT"

# Files to check
DOCS_TO_CHECK=(
    "docs/CI_VALIDATION.md"
)

# Regular expression for finding full HMAC (64 hex characters)
# HMAC should be truncated to format: first 16 characters + "..."
FULL_HMAC_PATTERN='[0-9a-f]{64}'
MASKED_HMAC_PATTERN='[0-9a-f]{16}\.\.\.'

ERRORS=0
WARNINGS=0

echo "=========================================="
echo "HMAC Masking Sanity Check"
echo "=========================================="
echo ""

for doc_file in "${DOCS_TO_CHECK[@]}"; do
    if [ ! -f "$doc_file" ]; then
        echo "[WARN] File not found: $doc_file"
        ((WARNINGS++))
        continue
    fi
    
    echo "Checking: $doc_file"
    
    # Search for full HMAC (64 hex characters) in example outputs
    # Exclude lines with comments and metadata
    full_hmacs=$(grep -nE "$FULL_HMAC_PATTERN" "$doc_file" | grep -vE "^[[:space:]]*#" | grep -vE "^[[:space:]]*\*" || true)
    
    if [ -n "$full_hmacs" ]; then
        echo "[FAIL] Found unmasked HMAC values in $doc_file:"
        echo "$full_hmacs" | while IFS= read -r line; do
            echo "  Line: $line"
        done
        ((ERRORS++))
    else
        echo "[OK] No unmasked HMAC values found"
    fi
    
    # Check for properly masked HMAC
    masked_hmacs=$(grep -cE "$MASKED_HMAC_PATTERN" "$doc_file" || echo "0")
    if [ "$masked_hmacs" -gt 0 ]; then
        echo "[OK] Found $masked_hmacs masked HMAC value(s)"
    fi
    
    echo ""
done

# Final result
echo "=========================================="
if [ $ERRORS -eq 0 ]; then
    echo "[OK] Sanity check passed: All HMAC values are properly masked"
    exit 0
else
    echo "[FAIL] Sanity check failed: Found $ERRORS file(s) with unmasked HMAC values"
    if [ $WARNINGS -gt 0 ]; then
        echo "[WARN] $WARNINGS warning(s)"
    fi
    exit 1
fi
