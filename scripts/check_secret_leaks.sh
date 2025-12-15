#!/bin/bash
# Secret Leak Detection Script
# Checks for potential secret leaks using unified pattern registry from secret_patterns.json
# Usage: bash scripts/check_secret_leaks.sh [--staged] [--all]
# Exit codes: 0 = OK, 2 = masking violation, 5 = leaks detected, 1 = error

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

cd "$PROJECT_ROOT"

# Configuration from manifest
MANIFEST_FILE=".trae/manifest.json"
PATTERNS_FILE="scripts/secret_patterns.json"
REPORT_DIR="reports/dry-run-logs"
REPORT_FILE="${REPORT_DIR}/security.log"
TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

# Create report directory
mkdir -p "$REPORT_DIR"

# Initialize report
{
    echo "=========================================="
    echo "Secret Leak Detection Report"
    echo "Timestamp: $TIMESTAMP"
    echo "=========================================="
    echo ""
} > "$REPORT_FILE"

# Load configuration from manifest
echo "=========================================="
echo "Secret Leak Detection"
echo "=========================================="
echo ""
echo "[INFO] Reading configuration from manifest..."
HMAC_SECRET_ENV=$(jq -r '.security.hmac_secret.env_variable' "$MANIFEST_FILE" 2>/dev/null || echo "BEAMLINE_HMAC_SECRET")
HMAC_MASKING_POLICY=$(jq -r '.security.hmac_masking.policy' "$MANIFEST_FILE" 2>/dev/null || echo "16+...")
echo "[INFO] HMAC Secret Env: $HMAC_SECRET_ENV" | tee -a "$REPORT_FILE"
echo "[INFO] HMAC Masking Policy: $HMAC_MASKING_POLICY" | tee -a "$REPORT_FILE"
echo "" | tee -a "$REPORT_FILE"

# Load patterns from JSON
if [ ! -f "$PATTERNS_FILE" ]; then
    echo "[ERROR] Pattern registry not found: $PATTERNS_FILE" | tee -a "$REPORT_FILE"
    exit 1
fi

# Extract patterns and exclusions from JSON
PATTERNS_JSON=$(cat "$PATTERNS_FILE")
EXIT_CODES=$(echo "$PATTERNS_JSON" | jq -r '.exit_codes')

# Build exclusion patterns
EXCLUDE_DIRS=$(echo "$PATTERNS_JSON" | jq -r '.exclusions.directories[]' | tr '\n' '|' | sed 's/|$//')
EXCLUDE_FILES=$(echo "$PATTERNS_JSON" | jq -r '.exclusions.file_patterns[]' | tr '\n' '|' | sed 's/|$//')
EXCLUDE_CODE=$(echo "$PATTERNS_JSON" | jq -r '.exclusions.code_patterns[]' | tr '\n' '|' | sed 's/|$//')

# Colors for output
RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

ERRORS=0
WARNINGS=0
MASKING_VIOLATIONS=0
LEAKS_DETECTED=0

# Function to mask secret in output
mask_secret() {
    local secret="$1"
    local pattern_type="${2:-default}"
    
    case "$pattern_type" in
        "hmac")
            echo "${secret:0:16}..."
            ;;
        "api_key"|"secret")
            echo "${secret:0:8}..."
            ;;
        "token")
            echo "${secret:0:12}..."
            ;;
        "ssh_key")
            echo "[REDACTED SSH KEY]"
            ;;
        *)
            echo "${secret:0:8}..."
            ;;
    esac
}

# Function to check if file should be excluded
should_exclude_file() {
    local file="$1"
    
    # Check directory exclusions
    if echo "$file" | grep -qE "$EXCLUDE_DIRS"; then
        return 0
    fi
    
    # Check file pattern exclusions
    if echo "$file" | grep -qE "$EXCLUDE_FILES"; then
        return 0
    fi
    
    return 1
}

# Function to check if line should be excluded
should_exclude_line() {
    local line="$1"
    
    # Check code pattern exclusions
    if echo "$line" | grep -qE "$EXCLUDE_CODE"; then
        return 0
    fi
    
    return 1
}

# Extract all patterns from JSON
extract_patterns() {
    local patterns_json="$1"
    
    # Extract HMAC pattern
    echo "$patterns_json" | jq -r '.patterns.hmac.pattern // empty'
    
    # Extract API key patterns
    echo "$patterns_json" | jq -r '.patterns.api_keys.patterns[]?.pattern // empty'
    
    # Extract token patterns
    echo "$patterns_json" | jq -r '.patterns.tokens.patterns[]?.pattern // empty'
    
    # Extract password patterns
    echo "$patterns_json" | jq -r '.patterns.passwords.patterns[]?.pattern // empty'
    
    # Extract secret patterns
    echo "$patterns_json" | jq -r '.patterns.secrets.patterns[]?.pattern // empty'
    
    # Extract SSH key patterns
    echo "$patterns_json" | jq -r '.patterns.ssh_keys.patterns[]?.pattern // empty'
    
    # Extract base64 patterns
    echo "$patterns_json" | jq -r '.patterns.base64_secrets.patterns[]?.pattern // empty'
}

# Determine files to check
if [ "${1:-}" = "--staged" ]; then
    FILES_TO_CHECK=$(git diff --cached --name-only --diff-filter=ACMR 2>/dev/null || echo "")
    echo "[INFO] Checking staged files only" | tee -a "$REPORT_FILE"
elif [ "${1:-}" = "--all" ]; then
    FILES_TO_CHECK=$(git ls-files 2>/dev/null || find . -type f -not -path "./.git/*" | head -1000 || echo "")
    echo "[INFO] Checking all tracked files" | tee -a "$REPORT_FILE"
else
    # Check modified files in current branch vs main
    FILES_TO_CHECK=$(git diff --name-only origin/main...HEAD 2>/dev/null || \
                     git diff --name-only main...HEAD 2>/dev/null || \
                     git diff --name-only --diff-filter=ACMR 2>/dev/null || echo "")
    echo "[INFO] Checking modified files" | tee -a "$REPORT_FILE"
fi

if [ -z "$FILES_TO_CHECK" ]; then
    echo "[WARN] No files to check" | tee -a "$REPORT_FILE"
    echo "" | tee -a "$REPORT_FILE"
    echo "==========================================" | tee -a "$REPORT_FILE"
    echo -e "${GREEN}[OK]${NC} No files to check" | tee -a "$REPORT_FILE"
    exit 0
fi

# Get all patterns
ALL_PATTERNS=$(extract_patterns "$PATTERNS_JSON")

EXCLUDE_PATHS=(
  "scripts/secret_patterns.json"
  "fixtures/leak_samples/"
  "reports/"
  "config/env/"
)

should_skip_file() {
  local f="$1"
  for p in "${EXCLUDE_PATHS[@]}"; do
    [[ "$p" == */ && "$f" == "$p"* ]] && return 0
    [[ "$p" == "$f" ]] && return 0
  done
  return 1
}

if should_skip_file "$file"; then
  continue
fi


# Check each file
for file in $FILES_TO_CHECK; do
    # Skip excluded files
    if should_exclude_file "$file"; then
        continue
    fi
    
    if [ ! -f "$file" ]; then
        continue
    fi
    
    # Check each pattern
    echo "$ALL_PATTERNS" | while IFS= read -r pattern; do
        [ -z "$pattern" ] && continue
        
        matches=$(grep -nE "$pattern" "$file" 2>/dev/null || true)
        
        if [ -n "$matches" ]; then
            # Filter out excluded lines
            filtered_matches=$(echo "$matches" | while IFS= read -r line; do
                if ! should_exclude_line "$line"; then
                    echo "$line"
                fi
            done)
            
            if [ -n "$filtered_matches" ]; then
                # Check for masking violations (full secrets in output)
                if echo "$filtered_matches" | grep -qE "[0-9a-f]{64}|[A-Za-z0-9+/]{40,}={0,2}"; then
                    ((MASKING_VIOLATIONS++))
                    echo -e "${RED}[FAIL]${NC} Masking violation in $file:" | tee -a "$REPORT_FILE"
                    echo "$filtered_matches" | head -5 | while IFS= read -r line; do
                        masked_line=$(echo "$line" | sed -E 's/([0-9a-f]{64})/'"$(echo "$line" | grep -oE '[0-9a-f]{64}' | head -1 | cut -c1-16)"'.../g')
                        echo "  Line: $masked_line" | tee -a "$REPORT_FILE"
                    done
                else
                    ((LEAKS_DETECTED++))
                    echo -e "${RED}[FAIL]${NC} Potential secret leak in $file:" | tee -a "$REPORT_FILE"
                    echo "$filtered_matches" | head -5 | while IFS= read -r line; do
                        echo "  Line: $line" | tee -a "$REPORT_FILE"
                    done
                fi
                ((ERRORS++))
            else
                # Masked or example values - warning only
                echo -e "${YELLOW}[WARN]${NC} Possible secret pattern in $file (may be masked/example):" | tee -a "$REPORT_FILE"
                echo "$matches" | head -3 | while IFS= read -r line; do
                    echo "  Line: $line" | tee -a "$REPORT_FILE"
                done
                ((WARNINGS++))
            fi
        fi
    done
done

# Check for common secret file patterns
SECRET_FILES=(
    ".env"
    ".secrets"
    "secrets.json"
    "config.json"
    "credentials.json"
)

for secret_file in "${SECRET_FILES[@]}"; do
    if git ls-files --error-unmatch "$secret_file" >/dev/null 2>&1; then
        echo -e "${RED}[FAIL]${NC} Secret file found in repository: $secret_file" | tee -a "$REPORT_FILE"
        echo "  Secret files should be in .gitignore and never committed" | tee -a "$REPORT_FILE"
        ((ERRORS++))
        ((LEAKS_DETECTED++))
    fi
done

# Check .gitignore for secret files
if [ -f ".gitignore" ]; then
    for secret_file in "${SECRET_FILES[@]}"; do
        if ! grep -qE "^${secret_file}$|^/${secret_file}$" .gitignore 2>/dev/null; then
            echo -e "${YELLOW}[WARN]${NC} Secret file pattern not in .gitignore: $secret_file" | tee -a "$REPORT_FILE"
            ((WARNINGS++))
        fi
    done
fi

# Summary
echo "" | tee -a "$REPORT_FILE"
echo "==========================================" | tee -a "$REPORT_FILE"
{
    echo "Summary:"
    echo "  Errors: $ERRORS"
    echo "  Warnings: $WARNINGS"
    echo "  Masking Violations: $MASKING_VIOLATIONS"
    echo "  Leaks Detected: $LEAKS_DETECTED"
    echo ""
} | tee -a "$REPORT_FILE"

# Determine exit code
EXIT_CODE=0
if [ $MASKING_VIOLATIONS -gt 0 ]; then
    EXIT_CODE=2
    echo -e "${RED}[FAIL]${NC} Masking violations detected: $MASKING_VIOLATIONS" | tee -a "$REPORT_FILE"
elif [ $LEAKS_DETECTED -gt 0 ]; then
    EXIT_CODE=5
    echo -e "${RED}[FAIL]${NC} Secret leaks detected: $LEAKS_DETECTED" | tee -a "$REPORT_FILE"
elif [ $ERRORS -eq 0 ] && [ $WARNINGS -eq 0 ]; then
    EXIT_CODE=0
    echo -e "${GREEN}[OK]${NC} No secret leaks detected" | tee -a "$REPORT_FILE"
elif [ $ERRORS -eq 0 ]; then
    EXIT_CODE=0
    echo -e "${YELLOW}[WARN]${NC} Secret leak check completed with $WARNINGS warning(s)" | tee -a "$REPORT_FILE"
    echo "  Review warnings above - they may be false positives (masked values, examples)" | tee -a "$REPORT_FILE"
else
    EXIT_CODE=5
    echo -e "${RED}[FAIL]${NC} Secret leak check failed: Found $ERRORS potential leak(s)" | tee -a "$REPORT_FILE"
fi

echo "" | tee -a "$REPORT_FILE"
echo "Report saved to: $REPORT_FILE" | tee -a "$REPORT_FILE"

exit $EXIT_CODE
