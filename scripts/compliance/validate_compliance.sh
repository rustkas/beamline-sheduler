#!/bin/bash
# validate_compliance.sh
# Compliance validation script for Beamline Constructor
# Checks file presence, structure, and absence of real secrets
# Exit codes: 0=OK, 2=External broken, 3=Local broken

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$PROJECT_ROOT"

# Directories
LOGS_DIR="${PROJECT_ROOT}/reports/dry-run-logs/compliance"
mkdir -p "$LOGS_DIR"

LOG_FILE="${LOGS_DIR}/validation.log"
SUMMARY_FILE="${LOGS_DIR}/summary.json"

# Exit codes
EXIT_OK=0
EXIT_EXTERNAL_BROKEN=2
EXIT_LOCAL_BROKEN=3

# Counters
FILES_CHECKED=0
SECRETS_MASKED=0
EXIT_CODE=$EXIT_OK

# Logging function
log() {
    local level="$1"
    shift
    local message="$*"
    local timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
    echo "[$timestamp] [$level] $message" | tee -a "$LOG_FILE"
}

# Initialize log file
echo "==========================================" > "$LOG_FILE"
echo "Compliance Validation Log" >> "$LOG_FILE"
echo "Started: $(date -u +"%Y-%m-%dT%H:%M:%SZ")" >> "$LOG_FILE"
echo "==========================================" >> "$LOG_FILE"
echo "" >> "$LOG_FILE"

log "INFO" "Starting compliance validation"

# Check required files
check_structure() {
    log "INFO" "Checking compliance structure..."
    
    local required_files=(
        "compliance/licenses/REGISTRY.md"
        "compliance/privacy/POLICY.md"
        "compliance/sbom/SBOM.template.json"
    )
    
    local missing_files=()
    
    for file in "${required_files[@]}"; do
        if [ ! -f "$file" ]; then
            missing_files+=("$file")
            log "ERROR" "Missing required file: $file"
        else
            FILES_CHECKED=$((FILES_CHECKED + 1))
            log "INFO" "Found: $file"
        fi
    done
    
    if [ ${#missing_files[@]} -gt 0 ]; then
        log "ERROR" "Missing ${#missing_files[@]} required file(s)"
        EXIT_CODE=$EXIT_LOCAL_BROKEN
        return 1
    fi
    
    log "INFO" "All required files present"
    return 0
}

# Check basic structure of files
check_file_structure() {
    log "INFO" "Checking file structure..."
    
    local errors=0
    
    # Check REGISTRY.md has license information
    if [ -f "compliance/licenses/REGISTRY.md" ]; then
        if ! grep -qi "Apache-2.0\|MIT\|allowlist\|denylist" "compliance/licenses/REGISTRY.md"; then
            log "ERROR" "REGISTRY.md missing license information"
            errors=$((errors + 1))
        else
            log "INFO" "REGISTRY.md structure OK"
        fi
    fi
    
    # Check POLICY.md has privacy information
    if [ -f "compliance/privacy/POLICY.md" ]; then
        if ! grep -qi "PII\|privacy\|logging\|retention" "compliance/privacy/POLICY.md"; then
            log "ERROR" "POLICY.md missing privacy information"
            errors=$((errors + 1))
        else
            log "INFO" "POLICY.md structure OK"
        fi
    fi
    
    # Check SBOM.template.json is valid JSON
    if [ -f "compliance/sbom/SBOM.template.json" ]; then
        if command -v jq >/dev/null 2>&1; then
            if ! jq empty "compliance/sbom/SBOM.template.json" 2>/dev/null; then
                log "ERROR" "SBOM.template.json is not valid JSON"
                errors=$((errors + 1))
            else
                # Check for required fields
                if ! jq -e '.name, .version, .components' "compliance/sbom/SBOM.template.json" >/dev/null 2>&1; then
                    log "ERROR" "SBOM.template.json missing required fields (name, version, components)"
                    errors=$((errors + 1))
                else
                    # Check components have required fields (name, version, license)
                    local components_ok=true
                    local component_count=$(jq '.components | length' "compliance/sbom/SBOM.template.json" 2>/dev/null || echo "0")
                    for ((i=0; i<component_count; i++)); do
                        if ! jq -e ".components[$i] | .name, .version, .license" "compliance/sbom/SBOM.template.json" >/dev/null 2>&1; then
                            log "ERROR" "SBOM component at index $i missing required fields (name, version, license)"
                            components_ok=false
                            errors=$((errors + 1))
                        fi
                    done
                    if [ "$components_ok" = "true" ]; then
                        log "INFO" "SBOM.template.json structure OK (all components have name, version, license)"
                    fi
                fi
            fi
        else
            log "WARN" "jq not available, skipping JSON validation"
        fi
    fi
    
    if [ $errors -gt 0 ]; then
        EXIT_CODE=$EXIT_LOCAL_BROKEN
        return 1
    fi
    
    return 0
}

# Check for secrets
check_secrets() {
    log "INFO" "Checking for secrets in compliance artifacts..."
    
    local compliance_files=(
        "compliance/licenses/REGISTRY.md"
        "compliance/privacy/POLICY.md"
        "compliance/sbom/SBOM.template.json"
    )
    
    # Secret patterns (forbidden if not masked)
    local secret_patterns=(
        "password\\s*=\\s*(?!PLACEHOLDER|EXAMPLE|TEST|YOUR)[a-zA-Z0-9_\\-]{8,}"
        "sk_live_[a-zA-Z0-9]{32,}"
        "sk_test_[a-zA-Z0-9]{32,}"
        "-----BEGIN.*PRIVATE KEY-----"
        "ghp_[a-zA-Z0-9]{36,}"
        "api[_-]?key\\s*=\\s*(?!PLACEHOLDER|EXAMPLE|TEST|YOUR)[a-zA-Z0-9_\\-]{16,}"
        "secret\\s*=\\s*(?!PLACEHOLDER|EXAMPLE|TEST|YOUR)[a-zA-Z0-9_\\-]{12,}"
        "token\\s*=\\s*(?!PLACEHOLDER|EXAMPLE|TEST|YOUR)[a-zA-Z0-9_\\-]{16,}"
    )
    
    local found_secrets=false
    
    for file in "${compliance_files[@]}"; do
        if [ ! -f "$file" ]; then
            continue
        fi
        
        for pattern in "${secret_patterns[@]}"; do
            # Use grep with extended regex
            if grep -rEi "$pattern" "$file" 2>/dev/null | grep -vE "PLACEHOLDER|EXAMPLE|TEST|YOUR|MASKED|REDACTED|template|placeholder" > /dev/null; then
                log "ERROR" "Potential secret found in $file (pattern: $pattern)"
                found_secrets=true
                EXIT_CODE=$EXIT_LOCAL_BROKEN
            fi
        done
    done
    
    if [ "$found_secrets" = "false" ]; then
        SECRETS_MASKED=1
        log "INFO" "No secrets detected in compliance artifacts"
    else
        log "ERROR" "Secrets detected - all secrets must be masked"
    fi
    
    return 0
}

# Check links (if check_links_v2.ps1 available)
check_links() {
    log "INFO" "Checking links..."
    
    if [ -f "scripts/check_links_v2.ps1" ]; then
        log "INFO" "Running link checker: scripts/check_links_v2.ps1"
        
        # Try to run PowerShell script
        if command -v pwsh >/dev/null 2>&1; then
            if ! pwsh -ExecutionPolicy Bypass -File "scripts/check_links_v2.ps1" 2>&1 | tee -a "$LOG_FILE"; then
                log "ERROR" "Link check failed"
                EXIT_CODE=$EXIT_EXTERNAL_BROKEN
                return 1
            fi
        elif command -v powershell >/dev/null 2>&1; then
            if ! powershell -ExecutionPolicy Bypass -File "scripts/check_links_v2.ps1" 2>&1 | tee -a "$LOG_FILE"; then
                log "ERROR" "Link check failed"
                EXIT_CODE=$EXIT_EXTERNAL_BROKEN
                return 1
            fi
        else
            log "WARN" "PowerShell not available, skipping link check"
        fi
    else
        log "WARN" "check_links_v2.ps1 not found, skipping link check"
    fi
    
    return 0
}

# Generate summary JSON
generate_summary() {
    log "INFO" "Generating summary..."
    
    local summary=$(cat <<EOF
{
  "files_checked": $FILES_CHECKED,
  "secrets_masked": $SECRETS_MASKED,
  "exit_code": $EXIT_CODE,
  "timestamp": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")"
}
EOF
)
    
    echo "$summary" > "$SUMMARY_FILE"
    log "INFO" "Summary saved to: $SUMMARY_FILE"
}

# Main execution
main() {
    log "INFO" "=========================================="
    log "INFO" "Compliance Validation"
    log "INFO" "=========================================="
    log "INFO" ""
    
    check_structure || true
    check_file_structure || true
    check_secrets || true
    check_links || true
    
    generate_summary
    
    log "INFO" ""
    log "INFO" "=========================================="
    log "INFO" "Validation completed"
    log "INFO" "Exit code: $EXIT_CODE"
    log "INFO" "=========================================="
    
    exit $EXIT_CODE
}

# Run main
main "$@"
