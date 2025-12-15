#!/bin/bash
# Infrastructure Validation Script (Bash)
# Validates infrastructure configuration files
# Exit codes: 0 = success, 2 = external errors, 3 = local errors (WARN does not cause exit 2)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
LOG_DIR="${PROJECT_ROOT}/reports/dry-run-logs/infra"
LOG_FILE="${LOG_DIR}/validation.log"

# Normalize path separators (support both / and \)
normalize_path() {
    local path="$1"
    # Convert backslashes to forward slashes (Unix-style)
    echo "$path" | sed 's|\\|/|g'
}

# Ensure log directory exists
mkdir -p "${LOG_DIR}"

ERRORS=0
WARNINGS=0
EXIT_CODE=0

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Mask function
mask() {
    local s="$1"
    if [ -z "$s" ]; then
        echo "$s"
        return
    fi
    echo "$s" | sed -E 's/([A-Za-z0-9+/]{16,})/[MASKED]/g'
}

log() {
    local level="$1"
    shift
    local message="$*"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    local log_message="[${timestamp}] [${level}] ${message}"
    
    echo "$log_message" >> "$LOG_FILE"
    
    case "$level" in
        ERROR)
            echo -e "${RED}${log_message}${NC}" >&2
            ((ERRORS++)) || true
            ;;
        WARN)
            echo -e "${YELLOW}${log_message}${NC}"
            ((WARNINGS++)) || true
            ;;
        INFO)
            echo -e "${GREEN}${log_message}${NC}"
            ;;
        *)
            echo "$log_message"
            ;;
    esac
}

# Check required files
check_required_files() {
    log "INFO" "Checking required infrastructure files..."
    
    local required_files=(
        "infra/compose/local-dev.yml:Docker Compose file"
        "infra/docker/README.md:Docker README"
    )
    
    for file_spec in "${required_files[@]}"; do
        IFS=':' read -r file_path file_name <<< "$file_spec"
        local normalized_path=$(normalize_path "$file_path")
        local full_path="${PROJECT_ROOT}/${normalized_path}"
        
        if [ -f "$full_path" ]; then
            log "INFO" "Found: ${file_name} - ${file_path}"
        else
            log "ERROR" "Missing: ${file_name} - ${file_path}"
            EXIT_CODE=3
        fi
    done
    
    # Check environment template (either .env.example or ENV_TEMPLATE.md)
    # Normalize paths to support both / and \
    local env_example_path=$(normalize_path "config/env/.env.example")
    local env_template_path=$(normalize_path "config/env/ENV_TEMPLATE.md")
    local env_example="${PROJECT_ROOT}/${env_example_path}"
    local env_template="${PROJECT_ROOT}/${env_template_path}"
    if [ -f "$env_example" ]; then
        log "INFO" "Found: Environment template - config/env/.env.example"
    elif [ -f "$env_template" ]; then
        log "INFO" "Found: Environment template - config/env/ENV_TEMPLATE.md"
    else
        log "ERROR" "Missing: Environment template (config/env/.env.example or ENV_TEMPLATE.md)"
        EXIT_CODE=3
    fi
}

# Validate docker-compose syntax
check_compose_syntax() {
    log "INFO" "Validating docker-compose syntax..."
    
    local compose_path=$(normalize_path "infra/compose/local-dev.yml")
    local compose_file="${PROJECT_ROOT}/${compose_path}"
    if [ ! -f "$compose_file" ]; then
        log "ERROR" "Compose file not found"
        EXIT_CODE=3
        return
    fi
    
    if ! command -v docker &> /dev/null; then
        log "WARN" "Docker not found, skipping compose syntax check"
        return
    fi
    
    if docker compose -f "$compose_file" config > /dev/null 2>&1; then
        log "INFO" "Docker Compose syntax is valid"
    else
        log "ERROR" "Docker Compose syntax validation failed"
        local output=$(docker compose -f "$compose_file" config 2>&1 || true)
        log "ERROR" "$(mask "$output")"
        EXIT_CODE=3
    fi
}

# Check port conflicts
check_port_conflicts() {
    log "INFO" "Checking for port conflicts..."
    
    local compose_path=$(normalize_path "infra/compose/local-dev.yml")
    local compose_file="${PROJECT_ROOT}/${compose_path}"
    if [ ! -f "$compose_file" ]; then
        return
    fi
    
    # Extract host ports from compose file
    local ports=$(grep -oE '[0-9]+:[0-9]+' "$compose_file" | cut -d: -f1 | sort)
    local unique_ports=$(echo "$ports" | uniq)
    local total_count=$(echo "$ports" | wc -l)
    local unique_count=$(echo "$unique_ports" | wc -l)
    
    if [ "$total_count" -gt "$unique_count" ]; then
        log "ERROR" "Port conflict detected: some ports are used multiple times"
        EXIT_CODE=3
    else
        log "INFO" "Found $unique_count unique host ports, no conflicts"
    fi
}

# Validate environment variables
check_environment_variables() {
    log "INFO" "Validating environment variables..."
    
    # Check for environment template (either .env.example or ENV_TEMPLATE.md)
    # Normalize paths to support both / and \
    local env_example_path=$(normalize_path "config/env/.env.example")
    local env_template_path=$(normalize_path "config/env/ENV_TEMPLATE.md")
    local env_example="${PROJECT_ROOT}/${env_example_path}"
    local env_template="${PROJECT_ROOT}/${env_template_path}"
    local env_file=""
    
    if [ -f "$env_example" ]; then
        env_file="$env_example"
    elif [ -f "$env_template" ]; then
        env_file="$env_template"
    else
        log "ERROR" "Environment template file not found (.env.example or ENV_TEMPLATE.md)"
        EXIT_CODE=3
        return
    fi
    
    # Check for placeholder values
    local placeholder_count=$(grep -cE 'PLACEHOLDER_\w+' "$env_file" || echo "0")
    if [ "$placeholder_count" -gt 0 ]; then
        log "INFO" "Found $placeholder_count placeholder values (expected)"
    else
        log "WARN" "No placeholder values found"
    fi
    
    # Check for potential secrets (only ERROR, not WARN)
    local secret_patterns=(
        'sk_[A-Za-z0-9]{24,}'
        'eyJ[A-Za-z0-9_-]{10,}\.[A-Za-z0-9_-]{10,}\.[A-Za-z0-9_-]{10,}'
        '[A-Za-z0-9+/]{32,}=?'
    )
    
    local secrets_found=false
    for pattern in "${secret_patterns[@]}"; do
        if grep -qE "$pattern" "$env_file" 2>/dev/null; then
            local match=$(grep -oE "$pattern" "$env_file" | head -1)
            log "ERROR" "Potential secret detected (masked): $(mask "$match")"
            secrets_found=true
            EXIT_CODE=3
        fi
    done
    
    if [ "$secrets_found" = false ]; then
        log "INFO" "No real secrets detected in environment file"
    fi
}

# Main execution
log "INFO" "Starting infrastructure validation..."
log "INFO" "Project root: ${PROJECT_ROOT}"
log "INFO" "Log file: ${LOG_FILE}"

check_required_files
check_compose_syntax
check_port_conflicts
check_environment_variables

# Summary
log "INFO" "Validation complete"
log "INFO" "Errors: ${ERRORS}, Warnings: ${WARNINGS}"

# Exit code logic: WARN does not cause exit 2, only ERROR causes exit 3
if [ $ERRORS -eq 0 ]; then
    log "INFO" "All checks passed"
    exit 0
elif [ $EXIT_CODE -eq 2 ]; then
    log "ERROR" "External errors occurred"
    exit 2
else
    log "ERROR" "Local problems detected"
    exit 3
fi
