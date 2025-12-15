#!/bin/bash
# Observability Validation Script (MVP)
# Validates JSON log format and /_health endpoint availability
# Exit codes: 0 = success, 2 = external endpoints unavailable, 3 = local configs missing or invalid

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
LOG_DIR="${PROJECT_ROOT}/reports/dry-run-logs/observability"
LOG_FILE="${LOG_DIR}/validation.log"
TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

# Create log directory
mkdir -p "${LOG_DIR}"

# Initialize counters
ERRORS=0
WARNINGS=0
EXTERNAL_ERRORS=0
LOCAL_ERRORS=0

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Mask function (similar to check_links.ps1)
mask() {
    local s="$1"
    if [[ -z "$s" ]]; then
        echo "$s"
        return
    fi
    # Mask token-like strings: [A-Za-z0-9+/]{16,} -> [MASKED]
    echo "$s" | sed -E 's/[A-Za-z0-9+\/]{16,}/[MASKED]/g'
}

log_error() {
    local msg=$(mask "$1")
    echo -e "${RED}[ERROR]${NC} $msg" >&2
    echo "[$TIMESTAMP] [ERROR] $msg" >> "$LOG_FILE"
    ((ERRORS++)) || true
    ((LOCAL_ERRORS++)) || true
}

log_warn() {
    local msg=$(mask "$1")
    echo -e "${YELLOW}[WARN]${NC} $msg"
    echo "[$TIMESTAMP] [WARN] $msg" >> "$LOG_FILE"
    ((WARNINGS++)) || true
}

log_info() {
    local msg=$(mask "$1")
    echo -e "${GREEN}[INFO]${NC} $msg"
    echo "[$TIMESTAMP] [INFO] $msg" >> "$LOG_FILE"
}

# Check logging.json configuration
check_logging_config() {
    log_info "Checking logging configuration..."
    
    local logging_file="${PROJECT_ROOT}/config/observability/logging.json"
    
    if [[ ! -f "$logging_file" ]]; then
        log_error "Logging config not found: $logging_file"
        return 1
    fi
    
    # Check JSON syntax
    if command -v jq &> /dev/null; then
        if ! jq empty "$logging_file" 2>/dev/null; then
            log_error "Invalid JSON syntax in logging.json"
            return 1
        fi
        log_info "Valid JSON syntax in logging.json"
    elif command -v python3 &> /dev/null; then
        if ! python3 -c "import json; json.load(open('$logging_file'))" 2>/dev/null; then
            log_error "Invalid JSON syntax in logging.json"
            return 1
        fi
        log_info "Valid JSON syntax in logging.json"
    else
        log_warn "No JSON validator found (jq or python3). Skipping JSON validation."
    fi
    
    # Check for required fields in format
    if grep -q '"timestamp"' "$logging_file" && \
       grep -q '"level"' "$logging_file" && \
       grep -q '"component"' "$logging_file" && \
       grep -q '"message"' "$logging_file"; then
        log_info "Logging format includes required fields (timestamp, level, component, message)"
    else
        log_error "Logging format missing required fields"
        return 1
    fi
    
    # Check for log levels
    local required_levels=("ERROR" "WARN" "INFO" "DEBUG")
    for level in "${required_levels[@]}"; do
        if grep -q "\"$level\"" "$logging_file"; then
            log_info "Found log level: $level"
        else
            log_warn "Log level missing: $level"
        fi
    done
    
    # Check for health endpoint definition
    if grep -q "\"/_health\"" "$logging_file" || grep -q "health_endpoint" "$logging_file"; then
        log_info "Health endpoint defined in logging.json"
    else
        log_warn "Health endpoint not defined in logging.json"
    fi
    
    log_info "Logging configuration validated"
    return 0
}

# Validate JSON log format (check example log)
validate_json_log_format() {
    log_info "Validating JSON log format..."
    
    local logging_file="${PROJECT_ROOT}/config/observability/logging.json"
    
    if [[ ! -f "$logging_file" ]]; then
        log_error "Cannot validate log format: logging.json not found"
        return 1
    fi
    
    # Create example log entry based on schema with CP1 fields
    local example_log='{
      "timestamp": "2025-01-27T12:00:00Z",
      "level": "INFO",
      "component": "router",
      "message": "Request processed",
      "tenant_id": "tenant_123",
      "run_id": "run_abc123",
      "flow_id": "flow_xyz789",
      "trace_id": "trace_def456",
      "context": {
        "provider": "openai",
        "latency_ms": 250
      }
    }'
    
    # Validate example log (if jq available)
    if command -v jq &> /dev/null; then
        if echo "$example_log" | jq empty 2>/dev/null; then
            log_info "Example JSON log format is valid"
            
            # Check for CP1 required fields in schema
            if grep -q "run_id" "$logging_file" && \
               grep -q "flow_id" "$logging_file" && \
               grep -q "step_id" "$logging_file"; then
                log_info "CP1 required fields (run_id, flow_id, step_id) defined in schema"
            else
                log_warn "CP1 required fields may be missing in schema definition"
            fi
        else
            log_warn "Example JSON log format validation failed"
        fi
    fi
    
    log_info "JSON log format validation complete"
    return 0
}

# Validate CP1 cross-cutting invariants in logging.json
validate_cp1_invariants() {
    log_info "Validating CP1 cross-cutting invariants..."
    
    local logging_file="${PROJECT_ROOT}/config/observability/logging.json"
    
    if [[ ! -f "$logging_file" ]]; then
        log_error "Cannot validate CP1 invariants: logging.json not found"
        return 1
    fi
    
    # Check for CP1 required fields section
    if grep -q "cp1_required_fields" "$logging_file"; then
        log_info "CP1 required fields section found in logging.json"
        
        # Check for specific CP1 fields
        local cp1_fields=("tenant_id" "run_id" "flow_id" "step_id" "trace_id")
        local found_fields=0
        
        for field in "${cp1_fields[@]}"; do
            if grep -q "\"$field\"" "$logging_file"; then
                log_info "CP1 field found: $field"
                ((found_fields++)) || true
            else
                log_warn "CP1 field missing: $field"
            fi
        done
        
        if [[ $found_fields -eq ${#cp1_fields[@]} ]]; then
            log_info "All CP1 required fields present in schema"
        else
            log_warn "Some CP1 required fields missing ($found_fields/${#cp1_fields[@]})"
        fi
    else
        log_warn "CP1 required fields section not found in logging.json"
    fi
    
    log_info "CP1 invariants validation complete"
    return 0
}

# Check /_health endpoint availability and format (200 OK + JSON)
check_health_endpoints() {
    log_info "Checking /_health endpoint availability and format (200 OK + JSON)..."
    
    # List of components that should have /_health endpoint
    # Router uses gRPC health service on port 9000 (not HTTP /_health)
    local components=("provider" "gateway" "usage")
    local base_urls=(
        "http://localhost:8081"
        "http://localhost:3000"
        "http://localhost:8083"
    )
    
    # Router health check (gRPC health service, not HTTP)
    log_info "Router health endpoint: gRPC health service on port 9000 (not HTTP /_health)"
    if command -v grpc_health_probe >/dev/null 2>&1; then
        if grpc_health_probe -addr=localhost:9000 >/dev/null 2>&1; then
            log_info "Router gRPC health service: OK (port 9000)"
            ((available++)) || true
        else
            log_warn "Router gRPC health service: unavailable (port 9000) - service may not be running"
            ((unavailable++)) || true
            ((EXTERNAL_ERRORS++)) || true
        fi
    else
        log_warn "grpc_health_probe not found - skipping Router gRPC health check"
        log_info "Router health: gRPC health service configured (port 9000, check with grpc_health_probe)"
    fi
    
    local available=0
    local unavailable=0
    local invalid_format=0
    
    for i in "${!components[@]}"; do
        local component="${components[$i]}"
        local url="${base_urls[$i]}/_health"
        
        # Try to check endpoint (with timeout)
        if command -v curl &> /dev/null; then
            local http_code
            local response_body
            http_code=$(curl -s -w "%{http_code}" -m 2 "$url" 2>/dev/null | tail -1)
            response_body=$(curl -s -m 2 "$url" 2>/dev/null)
            
            if [[ "$http_code" == "200" ]]; then
                # Check if response is valid JSON
                if command -v jq &> /dev/null; then
                    if echo "$response_body" | jq empty 2>/dev/null; then
                        # Check for required fields (status, timestamp)
                        if echo "$response_body" | jq -e '.status' > /dev/null 2>&1 && \
                           echo "$response_body" | jq -e '.timestamp' > /dev/null 2>&1; then
                            log_info "Health endpoint OK: $component ($url) - 200 OK, valid JSON with status/timestamp"
                            ((available++)) || true
                        else
                            log_warn "Health endpoint invalid format: $component ($url) - missing status or timestamp"
                            ((invalid_format++)) || true
                            ((EXTERNAL_ERRORS++)) || true
                        fi
                    else
                        log_warn "Health endpoint invalid JSON: $component ($url) - response is not valid JSON"
                        ((invalid_format++)) || true
                        ((EXTERNAL_ERRORS++)) || true
                    fi
                elif command -v python3 &> /dev/null; then
                    if echo "$response_body" | python3 -c "import json, sys; json.load(sys.stdin)" 2>/dev/null; then
                        # Check for required fields
                        if echo "$response_body" | python3 -c "import json, sys; d=json.load(sys.stdin); assert 'status' in d and 'timestamp' in d" 2>/dev/null; then
                            log_info "Health endpoint OK: $component ($url) - 200 OK, valid JSON with status/timestamp"
                            ((available++)) || true
                        else
                            log_warn "Health endpoint invalid format: $component ($url) - missing status or timestamp"
                            ((invalid_format++)) || true
                            ((EXTERNAL_ERRORS++)) || true
                        fi
                    else
                        log_warn "Health endpoint invalid JSON: $component ($url) - response is not valid JSON"
                        ((invalid_format++)) || true
                        ((EXTERNAL_ERRORS++)) || true
                    fi
                else
                    log_info "Health endpoint available: $component ($url) - 200 OK (JSON validation skipped, no jq/python3)"
                    ((available++)) || true
                fi
            else
                log_warn "Health endpoint unavailable: $component ($url) - HTTP $http_code (expected 200) - service may not be running"
                ((unavailable++)) || true
                ((EXTERNAL_ERRORS++)) || true
            fi
        elif command -v wget &> /dev/null; then
            local temp_file=$(mktemp)
            local http_code
            http_code=$(wget -q -T 2 -O "$temp_file" --server-response "$url" 2>&1 | grep -E "HTTP/" | tail -1 | awk '{print $2}')
            
            if [[ "$http_code" == "200" ]]; then
                local response_body=$(cat "$temp_file" 2>/dev/null)
                rm -f "$temp_file"
                
                # Check if response is valid JSON
                if command -v jq &> /dev/null; then
                    if echo "$response_body" | jq empty 2>/dev/null; then
                        if echo "$response_body" | jq -e '.status' > /dev/null 2>&1 && \
                           echo "$response_body" | jq -e '.timestamp' > /dev/null 2>&1; then
                            log_info "Health endpoint OK: $component ($url) - 200 OK, valid JSON with status/timestamp"
                            ((available++)) || true
                        else
                            log_warn "Health endpoint invalid format: $component ($url) - missing status or timestamp"
                            ((invalid_format++)) || true
                            ((EXTERNAL_ERRORS++)) || true
                        fi
                    else
                        log_warn "Health endpoint invalid JSON: $component ($url) - response is not valid JSON"
                        ((invalid_format++)) || true
                        ((EXTERNAL_ERRORS++)) || true
                    fi
                else
                    log_info "Health endpoint available: $component ($url) - 200 OK (JSON validation skipped)"
                    ((available++)) || true
                fi
            else
                rm -f "$temp_file"
                log_warn "Health endpoint unavailable: $component ($url) - HTTP $http_code (expected 200) - service may not be running"
                ((unavailable++)) || true
                ((EXTERNAL_ERRORS++)) || true
            fi
        else
            log_warn "No HTTP client found (curl or wget). Skipping health endpoint checks."
            log_warn "Note: Health endpoints are optional for local validation (services may not be running)"
            return 0
        fi
    done
    
    if [[ $invalid_format -gt 0 ]]; then
        log_warn "Some health endpoints have invalid format ($invalid_format/${#components[@]}) - check JSON structure"
    fi
    
    if [[ $unavailable -gt 0 ]]; then
        log_warn "Some health endpoints are unavailable ($unavailable/${#components[@]}) - services may not be running"
        log_warn "This is expected in local validation if services are not started"
    fi
    
    if [[ $available -gt 0 ]]; then
        log_info "Health endpoints OK: $available/${#components[@]} available with valid format (200 OK + JSON)"
    fi
    
    return 0
}

# Check for real secrets in configs
check_secrets() {
    log_info "Checking for real secrets in configuration files..."
    
    local config_dir="${PROJECT_ROOT}/config/observability"
    local secret_pattern="[A-Za-z0-9+/]{32,}"
    local found_secrets=0
    
    # Check logging.json
    if [[ -f "${config_dir}/logging.json" ]]; then
        while IFS= read -r line; do
            if echo "$line" | grep -qE "$secret_pattern" && ! echo "$line" | grep -q "PLACEHOLDER\|REDACTED\|example"; then
                local masked_line=$(mask "$line")
                log_warn "Potential secret found in logging.json: $masked_line"
                ((found_secrets++)) || true
            fi
        done < "${config_dir}/logging.json"
    fi
    
    if [[ $found_secrets -gt 0 ]]; then
        log_warn "Found $found_secrets potential secret(s) - should use PLACEHOLDER or [REDACTED] values"
    else
        log_info "No real secrets found (all use placeholders or examples)"
    fi
    
    return 0
}

# Main validation
main() {
    echo "=========================================" >> "$LOG_FILE"
    echo "Observability Validation (MVP) - $TIMESTAMP" >> "$LOG_FILE"
    echo "=========================================" >> "$LOG_FILE"
    
    log_info "Starting observability validation (MVP)..."
    echo ""
    
    # Check logging configuration
    if ! check_logging_config; then
        log_error "Logging configuration validation failed"
    fi
    echo ""
    
    # Validate JSON log format
    if ! validate_json_log_format; then
        log_error "JSON log format validation failed"
    fi
    echo ""
    
    # Validate CP1 cross-cutting invariants
    if ! validate_cp1_invariants; then
        log_error "CP1 invariants validation failed"
    fi
    echo ""
    
    # Check health endpoints (optional - services may not be running)
    check_health_endpoints
    echo ""
    
    # Check for secrets
    check_secrets
    echo ""
    
    # Summary
    log_info "Validation complete"
    echo ""
    echo "Summary:" | tee -a "$LOG_FILE"
    echo "  Errors: $ERRORS" | tee -a "$LOG_FILE"
    echo "  Warnings: $WARNINGS" | tee -a "$LOG_FILE"
    echo "  Local errors: $LOCAL_ERRORS" | tee -a "$LOG_FILE"
    echo "  External errors: $EXTERNAL_ERRORS" | tee -a "$LOG_FILE"
    echo ""
    
    # Exit codes
    if [[ $LOCAL_ERRORS -gt 0 ]]; then
        log_error "Validation failed with $LOCAL_ERRORS local error(s)"
        echo "Exit code: 3 (local paths/configs missing or invalid)" >> "$LOG_FILE"
        exit 3
    elif [[ $EXTERNAL_ERRORS -gt 0 ]]; then
        log_warn "Validation passed with $EXTERNAL_ERRORS external error(s) (health endpoints unavailable)"
        log_warn "Note: This is expected if services are not running"
        echo "Exit code: 2 (external endpoints unavailable)" >> "$LOG_FILE"
        exit 2
    elif [[ $WARNINGS -gt 0 ]]; then
        log_warn "Validation passed with $WARNINGS warning(s)"
        echo "Exit code: 0 (success with warnings)" >> "$LOG_FILE"
        exit 0
    else
        log_info "Validation passed successfully"
        echo "Exit code: 0 (success)" >> "$LOG_FILE"
        exit 0
    fi
}

main "$@"
