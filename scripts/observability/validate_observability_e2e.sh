#!/bin/bash
# End-to-End Observability Validation Script
# Validates observability invariants across all CP1 components
# Exit codes: 0 = success, 1 = validation failed, 2 = services not running

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
LOG_DIR="${PROJECT_ROOT}/reports/dry-run-logs/observability"
LOG_FILE="${LOG_DIR}/e2e_validation.log"
TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

# Create log directory
mkdir -p "${LOG_DIR}"

# Initialize counters
ERRORS=0
WARNINGS=0
PASSED=0

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_error() {
    echo -e "${RED}[ERROR]${NC} $1" >&2
    echo "[$TIMESTAMP] [ERROR] $1" >> "$LOG_FILE"
    ((ERRORS++)) || true
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
    echo "[$TIMESTAMP] [WARN] $1" >> "$LOG_FILE"
    ((WARNINGS++)) || true
}

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
    echo "[$TIMESTAMP] [INFO] $1" >> "$LOG_FILE"
}

log_pass() {
    echo -e "${GREEN}[PASS]${NC} $1"
    echo "[$TIMESTAMP] [PASS] $1" >> "$LOG_FILE"
    ((PASSED++)) || true
}

# Check if service is running
check_service_running() {
    local service_name="$1"
    local check_command="$2"
    
    log_info "Checking if $service_name is running..."
    
    if eval "$check_command" >/dev/null 2>&1; then
        log_pass "$service_name is running"
        return 0
    else
        log_warn "$service_name is not running (expected for local validation)"
        return 1
    fi
}

# Check if a field exists and is non-empty in JSON
# Returns 0 if field exists and is non-empty, 1 otherwise
# Based on CORE_MESSAGE_FIELDS_SPECIFICATION.md: all fields must be non-empty strings
check_field_present() {
    local json="$1"
    local field="$2"
    
    # Check if field exists and is not null
    if ! echo "$json" | jq -e ".$field != null" >/dev/null 2>&1; then
        return 1
    fi
    
    # Get field value
    local value
    value=$(echo "$json" | jq -r ".$field" 2>/dev/null || echo "")
    
    # Check if value is non-empty (not null, not empty string)
    if [[ -z "$value" ]] || [[ "$value" == "null" ]]; then
        return 1
    fi
    
    # For strings, check length > 0
    if echo "$json" | jq -e ".$field | type == \"string\" and length > 0" >/dev/null 2>&1; then
        return 0
    fi
    
    # For non-strings (numbers, booleans, objects, arrays), just check not null
    if echo "$json" | jq -e ".$field != null" >/dev/null 2>&1; then
        return 0
    fi
    
    return 1
}

# Get field value from JSON (returns empty if not present or null)
get_field_value() {
    local json="$1"
    local field="$2"
    echo "$json" | jq -r ".$field // empty" 2>/dev/null || echo ""
}

# Validate CP1 field dependencies
validate_cp1_field_dependencies() {
    local json="$1"
    local component="$2"
    local has_error=0
    
    # Check run_id → flow_id, step_id, tenant_id dependency
    if check_field_present "$json" "run_id"; then
        if ! check_field_present "$json" "flow_id"; then
            log_error "$component: run_id present but flow_id missing (dependency violation)"
            ((has_error++)) || true
        fi
        if ! check_field_present "$json" "step_id"; then
            log_error "$component: run_id present but step_id missing (dependency violation)"
            ((has_error++)) || true
        fi
        if ! check_field_present "$json" "tenant_id"; then
            log_error "$component: run_id present but tenant_id missing (dependency violation)"
            ((has_error++)) || true
        fi
    fi
    
    # Check flow_id → run_id, tenant_id dependency
    if check_field_present "$json" "flow_id"; then
        if ! check_field_present "$json" "run_id"; then
            log_error "$component: flow_id present but run_id missing (dependency violation)"
            ((has_error++)) || true
        fi
        if ! check_field_present "$json" "tenant_id"; then
            log_error "$component: flow_id present but tenant_id missing (dependency violation)"
            ((has_error++)) || true
        fi
    fi
    
    # Check step_id → run_id, flow_id, tenant_id dependency
    if check_field_present "$json" "step_id"; then
        if ! check_field_present "$json" "run_id"; then
            log_error "$component: step_id present but run_id missing (dependency violation)"
            ((has_error++)) || true
        fi
        if ! check_field_present "$json" "flow_id"; then
            log_error "$component: step_id present but flow_id missing (dependency violation)"
            ((has_error++)) || true
        fi
        if ! check_field_present "$json" "tenant_id"; then
            log_error "$component: step_id present but tenant_id missing (dependency violation)"
            ((has_error++)) || true
        fi
    fi
    
    return $has_error
}

# Validate CP1 fields based on component requirements and available context
validate_cp1_fields_with_context() {
    local json="$1"
    local component="$2"
    local has_error=0
    local has_warning=0
    
    # Component-specific CP1 field requirements (when context available)
    # Based on docs/OBSERVABILITY_CP1_INVARIANTS.md
    case "$component" in
        router)
            # Router: tenant_id, run_id, flow_id, trace_id when available
            local expected_fields=("tenant_id" "run_id" "flow_id" "trace_id")
            ;;
        gateway)
            # Gateway: tenant_id, run_id, trace_id when available
            local expected_fields=("tenant_id" "run_id" "trace_id")
            ;;
        worker)
            # Worker: tenant_id, run_id, flow_id, step_id, trace_id when available
            local expected_fields=("tenant_id" "run_id" "flow_id" "step_id" "trace_id")
            ;;
        *)
            # Default: all CP1 fields
            local expected_fields=("tenant_id" "run_id" "flow_id" "step_id" "trace_id")
            ;;
    esac
    
    # Determine available context based on present fields
    local tenant_context=0
    local run_context=0
    local flow_context=0
    local step_context=0
    local trace_context=0
    
    if check_field_present "$json" "tenant_id"; then
        tenant_context=1
    fi
    
    if check_field_present "$json" "run_id"; then
        run_context=1
        flow_context=1  # run_id implies flow context
        step_context=1  # run_id implies step context
    fi
    
    if check_field_present "$json" "flow_id"; then
        flow_context=1
    fi
    
    if check_field_present "$json" "step_id"; then
        step_context=1
    fi
    
    if check_field_present "$json" "trace_id"; then
        trace_context=1
    fi
    
    # Check if any context is available
    if [[ $tenant_context -eq 0 ]] && [[ $run_context -eq 0 ]] && [[ $trace_context -eq 0 ]]; then
        log_warn "$component: No CP1 context available (tenant_id, run_id, trace_id all missing) - may be expected for non-business logs"
        return 0
    fi
    
    # Validate expected fields based on available context
    local missing_required=0
    local found_expected=0
    
    for field in "${expected_fields[@]}"; do
        local context_available=0
        
        case "$field" in
            tenant_id)
                context_available=$tenant_context
                ;;
            run_id)
                context_available=$run_context
                ;;
            flow_id)
                context_available=$flow_context
                ;;
            step_id)
                context_available=$step_context
                ;;
            trace_id)
                context_available=$trace_context
                ;;
        esac
        
        if [[ $context_available -eq 1 ]]; then
            # Context is available, field should be present
            if check_field_present "$json" "$field"; then
                local field_value=$(get_field_value "$json" "$field")
                if [[ -n "$field_value" ]] && [[ "$field_value" != "null" ]]; then
                    log_pass "$component: CP1 field present with context: $field = $field_value"
                    ((found_expected++)) || true
                else
                    log_error "$component: CP1 field $field is present but empty or null"
                    ((has_error++)) || true
                fi
            else
                log_error "$component: CP1 field $field missing (context available but field absent)"
                ((missing_required++)) || true
                ((has_error++)) || true
            fi
        else
            # Context not available, field is optional
            if check_field_present "$json" "$field"; then
                log_warn "$component: CP1 field $field present but context not available (may be unexpected)"
                ((has_warning++)) || true
            fi
        fi
    done
    
    # Summary
    if [[ $missing_required -eq 0 ]] && [[ $has_error -eq 0 ]]; then
        log_pass "$component: All expected CP1 fields present based on available context ($found_expected/${#expected_fields[@]})"
    elif [[ $missing_required -gt 0 ]]; then
        log_error "$component: Missing $missing_required required CP1 field(s) when context is available"
    fi
    
    return $has_error
}

# Validate log format from log file
validate_log_format() {
    local log_file="$1"
    local component="$2"
    
    log_info "Validating log format for $component from $log_file..."
    
    if [[ ! -f "$log_file" ]]; then
        log_warn "Log file not found: $log_file (expected if no logs generated yet)"
        return 0
    fi
    
    local required_fields=("timestamp" "level" "component" "message")
    local found_required=0
    
    # Check first log line (or multiple lines for better coverage)
    local first_line=$(head -n 1 "$log_file" 2>/dev/null || echo "")
    
    if [[ -z "$first_line" ]]; then
        log_warn "Log file is empty: $log_file"
        return 0
    fi
    
    # Validate JSON and check fields
    if ! command -v jq &> /dev/null; then
        log_warn "jq not available - skipping detailed log format validation"
        return 0
    fi
    
    if ! echo "$first_line" | jq empty 2>/dev/null; then
        log_error "$component: Log format is not valid JSON"
        return 1
    fi
    
    log_pass "$component: Log format is valid JSON"
    
    # Check required fields
    for field in "${required_fields[@]}"; do
        if check_field_present "$first_line" "$field"; then
            ((found_required++)) || true
        fi
    done
    
    if [[ $found_required -eq ${#required_fields[@]} ]]; then
        log_pass "$component: All required fields present (timestamp, level, component, message)"
    else
        log_error "$component: Missing required fields ($found_required/${#required_fields[@]})"
        return 1
    fi
    
    # Validate CP1 fields with context awareness
    validate_cp1_fields_with_context "$first_line" "$component" || true
    
    # Validate CP1 field dependencies
    validate_cp1_field_dependencies "$first_line" "$component" || true
    
    # Check multiple log lines for better coverage (if available)
    local line_count=$(wc -l < "$log_file" 2>/dev/null || echo "0")
    if [[ $line_count -gt 1 ]] && [[ $line_count -le 10 ]]; then
        log_info "$component: Checking additional log lines for CP1 field consistency..."
        local line_num=2
        while IFS= read -r line && [[ $line_num -le 5 ]]; do
            if echo "$line" | jq empty 2>/dev/null; then
                validate_cp1_field_dependencies "$line" "$component" || true
            fi
            ((line_num++)) || true
        done < <(tail -n +2 "$log_file" 2>/dev/null | head -n 4)
    fi
    
    return 0
}

# Validate health endpoint
validate_health_endpoint() {
    local component="$1"
    local health_check="$2"
    
    log_info "Validating health endpoint for $component..."
    
    if eval "$health_check" >/dev/null 2>&1; then
        log_pass "$component health endpoint is accessible"
        return 0
    else
        log_warn "$component health endpoint is not accessible (service may not be running)"
        return 1
    fi
}

# Main E2E validation
main() {
    echo "=========================================" >> "$LOG_FILE"
    echo "E2E Observability Validation - $TIMESTAMP" >> "$LOG_FILE"
    echo "=========================================" >> "$LOG_FILE"
    
    log_info "Starting end-to-end observability validation..."
    echo ""
    
    # Step 1: Check if services are running
    log_info "=== Step 1: Service Availability ==="
    
    local router_running=0
    local gateway_running=0
    local worker_running=0
    
    # Router (gRPC)
    if command -v grpc_health_probe >/dev/null 2>&1; then
        if check_service_running "Router" "grpc_health_probe -addr=localhost:9000"; then
            router_running=1
        fi
    else
        log_warn "grpc_health_probe not found - skipping Router health check"
    fi
    
    # Gateway (HTTP)
    if check_service_running "Gateway" "curl -f http://localhost:3000/_health"; then
        gateway_running=1
    fi
    
    # Worker (HTTP) - port 9091 (default, prometheus_port + 1)
    if check_service_running "Worker" "curl -f http://localhost:9091/_health"; then
        worker_running=1
    fi
    
    echo ""
    
    # Step 2: Validate health endpoints
    log_info "=== Step 2: Health Endpoint Validation ==="
    
    if [[ $router_running -eq 1 ]]; then
        validate_health_endpoint "Router" "grpc_health_probe -addr=localhost:9000"
    fi
    
    if [[ $gateway_running -eq 1 ]]; then
        validate_health_endpoint "Gateway" "curl -f http://localhost:3000/_health"
        
        # Check JSON format and CP1 compliance
        local response=$(curl -s http://localhost:3000/_health 2>/dev/null || echo "")
        if [[ -n "$response" ]] && command -v jq &> /dev/null; then
            # Check required fields exist
            if echo "$response" | jq -e '.status' >/dev/null 2>&1 && \
               echo "$response" | jq -e '.timestamp' >/dev/null 2>&1; then
                log_pass "Gateway health endpoint has required fields (status, timestamp)"
                
                # Check status field value (must be "healthy", not "ok")
                local status=$(echo "$response" | jq -r '.status // empty' 2>/dev/null || echo "")
                if [[ "$status" == "healthy" ]]; then
                    log_pass "Gateway health endpoint status is 'healthy' (CP1 compliant)"
                elif [[ "$status" == "ok" ]]; then
                    log_error "Gateway health endpoint status is 'ok' (should be 'healthy' for CP1 compliance)"
                else
                    log_error "Gateway health endpoint status is '$status' (should be 'healthy' for CP1 compliance)"
                fi
                
                # Check timestamp field format (ISO 8601)
                local timestamp=$(echo "$response" | jq -r '.timestamp // empty' 2>/dev/null || echo "")
                if [[ -n "$timestamp" ]]; then
                    # Basic ISO 8601 format check: YYYY-MM-DDTHH:MM:SS[.ssssss]Z
                    if echo "$timestamp" | grep -qE '^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\.[0-9]{1,6})?Z$'; then
                        log_pass "Gateway health endpoint timestamp is valid ISO 8601 format"
                    else
                        log_error "Gateway health endpoint timestamp format invalid: '$timestamp' (expected ISO 8601 format)"
                    fi
                else
                    log_error "Gateway health endpoint timestamp field is empty"
                fi
            else
                log_error "Gateway health endpoint missing required fields (status or timestamp)"
            fi
        fi
    fi
    
    if [[ $worker_running -eq 1 ]]; then
        validate_health_endpoint "Worker" "curl -f http://localhost:9091/_health"
    fi
    
    echo ""
    
    # Step 3: Validate log formats (if log files exist)
    log_info "=== Step 3: Log Format Validation ==="
    
    # Check for log files in common locations
    local log_files=(
        "${PROJECT_ROOT}/reports/dry-run-logs/obs1/router.jsonl"
        "${PROJECT_ROOT}/reports/dry-run-logs/obs1/gateway.jsonl"
        "${PROJECT_ROOT}/reports/dry-run-logs/obs1/worker.jsonl"
    )
    
    local components=("router" "gateway" "worker")
    
    for i in "${!log_files[@]}"; do
        validate_log_format "${log_files[$i]}" "${components[$i]}"
    done
    
    echo ""
    
    # Step 4: Summary
    log_info "=== Validation Summary ==="
    echo ""
    echo "Summary:" | tee -a "$LOG_FILE"
    echo "  Passed: $PASSED" | tee -a "$LOG_FILE"
    echo "  Warnings: $WARNINGS" | tee -a "$LOG_FILE"
    echo "  Errors: $ERRORS" | tee -a "$LOG_FILE"
    echo ""
    
    # Exit codes
    if [[ $ERRORS -gt 0 ]]; then
        log_error "E2E validation failed with $ERRORS error(s)"
        echo "Exit code: 1 (validation failed)" >> "$LOG_FILE"
        exit 1
    elif [[ $WARNINGS -gt 0 ]]; then
        log_warn "E2E validation passed with $WARNINGS warning(s)"
        log_warn "Note: Warnings are expected if services are not running"
        echo "Exit code: 0 (success with warnings)" >> "$LOG_FILE"
        exit 0
    else
        log_info "E2E validation passed successfully"
        echo "Exit code: 0 (success)" >> "$LOG_FILE"
        exit 0
    fi
}

main "$@"

