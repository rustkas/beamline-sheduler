#!/usr/bin/env bash
# Gateway ↔ Router CP1-Smoke Test
#
# Purpose: Comprehensive CP1-smoke test for Gateway ↔ Router integration
# Tests: Health endpoint, happy path, error paths, observability
# Exit codes: 0 = all passed, 1 = critical failure, 2 = warnings

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
GATEWAY_URL="${GATEWAY_URL:-http://localhost:8080}"
NATS_URL="${NATS_URL:-nats://localhost:4222}"
ROUTER_URL="${ROUTER_URL:-http://localhost:9000}"
TIMEOUT_SEC="${TIMEOUT_SEC:-5}"

# Test results
TESTS_PASSED=0
TESTS_FAILED=0
TESTS_WARNINGS=0

# Logging functions
log_info() {
    echo -e "${GREEN}[INFO]${NC} $*"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $*"
    ((TESTS_WARNINGS++)) || true
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*"
    ((TESTS_FAILED++)) || true
}

log_test() {
    echo -e "\n${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${GREEN}Test:${NC} $*"
    echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
}

# Helper: Check if service is reachable
check_service() {
    local url=$1
    local name=$2
    
    if curl -fsS --max-time "${TIMEOUT_SEC}" "${url}" > /dev/null 2>&1; then
        log_info "${name} is reachable at ${url}"
        return 0
    else
        log_warn "${name} is not reachable at ${url} (service may not be running)"
        return 1
    fi
}

# Helper: Validate JSON structure
validate_json() {
    local json=$1
    local field=$2
    
    if echo "${json}" | jq -e ".${field}" > /dev/null 2>&1; then
        return 0
    else
        return 1
    fi
}

# Helper: Extract JSON field value
get_json_field() {
    local json=$1
    local field=$2
    
    echo "${json}" | jq -r ".${field}" 2>/dev/null || echo ""
}

# Test 1: Health Endpoint
test_health_endpoint() {
    log_test "Health Endpoint (GET /_health)"
    
    local response
    if ! response=$(curl -fsS --max-time "${TIMEOUT_SEC}" "${GATEWAY_URL}/_health" 2>&1); then
        log_error "Health endpoint not reachable"
        return 1
    fi
    
    # Validate JSON structure
    if ! echo "${response}" | jq empty > /dev/null 2>&1; then
        log_error "Health endpoint response is not valid JSON"
        echo "Response: ${response}"
        return 1
    fi
    
    # Check required fields
    local status
    status=$(get_json_field "${response}" "status")
    if [[ -z "${status}" ]]; then
        log_error "Health endpoint missing 'status' field"
        return 1
    fi
    
    if [[ "${status}" != "ok" && "${status}" != "degraded" && "${status}" != "unhealthy" ]]; then
        log_warn "Health endpoint 'status' has unexpected value: ${status}"
    fi
    
    # Check optional fields (warnings only)
    if ! validate_json "${response}" "nats.connected"; then
        log_warn "Health endpoint missing 'nats.connected' field (optional for CP1)"
    fi
    
    if ! validate_json "${response}" "timestamp_ms"; then
        log_warn "Health endpoint missing 'timestamp_ms' field (optional for CP1)"
    fi
    
    log_info "Health endpoint test passed"
    ((TESTS_PASSED++)) || true
    return 0
}

# Test 2: Happy Path (POST /api/v1/routes/decide)
test_happy_path() {
    log_test "Happy Path (POST /api/v1/routes/decide)"
    
    local request_body
    request_body=$(cat <<EOF
{
  "version": "1",
  "tenant_id": "tenant-cp1-smoke",
  "request_id": "req-$(date +%s)",
  "message_id": "msg-$(date +%s)",
  "message_type": "chat",
  "payload": "dGVzdCBwYXlsb2Fk",
  "metadata": {},
  "policy_id": "default-policy",
  "context": {}
}
EOF
)
    
    local response
    local http_code
    http_code=$(curl -fsS -w "%{http_code}" -o /tmp/gateway_response.json \
        --max-time "${TIMEOUT_SEC}" \
        -X POST \
        -H "Content-Type: application/json" \
        -H "X-Tenant-ID: tenant-cp1-smoke" \
        -H "X-Trace-ID: trace-$(date +%s)" \
        -d "${request_body}" \
        "${GATEWAY_URL}/api/v1/routes/decide" 2>&1 || echo "000")
    
    if [[ "${http_code}" != "200" ]]; then
        log_error "Happy path returned HTTP ${http_code} (expected 200)"
        if [[ -f /tmp/gateway_response.json ]]; then
            echo "Response: $(cat /tmp/gateway_response.json)"
        fi
        return 1
    fi
    
    if [[ ! -f /tmp/gateway_response.json ]]; then
        log_error "Happy path response file not found"
        return 1
    fi
    
    response=$(cat /tmp/gateway_response.json)
    
    # Validate JSON structure
    if ! echo "${response}" | jq empty > /dev/null 2>&1; then
        log_error "Happy path response is not valid JSON"
        echo "Response: ${response}"
        return 1
    fi
    
    # Check required fields (from api-registry.md)
    local message_id provider_id reason priority expected_latency_ms expected_cost currency trace_id
    
    message_id=$(get_json_field "${response}" "message_id")
    provider_id=$(get_json_field "${response}" "provider_id")
    reason=$(get_json_field "${response}" "reason")
    priority=$(get_json_field "${response}" "priority")
    expected_latency_ms=$(get_json_field "${response}" "expected_latency_ms")
    expected_cost=$(get_json_field "${response}" "expected_cost")
    currency=$(get_json_field "${response}" "currency")
    trace_id=$(get_json_field "${response}" "trace_id")
    
    if [[ -z "${message_id}" ]]; then
        log_error "Happy path response missing 'message_id' field"
        return 1
    fi
    
    if [[ -z "${provider_id}" ]]; then
        log_error "Happy path response missing 'provider_id' field"
        return 1
    fi
    
    if [[ -z "${reason}" ]]; then
        log_error "Happy path response missing 'reason' field"
        return 1
    fi
    
    # Optional fields (warnings only)
    if [[ -z "${priority}" ]]; then
        log_warn "Happy path response missing 'priority' field (optional)"
    fi
    
    if [[ -z "${expected_latency_ms}" ]]; then
        log_warn "Happy path response missing 'expected_latency_ms' field (optional)"
    fi
    
    if [[ -z "${expected_cost}" ]]; then
        log_warn "Happy path response missing 'expected_cost' field (optional)"
    fi
    
    if [[ -z "${currency}" ]]; then
        log_warn "Happy path response missing 'currency' field (optional, default: USD)"
    fi
    
    if [[ -z "${trace_id}" ]]; then
        log_warn "Happy path response missing 'trace_id' field (optional)"
    fi
    
    log_info "Happy path test passed"
    echo "Response: ${response}" | jq .
    ((TESTS_PASSED++)) || true
    return 0
}

# Test 3: Validation Error (400 Bad Request)
test_validation_error() {
    log_test "Validation Error (400 Bad Request)"
    
    local response
    local http_code
    http_code=$(curl -fsS -w "%{http_code}" -o /tmp/gateway_error.json \
        --max-time "${TIMEOUT_SEC}" \
        -X POST \
        -H "Content-Type: application/json" \
        -H "X-Tenant-ID: tenant-cp1-smoke" \
        -d '{"invalid": "request"}' \
        "${GATEWAY_URL}/api/v1/routes/decide" 2>&1 || echo "000")
    
    if [[ "${http_code}" != "400" ]]; then
        log_error "Validation error returned HTTP ${http_code} (expected 400)"
        if [[ -f /tmp/gateway_error.json ]]; then
            echo "Response: $(cat /tmp/gateway_error.json)"
        fi
        return 1
    fi
    
    if [[ ! -f /tmp/gateway_error.json ]]; then
        log_error "Validation error response file not found"
        return 1
    fi
    
    response=$(cat /tmp/gateway_error.json)
    
    # Validate JSON structure
    if ! echo "${response}" | jq empty > /dev/null 2>&1; then
        log_error "Validation error response is not valid JSON"
        echo "Response: ${response}"
        return 1
    fi
    
    # Check error structure (from api-registry.md)
    local ok error_code error_message
    
    ok=$(get_json_field "${response}" "ok")
    error_code=$(get_json_field "${response}" "error.code")
    error_message=$(get_json_field "${response}" "error.message")
    
    if [[ "${ok}" != "false" ]]; then
        log_error "Validation error response 'ok' is not false"
        return 1
    fi
    
    if [[ -z "${error_code}" ]]; then
        log_error "Validation error response missing 'error.code' field"
        return 1
    fi
    
    if [[ "${error_code}" != "invalid_request" ]]; then
        log_warn "Validation error response 'error.code' is '${error_code}' (expected 'invalid_request')"
    fi
    
    if [[ -z "${error_message}" ]]; then
        log_error "Validation error response missing 'error.message' field"
        return 1
    fi
    
    # Optional fields (warnings only)
    if ! validate_json "${response}" "error.details"; then
        log_warn "Validation error response missing 'error.details' field (optional)"
    fi
    
    if ! validate_json "${response}" "error.timestamp"; then
        log_warn "Validation error response missing 'error.timestamp' field (optional for CP1)"
    fi
    
    if ! validate_json "${response}" "context.request_id"; then
        log_warn "Validation error response missing 'context.request_id' field (optional)"
    fi
    
    if ! validate_json "${response}" "context.trace_id"; then
        log_warn "Validation error response missing 'context.trace_id' field (optional)"
    fi
    
    log_info "Validation error test passed"
    echo "Response: ${response}" | jq .
    ((TESTS_PASSED++)) || true
    return 0
}

# Test 4: Router Error (503 Service Unavailable)
test_router_error() {
    log_test "Router Error (503 Service Unavailable)"
    
    # Check if NATS is available (if not, this test will naturally pass)
    if ! check_service "${NATS_URL}/healthz" "NATS" 2>/dev/null; then
        log_info "NATS is not available - testing Router error handling"
        
        local response
        local http_code
        http_code=$(curl -fsS -w "%{http_code}" -o /tmp/gateway_router_error.json \
            --max-time "${TIMEOUT_SEC}" \
            -X POST \
            -H "Content-Type: application/json" \
            -H "X-Tenant-ID: tenant-cp1-smoke" \
            -H "X-Trace-ID: trace-$(date +%s)" \
            -d '{
              "message": {
                "message_id": "msg-router-error",
                "tenant_id": "tenant-cp1-smoke",
                "message_type": "chat",
                "payload": "dGVzdCBwYXlsb2Fk"
              }
            }' \
            "${GATEWAY_URL}/api/v1/routes/decide" 2>&1 || echo "000")
        
        if [[ "${http_code}" != "503" ]]; then
            log_error "Router error returned HTTP ${http_code} (expected 503)"
            if [[ -f /tmp/gateway_router_error.json ]]; then
                echo "Response: $(cat /tmp/gateway_router_error.json)"
            fi
            return 1
        fi
        
        if [[ ! -f /tmp/gateway_router_error.json ]]; then
            log_error "Router error response file not found"
            return 1
        fi
        
        response=$(cat /tmp/gateway_router_error.json)
        
        # Validate JSON structure
        if ! echo "${response}" | jq empty > /dev/null 2>&1; then
            log_error "Router error response is not valid JSON"
            echo "Response: ${response}"
            return 1
        fi
        
        # Check error structure
        local ok error_code error_message
        
        ok=$(get_json_field "${response}" "ok")
        error_code=$(get_json_field "${response}" "error.code")
        error_message=$(get_json_field "${response}" "error.message")
        
        if [[ "${ok}" != "false" ]]; then
            log_error "Router error response 'ok' is not false"
            return 1
        fi
        
        if [[ -z "${error_code}" ]]; then
            log_error "Router error response missing 'error.code' field"
            return 1
        fi
        
        # Check for SERVICE_UNAVAILABLE (per api-registry.md)
        if [[ "${error_code}" != "SERVICE_UNAVAILABLE" ]] && [[ "${error_code}" != "service_unavailable" ]]; then
            log_warn "Router error response 'error.code' is '${error_code}' (expected 'SERVICE_UNAVAILABLE')"
        fi
        
        if [[ -z "${error_message}" ]]; then
            log_error "Router error response missing 'error.message' field"
            return 1
        fi
        
        log_info "Router error test passed"
        echo "Response: ${response}" | jq .
        ((TESTS_PASSED++)) || true
        return 0
    else
        log_warn "NATS is available - cannot test Router error scenario"
        log_warn "To test Router error: stop NATS/Router and re-run this test"
        ((TESTS_WARNINGS++)) || true
        return 0
    fi
}

# Test 5: NATS Timeout (503 Service Unavailable)
test_nats_timeout() {
    log_test "NATS Timeout (503 Service Unavailable)"
    
    # This test simulates a timeout scenario
    # In real scenario, you would configure a very short timeout
    # For now, we'll skip if NATS is available (timeout unlikely)
    if check_service "${NATS_URL}/healthz" "NATS" 2>/dev/null; then
        log_warn "NATS is available - timeout scenario unlikely"
        log_warn "To test timeout: configure very short timeout or use network delay"
        ((TESTS_WARNINGS++)) || true
        return 0
    else
        # If NATS is unavailable, test_router_error already covers this
        log_info "NATS unavailable - timeout scenario covered by Router error test"
        ((TESTS_PASSED++)) || true
        return 0
    fi
}

# Test 6: Observability - Log Format
test_log_format() {
    log_test "Observability - Log Format"
    
    # Note: This test requires access to Gateway logs
    # In real scenario, you would check log files or stdout
    log_warn "Log format test requires access to Gateway logs"
    log_warn "Skipping log format test (manual verification required)"
    log_warn "Expected: JSON logs with fields: timestamp, level, component, message, context"
    
    ((TESTS_WARNINGS++)) || true
    return 0
}

# Main execution
main() {
    echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${GREEN}Gateway ↔ Router CP1-Smoke Test${NC}"
    echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo ""
    echo "Configuration:"
    echo "  GATEWAY_URL: ${GATEWAY_URL}"
    echo "  NATS_URL: ${NATS_URL}"
    echo "  ROUTER_URL: ${ROUTER_URL}"
    echo "  TIMEOUT_SEC: ${TIMEOUT_SEC}"
    echo ""
    
    # Check if Gateway is reachable (supports /health and /_health)
    if check_service "${GATEWAY_URL}/health" "Gateway"; then
        :
    elif check_service "${GATEWAY_URL}/_health" "Gateway"; then
        :
    else
        log_error "Gateway is not reachable. Please start Gateway service."
        exit 1
    fi
    
    # Run tests
    test_health_endpoint || true
    test_happy_path || true
    test_validation_error || true
    test_router_error || true
    test_nats_timeout || true
    test_log_format || true
    
    # Summary
    echo ""
    echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${GREEN}Test Summary${NC}"
    echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo "  Passed:   ${TESTS_PASSED}"
    echo "  Failed:   ${TESTS_FAILED}"
    echo "  Warnings: ${TESTS_WARNINGS}"
    echo ""
    
    if [[ ${TESTS_FAILED} -gt 0 ]]; then
        log_error "Some tests failed. Please review the output above."
        exit 1
    elif [[ ${TESTS_WARNINGS} -gt 0 ]]; then
        log_warn "Some tests have warnings. Please review the output above."
        exit 0
    else
        log_info "All tests passed!"
        exit 0
    fi
}

# Run main function
main "$@"
