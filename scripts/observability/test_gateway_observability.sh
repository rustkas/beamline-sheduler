#!/usr/bin/env bash
# Gateway Observability Test Script
#
# Purpose: Test Gateway observability with real HTTP requests
# Tests: Health endpoints, metrics endpoints, API endpoints with observability validation
# Exit codes: 0 = passed, 1 = failed, 2 = service not running (skip)

set -euo pipefail

GATEWAY_URL="${GATEWAY_URL:-http://localhost:3000}"
TIMEOUT_SEC="${TIMEOUT_SEC:-10}"
TEST_TENANT_ID="${TEST_TENANT_ID:-tenant-test-$(date +%s)}"
TEST_TRACE_ID="${TEST_TRACE_ID:-trace-test-$(date +%s)}"
TEST_RUN_ID="${TEST_RUN_ID:-run-test-$(date +%s)}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
PASSED=0
FAILED=0
WARNINGS=0

log_info() {
    echo -e "${GREEN}[INFO]${NC} $*"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $*"
    ((WARNINGS++)) || true
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*"
    ((FAILED++)) || true
}

log_test() {
    echo -e "${BLUE}[TEST]${NC} $*"
}

# Check if jq is available
if ! command -v jq &> /dev/null; then
    log_error "jq is required but not installed. Please install jq first."
    exit 1
fi

# Check if curl is available
if ! command -v curl &> /dev/null; then
    log_error "curl is required but not installed. Please install curl first."
    exit 1
fi

echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${GREEN}Gateway Observability Test${NC}"
echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""
log_info "Gateway URL: ${GATEWAY_URL}"
log_info "Test Tenant ID: ${TEST_TENANT_ID}"
log_info "Test Trace ID: ${TEST_TRACE_ID}"
log_info "Test Run ID: ${TEST_RUN_ID}"
echo ""

# Test 1: Health Endpoint (GET /health) - CP1 Format Validation
log_test "Test 1: Health Endpoint (GET /health) - CP1 Format Validation"
if ! curl -fsS --max-time "${TIMEOUT_SEC}" "${GATEWAY_URL}/health" > /tmp/gateway_health.json 2>&1; then
    log_warn "Gateway not reachable at ${GATEWAY_URL}/health, trying /_health"
    if ! curl -fsS --max-time "${TIMEOUT_SEC}" "${GATEWAY_URL}/_health" > /tmp/gateway_health.json 2>&1; then
        log_warn "Gateway not reachable at ${GATEWAY_URL}/_health, skipping (service not running)"
        exit 2
    fi
fi

# Validate health response JSON
if ! jq empty /tmp/gateway_health.json > /dev/null 2>&1; then
    log_error "Health endpoint response is not valid JSON"
    cat /tmp/gateway_health.json
    exit 1
fi

# Check required fields
STATUS=$(jq -r '.status // empty' /tmp/gateway_health.json)
TIMESTAMP=$(jq -r '.timestamp // empty' /tmp/gateway_health.json)

if [[ -z "${STATUS}" ]]; then
    log_error "Health endpoint missing required 'status' field"
    exit 1
fi

if [[ -z "${TIMESTAMP}" ]]; then
    log_error "Health endpoint missing required 'timestamp' field"
    exit 1
fi

# Validate status value (CP1 compliance: must be "healthy", not "ok")
if [[ "${STATUS}" == "healthy" ]]; then
    log_info "✓ Health endpoint status is 'healthy' (CP1 compliant)"
elif [[ "${STATUS}" == "ok" ]]; then
    log_error "Health endpoint status is 'ok' (should be 'healthy' for CP1 compliance)"
    exit 1
elif [[ "${STATUS}" == "degraded" ]] || [[ "${STATUS}" == "unhealthy" ]]; then
    log_warn "Health endpoint status is '${STATUS}' (acceptable but not 'healthy')"
else
    log_error "Health endpoint 'status' value is '${STATUS}' (expected: healthy, degraded, or unhealthy)"
    exit 1
fi

# Validate timestamp format (ISO 8601)
if [[ -n "${TIMESTAMP}" ]]; then
    # Basic ISO 8601 format check: YYYY-MM-DDTHH:MM:SS[.ssssss]Z
    if echo "${TIMESTAMP}" | grep -qE '^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\.[0-9]{1,6})?Z$'; then
        log_info "✓ Health endpoint timestamp is valid ISO 8601 format: ${TIMESTAMP}"
    else
        log_error "Health endpoint timestamp format invalid: '${TIMESTAMP}' (expected ISO 8601 format: YYYY-MM-DDTHH:MM:SS[.ssssss]Z)"
        exit 1
    fi
else
    log_error "Health endpoint timestamp field is empty"
    exit 1
fi

log_info "Health endpoint response (CP1 compliant):"
cat /tmp/gateway_health.json | jq .
((PASSED++)) || true
echo ""

# Test 2: Health Endpoint Alternative (GET /_health) - CP1 Format Validation
log_test "Test 2: Health Endpoint Alternative (GET /_health) - CP1 Format Validation"
if curl -fsS --max-time "${TIMEOUT_SEC}" "${GATEWAY_URL}/_health" > /tmp/gateway_health_alt.json 2>&1; then
    if ! jq empty /tmp/gateway_health_alt.json > /dev/null 2>&1; then
        log_error "Alternative health endpoint response is not valid JSON"
        cat /tmp/gateway_health_alt.json
        exit 1
    fi
    
    # Validate CP1 format for alternative endpoint
    STATUS_ALT=$(jq -r '.status // empty' /tmp/gateway_health_alt.json)
    TIMESTAMP_ALT=$(jq -r '.timestamp // empty' /tmp/gateway_health_alt.json)
    
    if [[ "${STATUS_ALT}" == "healthy" ]]; then
        log_info "✓ Alternative health endpoint status is 'healthy' (CP1 compliant)"
    elif [[ "${STATUS_ALT}" == "ok" ]]; then
        log_error "Alternative health endpoint status is 'ok' (should be 'healthy' for CP1 compliance)"
        exit 1
    else
        log_warn "Alternative health endpoint status is '${STATUS_ALT}'"
    fi
    
    if [[ -n "${TIMESTAMP_ALT}" ]] && echo "${TIMESTAMP_ALT}" | grep -qE '^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\.[0-9]{1,6})?Z$'; then
        log_info "✓ Alternative health endpoint timestamp is valid ISO 8601 format"
    else
        log_warn "Alternative health endpoint timestamp format may be invalid: '${TIMESTAMP_ALT}'"
    fi
    
    log_info "Alternative health endpoint response:"
    cat /tmp/gateway_health_alt.json | jq .
    ((PASSED++)) || true
else
    log_warn "Alternative health endpoint (/_health) not available"
fi
echo ""

# Test 3: Metrics Endpoint (GET /metrics)
log_test "Test 3: Metrics Endpoint (GET /metrics)"
if curl -fsS --max-time "${TIMEOUT_SEC}" "${GATEWAY_URL}/metrics" > /tmp/gateway_metrics.txt 2>&1; then
    METRICS_SIZE=$(wc -c < /tmp/gateway_metrics.txt | tr -d ' ')
    if [[ "${METRICS_SIZE}" -gt 0 ]]; then
        log_info "Metrics endpoint returned ${METRICS_SIZE} bytes"
        log_info "First 20 lines of metrics:"
        head -20 /tmp/gateway_metrics.txt
        ((PASSED++)) || true
    else
        log_warn "Metrics endpoint returned empty response"
    fi
else
    log_warn "Metrics endpoint (/metrics) not available"
fi
echo ""

# Test 4: Metrics JSON Endpoint (GET /_metrics)
log_test "Test 4: Metrics JSON Endpoint (GET /_metrics)"
if curl -fsS --max-time "${TIMEOUT_SEC}" "${GATEWAY_URL}/_metrics" > /tmp/gateway_metrics_json.json 2>&1; then
    if jq empty /tmp/gateway_metrics_json.json > /dev/null 2>&1; then
        log_info "Metrics JSON endpoint response:"
        cat /tmp/gateway_metrics_json.json | jq .
        ((PASSED++)) || true
    else
        log_warn "Metrics JSON endpoint response is not valid JSON"
        cat /tmp/gateway_metrics_json.json
    fi
else
    log_warn "Metrics JSON endpoint (/_metrics) not available"
fi
echo ""

# Test 5: POST /api/v1/routes/decide (Happy Path with Observability)
log_test "Test 5: POST /api/v1/routes/decide (Happy Path with Observability)"
REQUEST_BODY=$(cat <<EOF
{
  "message": {
    "message_id": "msg-$(date +%s)",
    "tenant_id": "${TEST_TENANT_ID}",
    "trace_id": "${TEST_TRACE_ID}",
    "message_type": "chat",
    "payload": "dGVzdCBwYXlsb2Fk",
    "metadata": {},
    "timestamp_ms": $(date +%s)000
  },
  "policy_id": "default-policy",
  "context": {
    "user_id": "user_001"
  }
}
EOF
)

HTTP_CODE=$(curl -sS -w "\n%{http_code}" -o /tmp/gateway_decide_response_full.txt \
  --max-time "${TIMEOUT_SEC}" \
  -X POST \
  -H "Content-Type: application/json" \
  -H "X-Tenant-ID: ${TEST_TENANT_ID}" \
  -H "X-Trace-ID: ${TEST_TRACE_ID}" \
  -H "X-Run-ID: ${TEST_RUN_ID}" \
  -d "${REQUEST_BODY}" \
  "${GATEWAY_URL}/api/v1/routes/decide" 2>&1 | tail -1 || echo "000")

# Extract response body (everything except last line which is HTTP code)
if [[ -f /tmp/gateway_decide_response_full.txt ]]; then
    head -n -1 /tmp/gateway_decide_response_full.txt > /tmp/gateway_decide_response.json 2>/dev/null || true
fi

# Clean HTTP code (remove any extra characters)
HTTP_CODE_CLEAN=$(echo "${HTTP_CODE}" | grep -oE '[0-9]{3}' | head -1 || echo "${HTTP_CODE}")

if [[ "${HTTP_CODE_CLEAN}" == "200" ]]; then
    # Validate response JSON
    if ! jq empty /tmp/gateway_decide_response.json > /dev/null 2>&1; then
        log_error "Decide endpoint response is not valid JSON"
        cat /tmp/gateway_decide_response.json
        exit 1
    fi
    
    # Check for observability fields in response
    RESPONSE_TRACE_ID=$(jq -r '.context.trace_id // .trace_id // empty' /tmp/gateway_decide_response.json)
    RESPONSE_REQUEST_ID=$(jq -r '.context.request_id // .request_id // empty' /tmp/gateway_decide_response.json)
    
    if [[ -n "${RESPONSE_TRACE_ID}" ]]; then
        log_info "Response contains trace_id: ${RESPONSE_TRACE_ID}"
        if [[ "${RESPONSE_TRACE_ID}" == "${TEST_TRACE_ID}" ]]; then
            log_info "✓ Trace ID matches request (${TEST_TRACE_ID})"
        else
            log_warn "Trace ID mismatch: expected ${TEST_TRACE_ID}, got ${RESPONSE_TRACE_ID}"
        fi
    else
        log_warn "Response missing trace_id field"
    fi
    
    log_info "Decide endpoint response:"
    cat /tmp/gateway_decide_response.json | jq .
    ((PASSED++)) || true
else
    log_error "Decide endpoint returned HTTP ${HTTP_CODE_CLEAN} (expected 200)"
    if [[ -f /tmp/gateway_decide_response.json ]]; then
        log_error "Response:"
        cat /tmp/gateway_decide_response.json | jq . 2>/dev/null || cat /tmp/gateway_decide_response.json
    fi
    # Also show full response for debugging
    if [[ -f /tmp/gateway_decide_response_full.txt ]]; then
        log_error "Full response:"
        cat /tmp/gateway_decide_response_full.txt
    fi
fi
echo ""

# Test 6: POST /api/v1/messages (Happy Path with Observability)
log_test "Test 6: POST /api/v1/messages (Happy Path with Observability)"
MESSAGE_BODY=$(cat <<EOF
{
  "tenant_id": "${TEST_TENANT_ID}",
  "message_type": "chat",
  "payload": "{\"text\": \"Hello, world!\"}",
  "trace_id": "${TEST_TRACE_ID}",
  "metadata": {
    "source": "test",
    "run_id": "${TEST_RUN_ID}"
  }
}
EOF
)

HTTP_CODE_MSG=$(curl -sS -w "\n%{http_code}" -o /tmp/gateway_message_response_full.txt \
  --max-time "${TIMEOUT_SEC}" \
  -X POST \
  -H "Content-Type: application/json" \
  -H "X-Tenant-ID: ${TEST_TENANT_ID}" \
  -H "X-Trace-ID: ${TEST_TRACE_ID}" \
  -H "X-Run-ID: ${TEST_RUN_ID}" \
  -d "${MESSAGE_BODY}" \
  "${GATEWAY_URL}/api/v1/messages" 2>&1 | tail -1 || echo "000")

# Extract response body (everything except last line which is HTTP code)
if [[ -f /tmp/gateway_message_response_full.txt ]]; then
    head -n -1 /tmp/gateway_message_response_full.txt > /tmp/gateway_message_response.json 2>/dev/null || true
fi

if [[ "${HTTP_CODE_MSG}" == "200" ]] || [[ "${HTTP_CODE_MSG}" == "201" ]]; then
    # Validate response JSON
    if ! jq empty /tmp/gateway_message_response.json > /dev/null 2>&1; then
        log_error "Messages endpoint response is not valid JSON"
        cat /tmp/gateway_message_response.json
        exit 1
    fi
    
    # Check for observability fields
    RESPONSE_MSG_ID=$(jq -r '.message_id // empty' /tmp/gateway_message_response.json)
    RESPONSE_ACK_TS=$(jq -r '.ack_timestamp_ms // empty' /tmp/gateway_message_response.json)
    
    if [[ -n "${RESPONSE_MSG_ID}" ]]; then
        log_info "Response contains message_id: ${RESPONSE_MSG_ID}"
    else
        log_warn "Response missing message_id field"
    fi
    
    log_info "Messages endpoint response:"
    cat /tmp/gateway_message_response.json | jq .
    ((PASSED++)) || true
else
    log_warn "Messages endpoint returned HTTP ${HTTP_CODE_MSG} (expected 200 or 201)"
    if [[ -f /tmp/gateway_message_response.json ]]; then
        log_info "Response:"
        cat /tmp/gateway_message_response.json | jq . 2>/dev/null || cat /tmp/gateway_message_response.json
    fi
fi
echo ""

# Test 7: Validation Error (400 Bad Request)
log_test "Test 7: Validation Error (400 Bad Request)"
HTTP_CODE_VALIDATION=$(curl -sS -w "\n%{http_code}" -o /tmp/gateway_validation_error_full.txt \
  --max-time "${TIMEOUT_SEC}" \
  -X POST \
  -H "Content-Type: application/json" \
  -H "X-Tenant-ID: ${TEST_TENANT_ID}" \
  -H "X-Trace-ID: ${TEST_TRACE_ID}" \
  -d '{"invalid": "request"}' \
  "${GATEWAY_URL}/api/v1/routes/decide" 2>&1 | tail -1 || echo "000")

# Extract response body (everything except last line which is HTTP code)
if [[ -f /tmp/gateway_validation_error_full.txt ]]; then
    head -n -1 /tmp/gateway_validation_error_full.txt > /tmp/gateway_validation_error.json 2>/dev/null || true
fi

if [[ "${HTTP_CODE_VALIDATION}" == "400" ]] || [[ "${HTTP_CODE_VALIDATION}" =~ ^400[0-9]*$ ]]; then
    # Validate error response JSON
    if ! jq empty /tmp/gateway_validation_error.json > /dev/null 2>&1; then
        log_error "Validation error response is not valid JSON"
        cat /tmp/gateway_validation_error.json
        exit 1
    fi
    
    # Check error structure
    ERROR_CODE=$(jq -r '.error.code // empty' /tmp/gateway_validation_error.json)
    ERROR_MESSAGE=$(jq -r '.error.message // empty' /tmp/gateway_validation_error.json)
    ERROR_TRACE_ID=$(jq -r '.error.trace_id // empty' /tmp/gateway_validation_error.json)
    
    if [[ -n "${ERROR_CODE}" ]]; then
        log_info "Error code: ${ERROR_CODE}"
    fi
    
    if [[ -n "${ERROR_TRACE_ID}" ]]; then
        log_info "Error response contains trace_id: ${ERROR_TRACE_ID}"
    else
        log_warn "Error response missing trace_id field"
    fi
    
    log_info "Validation error response:"
    cat /tmp/gateway_validation_error.json | jq .
    ((PASSED++)) || true
else
    # Check if it's actually 400 but with extra characters
    if [[ "${HTTP_CODE_VALIDATION}" =~ 400 ]]; then
        log_info "Validation error correctly returned 400 (with extra output)"
        HTTP_CODE_VALIDATION="400"
    else
        log_warn "Validation error returned HTTP ${HTTP_CODE_VALIDATION} (expected 400)"
    fi
    if [[ -f /tmp/gateway_validation_error.json ]]; then
        log_info "Response:"
        cat /tmp/gateway_validation_error.json | jq . 2>/dev/null || cat /tmp/gateway_validation_error.json
    fi
fi
echo ""

# Test 8: Missing Tenant ID Header (400 Bad Request)
log_test "Test 8: Missing Tenant ID Header (400 Bad Request)"
HTTP_CODE_NO_TENANT=$(curl -sS -w "\n%{http_code}" -o /tmp/gateway_no_tenant_error_full.txt \
  --max-time "${TIMEOUT_SEC}" \
  -X POST \
  -H "Content-Type: application/json" \
  -H "X-Trace-ID: ${TEST_TRACE_ID}" \
  -d '{"version": "1", "tenant_id": "test", "request_id": "req-123"}' \
  "${GATEWAY_URL}/api/v1/routes/decide" 2>&1 | tail -1 || echo "000")

# Extract response body (everything except last line which is HTTP code)
if [[ -f /tmp/gateway_no_tenant_error_full.txt ]]; then
    head -n -1 /tmp/gateway_no_tenant_error_full.txt > /tmp/gateway_no_tenant_error.json 2>/dev/null || true
fi

if [[ "${HTTP_CODE_NO_TENANT}" == "400" ]] || [[ "${HTTP_CODE_NO_TENANT}" =~ ^400[0-9]*$ ]]; then
    log_info "Missing tenant header correctly returned 400"
    if jq empty /tmp/gateway_no_tenant_error.json > /dev/null 2>&1; then
        log_info "Error response:"
        cat /tmp/gateway_no_tenant_error.json | jq .
    fi
    ((PASSED++)) || true
else
    # Check if it's actually 400 but with extra characters
    if [[ "${HTTP_CODE_NO_TENANT}" =~ 400 ]]; then
        log_info "Missing tenant header correctly returned 400 (with extra output)"
        HTTP_CODE_NO_TENANT="400"
    else
        log_warn "Missing tenant header returned HTTP ${HTTP_CODE_NO_TENANT} (expected 400)"
    fi
fi
echo ""

# Summary
echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${GREEN}Test Summary${NC}"
echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""
echo -e "${GREEN}[INFO]${NC} Passed: ${PASSED}"
echo -e "${YELLOW}[WARN]${NC} Warnings: ${WARNINGS}"
echo -e "${RED}[ERROR]${NC} Failed: ${FAILED}"
echo ""

if [[ "${FAILED}" -gt 0 ]]; then
    log_error "Some tests failed. Please review the output above."
    exit 1
else
    if [[ "${WARNINGS}" -gt 0 ]]; then
        log_warn "Some tests had warnings, but all critical tests passed."
    else
        log_info "All tests passed successfully!"
    fi
    exit 0
fi

