#!/usr/bin/env bash
# Worker Observability Test Script
#
# Purpose: Test Worker observability with real HTTP requests
# Tests: Health endpoints with CP1 format validation
# Exit codes: 0 = passed, 1 = failed, 2 = service not running (skip)

set -euo pipefail

WORKER_URL="${WORKER_URL:-http://localhost:9091}"
TIMEOUT_SEC="${TIMEOUT_SEC:-10}"

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
echo -e "${GREEN}Worker Observability Test${NC}"
echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""
log_info "Worker URL: ${WORKER_URL}"
log_info "Default port: 9091 (prometheus_port + 1)"
echo ""

# Test 1: Health Endpoint (GET /_health) - CP1 Format Validation
log_test "Test 1: Health Endpoint (GET /_health) - CP1 Format Validation"
if ! curl -fsS --max-time "${TIMEOUT_SEC}" "${WORKER_URL}/_health" > /tmp/worker_health.json 2>&1; then
    log_warn "Worker not reachable at ${WORKER_URL}/_health, skipping (service not running)"
    exit 2
fi

# Validate health response JSON
if ! jq empty /tmp/worker_health.json > /dev/null 2>&1; then
    log_error "Health endpoint response is not valid JSON"
    cat /tmp/worker_health.json
    exit 1
fi

# Check required fields
STATUS=$(jq -r '.status // empty' /tmp/worker_health.json)
TIMESTAMP=$(jq -r '.timestamp // empty' /tmp/worker_health.json)

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
        
        # Check for microseconds (6 digits) - CP1 requirement
        if echo "${TIMESTAMP}" | grep -qE '\.[0-9]{6}Z$'; then
            log_info "✓ Health endpoint timestamp has microseconds (6 digits) - CP1 compliant"
        elif echo "${TIMESTAMP}" | grep -qE '\.[0-9]+Z$'; then
            log_warn "Health endpoint timestamp has fractional seconds but not 6 digits (CP1 recommends 6 digits for microseconds)"
        else
            log_warn "Health endpoint timestamp has no fractional seconds (CP1 recommends microseconds with 6 digits)"
        fi
    else
        log_error "Health endpoint timestamp format invalid: '${TIMESTAMP}' (expected ISO 8601 format: YYYY-MM-DDTHH:MM:SS[.ssssss]Z)"
        exit 1
    fi
else
    log_error "Health endpoint timestamp field is empty"
    exit 1
fi

# Check for optional checks field
CHECKS=$(jq -r '.checks // empty' /tmp/worker_health.json)
if [[ -n "${CHECKS}" ]] && [[ "${CHECKS}" != "null" ]]; then
    log_info "✓ Health endpoint contains optional 'checks' field"
    CHECKS_COUNT=$(echo "${CHECKS}" | jq 'length' 2>/dev/null || echo "0")
    if [[ "${CHECKS_COUNT}" -gt 0 ]]; then
        log_info "  Checks count: ${CHECKS_COUNT}"
    fi
fi

log_info "Health endpoint response (CP1 compliant):"
cat /tmp/worker_health.json | jq .
((PASSED++)) || true
echo ""

# Test 2: HTTP Status Code Validation
log_test "Test 2: HTTP Status Code Validation"
HTTP_CODE=$(curl -sS -w "\n%{http_code}" -o /tmp/worker_health_status.json \
  --max-time "${TIMEOUT_SEC}" \
  "${WORKER_URL}/_health" 2>&1 | tail -1 || echo "000")

# Extract response body (everything except last line which is HTTP code)
if [[ -f /tmp/worker_health_status.json ]]; then
    head -n -1 /tmp/worker_health_status.json > /tmp/worker_health_body.json 2>/dev/null || true
fi

# Clean HTTP code (remove any extra characters)
HTTP_CODE_CLEAN=$(echo "${HTTP_CODE}" | grep -oE '[0-9]{3}' | head -1 || echo "${HTTP_CODE}")

if [[ "${HTTP_CODE_CLEAN}" == "200" ]]; then
    log_info "✓ Health endpoint returned HTTP 200 OK (CP1 compliant)"
    ((PASSED++)) || true
else
    log_error "Health endpoint returned HTTP ${HTTP_CODE_CLEAN} (expected 200 OK)"
    exit 1
fi
echo ""

# Test 3: Response Content-Type Validation
log_test "Test 3: Response Content-Type Validation"
CONTENT_TYPE=$(curl -sS -I --max-time "${TIMEOUT_SEC}" "${WORKER_URL}/_health" 2>&1 | \
  grep -i "content-type:" | head -1 | cut -d: -f2 | tr -d '\r\n' | xargs || echo "")

if [[ -n "${CONTENT_TYPE}" ]]; then
    if echo "${CONTENT_TYPE}" | grep -qi "application/json"; then
        log_info "✓ Health endpoint Content-Type is 'application/json' (CP1 compliant)"
        ((PASSED++)) || true
    else
        log_warn "Health endpoint Content-Type is '${CONTENT_TYPE}' (expected 'application/json')"
    fi
else
    log_warn "Could not determine Content-Type from response headers"
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

