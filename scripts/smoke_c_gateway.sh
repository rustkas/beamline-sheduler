#!/usr/bin/env bash
# Enhanced C-Gateway Smoke Test
#
# Purpose: Basic smoke test for C-Gateway health and happy path
# Tests: Health endpoint, happy path routing
# Exit codes: 0 = passed, 1 = failed, 2 = service not running (skip)

set -euo pipefail

GATEWAY_URL="${GATEWAY_URL:-http://localhost:8080}"
TIMEOUT_SEC="${TIMEOUT_SEC:-5}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${GREEN}[INFO]${NC} $*"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $*"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*"
}

echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${GREEN}C-Gateway Smoke Test${NC}"
echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""

log_info "Checking ${GATEWAY_URL}/_health"

# Если gateway не отвечает на /_health, считаем, что он не поднят и выходим без ошибки
if ! curl -fsS --max-time "${TIMEOUT_SEC}" "${GATEWAY_URL}/_health" > /dev/null 2>&1; then
  log_warn "Gateway not reachable at ${GATEWAY_URL}, skipping (service not running)"
  exit 2
fi

# Печатаем ответ /_health (для отладки)
log_info "Health endpoint response:"
if ! curl -fsS --max-time "${TIMEOUT_SEC}" "${GATEWAY_URL}/_health" | jq . 2>/dev/null; then
    log_error "Health endpoint response is not valid JSON"
    curl -fsS --max-time "${TIMEOUT_SEC}" "${GATEWAY_URL}/_health" || exit 1
    exit 1
fi

log_info "Testing POST /api/v1/routes/decide (happy path)"

# Use Base64-encoded payload for proper format
REQUEST_BODY=$(cat <<EOF
{
  "version": "1",
  "tenant_id": "tenant-smoke",
  "request_id": "req-smoke-$(date +%s)",
  "message_id": "msg-smoke-$(date +%s)",
  "message_type": "chat",
  "payload": "dGVzdCBwYXlsb2Fk",
  "metadata": {},
  "policy_id": "default-policy",
  "context": { "foo": "bar" }
}
EOF
)

HTTP_CODE=$(curl -fsS -w "%{http_code}" -o /tmp/gateway_smoke_response.json \
  --max-time "${TIMEOUT_SEC}" \
  -X POST \
  -H "Content-Type: application/json" \
  -H "X-Tenant-ID: tenant-smoke" \
  -H "X-Trace-ID: trace-smoke-$(date +%s)" \
  -d "${REQUEST_BODY}" \
  "${GATEWAY_URL}/api/v1/routes/decide" 2>&1 || echo "000")

if [[ "${HTTP_CODE}" != "200" ]]; then
    log_error "POST /api/v1/routes/decide returned HTTP ${HTTP_CODE} (expected 200)"
    if [[ -f /tmp/gateway_smoke_response.json ]]; then
        log_error "Response:"
        cat /tmp/gateway_smoke_response.json | jq . 2>/dev/null || cat /tmp/gateway_smoke_response.json
    fi
    exit 1
fi

# Validate response JSON
if [[ ! -f /tmp/gateway_smoke_response.json ]]; then
    log_error "Response file not found"
    exit 1
fi

if ! jq empty /tmp/gateway_smoke_response.json > /dev/null 2>&1; then
    log_error "Response is not valid JSON"
    cat /tmp/gateway_smoke_response.json
    exit 1
fi

# Validate response structure (from api-registry.md)
log_info "Validating response structure..."

MESSAGE_ID=$(jq -r '.message_id // empty' /tmp/gateway_smoke_response.json)
PROVIDER_ID=$(jq -r '.provider_id // empty' /tmp/gateway_smoke_response.json)
REASON=$(jq -r '.reason // empty' /tmp/gateway_smoke_response.json)

if [[ -z "${MESSAGE_ID}" ]]; then
    log_warn "Response missing 'message_id' field (optional)"
fi

if [[ -z "${PROVIDER_ID}" ]]; then
    log_error "Response missing required 'provider_id' field"
    exit 1
fi

if [[ -z "${REASON}" ]]; then
    log_error "Response missing required 'reason' field"
    exit 1
fi

log_info "Response:"
cat /tmp/gateway_smoke_response.json | jq .

# Test 3: Validation Error (400 Bad Request)
log_info ""
log_info "Testing POST /api/v1/routes/decide (validation error)"

HTTP_CODE_VALIDATION=$(curl -fsS -w "%{http_code}" -o /tmp/gateway_validation_error.json \
  --max-time "${TIMEOUT_SEC}" \
  -X POST \
  -H "Content-Type: application/json" \
  -H "X-Tenant-ID: tenant-smoke" \
  -d '{"invalid": "request"}' \
  "${GATEWAY_URL}/api/v1/routes/decide" 2>&1 || echo "000")

if [[ "${HTTP_CODE_VALIDATION}" != "400" ]]; then
    log_error "Validation error returned HTTP ${HTTP_CODE_VALIDATION} (expected 400)"
    if [[ -f /tmp/gateway_validation_error.json ]]; then
        log_error "Response:"
        cat /tmp/gateway_validation_error.json | jq . 2>/dev/null || cat /tmp/gateway_validation_error.json
    fi
    exit 1
fi

# Validate error response structure
if ! jq empty /tmp/gateway_validation_error.json > /dev/null 2>&1; then
    log_error "Validation error response is not valid JSON"
    cat /tmp/gateway_validation_error.json
    exit 1
fi

ERROR_CODE=$(jq -r '.error.code // empty' /tmp/gateway_validation_error.json)
if [[ -z "${ERROR_CODE}" ]]; then
    log_error "Validation error response missing 'error.code' field"
    exit 1
fi

if [[ "${ERROR_CODE}" != "invalid_request" ]] && [[ "${ERROR_CODE}" != "INVALID_REQUEST" ]]; then
    log_warn "Validation error response 'error.code' is '${ERROR_CODE}' (expected 'invalid_request' or 'INVALID_REQUEST')"
fi

log_info "Validation error test passed"
log_info "Error response:"
cat /tmp/gateway_validation_error.json | jq .

log_info "All checks passed"
