#!/bin/bash
# Test script for distributed rate limiting with Redis backend
# 
# Tests multi-instance rate limiting with Redis backend.
# This script is part of the official Gateway test suite.
# 
# Configuration:
# - GATEWAY_RATE_LIMIT_REDIS_HOST (default: localhost)
# - GATEWAY_RATE_LIMIT_REDIS_PORT (default: 6379)

set -euo pipefail

REDIS_HOST="${GATEWAY_RATE_LIMIT_REDIS_HOST:-localhost}"
REDIS_PORT="${GATEWAY_RATE_LIMIT_REDIS_PORT:-6379}"
GATEWAY_1_URL="http://localhost:8081"
GATEWAY_2_URL="http://localhost:8082"
GATEWAY_3_URL="http://localhost:8083"
RATE_LIMIT=10  # Requests per minute
TENANT_ID="test-tenant-$(date +%s)"

echo "=== Distributed Rate Limiting Test ==="
echo "Redis: ${REDIS_HOST}:${REDIS_PORT}"
echo "Rate Limit: ${RATE_LIMIT} req/min"
echo "Tenant ID: ${TENANT_ID}"
echo ""

# Check Redis availability
if ! command -v redis-cli &> /dev/null; then
    echo "⚠️  redis-cli not found. Install Redis client tools."
    exit 1
fi

if ! redis-cli -h "${REDIS_HOST}" -p "${REDIS_PORT}" ping > /dev/null 2>&1; then
    echo "⚠️  Redis not available at ${REDIS_HOST}:${REDIS_PORT}"
    echo "   Start Redis: docker-compose -f docker-compose.rate-limit-test.yml up redis"
    exit 1
fi

echo "✅ Redis is available"
echo ""

# Test: Send requests to multiple Gateway instances
echo "Test: Multi-instance rate limiting"
echo "  Sending ${RATE_LIMIT} requests distributed across 3 Gateway instances..."
echo ""

SUCCESS_COUNT=0
EXCEEDED_COUNT=0
ERROR_COUNT=0

# Send requests round-robin to 3 instances
for i in $(seq 1 $((RATE_LIMIT + 5))); do
    # Round-robin: gateway-1, gateway-2, gateway-3
    case $((i % 3)) in
        1) GATEWAY_URL="${GATEWAY_1_URL}" ;;
        2) GATEWAY_URL="${GATEWAY_2_URL}" ;;
        0) GATEWAY_URL="${GATEWAY_3_URL}" ;;
    esac
    
    # Send request
    RESPONSE=$(curl -s -w "\n%{http_code}" \
        -X POST "${GATEWAY_URL}/api/v1/routes/decide" \
        -H "Content-Type: application/json" \
        -H "X-Tenant-ID: ${TENANT_ID}" \
        -d "{\"version\":\"1\",\"tenant_id\":\"${TENANT_ID}\",\"request_id\":\"req-${i}\",\"task\":{\"type\":\"text.generate\",\"payload\":{}}}" \
        2>/dev/null || echo -e "\n000")
    
    HTTP_CODE=$(echo "${RESPONSE}" | tail -n 1)
    BODY=$(echo "${RESPONSE}" | head -n -1)
    
    case "${HTTP_CODE}" in
        200|201|202)
            SUCCESS_COUNT=$((SUCCESS_COUNT + 1))
            echo "  Request ${i} (${GATEWAY_URL}): ✅ ALLOWED (HTTP ${HTTP_CODE})"
            ;;
        429)
            EXCEEDED_COUNT=$((EXCEEDED_COUNT + 1))
            echo "  Request ${i} (${GATEWAY_URL}): ❌ RATE LIMIT EXCEEDED (HTTP 429)"
            ;;
        503)
            ERROR_COUNT=$((ERROR_COUNT + 1))
            echo "  Request ${i} (${GATEWAY_URL}): ⚠️  SERVICE UNAVAILABLE (HTTP 503)"
            ;;
        *)
            ERROR_COUNT=$((ERROR_COUNT + 1))
            echo "  Request ${i} (${GATEWAY_URL}): ⚠️  ERROR (HTTP ${HTTP_CODE})"
            ;;
    esac
    
    # Small delay to avoid overwhelming
    sleep 0.1
done

echo ""
echo "=== Test Results ==="
echo "  Total Requests: $((RATE_LIMIT + 5))"
echo "  Allowed (2xx): ${SUCCESS_COUNT}"
echo "  Rate Limited (429): ${EXCEEDED_COUNT}"
echo "  Errors (5xx): ${ERROR_COUNT}"
echo ""

# Verify consistency
if [ "${SUCCESS_COUNT}" -le "${RATE_LIMIT}" ]; then
    echo "✅ Rate limit enforced correctly (${SUCCESS_COUNT} <= ${RATE_LIMIT})"
else
    echo "❌ Rate limit not enforced correctly (${SUCCESS_COUNT} > ${RATE_LIMIT})"
    exit 1
fi

# Check Redis keys
echo ""
echo "=== Redis Keys (for debugging) ==="
redis-cli -h "${REDIS_HOST}" -p "${REDIS_PORT}" KEYS "rate_limit:*" | head -10

echo ""
echo "✅ Multi-instance rate limiting test passed"

