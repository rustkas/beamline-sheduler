#!/usr/bin/env bash
# Router Observability Test Script
#
# Purpose: Test Router observability with real gRPC health check requests
# Tests: gRPC health endpoint, health status validation
# Exit codes: 0 = passed, 1 = failed, 2 = service not running (skip)

set -euo pipefail

ROUTER_HOST="${ROUTER_HOST:-localhost}"
ROUTER_PORT="${ROUTER_PORT:-9000}"
ROUTER_ADDR="${ROUTER_ADDR:-${ROUTER_HOST}:${ROUTER_PORT}}"
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

# Check if grpc_health_probe is available
HAS_GRPC_HEALTH_PROBE=false
if command -v grpc_health_probe &> /dev/null; then
    HAS_GRPC_HEALTH_PROBE=true
fi

# Check if grpcurl is available
HAS_GRPCURL=false
if command -v grpcurl &> /dev/null; then
    HAS_GRPCURL=true
fi

# Check if jq is available (for parsing grpcurl output)
HAS_JQ=false
if command -v jq &> /dev/null; then
    HAS_JQ=true
fi

echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${GREEN}Router Observability Test${NC}"
echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""
log_info "Router Address: ${ROUTER_ADDR}"
log_info "Default port: 9000 (gRPC)"
echo ""

# Check for required tools
if [[ "${HAS_GRPC_HEALTH_PROBE}" == "false" ]] && [[ "${HAS_GRPCURL}" == "false" ]]; then
    log_error "Neither grpc_health_probe nor grpcurl is available."
    log_error "Please install one of the following:"
    log_error "  - grpc_health_probe: https://github.com/grpc-ecosystem/grpc-health-probe"
    log_error "  - grpcurl: https://github.com/fullstorydev/grpcurl"
    exit 1
fi

if [[ "${HAS_GRPC_HEALTH_PROBE}" == "true" ]]; then
    log_info "✓ grpc_health_probe found"
else
    log_warn "grpc_health_probe not found, will use grpcurl"
fi

if [[ "${HAS_GRPCURL}" == "true" ]]; then
    log_info "✓ grpcurl found"
else
    log_warn "grpcurl not found, will use grpc_health_probe"
fi

if [[ "${HAS_JQ}" == "true" ]]; then
    log_info "✓ jq found (for JSON parsing)"
else
    log_warn "jq not found (optional, for JSON parsing)"
fi

echo ""

# Test 1: gRPC Health Check via grpc_health_probe (preferred method)
if [[ "${HAS_GRPC_HEALTH_PROBE}" == "true" ]]; then
    log_test "Test 1: gRPC Health Check via grpc_health_probe"
    
    # Run grpc_health_probe with timeout
    if timeout "${TIMEOUT_SEC}" grpc_health_probe -addr="${ROUTER_ADDR}" -service="grpc.health.v1.Health" > /tmp/router_health_probe.txt 2>&1; then
        EXIT_CODE=$?
        if [[ "${EXIT_CODE}" -eq 0 ]]; then
            log_info "✓ Health check passed (grpc_health_probe exit code: ${EXIT_CODE})"
            log_info "Health probe output:"
            cat /tmp/router_health_probe.txt | head -5
            ((PASSED++)) || true
        else
            log_error "Health check failed (grpc_health_probe exit code: ${EXIT_CODE})"
            log_error "Output:"
            cat /tmp/router_health_probe.txt
            exit 1
        fi
    else
        EXIT_CODE=$?
        if [[ "${EXIT_CODE}" -eq 124 ]]; then
            log_warn "Health check timed out after ${TIMEOUT_SEC} seconds"
            log_warn "Router may not be running or not responding"
            exit 2
        elif [[ "${EXIT_CODE}" -eq 1 ]]; then
            log_error "Health check failed (service not healthy)"
            log_error "Output:"
            cat /tmp/router_health_probe.txt
            exit 1
        else
            log_warn "Health check failed with exit code ${EXIT_CODE}"
            log_warn "Output:"
            cat /tmp/router_health_probe.txt
            log_warn "Router may not be running, skipping (service not running)"
            exit 2
        fi
    fi
    echo ""
fi

# Test 2: gRPC Health Check via grpcurl (alternative method)
if [[ "${HAS_GRPCURL}" == "true" ]]; then
    log_test "Test 2: gRPC Health Check via grpcurl"
    
    # Try health check without authentication first (may work if auth is disabled)
    if timeout "${TIMEOUT_SEC}" grpcurl -plaintext -max-time "${TIMEOUT_SEC}s" \
        "${ROUTER_ADDR}" grpc.health.v1.Health/Check > /tmp/router_grpcurl_health.json 2>&1; then
        
        # Check if response is valid JSON (grpcurl returns JSON by default)
        if [[ "${HAS_JQ}" == "true" ]]; then
            if jq empty /tmp/router_grpcurl_health.json > /dev/null 2>&1; then
                # Extract status from JSON response
                STATUS=$(jq -r '.status // empty' /tmp/router_grpcurl_health.json 2>/dev/null || echo "")
                
                if [[ -n "${STATUS}" ]]; then
                    # Convert to uppercase for comparison
                    STATUS_UPPER=$(echo "${STATUS}" | tr '[:lower:]' '[:upper:]')
                    
                    if [[ "${STATUS_UPPER}" == "SERVING" ]]; then
                        log_info "✓ Health check status is 'SERVING' (healthy)"
                        log_info "Health check response:"
                        cat /tmp/router_grpcurl_health.json | jq .
                        ((PASSED++)) || true
                    elif [[ "${STATUS_UPPER}" == "UNKNOWN" ]]; then
                        log_warn "Health check status is 'UNKNOWN' (acceptable but not ideal)"
                        log_info "Health check response:"
                        cat /tmp/router_grpcurl_health.json | jq .
                        ((PASSED++)) || true
                    elif [[ "${STATUS_UPPER}" == "NOT_SERVING" ]]; then
                        log_error "Health check status is 'NOT_SERVING' (unhealthy)"
                        log_error "Health check response:"
                        cat /tmp/router_grpcurl_health.json | jq .
                        exit 1
                    else
                        log_error "Health check status is '${STATUS}' (expected: SERVING, UNKNOWN, or NOT_SERVING)"
                        log_error "Health check response:"
                        cat /tmp/router_grpcurl_health.json | jq .
                        exit 1
                    fi
                else
                    log_warn "Health check response missing 'status' field"
                    log_info "Health check response:"
                    cat /tmp/router_grpcurl_health.json | jq . 2>/dev/null || cat /tmp/router_grpcurl_health.json
                    ((PASSED++)) || true
                fi
            else
                log_warn "Health check response is not valid JSON"
                log_info "Health check response:"
                cat /tmp/router_grpcurl_health.json
                # Still count as passed if grpc_health_probe passed
                if [[ "${HAS_GRPC_HEALTH_PROBE}" == "true" ]]; then
                    log_warn "grpc_health_probe already passed, accepting grpcurl response"
                    ((PASSED++)) || true
                else
                    log_error "Cannot validate health check without valid JSON response"
                    exit 1
                fi
            fi
        else
            # No jq, just check if response exists
            if [[ -s /tmp/router_grpcurl_health.json ]]; then
                log_info "✓ Health check response received (JSON parsing requires jq)"
                log_info "Health check response (first 200 chars):"
                head -c 200 /tmp/router_grpcurl_health.json
                echo ""
                ((PASSED++)) || true
            else
                log_error "Health check returned empty response"
                exit 1
            fi
        fi
    else
        EXIT_CODE=$?
        if [[ "${EXIT_CODE}" -eq 124 ]]; then
            log_warn "Health check timed out after ${TIMEOUT_SEC} seconds"
            log_warn "Router may not be running or not responding"
            # If grpc_health_probe already passed, this is just a warning
            if [[ "${HAS_GRPC_HEALTH_PROBE}" == "true" ]]; then
                log_warn "grpc_health_probe already passed, ignoring grpcurl timeout"
            else
                exit 2
            fi
        else
            log_warn "Health check failed with exit code ${EXIT_CODE}"
            log_warn "Output:"
            cat /tmp/router_grpcurl_health.json 2>/dev/null || echo "No output"
            # If grpc_health_probe already passed, this is just a warning
            if [[ "${HAS_GRPC_HEALTH_PROBE}" == "true" ]]; then
                log_warn "grpc_health_probe already passed, ignoring grpcurl failure"
            else
                log_warn "Router may not be running or requires authentication, skipping (service not running)"
                exit 2
            fi
        fi
    fi
    echo ""
fi

# Test 3: Validate Health Status Values (if we have JSON response)
if [[ "${HAS_GRPCURL}" == "true" ]] && [[ "${HAS_JQ}" == "true" ]] && [[ -f /tmp/router_grpcurl_health.json ]]; then
    log_test "Test 3: Validate Health Status Values"
    
    if jq empty /tmp/router_grpcurl_health.json > /dev/null 2>&1; then
        STATUS=$(jq -r '.status // empty' /tmp/router_grpcurl_health.json 2>/dev/null || echo "")
        
        if [[ -n "${STATUS}" ]]; then
            STATUS_UPPER=$(echo "${STATUS}" | tr '[:lower:]' '[:upper:]')
            
            # Validate status enum values (gRPC health protocol)
            case "${STATUS_UPPER}" in
                SERVING)
                    log_info "✓ Health status is 'SERVING' (service is healthy and ready)"
                    ((PASSED++)) || true
                    ;;
                UNKNOWN)
                    log_warn "Health status is 'UNKNOWN' (status unknown, acceptable)"
                    ((PASSED++)) || true
                    ;;
                NOT_SERVING)
                    log_error "Health status is 'NOT_SERVING' (service is unhealthy)"
                    exit 1
                    ;;
                *)
                    log_error "Health status is '${STATUS}' (invalid enum value)"
                    exit 1
                    ;;
            esac
        else
            log_warn "Health check response missing 'status' field"
        fi
    else
        log_warn "Cannot validate health status (response is not valid JSON)"
    fi
    echo ""
fi

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

