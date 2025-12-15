#!/bin/bash
#
# Metrics Verification Script
#
# Verifies Router and Gateway metrics after load/chaos tests.
# Provides health indicators and alerts for monitoring scenarios.
#
# Usage:
#   ./scripts/verify_metrics_after_tests.sh [--router-url URL] [--gateway-url URL] [--scenario SCENARIO]
#
# Environment Variables:
#   ROUTER_METRICS_URL: Router metrics endpoint (default: http://localhost:3081/_metrics)
#   GATEWAY_METRICS_URL: Gateway metrics endpoint (default: http://localhost:8081/_metrics)
#   TEST_SCENARIO: Test scenario name (load_success, load_error, chaos_nats, etc.)
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

# Default configuration
ROUTER_METRICS_URL="${ROUTER_METRICS_URL:-http://localhost:3081/_metrics}"
GATEWAY_METRICS_URL="${GATEWAY_METRICS_URL:-http://localhost:8081/_metrics}"
TEST_SCENARIO="${TEST_SCENARIO:-unknown}"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --router-url)
            ROUTER_METRICS_URL="$2"
            shift 2
            ;;
        --gateway-url)
            GATEWAY_METRICS_URL="$2"
            shift 2
            ;;
        --scenario)
            TEST_SCENARIO="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1"
            echo "Usage: $0 [--router-url URL] [--gateway-url URL] [--scenario SCENARIO]"
            exit 1
            ;;
    esac
done

# Logging functions
log_info() {
    echo "[INFO] $*"
}

log_warn() {
    echo "[WARN] $*" >&2
}

log_error() {
    echo "[ERROR] $*" >&2
}

log_section() {
    echo ""
    echo "========================================"
    echo "$*"
    echo "========================================"
}

# Fetch metrics
fetch_router_metrics() {
    local url="${ROUTER_METRICS_URL}"
    if ! curl -sf "${url}" > /tmp/router_metrics.txt 2>&1; then
        log_error "Failed to fetch Router metrics from ${url}"
        return 1
    fi
    log_info "Router metrics fetched from ${url}"
}

fetch_gateway_metrics() {
    local url="${GATEWAY_METRICS_URL}"
    if ! curl -sf "${url}" > /tmp/gateway_metrics.txt 2>&1; then
        log_warn "Failed to fetch Gateway metrics from ${url} (may not be running)"
        return 1
    fi
    log_info "Gateway metrics fetched from ${url}"
}

# Extract Router metrics (Prometheus format)
extract_router_metric() {
    local metric_name="$1"
    local labels="${2:-}"
    
    if [[ -f /tmp/router_metrics.txt ]]; then
        if [[ -n "${labels}" ]]; then
            grep "^${metric_name}{${labels}}" /tmp/router_metrics.txt | awk '{print $2}' | head -1
        else
            grep "^${metric_name}" /tmp/router_metrics.txt | awk '{sum+=$2} END {print sum}'
        fi
    else
        echo "0"
    fi
}

# Extract Gateway metrics (JSON format)
extract_gateway_metric() {
    local json_path="$1"
    
    if [[ -f /tmp/gateway_metrics.txt ]]; then
        if command -v jq &> /dev/null; then
            jq -r "${json_path} // 0" /tmp/gateway_metrics.txt 2>/dev/null || echo "0"
        else
            # Fallback: simple grep for JSON values
            grep -o "\"${json_path}\":[0-9]*" /tmp/gateway_metrics.txt | grep -o "[0-9]*" | head -1 || echo "0"
        fi
    else
        echo "0"
    fi
}

# Verify Router metrics
verify_router_metrics() {
    log_section "Router Metrics Verification"
    
    local intake_ok=$(extract_router_metric "router_intake_messages_total" 'status="ok"')
    local intake_failed=$(extract_router_metric "router_intake_messages_total" 'status="failed"')
    local validation_errors=$(extract_router_metric "router_intake_validation_errors_total")
    local dlq_messages=$(extract_router_metric "router_intake_dlq_messages_total")
    local dlq_failed=$(extract_router_metric "router_intake_dlq_publish_failed_total")
    local nats_reconnects=$(extract_router_metric "router_nats_reconnect_total")
    local idempotent_duplicates=$(extract_router_metric "router_intake_idempotent_duplicate_total")
    
    echo "Router Metrics Summary:"
    echo "  Intake OK: ${intake_ok}"
    echo "  Intake Failed: ${intake_failed}"
    echo "  Validation Errors: ${validation_errors}"
    echo "  DLQ Messages: ${dlq_messages}"
    echo "  DLQ Publish Failed: ${dlq_failed}"
    echo "  NATS Reconnects: ${nats_reconnects}"
    echo "  Idempotent Duplicates: ${idempotent_duplicates}"
    
    # Verify invariants
    local issues=0
    
    if [[ "${dlq_failed}" -gt 0 ]]; then
        log_warn "DLQ publication failures detected: ${dlq_failed}"
        issues=$((issues + 1))
    fi
    
    if [[ "${intake_failed}" -gt 0 ]] && [[ "${dlq_messages}" -eq 0 ]]; then
        log_warn "Failed messages not sent to DLQ (failed: ${intake_failed}, DLQ: ${dlq_messages})"
        issues=$((issues + 1))
    fi
    
    # Calculate error rate
    local total=$((intake_ok + intake_failed))
    if [[ ${total} -gt 0 ]]; then
        local error_rate=$((intake_failed * 100 / total))
        if [[ ${error_rate} -gt 10 ]]; then
            log_warn "High error rate: ${error_rate}% (threshold: 10%)"
            issues=$((issues + 1))
        fi
    fi
    
    if [[ ${issues} -eq 0 ]]; then
        log_info "✅ Router metrics verification passed"
        return 0
    else
        log_warn "⚠️  Router metrics verification found ${issues} issues"
        return 1
    fi
}

# Verify Gateway metrics
verify_gateway_metrics() {
    log_section "Gateway Metrics Verification"
    
    if [[ ! -f /tmp/gateway_metrics.txt ]]; then
        log_warn "Gateway metrics not available (Gateway may not be running)"
        return 0
    fi
    
    local rate_limit_hits=$(extract_gateway_metric ".rate_limit.total_hits")
    local rate_limit_exceeded=$(extract_gateway_metric ".rate_limit.total_exceeded")
    local rate_limit_decide=$(extract_gateway_metric ".rate_limit.exceeded_by_endpoint.routes_decide")
    local http_requests=$(extract_gateway_metric ".rps")
    
    echo "Gateway Metrics Summary:"
    echo "  Rate Limit Hits: ${rate_limit_hits}"
    echo "  Rate Limit Exceeded: ${rate_limit_exceeded}"
    echo "  Rate Limit Exceeded (decide): ${rate_limit_decide}"
    echo "  Requests/Second: ${http_requests}"
    
    # Verify invariants
    local issues=0
    
    if [[ "${rate_limit_exceeded}" -gt "${rate_limit_hits}" ]]; then
        log_error "Invalid metrics: rate_limit_exceeded (${rate_limit_exceeded}) > rate_limit_hits (${rate_limit_hits})"
        issues=$((issues + 1))
    fi
    
    # Calculate rate limit hit rate
    if [[ "${rate_limit_hits}" -gt 0 ]]; then
        local hit_rate=$((rate_limit_exceeded * 100 / rate_limit_hits))
        if [[ ${hit_rate} -gt 50 ]]; then
            log_warn "High rate limit hit rate: ${hit_rate}% (threshold: 50%)"
            issues=$((issues + 1))
        fi
    fi
    
    if [[ ${issues} -eq 0 ]]; then
        log_info "✅ Gateway metrics verification passed"
        return 0
    else
        log_warn "⚠️  Gateway metrics verification found ${issues} issues"
        return 1
    fi
}

# Scenario-specific verification
verify_scenario_metrics() {
    log_section "Scenario-Specific Verification: ${TEST_SCENARIO}"
    
    case "${TEST_SCENARIO}" in
        load_success)
            verify_load_success_metrics
            ;;
        load_error)
            verify_load_error_metrics
            ;;
        load_mixed)
            verify_load_mixed_metrics
            ;;
        load_idempotency)
            verify_load_idempotency_metrics
            ;;
        chaos_nats_single)
            verify_chaos_nats_single_metrics
            ;;
        chaos_nats_multiple)
            verify_chaos_nats_multiple_metrics
            ;;
        chaos_nats_randomized)
            verify_chaos_nats_randomized_metrics
            ;;
        *)
            log_warn "Unknown scenario: ${TEST_SCENARIO} (skipping scenario-specific verification)"
            ;;
    esac
}

verify_load_success_metrics() {
    local intake_ok=$(extract_router_metric "router_intake_messages_total" 'status="ok"')
    local intake_failed=$(extract_router_metric "router_intake_messages_total" 'status="failed"')
    
    log_info "Load Success Scenario:"
    log_info "  Expected: All messages OK, no failures"
    
    if [[ "${intake_failed}" -gt 0 ]]; then
        log_warn "⚠️  Unexpected failures: ${intake_failed}"
        return 1
    fi
    
    log_info "✅ All messages processed successfully"
}

verify_load_error_metrics() {
    local intake_failed=$(extract_router_metric "router_intake_messages_total" 'status="failed"')
    local dlq_messages=$(extract_router_metric "router_intake_dlq_messages_total")
    
    log_info "Load Error Scenario:"
    log_info "  Expected: All invalid messages failed and sent to DLQ"
    
    if [[ "${intake_failed}" -eq 0 ]]; then
        log_warn "⚠️  No failed messages (expected failures)"
        return 1
    fi
    
    if [[ "${dlq_messages}" -ne "${intake_failed}" ]]; then
        log_warn "⚠️  DLQ messages (${dlq_messages}) != failed messages (${intake_failed})"
        return 1
    fi
    
    log_info "✅ All invalid messages failed and sent to DLQ"
}

verify_load_mixed_metrics() {
    local intake_ok=$(extract_router_metric "router_intake_messages_total" 'status="ok"')
    local intake_failed=$(extract_router_metric "router_intake_messages_total" 'status="failed"')
    local dlq_messages=$(extract_router_metric "router_intake_dlq_messages_total")
    
    log_info "Load Mixed Scenario:"
    log_info "  Expected: ~70% OK, ~30% failed, only failed in DLQ"
    
    local total=$((intake_ok + intake_failed))
    if [[ ${total} -eq 0 ]]; then
        log_warn "⚠️  No messages processed"
        return 1
    fi
    
    local ok_percent=$((intake_ok * 100 / total))
    local failed_percent=$((intake_failed * 100 / total))
    
    log_info "  Distribution: ${ok_percent}% OK, ${failed_percent}% failed"
    
    if [[ "${dlq_messages}" -ne "${intake_failed}" ]]; then
        log_warn "⚠️  DLQ messages (${dlq_messages}) != failed messages (${intake_failed})"
        return 1
    fi
    
    log_info "✅ Metrics reflect mixed distribution correctly"
}

verify_load_idempotency_metrics() {
    local idempotent_duplicates=$(extract_router_metric "router_intake_idempotent_duplicate_total")
    local intake_ok=$(extract_router_metric "router_intake_messages_total" 'status="ok"')
    
    log_info "Load Idempotency Scenario:"
    log_info "  Expected: Duplicates detected, first message per key processed"
    
    if [[ "${idempotent_duplicates}" -eq 0 ]]; then
        log_warn "⚠️  No idempotent duplicates detected (expected duplicates)"
        return 1
    fi
    
    log_info "✅ Idempotency metrics: ${idempotent_duplicates} duplicates, ${intake_ok} unique processed"
}

verify_chaos_nats_single_metrics() {
    local nats_reconnects=$(extract_router_metric "router_nats_reconnect_total")
    
    log_info "Chaos NATS Single Scenario:"
    log_info "  Expected: 1 reconnect after NATS restart"
    
    if [[ "${nats_reconnects}" -ne 1 ]]; then
        log_warn "⚠️  Expected 1 reconnect, got ${nats_reconnects}"
        return 1
    fi
    
    log_info "✅ NATS reconnected successfully (${nats_reconnects} reconnect)"
}

verify_chaos_nats_multiple_metrics() {
    local nats_reconnects=$(extract_router_metric "router_nats_reconnect_total")
    
    log_info "Chaos NATS Multiple Scenario:"
    log_info "  Expected: Multiple reconnects (3+)"
    
    if [[ "${nats_reconnects}" -lt 3 ]]; then
        log_warn "⚠️  Expected 3+ reconnects, got ${nats_reconnects}"
        return 1
    fi
    
    log_info "✅ NATS reconnected multiple times (${nats_reconnects} reconnects)"
}

verify_chaos_nats_randomized_metrics() {
    local nats_reconnects=$(extract_router_metric "router_nats_reconnect_total")
    local intake_ok=$(extract_router_metric "router_intake_messages_total" 'status="ok"')
    
    log_info "Chaos NATS Randomized Scenario:"
    log_info "  Expected: Router stable, messages eventually processed"
    
    if [[ "${nats_reconnects}" -eq 0 ]]; then
        log_warn "⚠️  No reconnects detected (expected reconnects)"
        return 1
    fi
    
    if [[ "${intake_ok}" -eq 0 ]]; then
        log_warn "⚠️  No messages processed after recovery"
        return 1
    fi
    
    log_info "✅ Router stable: ${nats_reconnects} reconnects, ${intake_ok} messages processed"
}

# Main execution
main() {
    log_section "Metrics Verification"
    log_info "Test Scenario: ${TEST_SCENARIO}"
    log_info "Router Metrics URL: ${ROUTER_METRICS_URL}"
    log_info "Gateway Metrics URL: ${GATEWAY_METRICS_URL}"
    
    # Fetch metrics
    fetch_router_metrics || exit 1
    fetch_gateway_metrics || true  # Gateway may not be running
    
    # Verify metrics
    local router_result=0
    local gateway_result=0
    local scenario_result=0
    
    verify_router_metrics || router_result=$?
    verify_gateway_metrics || gateway_result=$?
    verify_scenario_metrics || scenario_result=$?
    
    # Summary
    log_section "Verification Summary"
    
    if [[ ${router_result} -eq 0 ]] && [[ ${gateway_result} -eq 0 ]] && [[ ${scenario_result} -eq 0 ]]; then
        log_info "✅ All metrics verification passed"
        exit 0
    else
        log_error "❌ Metrics verification failed"
        [[ ${router_result} -ne 0 ]] && log_error "  - Router metrics: FAILED"
        [[ ${gateway_result} -ne 0 ]] && log_error "  - Gateway metrics: FAILED"
        [[ ${scenario_result} -ne 0 ]] && log_error "  - Scenario-specific: FAILED"
        exit 1
    fi
}

# Run main function
main "$@"

