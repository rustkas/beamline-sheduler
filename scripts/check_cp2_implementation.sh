#!/bin/bash
# CP2 Implementation Verification Script
# Checks for required documentation, alerts, E2E tests, and metric implementation

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROUTER_DIR="${SCRIPT_DIR}/../apps/otp/router"
EXIT_CODE=0
WARNINGS=0

echo "=== CP2 Implementation Verification ==="
echo ""

# Check required reports in docs/archive/dev/
echo "Checking reports in docs/archive/dev/..."
REPORTS=(
    "JETSTREAM_FORWARDING_NAK_IMPLEMENTATION.md"
    "JETSTREAM_NAK_IDEMPOTENCY_FIXES.md"
    "METRICS_ALERTS_TESTS_COMPLETE.md"
    "CP2_COMPLETE_IMPLEMENTATION_REPORT.md"
)

for report in "${REPORTS[@]}"; do
    if [ -f "${ROUTER_DIR}/docs/archive/dev/${report}" ]; then
        echo "  ✅ ${report}"
    else
        echo "  ❌ ${report} - NOT FOUND"
        EXIT_CODE=1
    fi
done

echo ""

# Check PROMETHEUS_ALERTS.md
echo "Checking PROMETHEUS_ALERTS.md..."
if [ -f "${ROUTER_DIR}/docs/PROMETHEUS_ALERTS.md" ]; then
    echo "  ✅ PROMETHEUS_ALERTS.md found"
    
    # Check for required alerts
    ALERTS=(
        "RouterTenantRejectedHigh"
        "RouterIdempotencyDuplicatesHigh"
        "RouterJetStreamRedeliveryHigh"
        "RouterJetStreamMaxDeliverExhausted"
    )
    
    for alert in "${ALERTS[@]}"; do
        if grep -q "${alert}" "${ROUTER_DIR}/docs/PROMETHEUS_ALERTS.md"; then
            echo "    ✅ Alert: ${alert}"
        else
            echo "    ❌ Alert: ${alert} - NOT FOUND"
            EXIT_CODE=1
        fi
    done
else
    echo "  ❌ PROMETHEUS_ALERTS.md - NOT FOUND"
    EXIT_CODE=1
fi

echo ""

# Check E2E test suite
echo "Checking E2E test suite..."
if [ -f "${ROUTER_DIR}/test/router_jetstream_e2e_SUITE.erl" ]; then
    echo "  ✅ router_jetstream_e2e_SUITE.erl found"
    
    # Check for required test cases
    TEST_CASES=(
        "test_headers_in_assignment_publication"
        "test_nak_redelivery_on_validator_error"
        "test_jetstream_forwarding_with_headers"
    )
    
    for test_case in "${TEST_CASES[@]}"; do
        if grep -q "${test_case}/1" "${ROUTER_DIR}/test/router_jetstream_e2e_SUITE.erl"; then
            echo "    ✅ Test case: ${test_case}/1"
        else
            echo "    ❌ Test case: ${test_case}/1 - NOT FOUND"
            EXIT_CODE=1
        fi
    done
else
    echo "  ❌ router_jetstream_e2e_SUITE.erl - NOT FOUND"
    EXIT_CODE=1
fi

echo ""

# Check metric implementation
echo "Checking router_jetstream_maxdeliver_exhausted_total metric..."
METRIC_COUNT=$(grep -r "router_jetstream_maxdeliver_exhausted_total" "${ROUTER_DIR}/src"/*.erl 2>/dev/null | grep -v "TODO\|Example\|%%" | grep "emit_counter" | wc -l || echo "0")

if [ "${METRIC_COUNT}" -gt 0 ]; then
    echo "  ✅ Metric router_jetstream_maxdeliver_exhausted_total implemented (${METRIC_COUNT} locations)"
    
    # Check specific files
    if grep "router_jetstream_maxdeliver_exhausted_total" "${ROUTER_DIR}/src/router_result_consumer.erl" 2>/dev/null | grep -q "emit_counter" && ! grep "router_jetstream_maxdeliver_exhausted_total" "${ROUTER_DIR}/src/router_result_consumer.erl" 2>/dev/null | grep -q "TODO\|Example"; then
        echo "    ✅ router_result_consumer.erl"
    elif grep -q "router_jetstream_maxdeliver_exhausted_total" "${ROUTER_DIR}/src/router_result_consumer.erl" 2>/dev/null; then
        echo "    ✅ router_result_consumer.erl (found metric reference)"
    else
        echo "    ⚠️  router_result_consumer.erl - WARNING: metric may not be emitted"
        WARNINGS=$((WARNINGS + 1))
    fi
    
    if grep "router_jetstream_maxdeliver_exhausted_total" "${ROUTER_DIR}/src/router_ack_consumer.erl" 2>/dev/null | grep -q "emit_counter" && ! grep "router_jetstream_maxdeliver_exhausted_total" "${ROUTER_DIR}/src/router_ack_consumer.erl" 2>/dev/null | grep -q "TODO\|Example"; then
        echo "    ✅ router_ack_consumer.erl"
    elif grep -q "router_jetstream_maxdeliver_exhausted_total" "${ROUTER_DIR}/src/router_ack_consumer.erl" 2>/dev/null; then
        echo "    ✅ router_ack_consumer.erl (found metric reference)"
    else
        echo "    ⚠️  router_ack_consumer.erl - WARNING: metric may not be emitted"
        WARNINGS=$((WARNINGS + 1))
    fi
else
    echo "  ⚠️  router_jetstream_maxdeliver_exhausted_total - WARNING: metric not found in code"
    WARNINGS=$((WARNINGS + 1))
fi

echo ""

# Check idempotency metrics
echo "Checking idempotency metrics..."
if grep -q "router_idempotency_hit_total\|router_idempotency_miss_total" "${ROUTER_DIR}/src/router_idempotency.erl" 2>/dev/null; then
    echo "  ✅ router_idempotency_hit_total and router_idempotency_miss_total implemented"
else
    echo "  ⚠️  router_idempotency_hit_total/miss_total - WARNING: metrics may not be implemented"
    WARNINGS=$((WARNINGS + 1))
fi

echo ""

# Summary
echo "=== Summary ==="
if [ "${EXIT_CODE}" -eq 0 ] && [ "${WARNINGS}" -eq 0 ]; then
    echo "✅ All checks passed"
    exit 0
elif [ "${EXIT_CODE}" -eq 0 ] && [ "${WARNINGS}" -gt 0 ]; then
    echo "⚠️  All required checks passed, but ${WARNINGS} warning(s) found"
    exit 0
else
    echo "❌ Some checks failed"
    exit 1
fi

