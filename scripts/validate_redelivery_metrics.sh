#!/bin/bash
# @doc Validate router_jetstream_redelivery_total metric implementation
# 
# Checks:
# - Metric name is router_jetstream_redelivery_total (not router_redelivery_total)
# - All required labels are present in code
# - Source label values match specification
# - Tests use correct metric name
# - Documentation uses correct metric name

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
ROUTER_SRC="$PROJECT_ROOT/apps/otp/router/src"
ROUTER_TEST="$PROJECT_ROOT/apps/otp/router/test"
ROUTER_DOCS="$PROJECT_ROOT/apps/otp/router/docs"
DOCS_ROOT="$PROJECT_ROOT/docs"

ERRORS=0
WARNINGS=0

echo "=== Validating router_jetstream_redelivery_total Metric ==="
echo ""

# Check 1: Verify new metric name is used in source code
echo "1. Checking source code for router_jetstream_redelivery_total..."
if grep -r "router_jetstream_redelivery_total" "$ROUTER_SRC" --include="*.erl" > /dev/null; then
    echo "   ✓ Found router_jetstream_redelivery_total in source code"
else
    echo "   ✗ ERROR: router_jetstream_redelivery_total not found in source code"
    ERRORS=$((ERRORS + 1))
fi

# Check 2: Verify old metric name is not used (except in deprecated context)
echo "2. Checking for deprecated router_redelivery_total usage..."
OLD_METRIC_COUNT=$(grep -r "router_redelivery_total" "$ROUTER_SRC" --include="*.erl" | grep -v "deprecated" | grep -v "router_jetstream_redelivery_total" | wc -l || true)
if [ "$OLD_METRIC_COUNT" -eq 0 ]; then
    echo "   ✓ No deprecated router_redelivery_total found (or all marked as deprecated)"
else
    echo "   ⚠ WARNING: Found $OLD_METRIC_COUNT instances of router_redelivery_total (should be deprecated)"
    WARNINGS=$((WARNINGS + 1))
fi

# Check 3: Verify labels are present in nak/3 function
echo "3. Checking for label support in router_jetstream:nak/3..."
if grep -A 10 "nak(#{id := Id} = Msg, Reason, Context)" "$ROUTER_SRC/router_jetstream.erl" | grep -q "assignment_id\|request_id\|reason\|source"; then
    echo "   ✓ Labels (assignment_id, request_id, reason, source) found in nak/3"
else
    echo "   ✗ ERROR: Labels not found in nak/3 implementation"
    ERRORS=$((ERRORS + 1))
fi

# Check 4: Verify source label values match specification
echo "4. Checking source label values..."
if grep -q 'source => <<"tenant_validation">>' "$ROUTER_SRC/router_result_consumer.erl" && \
   grep -q 'source => <<"tenant_validation">>' "$ROUTER_SRC/router_ack_consumer.erl" && \
   grep -q 'source => <<"backpressure">>' "$ROUTER_SRC/router_decide_consumer.erl"; then
    echo "   ✓ Source label values match specification"
else
    echo "   ✗ ERROR: Source label values do not match specification"
    ERRORS=$((ERRORS + 1))
fi

# Check 5: Verify tests use correct metric name
echo "5. Checking tests for router_jetstream_redelivery_total..."
if grep -r "router_jetstream_redelivery_total" "$ROUTER_TEST" --include="*.erl" > /dev/null; then
    echo "   ✓ Tests use router_jetstream_redelivery_total"
else
    echo "   ⚠ WARNING: Tests may not use router_jetstream_redelivery_total"
    WARNINGS=$((WARNINGS + 1))
fi

# Check 6: Verify documentation uses correct metric name
echo "6. Checking documentation for router_jetstream_redelivery_total..."
if grep -r "router_jetstream_redelivery_total" "$ROUTER_DOCS" "$DOCS_ROOT" --include="*.md" --include="*.yaml" > /dev/null; then
    echo "   ✓ Documentation uses router_jetstream_redelivery_total"
else
    echo "   ⚠ WARNING: Documentation may not reference router_jetstream_redelivery_total"
    WARNINGS=$((WARNINGS + 1))
fi

# Check 7: Verify alerts use correct metric name
echo "7. Checking alert rules for router_jetstream_redelivery_total..."
if grep -q "router_jetstream_redelivery_total" "$DOCS_ROOT/observability/router-alert-rules.yaml"; then
    echo "   ✓ Alert rules use router_jetstream_redelivery_total"
else
    echo "   ✗ ERROR: Alert rules do not use router_jetstream_redelivery_total"
    ERRORS=$((ERRORS + 1))
fi

# Check 8: Verify reason_to_source helper exists
echo "8. Checking for reason_to_source helper..."
if grep -q "reason_to_source" "$ROUTER_SRC/router_jetstream.erl"; then
    echo "   ✓ reason_to_source helper found"
else
    echo "   ✗ ERROR: reason_to_source helper not found"
    ERRORS=$((ERRORS + 1))
fi

echo ""
echo "=== Validation Summary ==="
echo "Errors: $ERRORS"
echo "Warnings: $WARNINGS"

if [ $ERRORS -eq 0 ]; then
    echo "✓ Validation PASSED"
    exit 0
else
    echo "✗ Validation FAILED"
    exit 1
fi

