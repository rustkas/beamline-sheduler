#!/bin/bash
# CP2 Validation Suite
# Validates CP2 features are enabled and properly configured
# Checks:
# - Feature flags enabled in app.src
# - CP2+ allowed check (current_cp >= CP2-LC)
# - Required modules present
# - JetStream configuration
# - Supervisor tree structure
# - Runtime validations (JetStream connectivity, test suites, tracing integration)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

cd "$PROJECT_ROOT"

# Exit codes
EXIT_SUCCESS=0
EXIT_FEATURE_FLAGS=1
EXIT_CP_CHECK=2
EXIT_MODULES=3
EXIT_CONFIG=4
EXIT_JETSTREAM=5
EXIT_IDEMPOTENCY=6
EXIT_TRACING=7
EXIT_TENANT=8
EXIT_ADMIN_GRPC=9

FAIL_COUNT=0
WARN_COUNT=0

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Helper functions
pass() {
    echo -e "${GREEN}[PASS]${NC} $1"
}

fail() {
    echo -e "${RED}[FAIL]${NC} $1"
    FAIL_COUNT=$((FAIL_COUNT + 1))
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
    WARN_COUNT=$((WARN_COUNT + 1))
}

info() {
    echo "[INFO] $1"
}

# Router app.src path
ROUTER_APP_SRC="apps/otp/router/src/beamline_router.app.src"
ROUTER_SRC_DIR="apps/otp/router/src"
ROUTER_TEST_DIR="apps/otp/router/test"
STATE_FILE=".trae/state.json"

# Validation functions

# Static validation: Feature flags
validate_feature_flags() {
    echo ""
    info "Checking CP2 feature flags in app.src..."
    
    if [ ! -f "$ROUTER_APP_SRC" ]; then
        fail "Router app.src not found: $ROUTER_APP_SRC"
        return 1
    fi
    
    # Check each CP2 feature flag
    IDEMPOTENCY_ENABLED=$(grep -E "^\s*\{idempotency_enabled," "$ROUTER_APP_SRC" | grep -oE "(true|false)" | head -1 || echo "false")
    TRACING_ENABLED=$(grep -E "^\s*\{tracing_enabled," "$ROUTER_APP_SRC" | grep -oE "(true|false)" | head -1 || echo "false")
    TENANT_VALIDATION_ENABLED=$(grep -E "^\s*\{tenant_validation_enabled," "$ROUTER_APP_SRC" | grep -oE "(true|false)" | head -1 || echo "false")
    ADMIN_GRPC_ENABLED=$(grep -E "^\s*\{admin_grpc_enabled," "$ROUTER_APP_SRC" | grep -oE "(true|false)" | head -1 || echo "false")
    
    if [ "$IDEMPOTENCY_ENABLED" = "true" ]; then
        pass "idempotency_enabled = true"
    else
        warn "idempotency_enabled = false (CP2 feature disabled)"
    fi
    
    if [ "$TRACING_ENABLED" = "true" ]; then
        pass "tracing_enabled = true"
    else
        warn "tracing_enabled = false (CP2 feature disabled)"
    fi
    
    if [ "$TENANT_VALIDATION_ENABLED" = "true" ]; then
        pass "tenant_validation_enabled = true"
    else
        warn "tenant_validation_enabled = false (CP2 feature disabled)"
    fi
    
    if [ "$ADMIN_GRPC_ENABLED" = "true" ]; then
        pass "admin_grpc_enabled = true"
    else
        warn "admin_grpc_enabled = false (CP2 feature disabled)"
    fi
    
    # Count enabled features
    ENABLED_COUNT=0
    [ "$IDEMPOTENCY_ENABLED" = "true" ] && ENABLED_COUNT=$((ENABLED_COUNT + 1))
    [ "$TRACING_ENABLED" = "true" ] && ENABLED_COUNT=$((ENABLED_COUNT + 1))
    [ "$TENANT_VALIDATION_ENABLED" = "true" ] && ENABLED_COUNT=$((ENABLED_COUNT + 1))
    [ "$ADMIN_GRPC_ENABLED" = "true" ] && ENABLED_COUNT=$((ENABLED_COUNT + 1))
    
    if [ $ENABLED_COUNT -eq 0 ]; then
        warn "No CP2 features enabled in app.src (all flags are false)"
        return 1
    elif [ $ENABLED_COUNT -lt 4 ]; then
        info "CP2 features partially enabled: $ENABLED_COUNT/4"
    else
        pass "All CP2 feature flags enabled in app.src"
    fi
    
    return 0
}

# Static validation: CP2+ allowed check
validate_cp_check() {
    echo ""
    info "Checking CP2+ allowed status (current_cp >= CP2-LC)..."
    
    if [ ! -f "$STATE_FILE" ]; then
        warn "State file not found: $STATE_FILE (CP2+ check will use fallback)"
        return 0
    fi
    
    if command -v jq &> /dev/null; then
        CURRENT_CP=$(jq -r '.current_cp // "CP1-LC"' "$STATE_FILE" 2>/dev/null || echo "CP1-LC")
        
        # Extract CP number from "CP{N}-{LABEL}" pattern
        CP_NUM=$(echo "$CURRENT_CP" | grep -oE "^CP([0-9]+)" | grep -oE "[0-9]+" || echo "1")
        
        if [ "$CP_NUM" -ge 2 ]; then
            pass "current_cp = $CURRENT_CP (CP2+ allowed)"
            return 0
        else
            warn "current_cp = $CURRENT_CP (CP2+ features will be blocked even if flags are enabled)"
            info "Note: CP2+ features require current_cp >= CP2-LC in .trae/state.json"
            return 2
        fi
    else
        warn "jq not found, skipping CP2+ allowed check"
        return 0
    fi
}

# Static validation: Required modules
validate_required_modules() {
    echo ""
    info "Checking required CP2 modules..."
    
    if [ ! -d "$ROUTER_SRC_DIR" ]; then
        fail "Router source directory not found: $ROUTER_SRC_DIR"
        return 1
    fi
    
    local module_errors=0
    
    # Check for CP2 modules
    if [ -f "$ROUTER_SRC_DIR/router_idempotency.erl" ]; then
        pass "router_idempotency.erl present"
    else
        fail "router_idempotency.erl missing (required for CP2 idempotency)"
        module_errors=$((module_errors + 1))
    fi
    
    if [ -f "$ROUTER_SRC_DIR/router_tracing.erl" ]; then
        pass "router_tracing.erl present"
    else
        fail "router_tracing.erl missing (required for CP2 tracing)"
        module_errors=$((module_errors + 1))
    fi
    
    if [ -f "$ROUTER_SRC_DIR/router_tenant_validator.erl" ]; then
        pass "router_tenant_validator.erl present"
    else
        fail "router_tenant_validator.erl missing (required for CP2 tenant validation)"
        module_errors=$((module_errors + 1))
    fi
    
    if [ -f "$ROUTER_SRC_DIR/router_admin_grpc.erl" ]; then
        pass "router_admin_grpc.erl present"
    else
        warn "router_admin_grpc.erl missing (optional for CP2 admin gRPC)"
    fi
    
    # Check supervisor integration
    if grep -q "router_idempotency" "$ROUTER_SRC_DIR/beamline_router_sup.erl" 2>/dev/null; then
        pass "router_idempotency integrated in supervisor"
    else
        warn "router_idempotency not found in supervisor (may be conditionally started)"
    fi
    
    if [ $module_errors -gt 0 ]; then
        return 1
    fi
    return 0
}

# Static validation: JetStream configuration
validate_jetstream_config() {
    echo ""
    info "Checking JetStream configuration..."
    
    if [ ! -f "$ROUTER_APP_SRC" ]; then
        warn "Router app.src not found: $ROUTER_APP_SRC"
        return 0
    fi
    
    # Check JetStream durable groups
    JS_DURABLE_GROUP_RESULTS=$(grep -E "^\s*\{nats_js_durable_group_results," "$ROUTER_APP_SRC" | grep -oE "<<\"[^\"]+\">>" | tr -d '<>"' || echo "")
    JS_DURABLE_GROUP_ACKS=$(grep -E "^\s*\{nats_js_durable_group_acks," "$ROUTER_APP_SRC" | grep -oE "<<\"[^\"]+\">>" | tr -d '<>"' || echo "")
    
    if [ -n "$JS_DURABLE_GROUP_RESULTS" ]; then
        pass "JetStream durable group for results: $JS_DURABLE_GROUP_RESULTS"
    else
        warn "JetStream durable group for results not configured"
    fi
    
    if [ -n "$JS_DURABLE_GROUP_ACKS" ]; then
        pass "JetStream durable group for ACKs: $JS_DURABLE_GROUP_ACKS"
    else
        warn "JetStream durable group for ACKs not configured"
    fi
    
    # Check JetStream delivery settings
    JS_MAX_DELIVER=$(grep -E "^\s*\{nats_js_max_deliver," "$ROUTER_APP_SRC" | grep -oE "[0-9]+" | head -1 || echo "")
    if [ -n "$JS_MAX_DELIVER" ]; then
        pass "JetStream max_deliver: $JS_MAX_DELIVER"
    else
        warn "JetStream max_deliver not configured"
    fi
    
    return 0
}

# Static validation: Supervisor gating
validate_supervisor_gating() {
    echo ""
    info "Checking supervisor tree structure for CP2+ gating..."
    
    if [ ! -f "$ROUTER_SRC_DIR/beamline_router_sup.erl" ]; then
        fail "Supervisor file not found: $ROUTER_SRC_DIR/beamline_router_sup.erl"
        return 1
    fi
    
    if grep -q "is_cp2_plus_allowed" "$ROUTER_SRC_DIR/beamline_router_sup.erl"; then
        pass "Supervisor uses is_cp2_plus_allowed() for CP2+ gating"
    else
        warn "Supervisor may not use is_cp2_plus_allowed() for CP2+ gating"
    fi
    
    if grep -q "router_state:is_cp2_plus_allowed" "$ROUTER_SRC_DIR/beamline_router_sup.erl"; then
        pass "Supervisor calls router_state:is_cp2_plus_allowed()"
    else
        warn "Supervisor may not call router_state:is_cp2_plus_allowed()"
    fi
    
    return 0
}

# Runtime validation: JetStream connectivity
validate_runtime_jetstream() {
    echo ""
    info "🔧 Validating JetStream runtime connectivity..."
    
    # Check if NATS CLI is installed
    if ! command -v nats &> /dev/null; then
        warn "NATS CLI not installed - skipping JetStream connectivity test"
        info "Install: curl -sf https://binaries.nats.dev/nats-io/natscli/nats@latest | sh"
        return 0
    fi
    
    # Check if NATS server is reachable
    NATS_URL="${NATS_URL:-nats://localhost:4222}"
    if ! timeout 5 nats server ping --server="$NATS_URL" &>/dev/null; then
        warn "NATS server not reachable at $NATS_URL"
        info "Start NATS: docker-compose up -d nats"
        return 0
    fi
    
    pass "NATS server is reachable at $NATS_URL"
    
    # Run JetStream E2E test suite
    cd "$PROJECT_ROOT/apps/otp/router"
    
    if [ ! -f "test/router_jetstream_e2e_SUITE.erl" ]; then
        warn "JetStream E2E test suite not found (test/router_jetstream_e2e_SUITE.erl)"
        cd "$PROJECT_ROOT"
        return 0
    fi
    
    # Run Extensions E2E test suite (CP2-LC required)
    if [ -f "test/router_extensions_e2e_SUITE.erl" ]; then
        info "Running Extensions E2E test suite (CP2-LC required)..."
        if rebar3 ct --suite router_extensions_e2e_SUITE --config test/ct.config > /tmp/extensions_e2e_test.log 2>&1; then
            pass "Extensions E2E test suite passed"
        else
            fail "Extensions E2E test suite failed (CP2-LC required)"
            cat /tmp/extensions_e2e_test.log | tail -50
            cd "$PROJECT_ROOT"
            return 1
        fi
    else
        warn "Extensions E2E test suite not found (test/router_extensions_e2e_SUITE.erl)"
    fi
    
    # Run Extensions Pipeline test suite (CP2-LC required)
    if [ -f "test/router_extensions_pipeline_SUITE.erl" ]; then
        info "Running Extensions Pipeline test suite (CP2-LC required)..."
        if rebar3 ct --suite router_extensions_pipeline_SUITE --config test/ct.config > /tmp/extensions_pipeline_test.log 2>&1; then
            pass "Extensions Pipeline test suite passed"
        else
            fail "Extensions Pipeline test suite failed (CP2-LC required)"
            cat /tmp/extensions_pipeline_test.log | tail -50
            cd "$PROJECT_ROOT"
            return 1
        fi
    else
        warn "Extensions Pipeline test suite not found (test/router_extensions_pipeline_SUITE.erl)"
    fi
    
    # Run Extensions Security test suite (CP2-LC required)
    if [ -f "test/router_extensions_security_SUITE.erl" ]; then
        info "Running Extensions Security test suite (CP2-LC required)..."
        if rebar3 ct --suite router_extensions_security_SUITE --config test/ct.config > /tmp/extensions_security_test.log 2>&1; then
            pass "Extensions Security test suite passed"
        else
            fail "Extensions Security test suite failed (CP2-LC required)"
            cat /tmp/extensions_security_test.log | tail -50
            cd "$PROJECT_ROOT"
            return 1
        fi
    else
        warn "Extensions Security test suite not found (test/router_extensions_security_SUITE.erl)"
    fi
    
    # Run Extensions Telemetry test suite (CP2-LC required)
    if [ -f "test/router_extension_invoker_telemetry_SUITE.erl" ]; then
        info "Running Extensions Telemetry test suite (CP2-LC required)..."
        if rebar3 ct --suite router_extension_invoker_telemetry_SUITE --config test/ct.config > /tmp/extensions_telemetry_test.log 2>&1; then
            pass "Extensions Telemetry test suite passed"
        else
            fail "Extensions Telemetry test suite failed (CP2-LC required)"
            cat /tmp/extensions_telemetry_test.log | tail -50
            cd "$PROJECT_ROOT"
            return 1
        fi
    else
        warn "Extensions Telemetry test suite not found (test/router_extension_invoker_telemetry_SUITE.erl)"
    fi
    
    info "Running JetStream E2E test suite (timeout: 5 minutes)..."
    set +e
    timeout 300 rebar3 ct --suite test/router_jetstream_e2e_SUITE --readable=false 2>&1 | tee /tmp/jetstream_test.log
    CT_EXIT_CODE=${PIPESTATUS[0]}
    set -e
    
    if [ $CT_EXIT_CODE -eq 124 ]; then
        fail "JetStream E2E tests TIMEOUT (5 minutes exceeded)"
        echo "Last 30 lines of test output:"
        tail -30 /tmp/jetstream_test.log
        cd "$PROJECT_ROOT"
        return 1
    elif [ $CT_EXIT_CODE -eq 0 ]; then
        pass "JetStream E2E tests passed"
    else
        fail "JetStream E2E tests failed (exit code: $CT_EXIT_CODE) - see /tmp/jetstream_test.log"
        echo "Last 30 lines of test output:"
        tail -30 /tmp/jetstream_test.log
        cd "$PROJECT_ROOT"
        return 1
    fi
    
    cd "$PROJECT_ROOT"
    return 0
}

# Runtime validation: Idempotency tests
validate_idempotency_runtime() {
    echo ""
    info "🔧 Testing idempotency layer runtime..."
    
    cd "$PROJECT_ROOT/apps/otp/router"
    
    # Check if test suite exists
    if [ ! -f "test/router_idempotency_SUITE.erl" ]; then
        fail "Idempotency test suite not found: test/router_idempotency_SUITE.erl"
        cd "$PROJECT_ROOT"
        return 1
    fi
    
    info "Running router_idempotency_SUITE (timeout: 5 minutes)..."
    set +e
    timeout 300 rebar3 ct --suite test/router_idempotency_SUITE --readable=false 2>&1 | tee /tmp/idempotency_test.log
    CT_EXIT_CODE=${PIPESTATUS[0]}
    set -e
    
    if [ $CT_EXIT_CODE -eq 124 ]; then
        fail "Idempotency tests TIMEOUT (5 minutes exceeded)"
        echo "Last 30 lines of test output:"
        tail -30 /tmp/idempotency_test.log
        cd "$PROJECT_ROOT"
        return 1
    elif [ $CT_EXIT_CODE -eq 0 ]; then
        pass "Idempotency tests passed"
        
        # Verify key test cases
        if grep -q "test_duplicate_request_returns_cached" /tmp/idempotency_test.log || \
           grep -q "duplicate.*request.*cached" /tmp/idempotency_test.log; then
            pass "  ✓ Duplicate request caching verified"
        fi
        if grep -q "test_expired_key_allows_reprocess" /tmp/idempotency_test.log || \
           grep -q "expired.*key.*reprocess" /tmp/idempotency_test.log; then
            pass "  ✓ TTL expiration verified"
        fi
    else
        fail "Idempotency tests failed (exit code: $CT_EXIT_CODE) - see /tmp/idempotency_test.log"
        echo "Last 30 lines of test output:"
        tail -30 /tmp/idempotency_test.log
        cd "$PROJECT_ROOT"
        return 1
    fi
    
    cd "$PROJECT_ROOT"
    return 0
}

# Runtime validation: Tracing integration
validate_tracing_integration() {
    echo ""
    info "🔧 Verifying OpenTelemetry tracing integration..."
    
    ROUTER_SRC="$PROJECT_ROOT/apps/otp/router/src"
    TRACE_SCORE=0
    
    # Check router_tracing.erl for OpenTelemetry integration
    if [ -f "$ROUTER_SRC/router_tracing.erl" ]; then
        if grep -q "opentelemetry" "$ROUTER_SRC/router_tracing.erl" 2>/dev/null || \
           grep -q "otel_tracer" "$ROUTER_SRC/router_tracing.erl" 2>/dev/null; then
            pass "OpenTelemetry integration found in router_tracing.erl"
            TRACE_SCORE=$((TRACE_SCORE + 1))
            
            # Check for critical OTel API patterns
            if grep -Eq "otel_tracer:start_span|opentelemetry:start_span|start_span" "$ROUTER_SRC/router_tracing.erl"; then
                pass "  ✓ Span creation API verified"
                TRACE_SCORE=$((TRACE_SCORE + 1))
            fi
            
            if grep -Eq "otel_span:set_attribute|opentelemetry:set_attribute|set_span_attribute" "$ROUTER_SRC/router_tracing.erl"; then
                pass "  ✓ Span attributes API verified"
                TRACE_SCORE=$((TRACE_SCORE + 1))
            fi
            
            if grep -Eq "otel_tracer:end_span|opentelemetry:end_span|end_span" "$ROUTER_SRC/router_tracing.erl"; then
                pass "  ✓ Span completion API verified"
                TRACE_SCORE=$((TRACE_SCORE + 1))
            fi
        else
            warn "OpenTelemetry integration not found in router_tracing.erl"
        fi
    else
        fail "router_tracing.erl not found"
        return 1
    fi
    
    # Check tracing in key modules (must have at least one)
    KEY_MODULES=("router_result_consumer.erl" "router_nats_subscriber.erl" "router_caf_adapter.erl")
    TRACED_COUNT=0
    
    for module in "${KEY_MODULES[@]}"; do
        if [ -f "$ROUTER_SRC/$module" ]; then
            if grep -Eq "router_tracing:|otel_tracer:|opentelemetry:" "$ROUTER_SRC/$module"; then
                pass "  ✓ Tracing found in $module"
                TRACED_COUNT=$((TRACED_COUNT + 1))
                TRACE_SCORE=$((TRACE_SCORE + 1))
            fi
        fi
    done
    
    # Scoring and final verdict
    if [ $TRACE_SCORE -ge 4 ]; then
        pass "Tracing integration verified (score: $TRACE_SCORE/7)"
    elif [ $TRACE_SCORE -ge 2 ]; then
        warn "Tracing integration partial (score: $TRACE_SCORE/7) - consider adding more instrumentation"
    else
        fail "Tracing integration insufficient (score: $TRACE_SCORE/7)"
        return 1
    fi
    
    return 0
}

# Runtime validation: Tenant validation tests
validate_tenant_validation_runtime() {
    echo ""
    info "🔧 Testing tenant validation runtime..."
    
    cd "$PROJECT_ROOT/apps/otp/router"
    
    # Run tenant allowlist tests
    if [ -f "test/router_tenant_allowlist_SUITE.erl" ]; then
        info "Running router_tenant_allowlist_SUITE (timeout: 5 minutes)..."
        set +e
        timeout 300 rebar3 ct --suite test/router_tenant_allowlist_SUITE --readable=false 2>&1 | tee /tmp/tenant_test.log
        CT_EXIT_CODE=${PIPESTATUS[0]}
        set -e
        
        if [ $CT_EXIT_CODE -eq 124 ]; then
            fail "Tenant allowlist tests TIMEOUT (5 minutes exceeded)"
            echo "Last 30 lines of test output:"
            tail -30 /tmp/tenant_test.log
            cd "$PROJECT_ROOT"
            return 1
        elif [ $CT_EXIT_CODE -eq 0 ]; then
            pass "Tenant allowlist tests passed"
        else
            fail "Tenant allowlist tests failed (exit code: $CT_EXIT_CODE) - see /tmp/tenant_test.log"
            echo "Last 30 lines of test output:"
            tail -30 /tmp/tenant_test.log
            cd "$PROJECT_ROOT"
            return 1
        fi
    else
        fail "Tenant allowlist test suite not found"
        cd "$PROJECT_ROOT"
        return 1
    fi
    
    # Run multi-tenant smoke tests (optional but recommended)
    if [ -f "test/router_tenant_multitenant_smoke_SUITE.erl" ]; then
        info "Running router_tenant_multitenant_smoke_SUITE (timeout: 5 minutes)..."
        set +e
        timeout 300 rebar3 ct --suite test/router_tenant_multitenant_smoke_SUITE --readable=false 2>&1 | tee /tmp/multitenant_test.log
        CT_EXIT_CODE=${PIPESTATUS[0]}
        set -e
        
        if [ $CT_EXIT_CODE -eq 124 ]; then
            warn "Multi-tenant smoke tests TIMEOUT (non-blocking)"
            tail -20 /tmp/multitenant_test.log
        elif [ $CT_EXIT_CODE -eq 0 ]; then
            pass "Multi-tenant smoke tests passed"
        else
            warn "Multi-tenant smoke tests failed (non-blocking, exit code: $CT_EXIT_CODE)"
            tail -20 /tmp/multitenant_test.log
        fi
    fi
    
    cd "$PROJECT_ROOT"
    return 0
}

# Runtime validation: Admin gRPC service
validate_admin_grpc_runtime() {
    echo ""
    info "🔧 Testing Admin gRPC service runtime..."
    
    cd "$PROJECT_ROOT/apps/otp/router"
    
    # Check if admin gRPC tests exist
    if [ -f "test/router_admin_grpc_integration_SUITE.erl" ]; then
        info "Running router_admin_grpc_integration_SUITE (timeout: 5 minutes)..."
        set +e
        timeout 300 rebar3 ct --suite test/router_admin_grpc_integration_SUITE --readable=false 2>&1 | tee /tmp/admin_grpc_test.log
        CT_EXIT_CODE=${PIPESTATUS[0]}
        set -e
        
        if [ $CT_EXIT_CODE -eq 124 ]; then
            warn "Admin gRPC integration tests TIMEOUT (optional feature)"
            tail -20 /tmp/admin_grpc_test.log
        elif [ $CT_EXIT_CODE -eq 0 ]; then
            pass "Admin gRPC integration tests passed"
        else
            # Admin gRPC is optional in CP2, so this is a warning
            warn "Admin gRPC integration tests failed (optional feature, exit code: $CT_EXIT_CODE)"
            tail -20 /tmp/admin_grpc_test.log
        fi
    else
        info "Admin gRPC test suite not found (optional feature)"
    fi
    
    cd "$PROJECT_ROOT"
    return 0
}

# Main execution
echo "=========================================="
echo "CP2 Validation Suite"
echo "=========================================="
echo ""

# Static validations
validate_feature_flags || exit $EXIT_FEATURE_FLAGS
validate_cp_check || true  # Non-blocking warning
validate_required_modules || exit $EXIT_MODULES
validate_jetstream_config || exit $EXIT_CONFIG
validate_supervisor_gating || true  # Warnings only

# Runtime validations
echo ""
echo "=========================================="
echo "Running CP2 Runtime Validations"
echo "=========================================="

validate_runtime_jetstream || exit $EXIT_JETSTREAM
validate_idempotency_runtime || exit $EXIT_IDEMPOTENCY
validate_tracing_integration || exit $EXIT_TRACING
validate_tenant_validation_runtime || exit $EXIT_TENANT
validate_admin_grpc_runtime || true  # Non-blocking

# Summary
echo ""
echo "=========================================="
echo "CP2 Validation Summary"
echo "=========================================="
echo ""

# Static checks summary
STATIC_PASSED=true
if [ $FAIL_COUNT -gt 0 ]; then
    STATIC_PASSED=false
fi

echo "Static Checks:"
echo "  - Feature Flags: $([ "$STATIC_PASSED" = "true" ] && echo "✅ PASS" || echo "❌ FAIL")"
echo "  - CP Check: ✅ PASS (warnings allowed)"
echo "  - Modules: ✅ PASS"
echo "  - JetStream Config: ✅ PASS (warnings allowed)"
echo "  - Supervisor Gating: ✅ PASS (warnings allowed)"
echo ""

# Runtime checks summary (based on test log presence and exit codes)
echo "Runtime Checks:"
if [ -f /tmp/jetstream_test.log ] && [ -s /tmp/jetstream_test.log ]; then
    # Check if log contains success indicators (Common Test success patterns)
    if grep -qiE "(passed|success|ok)" /tmp/jetstream_test.log 2>/dev/null && \
       ! grep -qiE "(failed|error|timeout|aborted)" /tmp/jetstream_test.log 2>/dev/null; then
        echo "  - JetStream: ✅ PASS"
    else
        echo "  - JetStream: ❌ FAIL"
    fi
else
    echo "  - JetStream: ⏭️  SKIP (NATS unavailable or test suite not found)"
fi

if [ -f /tmp/idempotency_test.log ] && [ -s /tmp/idempotency_test.log ]; then
    if grep -qiE "(passed|success|ok)" /tmp/idempotency_test.log 2>/dev/null && \
       ! grep -qiE "(failed|error|timeout|aborted)" /tmp/idempotency_test.log 2>/dev/null; then
        echo "  - Idempotency: ✅ PASS"
    else
        echo "  - Idempotency: ❌ FAIL"
    fi
else
    echo "  - Idempotency: ⏭️  SKIP (test suite not run)"
fi

# Tracing is code inspection, always shows as pass if function returned 0
echo "  - Tracing: ✅ PASS (code inspection)"

if [ -f /tmp/tenant_test.log ] && [ -s /tmp/tenant_test.log ]; then
    if grep -qiE "(passed|success|ok)" /tmp/tenant_test.log 2>/dev/null && \
       ! grep -qiE "(failed|error|timeout|aborted)" /tmp/tenant_test.log 2>/dev/null; then
        echo "  - Tenant Validation: ✅ PASS"
    else
        echo "  - Tenant Validation: ❌ FAIL"
    fi
else
    echo "  - Tenant Validation: ⏭️  SKIP (test suite not run)"
fi

if [ -f /tmp/admin_grpc_test.log ] && [ -s /tmp/admin_grpc_test.log ]; then
    if grep -qiE "(passed|success|ok)" /tmp/admin_grpc_test.log 2>/dev/null && \
       ! grep -qiE "(failed|error|timeout|aborted)" /tmp/admin_grpc_test.log 2>/dev/null; then
        echo "  - Admin gRPC: ✅ PASS (optional)"
    else
        echo "  - Admin gRPC: ⚠️  WARN (optional feature)"
    fi
else
    echo "  - Admin gRPC: ⏭️  SKIP (optional feature)"
fi

echo ""
echo "Overall:"
echo "  Failures: $FAIL_COUNT"
echo "  Warnings: $WARN_COUNT"
echo ""

if [ $FAIL_COUNT -eq 0 ]; then
    if [ $WARN_COUNT -eq 0 ]; then
        pass "✅ All CP2 validations passed (static + runtime)"
        exit $EXIT_SUCCESS
    else
        warn "⚠️  CP2 validation passed with $WARN_COUNT warnings"
        exit $EXIT_SUCCESS
    fi
else
    fail "❌ CP2 validation failed ($FAIL_COUNT errors)"
    exit $EXIT_FEATURE_FLAGS
fi

