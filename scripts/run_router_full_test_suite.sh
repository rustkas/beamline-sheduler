#!/bin/bash
#
# Router Full Test Suite Runner
#
# Runs all Router tests (e2e, chaos, load) with real NATS and generates
# comprehensive test report artifacts for CP2 readiness verification.
#
# Usage:
#   ./scripts/run_router_full_test_suite.sh [--e2e-only] [--chaos-only] [--load-only] [--output-dir DIR]
#
# Environment Variables:
#   NATS_URL: NATS server URL (default: nats://localhost:4222)
#   ROUTER_TEST_TIMEOUT: Test timeout in seconds (default: 300)
#   LOAD_TEST_MESSAGE_COUNT: Number of messages for load tests (default: 2000)
#   LOAD_TEST_PARALLEL_WORKERS: Number of parallel workers (default: 1)
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
ROUTER_DIR="${PROJECT_ROOT}/apps/otp/router"
OUTPUT_DIR="${PROJECT_ROOT}/reports/router-test-results"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
REPORT_DIR="${OUTPUT_DIR}/${TIMESTAMP}"

# Default configuration
NATS_URL="${NATS_URL:-nats://localhost:4222}"
ROUTER_TEST_TIMEOUT="${ROUTER_TEST_TIMEOUT:-300}"
LOAD_TEST_MESSAGE_COUNT="${LOAD_TEST_MESSAGE_COUNT:-2000}"
LOAD_TEST_PARALLEL_WORKERS="${LOAD_TEST_PARALLEL_WORKERS:-1}"

# Test flags
RUN_E2E=true
RUN_CHAOS=true
RUN_LOAD=true
PRE_RELEASE=false

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --e2e-only)
            RUN_E2E=true
            RUN_CHAOS=false
            RUN_LOAD=false
            PRE_RELEASE=false
            shift
            ;;
        --chaos-only)
            RUN_E2E=false
            RUN_CHAOS=true
            RUN_LOAD=false
            PRE_RELEASE=false
            shift
            ;;
        --load-only)
            RUN_E2E=false
            RUN_CHAOS=false
            RUN_LOAD=true
            PRE_RELEASE=false
            shift
            ;;
        --pre-release)
            # Pre-Release regression profile: JetStream E2E + Idempotency + RateLimit + CP2 Features E2E + Chaos
            RUN_E2E=true
            RUN_CHAOS=true
            RUN_LOAD=false
            PRE_RELEASE=true
            shift
            ;;
        --output-dir)
            REPORT_DIR="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1"
            echo "Usage: $0 [--e2e-only] [--chaos-only] [--load-only] [--pre-release] [--output-dir DIR]"
            exit 1
            ;;
    esac
done

# Create report directory
mkdir -p "${REPORT_DIR}"

# Logging functions
log_info() {
    echo "[INFO] $(date +%Y-%m-%d\ %H:%M:%S) $*" | tee -a "${REPORT_DIR}/test-run.log"
}

log_error() {
    echo "[ERROR] $(date +%Y-%m-%d\ %H:%M:%S) $*" | tee -a "${REPORT_DIR}/test-run.log" >&2
}

log_section() {
    echo "" | tee -a "${REPORT_DIR}/test-run.log"
    echo "========================================" | tee -a "${REPORT_DIR}/test-run.log"
    echo "$*" | tee -a "${REPORT_DIR}/test-run.log"
    echo "========================================" | tee -a "${REPORT_DIR}/test-run.log"
}

# Check prerequisites
check_prerequisites() {
    log_section "Checking Prerequisites"
    
    # Check NATS availability
    if ! command -v nats &> /dev/null; then
        log_error "nats CLI not found. Install with: go install github.com/nats-io/natscli/nats@latest"
        return 1
    fi
    
    # Check NATS connection
    if ! nats server check --server "${NATS_URL}" &> /dev/null; then
        log_error "NATS server not available at ${NATS_URL}"
        log_error "Start NATS with: docker-compose up -d nats"
        return 1
    fi
    log_info "NATS server available at ${NATS_URL}"
    
    # Check Erlang/OTP
    if ! command -v rebar3 &> /dev/null; then
        log_error "rebar3 not found. Install Erlang/OTP and rebar3"
        return 1
    fi
    log_info "rebar3 available: $(rebar3 version)"
    
    # Check Router directory
    if [[ ! -d "${ROUTER_DIR}" ]]; then
        log_error "Router directory not found: ${ROUTER_DIR}"
        return 1
    fi
    log_info "Router directory: ${ROUTER_DIR}"
    
    return 0
}

# Run E2E tests
run_e2e_tests() {
    log_section "Running E2E Tests"
    
    local e2e_report="${REPORT_DIR}/e2e-test-results.xml"
    local e2e_log="${REPORT_DIR}/e2e-test.log"
    
    log_info "Starting E2E test suite..."
    if [[ "${PRE_RELEASE}" == "true" ]]; then
        log_info "Pre-Release regression profile: Running CP2-focused E2E tests"
    fi
    
    cd "${ROUTER_DIR}"
    
    local e2e_passed=true
    
    # Pre-Release profile: Skip standard E2E, focus on CP2 features
    if [[ "${PRE_RELEASE}" != "true" ]]; then
        # Run standard E2E tests
        if ! rebar3 ct \
            --suite router_intake_e2e_SUITE \
            --dir "${REPORT_DIR}/e2e-ct-logs" \
            --logdir "${REPORT_DIR}/e2e-ct-logs" \
            --config "${ROUTER_DIR}/test/ct.config" \
            > "${e2e_log}" 2>&1; then
            log_error "Standard E2E tests FAILED"
            e2e_passed=false
        else
            log_info "Standard E2E tests PASSED"
        fi
    fi
    
    # Pre-Release profile: Run JetStream E2E (CP2 core feature)
    if [[ "${PRE_RELEASE}" == "true" ]]; then
        log_info "Running JetStream E2E tests (Pre-Release profile)..."
        if ! rebar3 ct \
            --suite router_jetstream_e2e_SUITE \
            --dir "${REPORT_DIR}/e2e-jetstream-ct-logs" \
            --logdir "${REPORT_DIR}/e2e-jetstream-ct-logs" \
            --config "${ROUTER_DIR}/test/ct.config" \
            >> "${e2e_log}" 2>&1; then
            log_error "JetStream E2E tests FAILED"
            e2e_passed=false
        else
            log_info "JetStream E2E tests PASSED"
        fi
    fi
    
    # Run Extensions E2E tests (CP2-LC)
    log_info "Running Extensions E2E tests..."
    if ! rebar3 ct \
        --suite router_extensions_e2e_SUITE \
        --dir "${REPORT_DIR}/e2e-extensions-ct-logs" \
        --logdir "${REPORT_DIR}/e2e-extensions-ct-logs" \
        --config "${ROUTER_DIR}/test/ct.config" \
        >> "${e2e_log}" 2>&1; then
        log_error "Extensions E2E tests FAILED"
        e2e_passed=false
    else
        log_info "Extensions E2E tests PASSED"
    fi
    
    # Run CP2 Features E2E tests (CP2-LC Integration) ⭐ Single source CP2 verification
    log_info "Running CP2 Features E2E tests (single source CP2 integration check)..."
    if ! rebar3 ct \
        --suite router_cp2_features_e2e_SUITE \
        --dir "${REPORT_DIR}/e2e-cp2-features-ct-logs" \
        --logdir "${REPORT_DIR}/e2e-cp2-features-ct-logs" \
        --config "${ROUTER_DIR}/test/ct.config" \
        >> "${e2e_log}" 2>&1; then
        log_error "CP2 Features E2E tests FAILED"
        e2e_passed=false
    else
        log_info "CP2 Features E2E tests PASSED"
    fi
    
    # Pre-Release profile: Run Idempotency + RateLimit combo
    if [[ "${PRE_RELEASE}" == "true" ]]; then
        log_info "Running Idempotency + RateLimit combo tests (Pre-Release profile)..."
        # Run idempotency tests
        if ! rebar3 ct \
            --suite router_idem_SUITE \
            --dir "${REPORT_DIR}/e2e-idem-ct-logs" \
            --logdir "${REPORT_DIR}/e2e-idem-ct-logs" \
            --config "${ROUTER_DIR}/test/ct.config" \
            >> "${e2e_log}" 2>&1; then
            log_error "Idempotency tests FAILED"
            e2e_passed=false
        else
            log_info "Idempotency tests PASSED"
        fi
        # Run rate limit tests
        if ! rebar3 ct \
            --suite router_rate_limit_store_SUITE \
            --dir "${REPORT_DIR}/e2e-ratelimit-ct-logs" \
            --logdir "${REPORT_DIR}/e2e-ratelimit-ct-logs" \
            --config "${ROUTER_DIR}/test/ct.config" \
            >> "${e2e_log}" 2>&1; then
            log_error "RateLimit tests FAILED"
            e2e_passed=false
        else
            log_info "RateLimit tests PASSED"
        fi
    fi
    
    if [[ "${e2e_passed}" == "true" ]]; then
        echo "PASSED" > "${REPORT_DIR}/e2e-status.txt"
        return 0
    else
        echo "FAILED" > "${REPORT_DIR}/e2e-status.txt"
        return 1
    fi
}

# Run Chaos tests
run_chaos_tests() {
    log_section "Running Chaos Tests"
    
    local chaos_report="${REPORT_DIR}/chaos-test-results.xml"
    local chaos_log="${REPORT_DIR}/chaos-test.log"
    
    log_info "Starting Chaos test suite..."
    
    cd "${ROUTER_DIR}"
    
    if rebar3 ct \
        --suite router_intake_chaos_SUITE \
        --dir "${REPORT_DIR}/chaos-ct-logs" \
        --logdir "${REPORT_DIR}/chaos-ct-logs" \
        --config "${ROUTER_DIR}/test/ct.config" \
        > "${chaos_log}" 2>&1; then
        log_info "Chaos tests PASSED"
        echo "PASSED" > "${REPORT_DIR}/chaos-status.txt"
        return 0
    else
        log_error "Chaos tests FAILED"
        echo "FAILED" > "${REPORT_DIR}/chaos-status.txt"
        return 1
    fi
}

# Run Load tests
run_load_tests() {
    log_section "Running Load Tests"
    
    local load_log="${REPORT_DIR}/load-test.log"
    
    log_info "Starting Load test suite..."
    log_info "Configuration: MESSAGE_COUNT=${LOAD_TEST_MESSAGE_COUNT}, PARALLEL_WORKERS=${LOAD_TEST_PARALLEL_WORKERS}"
    
    cd "${ROUTER_DIR}"
    
    export LOAD_TEST_MESSAGE_COUNT
    export LOAD_TEST_PARALLEL_WORKERS
    
    # Run standard load tests
    local load_passed=true
    if ! rebar3 ct \
        --suite router_intake_e2e_SUITE \
        --group load_tests \
        --dir "${REPORT_DIR}/load-ct-logs" \
        --logdir "${REPORT_DIR}/load-ct-logs" \
        --config "${ROUTER_DIR}/test/ct.config" \
        > "${load_log}" 2>&1; then
        log_error "Standard load tests FAILED"
        load_passed=false
    else
        log_info "Standard load tests PASSED"
    fi
    
    # Run Extensions Pipeline Load tests (Pre-Release)
    log_info "Running Extensions Pipeline Load tests (Pre-Release)..."
    if ! rebar3 ct \
        --suite router_extensions_pipeline_load_SUITE \
        --dir "${REPORT_DIR}/load-extensions-ct-logs" \
        --logdir "${REPORT_DIR}/load-extensions-ct-logs" \
        --config "${ROUTER_DIR}/test/ct.config" \
        >> "${load_log}" 2>&1; then
        log_error "Extensions Pipeline Load tests FAILED (Pre-Release)"
        # Don't fail overall load tests for Pre-Release suite
        log_info "Extensions Pipeline Load tests are Pre-Release only, continuing..."
    else
        log_info "Extensions Pipeline Load tests PASSED"
    fi
    
    if [[ "${load_passed}" == "true" ]]; then
        echo "PASSED" > "${REPORT_DIR}/load-status.txt"
        return 0
    else
        echo "FAILED" > "${REPORT_DIR}/load-status.txt"
        return 1
    fi
}

# Generate summary report
generate_summary_report() {
    log_section "Generating Summary Report"
    
    local summary_file="${REPORT_DIR}/test-summary.json"
    local summary_md="${REPORT_DIR}/test-summary.md"
    
    # Read test statuses
    local e2e_status="UNKNOWN"
    local chaos_status="UNKNOWN"
    local load_status="UNKNOWN"
    
    [[ -f "${REPORT_DIR}/e2e-status.txt" ]] && e2e_status=$(cat "${REPORT_DIR}/e2e-status.txt")
    [[ -f "${REPORT_DIR}/chaos-status.txt" ]] && chaos_status=$(cat "${REPORT_DIR}/chaos-status.txt")
    [[ -f "${REPORT_DIR}/load-status.txt" ]] && load_status=$(cat "${REPORT_DIR}/load-status.txt")
    
    # Calculate overall status
    local overall_status="PASSED"
    if [[ "${e2e_status}" == "FAILED" ]] || [[ "${chaos_status}" == "FAILED" ]] || [[ "${load_status}" == "FAILED" ]]; then
        overall_status="FAILED"
    fi
    
    # Generate JSON summary
    cat > "${summary_file}" <<EOF
{
  "timestamp": "${TIMESTAMP}",
  "overall_status": "${overall_status}",
  "test_suites": {
    "e2e": {
      "status": "${e2e_status}",
      "enabled": ${RUN_E2E}
    },
    "chaos": {
      "status": "${chaos_status}",
      "enabled": ${RUN_CHAOS}
    },
    "load": {
      "status": "${load_status}",
      "enabled": ${RUN_LOAD}
    }
  },
  "configuration": {
    "nats_url": "${NATS_URL}",
    "timeout": "${ROUTER_TEST_TIMEOUT}",
    "load_test_message_count": "${LOAD_TEST_MESSAGE_COUNT}",
    "load_test_parallel_workers": "${LOAD_TEST_PARALLEL_WORKERS}"
  },
  "artifacts": {
    "report_dir": "${REPORT_DIR}",
    "log_file": "${REPORT_DIR}/test-run.log"
  }
}
EOF
    
    # Generate Markdown summary
    cat > "${summary_md}" <<EOF
# Router Test Suite Results

**Timestamp**: ${TIMESTAMP}  
**Overall Status**: **${overall_status}**

## Test Suite Status

| Suite | Status | Enabled |
|-------|--------|---------|
| E2E Tests | ${e2e_status} | ${RUN_E2E} |
| Chaos Tests | ${chaos_status} | ${RUN_CHAOS} |
| Load Tests | ${load_status} | ${RUN_LOAD} |

## Configuration

- **NATS URL**: ${NATS_URL}
- **Test Timeout**: ${ROUTER_TEST_TIMEOUT}s
- **Load Test Message Count**: ${LOAD_TEST_MESSAGE_COUNT}
- **Load Test Parallel Workers**: ${LOAD_TEST_PARALLEL_WORKERS}

## Artifacts

- **Report Directory**: \`${REPORT_DIR}\`
- **Test Log**: \`${REPORT_DIR}/test-run.log\`
- **E2E Log**: \`${REPORT_DIR}/e2e-test.log\`
- **Chaos Log**: \`${REPORT_DIR}/chaos-test.log\`
- **Load Log**: \`${REPORT_DIR}/load-test.log\`

## Next Steps

1. Review test logs in \`${REPORT_DIR}\`
2. Check Common Test logs in \`${REPORT_DIR}/*-ct-logs/\`
3. Verify NATS JetStream state and DLQ contents
4. Review metrics and audit logs if available

EOF
    
    log_info "Summary report generated: ${summary_md}"
    log_info "JSON summary: ${summary_file}"
    
    # Print summary to console
    cat "${summary_md}"
}

# Main execution
main() {
    log_section "Router Full Test Suite Runner"
    log_info "Starting test run at $(date)"
    log_info "Report directory: ${REPORT_DIR}"
    
    # Save environment info
    {
        echo "=== Environment Information ==="
        echo "Timestamp: ${TIMESTAMP}"
        echo "NATS URL: ${NATS_URL}"
        echo "Router Directory: ${ROUTER_DIR}"
        echo "Report Directory: ${REPORT_DIR}"
        echo ""
        echo "=== System Information ==="
        uname -a
        echo ""
        echo "=== Erlang/OTP Version ==="
        erl -version 2>&1 || echo "Erlang not available"
        echo ""
        echo "=== Rebar3 Version ==="
        rebar3 version || echo "Rebar3 not available"
        echo ""
    } > "${REPORT_DIR}/environment.txt"
    
    # Check prerequisites
    if ! check_prerequisites; then
        log_error "Prerequisites check failed"
        exit 1
    fi
    
    # Run test suites
    local e2e_result=0
    local chaos_result=0
    local load_result=0
    
    if [[ "${RUN_E2E}" == "true" ]]; then
        run_e2e_tests || e2e_result=$?
    else
        log_info "E2E tests skipped"
        echo "SKIPPED" > "${REPORT_DIR}/e2e-status.txt"
    fi
    
    if [[ "${RUN_CHAOS}" == "true" ]]; then
        run_chaos_tests || chaos_result=$?
    else
        log_info "Chaos tests skipped"
        echo "SKIPPED" > "${REPORT_DIR}/chaos-status.txt"
    fi
    
    if [[ "${RUN_LOAD}" == "true" ]]; then
        run_load_tests || load_result=$?
    else
        log_info "Load tests skipped"
        echo "SKIPPED" > "${REPORT_DIR}/load-status.txt"
    fi
    
    # Generate summary
    generate_summary_report
    
    # Verify metrics after tests
    if command -v "${PROJECT_ROOT}/scripts/verify_metrics_after_tests.sh" &> /dev/null; then
        log_section "Verifying Metrics After Tests"
        
        # Determine scenario based on which tests ran
        local scenario="unknown"
        if [[ "${RUN_LOAD}" == "true" ]]; then
            scenario="load_mixed"  # Default to mixed for load tests
        elif [[ "${RUN_CHAOS}" == "true" ]]; then
            scenario="chaos_nats_multiple"  # Default to multiple for chaos tests
        elif [[ "${RUN_E2E}" == "true" ]]; then
            scenario="load_success"  # Default to success for e2e tests
        fi
        
        if "${PROJECT_ROOT}/scripts/verify_metrics_after_tests.sh" \
            --router-url "${NATS_URL/http/nats}" \
            --scenario "${scenario}" >> "${REPORT_DIR}/metrics-verification.log" 2>&1; then
            log_info "Metrics verification passed"
            echo "PASSED" > "${REPORT_DIR}/metrics-verification-status.txt"
        else
            log_warn "Metrics verification found issues (check ${REPORT_DIR}/metrics-verification.log)"
            echo "FAILED" > "${REPORT_DIR}/metrics-verification-status.txt"
        fi
    else
        log_warn "Metrics verification script not found (skipping)"
    fi
    
    # Exit with appropriate code
    if [[ ${e2e_result} -ne 0 ]] || [[ ${chaos_result} -ne 0 ]] || [[ ${load_result} -ne 0 ]]; then
        log_error "Some tests failed. Check logs in ${REPORT_DIR}"
        exit 1
    else
        log_info "All tests passed!"
        exit 0
    fi
}

# Run main function
main "$@"

