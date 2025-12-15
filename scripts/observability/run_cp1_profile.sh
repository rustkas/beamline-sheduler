#!/usr/bin/env bash
# CP1 Observability Test Profile Runner
#
# Purpose: Run all CP1 core observability tests as a unified "package"
# Tests: Router, Gateway, Worker CP1 core tests + validation scripts
# Exit codes: 0 = all passed, 1 = one or more failed, 2 = services not running (E2E skipped)
#
# CP1 Profile Scope: This script runs CP1 core tests only. CP2 extensions are available
# via --include-cp2 flag but are NOT part of the CP1 profile. For full CP2 test profile,
# see docs/OBSERVABILITY_CP2_TEST_PROFILE.md and scripts/observability/run_cp2_profile.sh (planned).
#
# Usage:
#   bash scripts/observability/run_cp1_profile.sh
#   bash scripts/observability/run_cp1_profile.sh --component router
#   bash scripts/observability/run_cp1_profile.sh --skip-e2e
#   bash scripts/observability/run_cp1_profile.sh --include-cp2  # Optional: includes CP2+ tests (performance, edge cases)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0
SKIPPED_TESTS=0
WARNINGS=0

# Options
COMPONENT=""  # router, gateway, worker, or empty for all
SKIP_E2E=false
INCLUDE_CP2=false

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --component)
            COMPONENT="$2"
            shift 2
            ;;
        --skip-e2e)
            SKIP_E2E=true
            shift
            ;;
        --include-cp2)
            INCLUDE_CP2=true
            shift
            ;;
        --help|-h)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --component COMPONENT    Run tests for specific component (router, gateway, worker)"
            echo "  --skip-e2e               Skip E2E test scripts (unit/integration only)"
            echo "  --include-cp2            Include CP2+ tests (performance, edge cases)"
            echo "  --help, -h               Show this help message"
            echo ""
            echo "Examples:"
            echo "  $0                                    # Run all CP1 core tests"
            echo "  $0 --component router                # Run Router CP1 tests only"
            echo "  $0 --skip-e2e                        # Skip E2E tests"
            echo "  $0 --include-cp2                     # Include CP2+ tests"
            exit 0
            ;;
        *)
            echo -e "${RED}[ERROR]${NC} Unknown option: $1"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
done

log_info() {
    echo -e "${GREEN}[INFO]${NC} $*"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $*"
    ((WARNINGS++)) || true
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*"
}

log_test() {
    echo -e "${BLUE}[TEST]${NC} $*"
}

log_section() {
    echo ""
    echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${GREEN}$1${NC}"
    echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo ""
}

# Run test command and track results
run_test() {
    local test_name="$1"
    local test_command="$2"
    local test_dir="${3:-}"
    
    ((TOTAL_TESTS++)) || true
    log_test "Running: $test_name"
    
    if [[ -n "$test_dir" ]]; then
        cd "$test_dir" || {
            log_error "Cannot change to directory: $test_dir"
            ((FAILED_TESTS++)) || true
            return 1
        }
    fi
    
    if eval "$test_command"; then
        log_info "✓ $test_name passed"
        ((PASSED_TESTS++)) || true
        if [[ -n "$test_dir" ]]; then
            cd "$PROJECT_ROOT" || true
        fi
        return 0
    else
        local exit_code=$?
        log_error "✗ $test_name failed (exit code: $exit_code)"
        ((FAILED_TESTS++)) || true
        if [[ -n "$test_dir" ]]; then
            cd "$PROJECT_ROOT" || true
        fi
        return 1
    fi
}

# Run test script and handle exit codes
run_test_script() {
    local test_name="$1"
    local script_path="$2"
    
    ((TOTAL_TESTS++)) || true
    log_test "Running: $test_name"
    
    if [[ ! -f "$script_path" ]]; then
        log_error "Script not found: $script_path"
        ((FAILED_TESTS++)) || true
        return 1
    fi
    
    if bash "$script_path"; then
        log_info "✓ $test_name passed"
        ((PASSED_TESTS++)) || true
        return 0
    else
        local exit_code=$?
        if [[ $exit_code -eq 2 ]]; then
            log_warn "⚠ $test_name skipped (service not running)"
            ((SKIPPED_TESTS++)) || true
            return 2
        else
            log_error "✗ $test_name failed (exit code: $exit_code)"
            ((FAILED_TESTS++)) || true
            return 1
        fi
    fi
}

# General Validation
run_general_validation() {
    log_section "Step 1: General Observability Validation"
    
    run_test_script "General Observability Validation" \
        "${SCRIPT_DIR}/validate_observability.sh"
}

# Router CP1 Core Tests
run_router_tests() {
    log_section "Step 2: Router CP1 Core Tests"
    
    local router_dir="${PROJECT_ROOT}/apps/otp/router"
    
    # Unit Tests (CP1 core groups only)
    log_info "Running Router unit tests (CP1 core)..."
    run_test "Router Unit Tests (CP1 Core)" \
        "rebar3 ct --suite test/router_observability_SUITE --group log_format_tests --group pii_filtering_tests --group health_endpoint_tests --group logging_scenarios_tests" \
        "$router_dir"
    
    # Integration Tests
    log_info "Running Router integration tests..."
    run_test "Router Integration Tests (Health Endpoint)" \
        "rebar3 ct --suite test/router_health_integration_SUITE" \
        "$router_dir"
    
    # E2E Test Script
    if [[ "$SKIP_E2E" == "false" ]]; then
        log_info "Running Router E2E test script..."
        run_test_script "Router E2E Test" \
            "${SCRIPT_DIR}/test_router_observability.sh"
    else
        log_warn "Skipping Router E2E test (--skip-e2e flag)"
        ((SKIPPED_TESTS++)) || true
    fi
    
    # CP2+ Tests (optional)
    if [[ "$INCLUDE_CP2" == "true" ]]; then
        log_info "Running Router CP2+ tests (performance, edge cases)..."
        
        # Performance Tests
        run_test "Router Performance Tests" \
            "rebar3 ct --suite test/router_observability_performance_SUITE" \
            "$router_dir"
        
        # Edge Case Tests
        run_test "Router Edge Case Tests" \
            "rebar3 ct --suite test/router_observability_SUITE --group edge_case_tests" \
            "$router_dir"
    fi
}

# Gateway CP1 Core Tests
run_gateway_tests() {
    log_section "Step 3: Gateway CP1 Core Tests"
    
    local gateway_dir="${PROJECT_ROOT}/apps/c-gateway"
    local gateway_build_dir="${gateway_dir}/build"
    
    # Unit Tests
    log_info "Running Gateway unit tests..."
    if [[ -f "${gateway_dir}/Makefile" ]]; then
        run_test "Gateway Unit Tests (Observability)" \
            "make test-observability" \
            "$gateway_dir"
    elif [[ -d "$gateway_build_dir" ]]; then
        run_test "Gateway Unit Tests (Observability)" \
            "cd build && make c-gateway-observability-test && ./c-gateway-observability-test" \
            "$gateway_dir"
    else
        log_warn "Gateway build directory not found. Building first..."
        run_test "Gateway Build and Unit Tests" \
            "mkdir -p build && cd build && cmake .. && make c-gateway-observability-test && ./c-gateway-observability-test" \
            "$gateway_dir"
    fi
    
    # Integration Tests
    log_info "Running Gateway integration tests..."
    if [[ -f "${gateway_dir}/Makefile" ]]; then
        run_test "Gateway Integration Tests (Health Endpoint)" \
            "make test-health" \
            "$gateway_dir"
    elif [[ -d "$gateway_build_dir" ]]; then
        run_test "Gateway Integration Tests (Health Endpoint)" \
            "cd build && make c-gateway-health-test && ./c-gateway-health-test" \
            "$gateway_dir"
    else
        run_test "Gateway Build and Integration Tests" \
            "mkdir -p build && cd build && cmake .. && make c-gateway-health-test && ./c-gateway-health-test" \
            "$gateway_dir"
    fi
    
    # E2E Test Script
    if [[ "$SKIP_E2E" == "false" ]]; then
        log_info "Running Gateway E2E test script..."
        run_test_script "Gateway E2E Test" \
            "${SCRIPT_DIR}/test_gateway_observability.sh"
    else
        log_warn "Skipping Gateway E2E test (--skip-e2e flag)"
        ((SKIPPED_TESTS++)) || true
    fi
    
    # CP2+ Tests (optional)
    if [[ "$INCLUDE_CP2" == "true" ]]; then
        log_info "Running Gateway CP2+ tests (performance)..."
        if [[ -f "${gateway_dir}/Makefile" ]]; then
            run_test "Gateway Performance Tests" \
                "make test-performance" \
                "$gateway_dir"
        else
            log_warn "Gateway performance tests not available"
        fi
    fi
}

# Worker CP1 Core Tests
run_worker_tests() {
    log_section "Step 4: Worker CP1 Core Tests"
    
    local worker_dir="${PROJECT_ROOT}/apps/caf/processor"
    local worker_build_dir="${worker_dir}/build"
    
    # Unit Tests
    log_info "Running Worker unit tests..."
    if [[ -d "$worker_build_dir" ]] && [[ -f "${worker_build_dir}/test_observability" ]]; then
        run_test "Worker Unit Tests (Observability)" \
            "./test_observability" \
            "$worker_build_dir"
    else
        log_warn "Worker build directory not found. Building first..."
        run_test "Worker Build and Unit Tests" \
            "mkdir -p build && cd build && cmake .. && make test_observability && ./test_observability" \
            "$worker_dir"
    fi
    
    # Integration Tests
    log_info "Running Worker integration tests..."
    if [[ -d "$worker_build_dir" ]] && [[ -f "${worker_build_dir}/test_health_endpoint" ]]; then
        run_test "Worker Integration Tests (Health Endpoint)" \
            "./test_health_endpoint" \
            "$worker_build_dir"
    else
        run_test "Worker Build and Integration Tests" \
            "mkdir -p build && cd build && cmake .. && make test_health_endpoint && ./test_health_endpoint" \
            "$worker_dir"
    fi
    
    # E2E Test Script
    if [[ "$SKIP_E2E" == "false" ]]; then
        log_info "Running Worker E2E test script..."
        run_test_script "Worker E2E Test" \
            "${SCRIPT_DIR}/test_worker_observability.sh"
    else
        log_warn "Skipping Worker E2E test (--skip-e2e flag)"
        ((SKIPPED_TESTS++)) || true
    fi
    
    # CP2+ Tests (optional)
    if [[ "$INCLUDE_CP2" == "true" ]]; then
        log_info "Running Worker CP2+ tests (performance, edge cases)..."
        
        # Performance Tests
        if [[ -d "$worker_build_dir" ]] && [[ -f "${worker_build_dir}/test_observability_performance" ]]; then
            run_test "Worker Performance Tests" \
                "./test_observability_performance" \
                "$worker_build_dir"
        else
            run_test "Worker Build and Performance Tests" \
                "mkdir -p build && cd build && cmake .. && make test_observability_performance && ./test_observability_performance" \
                "$worker_dir"
        fi
    fi
}

# E2E Validation
run_e2e_validation() {
    if [[ "$SKIP_E2E" == "false" ]]; then
        log_section "Step 5: E2E Observability Validation"
        
        run_test_script "E2E Observability Validation" \
            "${SCRIPT_DIR}/validate_observability_e2e.sh"
    else
        log_warn "Skipping E2E validation (--skip-e2e flag)"
        ((SKIPPED_TESTS++)) || true
    fi
}

# Main execution
main() {
    echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${GREEN}CP1 Observability Test Profile${NC}"
    echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo ""
    log_info "Project Root: ${PROJECT_ROOT}"
    log_info "Component Filter: ${COMPONENT:-all}"
    log_info "Skip E2E: ${SKIP_E2E}"
    log_info "Include CP2+: ${INCLUDE_CP2}"
    echo ""
    
    # General Validation (always run)
    run_general_validation
    
    # Component-specific tests
    if [[ -z "$COMPONENT" ]] || [[ "$COMPONENT" == "router" ]]; then
        run_router_tests
    fi
    
    if [[ -z "$COMPONENT" ]] || [[ "$COMPONENT" == "gateway" ]]; then
        run_gateway_tests
    fi
    
    if [[ -z "$COMPONENT" ]] || [[ "$COMPONENT" == "worker" ]]; then
        run_worker_tests
    fi
    
    # E2E Validation
    run_e2e_validation
    
    # Summary
    log_section "Test Summary"
    echo -e "${GREEN}[INFO]${NC} Total Tests: ${TOTAL_TESTS}"
    echo -e "${GREEN}[INFO]${NC} Passed: ${PASSED_TESTS}"
    echo -e "${RED}[ERROR]${NC} Failed: ${FAILED_TESTS}"
    echo -e "${YELLOW}[WARN]${NC} Skipped: ${SKIPPED_TESTS}"
    echo -e "${YELLOW}[WARN]${NC} Warnings: ${WARNINGS}"
    echo ""
    
    if [[ $FAILED_TESTS -gt 0 ]]; then
        log_error "CP1 observability test profile failed: ${FAILED_TESTS} test(s) failed"
        exit 1
    elif [[ $SKIPPED_TESTS -gt 0 ]]; then
        log_warn "CP1 observability test profile completed with ${SKIPPED_TESTS} skipped test(s)"
        log_warn "Note: Skipped tests are expected if services are not running"
        exit 0
    else
        log_info "CP1 observability test profile passed successfully!"
        exit 0
    fi
}

main "$@"

