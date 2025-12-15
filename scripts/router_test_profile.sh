#!/bin/bash
# Router Test Profile Script
# Profiles test suites to measure execution time and size

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
ROUTER_DIR="$PROJECT_ROOT/apps/otp/router"
REPORTS_DIR="$PROJECT_ROOT/reports/router"
PROFILE_DIR="$REPORTS_DIR/test_profiles"

# Show help
show_help() {
    cat <<EOF
Router Test Profile Script

DESCRIPTION:
  Profiles Router test suites to measure execution time and size.
  Helps identify heavy test suites for optimization and CI pipeline decisions.

USAGE:
  $0 [OPTIONS]

OPTIONS:
  --fast              Profile fast test suites (default)
  --cp1-smoke         Profile CP1 smoke test suites
  --slow              Profile slow test suites (JetStream E2E, property, load)
  --jetstream         Profile JetStream E2E test suites
  --all               Profile all test suites (fast + slow)
  --suites SUITE1,SUITE2  Profile specific test suites (comma-separated)
  -h, --help          Show this help message

EXAMPLES:
  # Profile fast test suites
  $0 --fast

  # Profile CP1 smoke test suites
  $0 --cp1-smoke

  # Profile slow test suites
  $0 --slow

  # Profile JetStream E2E test suites
  $0 --jetstream

  # Profile all test suites
  $0 --all

  # Profile specific test suites
  $0 --suites router_core_SUITE,router_e2e_smoke_SUITE

EXIT CODES:
  0  - Success (profile generated)
  1  - Test execution failed (one or more tests failed)
  2  - Prerequisites not met (see error message for details)
  3  - Invalid arguments (see usage above)

OUTPUT:
  Profile report saved to: reports/router/test_profiles/
  Format: test_profile_<timestamp>.md

For more information, see: docs/dev/ROUTER_TEST_PROFILE.md

EOF
}

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
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

log_progress() {
    echo -e "${BLUE}[PROFILE]${NC} $*"
}

# Parse arguments
MODE="fast"
CUSTOM_SUITES=""
while [ $# -gt 0 ]; do
    case "$1" in
        -h|--help)
            show_help
            exit 0
            ;;
        --fast|--cp1-smoke|--slow|--jetstream|--all)
            MODE="${1#--}"
            shift
            ;;
        --suites)
            if [ -z "${2:-}" ]; then
                log_error "Error: --suites requires a comma-separated list of suite names"
                echo ""
                show_help
                exit 3
            fi
            CUSTOM_SUITES="$2"
            MODE="custom"
            shift 2
            ;;
        *)
            log_error "Error: Unknown option '$1'"
            echo ""
            show_help
            exit 3
            ;;
    esac
done

# Check prerequisites
check_prerequisites() {
    log_info "Checking prerequisites..."
    
    if [ ! -d "$ROUTER_DIR" ]; then
        log_error "Error: Router directory not found: $ROUTER_DIR"
        log_error ""
        log_error "To fix this:"
        log_error "  1. Verify project structure: ls -la apps/otp/router"
        log_error "  2. Check that you're running from project root"
        log_error ""
        exit 2
    fi
    
    if [ ! -f "$ROUTER_DIR/rebar.config" ]; then
        log_error "Error: Router rebar.config not found"
        log_error ""
        log_error "To fix this:"
        log_error "  1. Verify Router is properly set up: ls -la $ROUTER_DIR"
        log_error "  2. Check that rebar.config exists in Router directory"
        log_error ""
        exit 2
    fi
    
    if ! command -v rebar3 >/dev/null 2>&1; then
        log_error "Error: rebar3 not found in PATH"
        log_error ""
        log_error "To fix this:"
        log_error "  1. Install Erlang/OTP: https://www.erlang.org/downloads"
        log_error "  2. Install rebar3: https://www.rebar3.org/docs/getting-started"
        log_error "  3. Verify: rebar3 version"
        log_error ""
        exit 2
    fi
    
    # Create reports directory if it doesn't exist
    mkdir -p "$PROFILE_DIR"
    
    log_info "Prerequisites check passed"
    return 0
}

# Get test suites by mode
get_test_suites() {
    local mode="$1"
    local suites=()
    
    case "$mode" in
        fast)
            suites=(
                "router_core_SUITE"
                "router_e2e_smoke_SUITE"
                "router_rbac_SUITE"
                "router_policy_enforcement_SUITE"
                "router_decider_SUITE"
                "router_policy_store_SUITE"
                "router_error_SUITE"
                "router_grpc_SUITE"
                "router_grpc_integration_SUITE"
                "router_caf_adapter_unit_SUITE"
                "router_core_telemetry_contract_SUITE"
                "router_secrets_logging_SUITE"
                "router_nats_contract_validation_SUITE"
                "router_gateway_contract_smoke_SUITE"
                "router_cp1_minimal_mode_SUITE"
            )
            ;;
        cp1-smoke)
            suites=(
                "router_core_SUITE"
                "router_e2e_smoke_SUITE"
                "router_rbac_SUITE"
                "router_policy_enforcement_SUITE"
                "router_decider_SUITE"
                "router_policy_store_SUITE"
                "router_error_SUITE"
            )
            ;;
        slow)
            suites=(
                "router_jetstream_e2e_SUITE"
                "router_delivery_count_tracking_SUITE"
                "router_result_consumer_SUITE"
                "router_caf_adapter_SUITE"
                "router_caf_adapter_enhanced_SUITE"
                "router_nats_subscriber_caf_SUITE"
                "router_jetstream_fault_injection_SUITE"
                "router_caf_adapter_load_thresholds_SUITE"
                "router_decider_prop_SUITE"
                "router_policy_store_prop_SUITE"
                "router_normalize_boolean_prop_SUITE"
                "router_options_merge_prop_SUITE"
                "router_policy_store_load_SUITE"
            )
            ;;
        jetstream)
            suites=(
                "router_jetstream_e2e_SUITE"
                "router_delivery_count_tracking_SUITE"
                "router_result_consumer_SUITE"
                "router_caf_adapter_SUITE"
                "router_caf_adapter_enhanced_SUITE"
                "router_nats_subscriber_caf_SUITE"
                "router_jetstream_fault_injection_SUITE"
            )
            ;;
        all)
            # Combine fast and slow
            suites=(
                "router_core_SUITE"
                "router_e2e_smoke_SUITE"
                "router_rbac_SUITE"
                "router_policy_enforcement_SUITE"
                "router_decider_SUITE"
                "router_policy_store_SUITE"
                "router_error_SUITE"
                "router_grpc_SUITE"
                "router_grpc_integration_SUITE"
                "router_caf_adapter_unit_SUITE"
                "router_core_telemetry_contract_SUITE"
                "router_secrets_logging_SUITE"
                "router_nats_contract_validation_SUITE"
                "router_gateway_contract_smoke_SUITE"
                "router_cp1_minimal_mode_SUITE"
                "router_jetstream_e2e_SUITE"
                "router_delivery_count_tracking_SUITE"
                "router_result_consumer_SUITE"
                "router_caf_adapter_SUITE"
                "router_caf_adapter_enhanced_SUITE"
                "router_nats_subscriber_caf_SUITE"
                "router_jetstream_fault_injection_SUITE"
                "router_caf_adapter_load_thresholds_SUITE"
                "router_decider_prop_SUITE"
                "router_policy_store_prop_SUITE"
                "router_normalize_boolean_prop_SUITE"
                "router_options_merge_prop_SUITE"
                "router_policy_store_load_SUITE"
            )
            ;;
        custom)
            # Parse comma-separated list
            IFS=',' read -ra suite_array <<< "$CUSTOM_SUITES"
            for suite in "${suite_array[@]}"; do
                # Remove .erl extension if present
                suite="${suite%.erl}"
                suites+=("$suite")
            done
            ;;
        *)
            log_error "Error: Invalid mode '$mode'"
            exit 3
            ;;
    esac
    
    # Return suites as space-separated string
    echo "${suites[@]}"
}

# Get current timestamp
get_timestamp() {
    date -u +"%Y-%m-%dT%H:%M:%SZ" 2>/dev/null || date +"%Y-%m-%dT%H:%M:%SZ"
}

# Get git commit hash (short)
get_commit_hash() {
    if command -v git >/dev/null 2>&1; then
        cd "$PROJECT_ROOT" && git rev-parse --short HEAD 2>/dev/null || echo "unknown"
    else
        echo "unknown"
    fi
}

# Profile a single test suite
profile_suite() {
    local suite="$1"
    local router_dir="$2"
    local output_file="$3"
    
    log_progress "Profiling suite: $suite"
    
    cd "$router_dir" || return 1
    
    # Measure execution time
    local start_time
    if command -v date >/dev/null 2>&1 && date +%s.%N >/dev/null 2>&1; then
        start_time=$(date +%s.%N)
    else
        start_time=$(date +%s)
    fi
    
    local test_output
    local exit_code=0
    
    # Run test suite and capture output
    test_output=$(rebar3 ct --suite "$suite" 2>&1) || exit_code=$?
    
    local end_time
    if command -v date >/dev/null 2>&1 && date +%s.%N >/dev/null 2>&1; then
        end_time=$(date +%s.%N)
    else
        end_time=$(date +%s)
    fi
    
    # Calculate duration (use awk if bc not available)
    local duration
    if command -v bc >/dev/null 2>&1; then
        duration=$(echo "$end_time - $start_time" | bc)
    else
        # Fallback: use awk for floating point arithmetic
        duration=$(awk "BEGIN {printf \"%.2f\", $end_time - $start_time}")
    fi
    
    # Extract test statistics from output
    local total_tests=0
    local passed_tests=0
    local failed_tests=0
    local skipped_tests=0
    
    # Parse Common Test output
    # Common Test output format examples:
    # "TEST COMPLETE, 25 ok, 0 failed of 25 test cases"
    # "TEST COMPLETE, 25 ok, 0 failed, 0 skipped of 25 test cases"
    if echo "$test_output" | grep -q "TEST COMPLETE"; then
        # Extract test counts using multiple patterns
        # Pattern 1: "X ok, Y failed of Z test cases"
        local test_line=$(echo "$test_output" | grep "TEST COMPLETE" | head -1)
        
        # Extract total tests (last number before "test cases")
        total_tests=$(echo "$test_line" | grep -oE '[0-9]+ test cases' | grep -oE '[0-9]+' | head -1 || echo "0")
        
        # Extract passed tests (number before "ok")
        passed_tests=$(echo "$test_line" | grep -oE '[0-9]+ ok' | grep -oE '[0-9]+' | head -1 || echo "0")
        
        # Extract failed tests (number before "failed")
        failed_tests=$(echo "$test_line" | grep -oE '[0-9]+ failed' | grep -oE '[0-9]+' | head -1 || echo "0")
        
        # Extract skipped tests (number before "skipped", if present)
        skipped_tests=$(echo "$test_line" | grep -oE '[0-9]+ skipped' | grep -oE '[0-9]+' | head -1 || echo "0")
        
        # If total_tests is 0, try to calculate from passed + failed + skipped
        if [ "$total_tests" = "0" ] && { [ "$passed_tests" != "0" ] || [ "$failed_tests" != "0" ] || [ "$skipped_tests" != "0" ]; }; then
            total_tests=$((passed_tests + failed_tests + skipped_tests))
        fi
    fi
    
    # Write suite profile to output file
    {
        echo "### $suite"
        echo ""
        echo "- **Duration**: $(printf "%.2f" "$duration") seconds"
        echo "- **Total Tests**: $total_tests"
        echo "- **Passed**: $passed_tests"
        echo "- **Failed**: $failed_tests"
        echo "- **Skipped**: $skipped_tests"
        echo "- **Exit Code**: $exit_code"
        echo ""
    } >> "$output_file"
    
    # Return exit code
    return $exit_code
}

# Generate profile report
generate_profile() {
    local mode="$1"
    local suites_str="$2"
    
    log_info "Generating test profile for mode: $mode"
    
    # Convert space-separated string to array
    read -ra suites <<< "$suites_str"
    
    if [ ${#suites[@]} -eq 0 ]; then
        log_error "Error: No test suites found for mode: $mode"
        exit 3
    fi
    
    log_info "Found ${#suites[@]} test suite(s) to profile"
    
    # Generate report filename
    local timestamp=$(get_timestamp | tr ':' '-' | tr 'T' '_' | cut -d'.' -f1)
    local commit_hash=$(get_commit_hash)
    local report_file="$PROFILE_DIR/test_profile_${mode}_${commit_hash}_${timestamp}.md"
    
    # Initialize report
    {
        echo "# Router Test Profile Report"
        echo ""
        echo "**Generated**: $(get_timestamp)"
        echo "**Commit**: $commit_hash"
        echo "**Branch**: $(cd "$PROJECT_ROOT" && git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "unknown")"
        echo "**Mode**: $mode"
        echo "**Total Suites**: ${#suites[@]}"
        echo ""
        echo "## Summary"
        echo ""
        echo "This profile measures execution time and size for Router test suites."
        echo "Use this data to:"
        echo "- Identify heavy test suites for optimization"
        echo "- Make CI pipeline decisions (fast CI vs nightly)"
        echo "- Track test performance over time"
        echo ""
        echo "## Test Suite Profiles"
        echo ""
    } > "$report_file"
    
    # Profile each suite
    local total_duration=0
    local total_tests=0
    local failed_suites=0
    
    for suite in "${suites[@]}"; do
        local suite_start
        if command -v date >/dev/null 2>&1 && date +%s.%N >/dev/null 2>&1; then
            suite_start=$(date +%s.%N)
        else
            suite_start=$(date +%s)
        fi
        
        if profile_suite "$suite" "$ROUTER_DIR" "$report_file"; then
            log_info "✓ $suite profiled successfully"
        else
            log_warn "⚠ $suite had failures (check report for details)"
            failed_suites=$((failed_suites + 1))
        fi
        
        local suite_end
        if command -v date >/dev/null 2>&1 && date +%s.%N >/dev/null 2>&1; then
            suite_end=$(date +%s.%N)
        else
            suite_end=$(date +%s)
        fi
        
        local suite_duration
        if command -v bc >/dev/null 2>&1; then
            suite_duration=$(echo "$suite_end - $suite_start" | bc)
            total_duration=$(echo "$total_duration + $suite_duration" | bc)
        else
            # Fallback: use awk for floating point arithmetic
            suite_duration=$(awk "BEGIN {printf \"%.2f\", $suite_end - $suite_start}")
            total_duration=$(awk "BEGIN {printf \"%.2f\", $total_duration + $suite_duration}")
        fi
    done
    
    # Add summary statistics
    {
        echo ""
        echo "## Summary Statistics"
        echo ""
        local total_minutes
        if command -v bc >/dev/null 2>&1; then
            total_minutes=$(echo "scale=2; $total_duration / 60" | bc)
        else
            total_minutes=$(awk "BEGIN {printf \"%.2f\", $total_duration / 60}")
        fi
        echo "- **Total Duration**: $(printf "%.2f" "$total_duration") seconds ($(printf "%.2f" "$total_minutes") minutes)"
        echo "- **Total Suites**: ${#suites[@]}"
        echo "- **Failed Suites**: $failed_suites"
        local success_rate
        if command -v bc >/dev/null 2>&1; then
            success_rate=$(echo "scale=1; (${#suites[@]} - $failed_suites) * 100 / ${#suites[@]}" | bc)
        else
            success_rate=$(awk "BEGIN {printf \"%.1f\", (${#suites[@]} - $failed_suites) * 100 / ${#suites[@]}}")
        fi
        echo "- **Success Rate**: $(printf "%.1f" "$success_rate")%"
        echo ""
        echo "## Recommendations"
        echo ""
        echo "### For Fast CI (PR checks)"
        echo "- Run suites with duration < 30 seconds"
        echo "- Focus on CP1 smoke tests and fast contract tests"
        echo ""
        echo "### For Nightly CI (comprehensive)"
        echo "- Run all test suites including slow/JetStream E2E tests"
        echo "- Monitor duration trends over time"
        echo ""
        echo "### For Optimization"
        echo "- Identify suites with duration > 5 minutes"
        echo "- Consider splitting large suites or optimizing slow tests"
        echo ""
    } >> "$report_file"
    
    log_info "Profile report generated: $report_file"
    echo "$report_file"
}

# Main execution
main() {
    log_info "Router Test Profile Script"
    log_info "Mode: $MODE"
    
    check_prerequisites
    
    # Get test suites for the selected mode
    local suites_str
    suites_str=$(get_test_suites "$MODE")
    
    if [ -z "$suites_str" ]; then
        log_error "Error: No test suites found for mode: $MODE"
        exit 3
    fi
    
    # Generate profile
    local report_file
    report_file=$(generate_profile "$MODE" "$suites_str")
    
    log_info ""
    log_info "=== Profile Complete ==="
    log_info "Report: $report_file"
    log_info ""
    log_info "View the report to see:"
    log_info "  - Execution time per suite"
    log_info "  - Test counts (total, passed, failed, skipped)"
    log_info "  - Summary statistics"
    log_info "  - Recommendations for CI pipeline decisions"
    log_info ""
    
    exit 0
}

main "$@"

