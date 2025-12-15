#!/bin/bash
# Router Regression Snapshot Report
# Generates a snapshot report of key Router metrics and log patterns

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
ROUTER_DIR="$PROJECT_ROOT/apps/otp/router"
REPORTS_DIR="$PROJECT_ROOT/reports/router"
SNAPSHOT_DIR="$REPORTS_DIR/snapshots"

# Show help
show_help() {
    cat <<EOF
Router Regression Snapshot Report

DESCRIPTION:
  Generates a snapshot report of key Router metrics and log patterns to help
  catch regressions between releases. Captures:
  - DevState/CP state metrics
  - NATS contract validation metrics
  - JetStream metrics (redelivery, MaxDeliver)
  - Processing metrics (results, errors)
  - CAF adapter metrics
  - Log patterns (cp_fallback, contract_violation, MaxDeliver, errors)
  - Test results summary

USAGE:
  $0 [OPTIONS]

OPTIONS:
  --baseline              Generate a new baseline snapshot (default)
  --compare BASELINE_FILE Compare current state against a baseline snapshot
  -h, --help              Show this help message

EXAMPLES:
  # Generate a baseline snapshot
  $0 --baseline
  $0  # (same as --baseline)

  # Compare current state against a baseline
  $0 --compare reports/router/snapshots/router_snapshot_abc123_2025-01-27_12-00-00.md

EXIT CODES:
  0  - Success (snapshot generated or comparison completed)
  1  - Test failure (tests failed during snapshot generation)
  2  - Prerequisites not met (see error message for details)
  3  - Invalid arguments (see usage above)

TROUBLESHOOTING:
  Exit code 2 (Prerequisites not met):
    - Router directory not found: Check that apps/otp/router exists
    - Router not compiled: Run 'cd apps/otp/router && rebar3 compile'
    - rebar3 not found: Install Erlang/OTP and rebar3

  Exit code 3 (Invalid arguments):
    - Check that mode is one of: --baseline, --compare BASELINE_FILE
    - For --compare, ensure baseline file path is provided and file exists
    - Use --help to see all available options

OUTPUT:
  Snapshots are saved to: reports/router/snapshots/
  Format: router_snapshot_<commit>_<timestamp>.md

For more information, see: docs/dev/ROUTER_REGRESSION_SNAPSHOTS.md

EOF
}

# Parse arguments
MODE="baseline"
BASELINE_FILE=""
while [ $# -gt 0 ]; do
    case "$1" in
        -h|--help)
            show_help
            exit 0
            ;;
        --baseline)
            MODE="baseline"
            shift
            ;;
        --compare)
            if [ -z "${2:-}" ]; then
                log_error "Error: --compare requires a baseline file path"
                echo ""
                show_help
                exit 3
            fi
            MODE="compare"
            BASELINE_FILE="$2"
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
        return 2
    fi
    
    if [ ! -f "$ROUTER_DIR/rebar.config" ]; then
        log_error "Error: Router rebar.config not found"
        log_error ""
        log_error "To fix this:"
        log_error "  1. Verify Router is properly set up: ls -la $ROUTER_DIR"
        log_error "  2. Check that rebar.config exists in Router directory"
        log_error ""
        return 2
    fi
    
    # Create reports directory if it doesn't exist
    mkdir -p "$SNAPSHOT_DIR"
    
    log_info "Prerequisites check passed"
    return 0
}

# Get git commit hash (short)
get_commit_hash() {
    if command -v git >/dev/null 2>&1; then
        cd "$PROJECT_ROOT" && git rev-parse --short HEAD 2>/dev/null || echo "unknown"
    else
        echo "unknown"
    fi
}

# Get current date/time
get_timestamp() {
    date -u +"%Y-%m-%dT%H:%M:%SZ" 2>/dev/null || date +"%Y-%m-%dT%H:%M:%SZ"
}

# Run minimal test scenario
run_test_scenario() {
    log_info "Running minimal test scenario..."
    
    cd "$ROUTER_DIR" || return 1
    
    # Run CP1 smoke tests (fast, minimal scenario)
    if command -v rebar3 >/dev/null 2>&1; then
        log_info "Running CP1 smoke tests..."
        rebar3 ct --suite router_cp1_minimal_mode_SUITE --suite router_gateway_contract_smoke_SUITE || {
            log_warn "Some tests failed, but continuing with snapshot collection"
        }
    else
        log_error "Error: rebar3 not found in PATH"
        log_error ""
        log_error "To fix this:"
        log_error "  1. Install Erlang/OTP: https://www.erlang.org/downloads"
        log_error "  2. Install rebar3: https://www.rebar3.org/docs/getting-started"
        log_error "  3. Verify: rebar3 version"
        log_error ""
        return 2
    fi
    
    log_info "Test scenario completed"
    return 0
}

# Collect metrics snapshot
collect_metrics_snapshot() {
    log_info "Collecting metrics snapshot..."
    
    local snapshot_file="$1"
    
    {
        echo "# Router Metrics Snapshot"
        echo "# Generated: $(get_timestamp)"
        echo "# Commit: $(get_commit_hash)"
        echo ""
        echo "## Key Metrics"
        echo ""
        
        # Note: In CP1, we don't have a /metrics endpoint yet
        # Instead, we collect metrics from telemetry events or logs
        # For now, we'll document the expected metrics and their values
        
        echo "### DevState/CP State Metrics"
        echo "- router_cp_state_errors_total: (check logs for actual values)"
        echo "- router_cp_state_fallback_total: (check logs for actual values)"
        echo ""
        
        echo "### NATS Contract Validation Metrics"
        echo "- router_nats_contract_violations_total: (check logs for actual values)"
        echo ""
        
        echo "### JetStream Metrics"
        echo "- router_jetstream_redelivery_total: (check logs for actual values)"
        echo "- router_jetstream_maxdeliver_exhausted_total: (check logs for actual values)"
        echo ""
        
        echo "### Processing Metrics"
        echo "- router_results_total: (check logs for actual values)"
        echo "- router_results_parse_failed_total: (check logs for actual values)"
        echo "- router_results_tenant_rejected_total: (check logs for actual values)"
        echo ""
        
        echo "### CAF Adapter Metrics"
        echo "- router_assignment_retry_total: (check logs for actual values)"
        echo "- router_retry_exhausted_total: (check logs for actual values)"
        echo "- router_assignment_publish_failures_total: (check logs for actual values)"
        echo ""
        
    } >> "$snapshot_file"
    
    log_info "Metrics snapshot collected"
}

# Collect log patterns snapshot
collect_log_patterns_snapshot() {
    log_info "Collecting log patterns snapshot..."
    
    local snapshot_file="$1"
    local log_dir="${2:-$PROJECT_ROOT/.windsurf/reports}"
    
    {
        echo ""
        echo "## Log Patterns"
        echo ""
        
        # Find log files
        local log_files=()
        if [ -d "$log_dir" ]; then
            while IFS= read -r -d '' file; do
                log_files+=("$file")
            done < <(find "$log_dir" -name "router_*.jsonl" -type f -print0 2>/dev/null | head -z -n 5)
        fi
        
        if [ ${#log_files[@]} -eq 0 ]; then
            echo "No log files found in $log_dir"
            echo ""
            return 0
        fi
        
        echo "### CP Fallback Patterns"
        echo "Searching for: cp_fallback, CP1-baseline"
        echo ""
        for log_file in "${log_files[@]}"; do
            if [ -f "$log_file" ]; then
                local count=$(grep -c "cp_fallback\|CP1-baseline" "$log_file" 2>/dev/null || echo "0")
                echo "- $(basename "$log_file"): $count occurrences"
            fi
        done
        echo ""
        
        echo "### Contract Violation Patterns"
        echo "Searching for: contract_violation, violation"
        echo ""
        for log_file in "${log_files[@]}"; do
            if [ -f "$log_file" ]; then
                local count=$(grep -c "contract_violation\|violation" "$log_file" 2>/dev/null || echo "0")
                echo "- $(basename "$log_file"): $count occurrences"
            fi
        done
        echo ""
        
        echo "### MaxDeliver Exhaustion Patterns"
        echo "Searching for: MaxDeliver, maxdeliver_exhausted"
        echo ""
        for log_file in "${log_files[@]}"; do
            if [ -f "$log_file" ]; then
                local count=$(grep -c "MaxDeliver\|maxdeliver_exhausted" "$log_file" 2>/dev/null || echo "0")
                echo "- $(basename "$log_file"): $count occurrences"
            fi
        done
        echo ""
        
        echo "### Error Patterns"
        echo "Searching for: ERROR, error, failed"
        echo ""
        for log_file in "${log_files[@]}"; do
            if [ -f "$log_file" ]; then
                local count=$(grep -c "\"level\":\"ERROR\"\|\"error\"\|\"failed\"" "$log_file" 2>/dev/null || echo "0")
                echo "- $(basename "$log_file"): $count occurrences"
            fi
        done
        echo ""
        
    } >> "$snapshot_file"
    
    log_info "Log patterns snapshot collected"
}

# Collect test results snapshot
collect_test_results_snapshot() {
    log_info "Collecting test results snapshot..."
    
    local snapshot_file="$1"
    
    {
        echo ""
        echo "## Test Results"
        echo ""
        
        # Check if test results exist
        local test_log_dir="$ROUTER_DIR/_build/test/logs"
        if [ -d "$test_log_dir" ]; then
            echo "### Test Suites Run"
            echo ""
            local suite_count=$(find "$test_log_dir" -name "*.log" -type f | wc -l)
            echo "- Total test suites: $suite_count"
            echo ""
            
            # Extract test results from logs
            echo "### Test Summary"
            echo ""
            for log_file in "$test_log_dir"/*.log; do
                if [ -f "$log_file" ]; then
                    local suite_name=$(basename "$log_file" .log)
                    local passed=$(grep -c "PASSED" "$log_file" 2>/dev/null || echo "0")
                    local failed=$(grep -c "FAILED" "$log_file" 2>/dev/null || echo "0")
                    if [ "$passed" -gt 0 ] || [ "$failed" -gt 0 ]; then
                        echo "- $suite_name: $passed passed, $failed failed"
                    fi
                fi
            done
            echo ""
        else
            echo "No test logs found in $test_log_dir"
            echo ""
        fi
        
    } >> "$snapshot_file"
    
    log_info "Test results snapshot collected"
}

# Generate baseline snapshot
generate_baseline() {
    log_info "Generating baseline snapshot..."
    
    local commit_hash=$(get_commit_hash)
    local timestamp=$(get_timestamp | tr ':' '-' | tr 'T' '_' | cut -d'.' -f1)
    local snapshot_file="$SNAPSHOT_DIR/router_snapshot_${commit_hash}_${timestamp}.md"
    
    # Initialize snapshot file
    {
        echo "# Router Regression Snapshot"
        echo ""
        echo "**Generated**: $(get_timestamp)"
        echo "**Commit**: $commit_hash"
        echo "**Branch**: $(cd "$PROJECT_ROOT" && git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "unknown")"
        echo ""
    } > "$snapshot_file"
    
    # Collect snapshots
    collect_metrics_snapshot "$snapshot_file"
    collect_log_patterns_snapshot "$snapshot_file"
    collect_test_results_snapshot "$snapshot_file"
    
    # Add summary
    {
        echo ""
        echo "## Summary"
        echo ""
        echo "This snapshot captures the state of Router at commit $commit_hash."
        echo "Use this as a baseline for regression testing."
        echo ""
        echo "### Key Numbers to Monitor"
        echo ""
        echo "1. **CP State Errors**: Should be 0 in normal operation"
        echo "2. **Contract Violations**: Should be 0 in normal operation"
        echo "3. **MaxDeliver Exhaustions**: Should be 0 in normal operation"
        echo "4. **Test Pass Rate**: Should be 100%"
        echo "5. **Error Log Count**: Should be minimal (only expected errors)"
        echo ""
    } >> "$snapshot_file"
    
    log_info "Baseline snapshot generated: $snapshot_file"
    echo "$snapshot_file"
}

# Compare two snapshots
compare_snapshots() {
    local baseline_file="$1"
    local current_file="$2"
    
    log_info "Comparing snapshots..."
    log_info "Baseline: $baseline_file"
    log_info "Current: $current_file"
    
    if [ ! -f "$baseline_file" ]; then
        log_error "Baseline file not found: $baseline_file"
        return 1
    fi
    
    if [ ! -f "$current_file" ]; then
        log_error "Current file not found: $current_file"
        return 1
    fi
    
    # Simple comparison: extract key numbers and compare
    log_info ""
    log_info "=== Snapshot Comparison ==="
    log_info ""
    
    # Compare log pattern counts
    log_info "Log Pattern Comparison:"
    echo "Pattern | Baseline | Current | Change"
    echo "--------|----------|---------|-------"
    
    # Extract counts from baseline
    local baseline_cp_fallback=$(grep -A 10 "CP Fallback Patterns" "$baseline_file" | grep "occurrences" | awk '{sum+=$NF} END {print sum+0}')
    local current_cp_fallback=$(grep -A 10 "CP Fallback Patterns" "$current_file" | grep "occurrences" | awk '{sum+=$NF} END {print sum+0}')
    
    local baseline_violations=$(grep -A 10 "Contract Violation Patterns" "$baseline_file" | grep "occurrences" | awk '{sum+=$NF} END {print sum+0}')
    local current_violations=$(grep -A 10 "Contract Violation Patterns" "$current_file" | grep "occurrences" | awk '{sum+=$NF} END {print sum+0}')
    
    local baseline_maxdeliver=$(grep -A 10 "MaxDeliver Exhaustion Patterns" "$baseline_file" | grep "occurrences" | awk '{sum+=$NF} END {print sum+0}')
    local current_maxdeliver=$(grep -A 10 "MaxDeliver Exhaustion Patterns" "$current_file" | grep "occurrences" | awk '{sum+=$NF} END {print sum+0}')
    
    local baseline_errors=$(grep -A 10 "Error Patterns" "$baseline_file" | grep "occurrences" | awk '{sum+=$NF} END {print sum+0}')
    local current_errors=$(grep -A 10 "Error Patterns" "$current_file" | grep "occurrences" | awk '{sum+=$NF} END {print sum+0}')
    
    printf "CP Fallback        | %-8s | %-7s | %s\n" \
        "${baseline_cp_fallback:-0}" "${current_cp_fallback:-0}" \
        "$(if [ "${current_cp_fallback:-0}" -gt "${baseline_cp_fallback:-0}" ]; then echo "⚠️  INCREASED"; elif [ "${current_cp_fallback:-0}" -lt "${baseline_cp_fallback:-0}" ]; then echo "✅ DECREASED"; else echo "➡️  SAME"; fi)"
    
    printf "Contract Violations| %-8s | %-7s | %s\n" \
        "${baseline_violations:-0}" "${current_violations:-0}" \
        "$(if [ "${current_violations:-0}" -gt "${baseline_violations:-0}" ]; then echo "⚠️  INCREASED"; elif [ "${current_violations:-0}" -lt "${baseline_violations:-0}" ]; then echo "✅ DECREASED"; else echo "➡️  SAME"; fi)"
    
    printf "MaxDeliver Exhaust | %-8s | %-7s | %s\n" \
        "${baseline_maxdeliver:-0}" "${current_maxdeliver:-0}" \
        "$(if [ "${current_maxdeliver:-0}" -gt "${baseline_maxdeliver:-0}" ]; then echo "⚠️  INCREASED"; elif [ "${current_maxdeliver:-0}" -lt "${baseline_maxdeliver:-0}" ]; then echo "✅ DECREASED"; else echo "➡️  SAME"; fi)"
    
    printf "Error Logs         | %-8s | %-7s | %s\n" \
        "${baseline_errors:-0}" "${current_errors:-0}" \
        "$(if [ "${current_errors:-0}" -gt "${baseline_errors:-0}" ]; then echo "⚠️  INCREASED"; elif [ "${current_errors:-0}" -lt "${baseline_errors:-0}" ]; then echo "✅ DECREASED"; else echo "➡️  SAME"; fi)"
    
    log_info ""
    log_info "=== Comparison Complete ==="
    log_info ""
    log_info "Review the detailed snapshots for more information:"
    log_info "  Baseline: $baseline_file"
    log_info "  Current:  $current_file"
}

# Main execution
main() {
    log_info "Router Regression Snapshot Report"
    log_info "Mode: $MODE"
    
    check_prerequisites || exit 2
    
    case "$MODE" in
        baseline)
            # Run test scenario
            run_test_scenario || {
                log_warn "Test scenario had issues, but continuing with snapshot"
            }
            
            # Generate baseline snapshot
            generate_baseline || exit 1
            
            log_info "Baseline snapshot generated successfully"
            ;;
        compare)
            if [ -z "${BASELINE_FILE}" ]; then
                log_error "Error: --compare requires a baseline file path"
                log_error ""
                log_error "Usage: $0 --compare BASELINE_FILE"
                log_error ""
                log_error "Example:"
                log_error "  $0 --compare reports/router/snapshots/router_snapshot_abc123_2025-01-27_12-00-00.md"
                log_error ""
                log_error "Use --help for more information"
                exit 3
            fi
            
            local baseline_file="${BASELINE_FILE}"
            
            if [ ! -f "$baseline_file" ]; then
                log_error "Error: Baseline file not found: $baseline_file"
                log_error ""
                log_error "To fix this:"
                log_error "  1. Check that the file path is correct"
                log_error "  2. List available snapshots: ls -la $SNAPSHOT_DIR"
                log_error "  3. Use --baseline to generate a new snapshot first"
                log_error ""
                exit 3
            fi
            
            # Generate current snapshot
            run_test_scenario || {
                log_warn "Test scenario had issues, but continuing with snapshot"
            }
            
            local current_file=$(generate_baseline)
            
            # Compare snapshots
            compare_snapshots "$baseline_file" "$current_file" || exit 1
            
            log_info "Snapshot comparison completed"
            ;;
        *)
            log_error "Error: Invalid mode '$MODE'"
            log_error ""
            log_error "Valid modes:"
            log_error "  --baseline              Generate a new baseline snapshot"
            log_error "  --compare BASELINE_FILE Compare current state against a baseline"
            log_error ""
            log_error "Use --help for more information"
            exit 3
            ;;
    esac
    
    exit 0
}

main "$@"

