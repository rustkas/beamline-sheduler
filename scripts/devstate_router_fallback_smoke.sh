#!/bin/bash
# DevState Router Fallback Smoke Test
# Semi-automated script to verify Router fallback behavior in various DevState conditions

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
cd "${ROOT_DIR}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Show help
show_help() {
    cat <<EOF
DevState Router Fallback Smoke Test

DESCRIPTION:
  Verifies Router fallback behavior in various DevState conditions:
  - Missing state file
  - Invalid JSON in state file
  - no_drift=false (drift detected)

USAGE:
  $0 [OPTIONS]

OPTIONS:
  --scenario SCENARIO    Run specific scenario (default: missing_state)
  -h, --help             Show this help message

SCENARIOS:
  missing_state          Router falls back when state file is missing (default)
  invalid_json           Router falls back when state file contains invalid JSON
  no_drift_false         Router falls back when no_drift=false (drift detected)
  all                    Run all scenarios sequentially

EXAMPLES:
  # Run default scenario (missing_state)
  $0

  # Run specific scenario
  $0 --scenario invalid_json
  $0 --scenario no_drift_false

  # Run all scenarios
  $0 --scenario all

EXIT CODES:
  0  - Smoke test passed (Router falls back correctly)
  1  - Smoke test failed (Router doesn't fall back or crashes)
  2  - Prerequisites not met (see error message for details)
  3  - Invalid scenario specified (see valid scenarios above)

TROUBLESHOOTING:
  Exit code 2 (Prerequisites not met):
    - DevState not running: Start with 'make devstate-up'
    - Router not compiled: Run 'cd apps/otp/router && rebar3 compile'

  Exit code 3 (Invalid scenario):
    - Check that scenario name matches one of: missing_state, invalid_json, no_drift_false, all
    - Use --help to see all available scenarios

EOF
}

# Parse arguments
SCENARIO="missing_state"
while [ $# -gt 0 ]; do
    case "$1" in
        -h|--help)
            show_help
            exit 0
            ;;
        --scenario)
            if [ -z "${2:-}" ]; then
                log_error "Error: --scenario requires a scenario name"
                echo ""
                show_help
                exit 3
            fi
            SCENARIO="$2"
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

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1" >&2
}

# Validate scenario
validate_scenario() {
    case "${SCENARIO}" in
        missing_state|invalid_json|no_drift_false|all)
            return 0
            ;;
        *)
            log_error "Error: Invalid scenario '${SCENARIO}'"
            log_error ""
            log_error "Valid scenarios:"
            log_error "  - missing_state    Router falls back when state file is missing"
            log_error "  - invalid_json     Router falls back when state file contains invalid JSON"
            log_error "  - no_drift_false   Router falls back when no_drift=false (drift detected)"
            log_error "  - all              Run all scenarios sequentially"
            log_error ""
            log_error "Use --help for more information"
            exit 3
            ;;
    esac
}

# Run scenario
run_scenario() {
    local scenario_name="$1"
    log_info "Running scenario: ${scenario_name}"
    
    case "${scenario_name}" in
        missing_state)
            run_missing_state_scenario
            ;;
        invalid_json)
            run_invalid_json_scenario
            ;;
        no_drift_false)
            run_no_drift_false_scenario
            ;;
        *)
            log_error "Unknown scenario: ${scenario_name}"
            return 1
            ;;
    esac
}

# Check prerequisites
check_prerequisites() {
    log_info "Checking prerequisites..."

    # Check if DevState is running
    if ! curl -fsS http://localhost:3080/health > /dev/null 2>&1; then
        log_error "Error: DevState is not running"
        log_error ""
        log_error "To fix this:"
        log_error "  1. Start DevState service: make devstate-up"
        log_error "  2. Wait a few seconds for service to start"
        log_error "  3. Verify: curl -fsS http://localhost:3080/health"
        log_error ""
        log_error "For more information, see: devstate/docs/README.md"
        exit 2
    fi
    log_info "DevState is running"

    # Check if Router is compiled
    if [ ! -d "apps/otp/router/_build" ]; then
        log_error "Error: Router is not compiled"
        log_error ""
        log_error "To fix this:"
        log_error "  1. Navigate to Router directory: cd apps/otp/router"
        log_error "  2. Compile Router: rebar3 compile"
        log_error "  3. Verify: ls -la _build/default/lib/*/ebin"
        log_error ""
        exit 2
    fi
    log_info "Router is compiled"
}

# Setup valid state file
setup_valid_state() {
    local state_file="$1"
    log_info "Setting up valid state file (CP1-LC)..."
    make devstate-set-cp CP=CP1-LC > /dev/null 2>&1 || {
        log_error "Failed to set CP via DevState"
        return 1
    }
    log_info "State file created: $(cat ${state_file} | jq -r '.current_cp')"
}

# Check Router state detection
check_router_state() {
    local expected_failure="$1"
    local state_file="$2"
    
    cd apps/otp/router
    
    # Use erl to call router_state:check_and_log_cp_state/0
    # This simulates what Router does on startup
    ERL_SCRIPT=$(cat <<EOF
{ok, _} = application:load(beamline_router),
ok = application:set_env(beamline_router, devstate_state_path, "${state_file}"),
ok = router_state:check_and_log_cp_state(),
io:format("Router state check completed~n"),
halt(0).
EOF
)
    
    local output
    output=$(erl -pa _build/default/lib/*/ebin -eval "${ERL_SCRIPT}" -noshell 2>&1)
    
    if echo "${output}" | grep -q "DevState/CP state failure"; then
        if [ -n "${expected_failure}" ]; then
            if echo "${output}" | grep -q "${expected_failure}"; then
                log_info "Router correctly detected expected failure: ${expected_failure}"
                cd "${ROOT_DIR}"
                return 0
            else
                log_error "Router detected failure but wrong type. Expected: ${expected_failure}"
                cd "${ROOT_DIR}"
                return 1
            fi
        else
            log_info "Router correctly detected state failure"
            cd "${ROOT_DIR}"
            return 0
        fi
    else
        if [ -z "${expected_failure}" ]; then
            log_info "Router correctly detected no failure (recovery)"
            cd "${ROOT_DIR}"
            return 0
        else
            log_error "Router did not detect expected failure: ${expected_failure}"
            cd "${ROOT_DIR}"
            return 1
        fi
    fi
}

# Scenario: Missing State File
run_missing_state_scenario() {
    local state_file=".trae/state.json"
    local state_backup=".trae/state.json.smoke_backup"
    
    # Backup state file
    if [ -f "${state_file}" ]; then
        log_info "Backing up state file to ${state_backup}"
        cp "${state_file}" "${state_backup}"
    fi
    
    # Setup: Create valid state file
    setup_valid_state "${state_file}" || return 1
    
    # Test: Remove state file and check Router fallback
    log_info "Testing Router fallback behavior (missing state)..."
    log_warn "Removing state file to simulate missing state (local testing only!)"
    mv "${state_file}" "${state_file}.removed"
    
    # Check if Router can detect missing state
    if ! check_router_state "missing_state" "${state_file}"; then
        mv "${state_file}.removed" "${state_file}" 2>/dev/null || true
        return 1
    fi
    
    # Restore state file
    log_info "Restoring state file..."
    if [ -f "${state_file}.removed" ]; then
        mv "${state_file}.removed" "${state_file}"
        log_info "State file restored"
    fi
    
    # Verify recovery
    log_info "Verifying recovery..."
    if ! check_router_state "" "${state_file}"; then
        return 1
    fi
    
    # Cleanup
    if [ -f "${state_backup}" ]; then
        log_info "Restoring original state file from backup..."
        mv "${state_backup}" "${state_file}" 2>/dev/null || true
    fi
    
    log_info "Scenario 'missing_state' passed"
    return 0
}

# Scenario: Invalid JSON
run_invalid_json_scenario() {
    local state_file=".trae/state.json"
    local state_backup=".trae/state.json.smoke_backup"
    
    # Backup state file
    if [ -f "${state_file}" ]; then
        log_info "Backing up state file to ${state_backup}"
        cp "${state_file}" "${state_backup}"
    fi
    
    # Setup: Create valid state file
    setup_valid_state "${state_file}" || return 1
    
    # Test: Corrupt state file with invalid JSON
    log_info "Testing Router fallback behavior (invalid JSON)..."
    log_warn "Corrupting state file with invalid JSON (local testing only!)"
    echo '{"current_cp":"CP2-LC","no_drift":true' > "${state_file}"
    # Note: Missing closing brace - invalid JSON
    
    # Check if Router can detect invalid JSON
    if ! check_router_state "decode_error" "${state_file}"; then
        # Restore from backup
        if [ -f "${state_backup}" ]; then
            mv "${state_backup}" "${state_file}" 2>/dev/null || true
        else
            setup_valid_state "${state_file}" || true
        fi
        return 1
    fi
    
    # Restore valid state file
    log_info "Restoring valid state file..."
    if [ -f "${state_backup}" ]; then
        mv "${state_backup}" "${state_file}"
    else
        setup_valid_state "${state_file}" || return 1
    fi
    log_info "State file restored"
    
    # Verify recovery
    log_info "Verifying recovery..."
    if ! check_router_state "" "${state_file}"; then
        return 1
    fi
    
    log_info "Scenario 'invalid_json' passed"
    return 0
}

# Scenario: no_drift=false
run_no_drift_false_scenario() {
    local state_file=".trae/state.json"
    local state_backup=".trae/state.json.smoke_backup"
    
    # Backup state file
    if [ -f "${state_file}" ]; then
        log_info "Backing up state file to ${state_backup}"
        cp "${state_file}" "${state_backup}"
    fi
    
    # Setup: Create valid state file with CP2-LC
    log_info "Setting up valid state file (CP2-LC)..."
    make devstate-set-cp CP=CP2-LC > /dev/null 2>&1 || {
        log_error "Failed to set CP via DevState"
        return 1
    }
    log_info "State file created: $(cat ${state_file} | jq -r '.current_cp')"
    
    # Test: Set no_drift=false
    log_info "Testing Router fallback behavior (no_drift=false)..."
    log_warn "Setting no_drift=false to simulate drift detection (local testing only!)"
    cat "${state_file}" | jq '. + {no_drift: false}' > "${state_file}.tmp"
    mv "${state_file}.tmp" "${state_file}"
    
    # Check if Router can detect no_drift violation
    if ! check_router_state "no_drift_violation" "${state_file}"; then
        # Restore from backup
        if [ -f "${state_backup}" ]; then
            mv "${state_backup}" "${state_file}" 2>/dev/null || true
        else
            setup_valid_state "${state_file}" || true
        fi
        return 1
    fi
    
    # Restore valid state file
    log_info "Restoring valid state file..."
    if [ -f "${state_backup}" ]; then
        mv "${state_backup}" "${state_file}"
    else
        setup_valid_state "${state_file}" || return 1
    fi
    log_info "State file restored"
    
    # Verify recovery
    log_info "Verifying recovery..."
    if ! check_router_state "" "${state_file}"; then
        return 1
    fi
    
    log_info "Scenario 'no_drift_false' passed"
    return 0
}

# Main execution
main() {
    validate_scenario
    
    check_prerequisites
    
    if [ "${SCENARIO}" = "all" ]; then
        log_info "Running all scenarios..."
        local failed=0
        for scenario in missing_state invalid_json no_drift_false; do
            if ! run_scenario "${scenario}"; then
                log_error "Scenario '${scenario}' failed"
                failed=$((failed + 1))
            fi
        done
        
        if [ ${failed} -eq 0 ]; then
            log_info "All scenarios passed: Router correctly falls back and recovers"
            exit 0
        else
            log_error "${failed} scenario(s) failed"
            exit 1
        fi
    else
        if run_scenario "${SCENARIO}"; then
            log_info "Smoke test passed: Router correctly falls back and recovers"
            exit 0
        else
            log_error "Smoke test failed"
            exit 1
        fi
    fi
}

# Run main
main

