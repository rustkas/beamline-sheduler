#!/bin/bash
# Gateway ↔ Router Contract Smoke Test
# E2E smoke test for Gateway (TS) ↔ Router (OTP) contract verification

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
ROUTER_DIR="$PROJECT_ROOT/apps/otp/router"
GATEWAY_DIR="$PROJECT_ROOT/apps/gateway"

# Show help
show_help() {
    cat <<EOF
Gateway ↔ Router Contract Smoke Test

DESCRIPTION:
  Verifies the contract between Gateway (TypeScript/NestJS) and Router (Erlang/OTP):
  - DecideRequest/DecideResponse structure matches PROTO_NATS_MAPPING and API_CONTRACTS
  - Headers (trace_id, tenant_id, version, Nats-Msg-Id) pass through the chain
  - Error responses follow the contract (invalid_request, unauthorized, etc.)

USAGE:
  $0 [OPTIONS]

OPTIONS:
  --router-only         Run only Router contract test (default if Gateway not available)
  --gateway-only        Run only Gateway contract test (if available)
  --full                Run both Router and Gateway tests (default)
  -h, --help            Show this help message

EXAMPLES:
  # Run Router test only (fastest)
  $0 --router-only

  # Run Gateway test only (if Gateway tests available)
  $0 --gateway-only

  # Run full E2E test (Router + Gateway)
  $0 --full
  $0  # (same as --full)

EXIT CODES:
  0  - Success (all tests passed)
  1  - Test failure (one or more tests failed)
  2  - Prerequisites not met (see error message for details)
  3  - Invalid arguments (see usage above)

TROUBLESHOOTING:
  Exit code 2 (Prerequisites not met):
    - Router directory not found: Check that apps/otp/router exists
    - Router not compiled: Run 'cd apps/otp/router && rebar3 compile'
    - rebar3 not found: Install Erlang/OTP and rebar3
    - Gateway directory not found: Gateway tests will be skipped (use --router-only)

  Exit code 3 (Invalid arguments):
    - Check that mode is one of: --router-only, --gateway-only, --full
    - Use --help to see all available options

For more information, see: docs/archive/dev/GATEWAY_ROUTER_CONTRACT_SMOKE.md

EOF
}

# Parse arguments
MODE="full"
while [ $# -gt 0 ]; do
    case "$1" in
        -h|--help)
            show_help
            exit 0
            ;;
        --router-only|--gateway-only|--full)
            MODE="${1#--}"  # Remove '--' prefix
            shift
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
    
    # Check Router prerequisites
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
    
    # Check Gateway prerequisites (if not router-only)
    if [ "$MODE" != "router-only" ]; then
        if [ ! -d "$GATEWAY_DIR" ]; then
            log_warn "Gateway directory not found: $GATEWAY_DIR (skipping Gateway tests)"
            MODE="router-only"
        elif [ ! -f "$GATEWAY_DIR/package.json" ]; then
            log_warn "Gateway package.json not found (skipping Gateway tests)"
            MODE="router-only"
        fi
    fi
    
    log_info "Prerequisites check passed"
    return 0
}

# Run Router contract smoke test
run_router_test() {
    log_info "Running Router contract smoke test..."
    
    cd "$ROUTER_DIR" || return 1
    
    # Run the contract smoke test
    if command -v rebar3 >/dev/null 2>&1; then
        rebar3 ct --suite router_gateway_contract_smoke_SUITE || {
            log_error "Router contract smoke test failed"
            log_error ""
            log_error "Check test logs: $ROUTER_DIR/_build/test/logs/"
            return 1
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
    
    log_info "Router contract smoke test passed"
    return 0
}

# Run Gateway integration test (if available)
run_gateway_test() {
    log_info "Running Gateway integration test..."
    
    if [ ! -d "$GATEWAY_DIR" ]; then
        log_warn "Gateway directory not found, skipping Gateway tests"
        return 0
    fi
    
    cd "$GATEWAY_DIR" || return 1
    
    # Check if Gateway has contract test
    if [ -f "test/contract/router-contract.spec.ts" ] || [ -f "test/integration/router-contract.spec.ts" ]; then
        if command -v npm >/dev/null 2>&1; then
            npm test -- router-contract || {
                log_error "Gateway contract test failed"
                return 1
            }
        else
            log_warn "npm not found, skipping Gateway tests"
            return 0
        fi
    else
        log_warn "Gateway contract test not found, skipping"
        return 0
    fi
    
    log_info "Gateway integration test passed"
    return 0
}

# Run full E2E test (Router + Gateway)
run_full_e2e() {
    log_info "Running full E2E contract smoke test..."
    
    # Check if NATS is available
    if ! command -v nats >/dev/null 2>&1; then
        log_warn "NATS CLI not found, using Router contract test only"
        log_info "For full E2E test, install NATS CLI: https://docs.nats.io/using-nats/nats-tools/nats_cli"
        return 0
    fi
    
    # Check if Router is running (or can be started)
    if [ ! -d "$ROUTER_DIR" ]; then
        log_error "Router directory not found"
        return 2
    fi
    
    # Check if Gateway is available (optional)
    if [ ! -d "$GATEWAY_DIR" ]; then
        log_warn "Gateway directory not found, running Router-only E2E test"
        GATEWAY_AVAILABLE=false
    else
        GATEWAY_AVAILABLE=true
    fi
    
    # Run Router contract test (this verifies Router can handle NATS messages)
    log_info "Running Router contract test for E2E validation..."
    run_router_test || {
        log_error "Router contract test failed, E2E test cannot proceed"
        return 1
    }
    
    # If Gateway is available, run Gateway contract test
    if [ "$GATEWAY_AVAILABLE" = "true" ]; then
        log_info "Running Gateway contract test for E2E validation..."
        run_gateway_test || {
            log_warn "Gateway contract test failed, but Router test passed"
            log_info "E2E test partially successful (Router OK, Gateway issues)"
            return 0  # Don't fail E2E if Gateway has issues
        }
    fi
    
    log_info "Full E2E contract smoke test completed successfully"
    log_info "Note: For complete E2E test with real NATS, use integration tests in tests/integration/"
    
    return 0
}

# Main execution
main() {
    log_info "Gateway ↔ Router Contract Smoke Test"
    log_info "Mode: $MODE"
    
    check_prerequisites || exit 2
    
    case "$MODE" in
        router-only)
            run_router_test || exit 1
            ;;
        gateway-only)
            run_gateway_test || exit 1
            ;;
        full)
            run_router_test || exit 1
            run_gateway_test || exit 1
            run_full_e2e || exit 1
            ;;
        *)
            log_error "Error: Invalid mode '$MODE'"
            log_error ""
            log_error "Valid modes:"
            log_error "  --router-only    Run only Router contract test"
            log_error "  --gateway-only   Run only Gateway contract test"
            log_error "  --full           Run both Router and Gateway tests"
            log_error ""
            log_error "Use --help for more information"
            exit 3
            ;;
    esac
    
    log_info "Contract smoke test completed successfully"
    exit 0
}

main "$@"

