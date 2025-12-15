#!/bin/bash
# CP2 Readiness Self-Check Script
# Local helper to verify CP2 readiness using the same profile as CI
#
# Usage:
#   bash scripts/cp2_self_check.sh [--pre-release]
#
# Options:
#   --pre-release    Run pre-release regression profile (includes chaos tests)
#
# Exit codes:
#   0 - All checks passed
#   1 - CP2 Features E2E test failed
#   2 - Pre-release profile failed (if --pre-release specified)
#   3 - Script error

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
ROUTER_DIR="${ROOT_DIR}/apps/otp/router"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

# Parse arguments
RUN_PRE_RELEASE=false
while [[ $# -gt 0 ]]; do
    case $1 in
        --pre-release)
            RUN_PRE_RELEASE=true
            shift
            ;;
        *)
            log_error "Unknown option: $1"
            echo "Usage: $0 [--pre-release]"
            exit 3
            ;;
    esac
done

# Change to router directory
cd "${ROUTER_DIR}" || {
    log_error "Failed to change to router directory: ${ROUTER_DIR}"
    exit 3
}

log_info "Starting CP2 Readiness Self-Check..."
log_info "Router directory: ${ROUTER_DIR}"
echo ""

# Step 1: Run CP2 Features E2E test (single source of truth)
log_info "Step 1: Running CP2 Features E2E test (single source CP2 verification)..."
log_info "Command: rebar3 ct --suite router_cp2_features_e2e_SUITE"
echo ""

if rebar3 ct --suite router_cp2_features_e2e_SUITE; then
    log_info "✅ CP2 Features E2E test PASSED"
    cp2_e2e_passed=true
else
    log_error "❌ CP2 Features E2E test FAILED"
    cp2_e2e_passed=false
fi

echo ""

# Step 2: Run pre-release profile (if requested)
if [ "${RUN_PRE_RELEASE}" = true ]; then
    log_info "Step 2: Running pre-release regression profile..."
    log_info "Command: bash scripts/run_router_full_test_suite.sh --pre-release"
    echo ""

    cd "${ROOT_DIR}" || {
        log_error "Failed to change to root directory: ${ROOT_DIR}"
        exit 3
    }

    if bash scripts/run_router_full_test_suite.sh --pre-release; then
        log_info "✅ Pre-release regression profile PASSED"
        pre_release_passed=true
    else
        log_error "❌ Pre-release regression profile FAILED"
        pre_release_passed=false
    fi

    echo ""
else
    log_info "Step 2: Skipped (use --pre-release to run full regression profile)"
    pre_release_passed=true
fi

# Summary
echo "=========================================="
log_info "CP2 Readiness Self-Check Summary"
echo "=========================================="
echo ""

if [ "${cp2_e2e_passed}" = true ]; then
    log_info "✅ CP2 Features E2E: PASSED"
else
    log_error "❌ CP2 Features E2E: FAILED"
fi

if [ "${RUN_PRE_RELEASE}" = true ]; then
    if [ "${pre_release_passed}" = true ]; then
        log_info "✅ Pre-release regression profile: PASSED"
    else
        log_error "❌ Pre-release regression profile: FAILED"
    fi
else
    log_info "⏭️  Pre-release regression profile: SKIPPED (use --pre-release to run)"
fi

echo ""

# Log locations
log_info "Test logs location:"
log_info "  - CP2 Features E2E: ${ROUTER_DIR}/_build/test/logs/"
if [ "${RUN_PRE_RELEASE}" = true ]; then
    log_info "  - Pre-release profile: reports/dry-run-logs/"
fi

echo ""

# Exit code
if [ "${cp2_e2e_passed}" = false ]; then
    log_error "CP2 Readiness Self-Check FAILED (CP2 Features E2E test failed)"
    exit 1
elif [ "${RUN_PRE_RELEASE}" = true ] && [ "${pre_release_passed}" = false ]; then
    log_error "CP2 Readiness Self-Check FAILED (Pre-release profile failed)"
    exit 2
else
    log_info "✅ CP2 Readiness Self-Check PASSED"
    exit 0
fi

