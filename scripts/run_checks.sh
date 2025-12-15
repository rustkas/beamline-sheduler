#!/bin/bash
# Quick local checks script
# Runs minimal validation: Router compile/test, Gateway tests, ABI checks
# Exit codes: 0=success, 1=router error, 2=gateway error, 3=abi error

set -eo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

cd "$PROJECT_ROOT"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Exit codes
EXIT_SUCCESS=0
EXIT_ROUTER_ERROR=1
EXIT_GATEWAY_ERROR=2
EXIT_ABI_ERROR=3

# Track failures
FAILED_CHECKS=0

echo "=========================================="
echo "Local Checks Runner"
echo "=========================================="
echo ""

# Check required tools
check_tool() {
    local tool=$1
    if ! command -v "$tool" &> /dev/null; then
        echo -e "${RED}✗${NC} $tool not found. Please install it first."
        return 1
    fi
    return 0
}

check_c_gateway_json_tests() {
  echo "[c-gateway] JSON tests (optional)"

  local BIN="apps/c-gateway/build/c-gateway-json-test"

  if [ ! -x "${BIN}" ]; then
    echo "[c-gateway] JSON test binary not found (${BIN}), skipping"
    return 0
  fi

  if "${BIN}"; then
    echo "[c-gateway] JSON tests: OK"
  else
    echo "[c-gateway] JSON tests: FAILED (optional, not failing pipeline)"
  fi
}

check_c_gateway_smoke() {
  echo "[c-gateway] HTTP smoke (optional)"

  if [ ! -x "scripts/smoke_c_gateway.sh" ]; then
    echo "[c-gateway] scripts/smoke_c_gateway.sh not found, skipping"
    return 0
  fi

  if scripts/smoke_c_gateway.sh; then
    echo "[c-gateway] smoke: OK"
  else
    echo "[c-gateway] smoke: FAILED (optional, not failing pipeline)"
  fi
}

# C-Gateway checks: build (no tests yet)
check_c_gateway() {
    echo "----------------------------------------"
    echo "C-Gateway (C HTTP Gateway) Checks"
    echo "----------------------------------------"
    echo ""

    local c_gateway_dir="$PROJECT_ROOT/apps/c-gateway"

    if [ ! -f "$c_gateway_dir/Makefile" ]; then
        echo -e "${YELLOW}⚠${NC} C-Gateway Makefile not found, skipping C-Gateway checks"
        return 0
    fi

    cd "$c_gateway_dir"

    echo "Running: make (C-Gateway build)"
    if make; then
        echo -e "${GREEN}✓${NC} C-Gateway build: PASSED"
    else
        echo -e "${RED}✗${NC} C-Gateway build: FAILED"
        FAILED_CHECKS=$((FAILED_CHECKS + 1))
        # Do not introduce a separate exit code yet; treat as generic failure.
        return 1
    fi

    echo ""
    return $EXIT_SUCCESS
}

# Router checks: compile + tests
check_router() {
    echo "----------------------------------------"
    echo "Router (Erlang/OTP) Checks"
    echo "----------------------------------------"
    echo ""

    local router_dir="$PROJECT_ROOT/apps/otp/router"
    
    if [ ! -f "$router_dir/rebar.config" ]; then
        echo -e "${YELLOW}⚠${NC} Router rebar.config not found, skipping Router checks"
        return 0
    fi

    cd "$router_dir"

    # Check rebar3
    if ! check_tool rebar3; then
        echo -e "${RED}✗${NC} rebar3 not found. Skipping Router checks."
        return $EXIT_ROUTER_ERROR
    fi

    # Compile
    echo "Running: rebar3 compile"
    if rebar3 compile; then
        echo -e "${GREEN}✓${NC} Router compilation: PASSED"
    else
        echo -e "${RED}✗${NC} Router compilation: FAILED"
        FAILED_CHECKS=$((FAILED_CHECKS + 1))
        return $EXIT_ROUTER_ERROR
    fi

    echo ""

    # Tests (fast tests only for CP1 CI)
    echo "Running: make test-fast (fast tests only)"
    if make test-fast; then
        echo -e "${GREEN}✓${NC} Router tests: PASSED"
    else
        echo -e "${RED}✗${NC} Router tests: FAILED"
        FAILED_CHECKS=$((FAILED_CHECKS + 1))
        return $EXIT_ROUTER_ERROR
    fi

    echo ""
    return $EXIT_SUCCESS
}

# Gateway checks: lint → build → test (sequence)
check_gateway() {
    echo "----------------------------------------"
    echo "Gateway (NestJS) Checks"
    echo "----------------------------------------"
    echo ""

    local gateway_dir="$PROJECT_ROOT/apps/gateway"
    
    if [ ! -f "$gateway_dir/package.json" ]; then
        echo -e "${YELLOW}⚠${NC} Gateway package.json not found, skipping Gateway checks"
        return 0
    fi

    cd "$gateway_dir"

    # Check npm
    if ! check_tool npm; then
        echo -e "${YELLOW}⚠${NC} npm not found. Skipping Gateway checks."
        return 0
    fi

    # Step 1: Lint
    echo "Step 1: Running lint"
    if npm run lint:check; then
        echo -e "${GREEN}✓${NC} Gateway lint: PASSED"
    else
        echo -e "${RED}✗${NC} Gateway lint: FAILED"
        FAILED_CHECKS=$((FAILED_CHECKS + 1))
        return $EXIT_GATEWAY_ERROR
    fi

    echo ""

    # Step 2: Build
    echo "Step 2: Running build"
    if npm run build; then
        echo -e "${GREEN}✓${NC} Gateway build: PASSED"
    else
        echo -e "${RED}✗${NC} Gateway build: FAILED"
        FAILED_CHECKS=$((FAILED_CHECKS + 1))
        return $EXIT_GATEWAY_ERROR
    fi

    echo ""

    # Step 3: Tests
    echo "Step 3: Running tests"
    if npm run test; then
        echo -e "${GREEN}✓${NC} Gateway tests: PASSED"
    else
        echo -e "${RED}✗${NC} Gateway tests: FAILED"
        FAILED_CHECKS=$((FAILED_CHECKS + 1))
        return $EXIT_GATEWAY_ERROR
    fi

    echo ""
    return $EXIT_SUCCESS
}

# Policy schema validation
check_policy_schema() {
    echo "----------------------------------------"
    echo "Policy Schema and Fixtures Validation"
    echo "----------------------------------------"
    echo ""

    local policy_schema_script="$PROJECT_ROOT/scripts/check_policy_schema.sh"
    
    if [ ! -f "$policy_schema_script" ]; then
        echo -e "${YELLOW}⚠${NC} Policy schema validation script not found, skipping"
        return 0
    fi

    if bash "$policy_schema_script"; then
        echo -e "${GREEN}✓${NC} Policy schema validation: PASSED"
        return $EXIT_SUCCESS
    else
        echo -e "${RED}✗${NC} Policy schema validation: FAILED"
        FAILED_CHECKS=$((FAILED_CHECKS + 1))
        return 1
    fi
}

# ABI checks: buf lint + buf breaking
check_abi() {
    echo "----------------------------------------"
    echo "ABI (Protobuf) Checks"
    echo "----------------------------------------"
    echo ""

    local proto_dir="$PROJECT_ROOT/proto"
    
    if [ ! -d "$proto_dir" ]; then
        echo -e "${YELLOW}⚠${NC} proto directory not found, skipping ABI checks"
        return 0
    fi

    # Check buf
    if ! check_tool buf; then
        echo -e "${YELLOW}⚠${NC} buf not found. Install from https://buf.build/docs/installation"
        echo -e "${YELLOW}⚠${NC} Skipping ABI checks."
        return 0
    fi

    cd "$PROJECT_ROOT"

    # Check for buf.yaml or buf.work.yaml
    if [ ! -f "buf.yaml" ] && [ ! -f "buf.work.yaml" ]; then
        echo -e "${YELLOW}⚠${NC} buf.yaml or buf.work.yaml not found, skipping ABI checks"
        return 0
    fi

    # buf lint
    echo "Running: buf lint"
    if buf lint; then
        echo -e "${GREEN}✓${NC} buf lint: PASSED"
    else
        echo -e "${RED}✗${NC} buf lint: FAILED"
        FAILED_CHECKS=$((FAILED_CHECKS + 1))
        return $EXIT_ABI_ERROR
    fi

    echo ""

    # buf breaking (against main/master branch if available, otherwise skip)
    echo "Running: buf breaking"
    if git rev-parse --verify origin/main >/dev/null 2>&1 || git rev-parse --verify origin/master >/dev/null 2>&1; then
        local base_branch="origin/main"
        if ! git rev-parse --verify origin/main >/dev/null 2>&1; then
            base_branch="origin/master"
        fi
        
        if buf breaking --against "$base_branch"; then
            echo -e "${GREEN}✓${NC} buf breaking: PASSED"
        else
            echo -e "${RED}✗${NC} buf breaking: FAILED (breaking changes detected)"
            FAILED_CHECKS=$((FAILED_CHECKS + 1))
            return $EXIT_ABI_ERROR
        fi
    else
        echo -e "${YELLOW}⚠${NC} No origin/main or origin/master found, skipping buf breaking"
        echo -e "${YELLOW}⚠${NC} (This is normal for local development without remote)"
    fi

    echo ""
    return $EXIT_SUCCESS
}

# Main execution
main() {
    local router_result=$EXIT_SUCCESS
    local gateway_result=$EXIT_SUCCESS
    local abi_result=$EXIT_SUCCESS
    local c_gateway_result=$EXIT_SUCCESS
    local policy_schema_result=$EXIT_SUCCESS

    # Router checks
    check_router || router_result=$?

    cd "$PROJECT_ROOT"

    # Gateway checks
    check_gateway || gateway_result=$?

    cd "$PROJECT_ROOT"

    # C-Gateway checks
    check_c_gateway || c_gateway_result=$?

    cd "$PROJECT_ROOT"

    # Policy schema validation
    check_policy_schema || policy_schema_result=$?

    cd "$PROJECT_ROOT"

    # ABI checks
    check_abi || abi_result=$?

    # DevState verify
    echo "----------------------------------------"
    echo "DevState (State/Audit) Verify"
    echo "----------------------------------------"
    echo ""

    if [ -f "$PROJECT_ROOT/scripts/devstate.sh" ]; then
        echo "Running: scripts/devstate.sh verify"
        if bash "$PROJECT_ROOT/scripts/devstate.sh" verify; then
            echo -e "${GREEN}✓${NC} DevState verify: PASSED"
        else
            echo -e "${RED}✗${NC} DevState verify: FAILED"
            FAILED_CHECKS=$((FAILED_CHECKS + 1))
        fi
    else
        echo -e "${YELLOW}⚠${NC} DevState CLI not found, skipping"
    fi

    # Summary
    echo "=========================================="
    echo "Summary"
    echo "=========================================="
    echo ""

    if [ $router_result -eq $EXIT_SUCCESS ]; then
        echo -e "${GREEN}✓${NC} Router: PASSED"
    else
        echo -e "${RED}✗${NC} Router: FAILED"
    fi

    if [ $gateway_result -eq $EXIT_SUCCESS ]; then
        echo -e "${GREEN}✓${NC} Gateway: PASSED"
    else
        echo -e "${RED}✗${NC} Gateway: FAILED"
    fi

    if [ $abi_result -eq $EXIT_SUCCESS ]; then
        echo -e "${GREEN}✓${NC} ABI: PASSED"
    else
        echo -e "${RED}✗${NC} ABI: FAILED"
    fi

    if [ $c_gateway_result -eq $EXIT_SUCCESS ]; then
        echo -e "${GREEN}✓${NC} C-Gateway: PASSED"
    else
        echo -e "${RED}✗${NC} C-Gateway: FAILED"
    fi

    if [ $policy_schema_result -eq $EXIT_SUCCESS ]; then
        echo -e "${GREEN}✓${NC} Policy Schema: PASSED"
    else
        echo -e "${RED}✗${NC} Policy Schema: FAILED"
    fi

    echo ""

    # OBS-1 validators (optional)
    echo "----------------------------------------"
    echo "OBS-1 Log Conformance (Node + jq + Python)"
    echo "----------------------------------------"
    echo ""

    if [ -f "$PROJECT_ROOT/scripts/observability/run_obs1_validators.sh" ]; then
        echo "Running: scripts/observability/run_obs1_validators.sh"
        if bash "$PROJECT_ROOT/scripts/observability/run_obs1_validators.sh" --paths "$PROJECT_ROOT/reports/dry-run-logs/obs1/*.jsonl" "$PROJECT_ROOT/.windsurf/reports/*.jsonl"; then
            echo -e "${GREEN}✓${NC} OBS-1 validators: PASSED or SKIPPED (no logs)"
        else
            echo -e "${YELLOW}⚠${NC} OBS-1 validators: issues detected (see summaries in .windsurf/reports)"
            FAILED_CHECKS=$((FAILED_CHECKS + 1))
        fi
    else
        echo -e "${YELLOW}⚠${NC} OBS-1 wrapper not found; skip log conformance"
    fi

    echo ""

    # Overall result
    if [ $FAILED_CHECKS -eq 0 ]; then
        echo -e "${GREEN}✓ All checks passed${NC}"
        exit $EXIT_SUCCESS
    else
        echo -e "${RED}✗ $FAILED_CHECKS check(s) failed${NC}"
        # Return the first non-zero exit code
        if [ $router_result -ne $EXIT_SUCCESS ]; then
            exit $EXIT_ROUTER_ERROR
        elif [ $gateway_result -ne $EXIT_SUCCESS ]; then
            exit $EXIT_GATEWAY_ERROR
        elif [ $abi_result -ne $EXIT_SUCCESS ]; then
            exit $EXIT_ABI_ERROR
        fi
        # C-Gateway failures fall through as generic error for now
        exit 1
    fi
}

# Run main
main "$@"
