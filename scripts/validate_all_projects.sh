#!/usr/bin/env bash
# Validate All Sub-Projects for CP1
# Usage: ./scripts/validate_all_projects.sh

set -euo pipefail

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$PROJECT_ROOT"

echo "=== CP1 Multi-Project Validation ==="
echo ""

TOTAL_CHECKS=0
PASSED_CHECKS=0
FAILED_CHECKS=0

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

function check() {
    local name="$1"
    local command="$2"
    
    ((TOTAL_CHECKS++))
    echo -n "Checking $name... "
    
    if eval "$command" &>/dev/null; then
        echo -e "${GREEN}✅ PASS${NC}"
        ((PASSED_CHECKS++))
        return 0
    else
        echo -e "${RED}❌ FAIL${NC}"
        ((FAILED_CHECKS++))
        return 1
    fi
}

function section() {
    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "$1"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
}

# ==========================================
# 1. Router (Erlang/OTP)
# ==========================================
section "1. Router (Erlang/OTP)"

if [ -d "apps/otp/router" ]; then
    cd apps/otp/router
    
    check "Router compilation" "rebar3 compile"
    check "Router Dialyzer" "rebar3 dialyzer"
    check "Router tests" "rebar3 eunit"
    
    cd "$PROJECT_ROOT"
else
    echo -e "${YELLOW}⚠️  Router directory not found${NC}"
fi

# ==========================================
# 2. Gateway (NestJS/TypeScript)
# ==========================================
section "2. Gateway (NestJS/TypeScript)"

if [ -d "apps/gateway" ] && [ -f "apps/gateway/package.json" ]; then
    cd apps/gateway
    
    # Check if node_modules exists
    if [ ! -d "node_modules" ]; then
        echo -e "${YELLOW}⚠️  Installing dependencies...${NC}"
        npm install &>/dev/null || true
    fi
    
    check "Gateway TypeScript compilation" "npm run build"
    check "Gateway linting" "npm run lint:check"
    check "Gateway unit tests" "npm run test:unit"
    
    cd "$PROJECT_ROOT"
else
    echo -e "${YELLOW}⚠️  Gateway directory not found${NC}"
fi

# ==========================================
# 2.1. Gateway ↔ Router Contract Smoke Test
# ==========================================
section "2.1. Gateway ↔ Router Contract Smoke Test"

if [ -f "scripts/gateway_router_contract_smoke.sh" ]; then
    check "Gateway ↔ Router contract smoke test" "bash scripts/gateway_router_contract_smoke.sh --router-only"
else
    echo -e "${YELLOW}⚠️  Gateway ↔ Router contract smoke test script not found${NC}"
fi

# ==========================================
# 3. Provider (Erlang/OTP)
# ==========================================
section "3. Provider (Erlang/OTP)"

if [ -d "apps/otp/provider" ]; then
    cd apps/otp/provider
    
    # Check if rebar.config exists
    if [ -f "rebar.config" ]; then
        check "Provider compilation" "rebar3 compile"
    else
        echo -e "${YELLOW}⚠️  Provider not yet implemented (empty directory)${NC}"
    fi
    
    cd "$PROJECT_ROOT"
else
    echo -e "${YELLOW}⚠️  Provider directory not found${NC}"
fi

# ==========================================
# 4. Usage (Erlang/OTP)
# ==========================================
section "4. Usage (Erlang/OTP)"

if [ -d "apps/otp/usage" ]; then
    if [ -f "apps/otp/usage/rebar.config" ]; then
        cd apps/otp/usage
        check "Usage compilation" "rebar3 compile"
        cd "$PROJECT_ROOT"
    else
        echo -e "${YELLOW}⚠️  Usage not implemented (directory exists but empty)${NC}"
    fi
else
    echo -e "${YELLOW}⚠️  Usage directory not found${NC}"
fi

# ==========================================
# 5. UI (legacy SvelteKit / Phoenix LiveView)
# ==========================================
section "5. UI (legacy SvelteKit / Phoenix LiveView)"

if [ -d "frontend" ] && [ -f "frontend/package.json" ]; then
    cd frontend
    
    echo -e "${YELLOW}ℹ️  Running checks for legacy SvelteKit UI in frontend/. Main UI is apps/ui_web (Phoenix LiveView).${NC}"

    # Check if node_modules exists
    if [ ! -d "node_modules" ]; then
        echo -e "${YELLOW}⚠️  Installing dependencies...${NC}"
        pnpm install &>/dev/null || npm install &>/dev/null || true
    fi
    
    check "Frontend TypeScript check" "pnpm run check || npm run check"
    check "Frontend tests" "pnpm run test || npm run test"
    
    cd "$PROJECT_ROOT"
else
    echo -e "${YELLOW}ℹ️  SvelteKit frontend (frontend/) not found. This is expected after migration to apps/ui_web (Phoenix LiveView).${NC}"
fi

# ==========================================
# 6. CAF Components (C++)
# ==========================================
section "6. CAF Components (C++)"

if [ -d "apps/caf/ingress" ] || [ -d "apps/caf/processor" ]; then
    echo -e "${YELLOW}ℹ️  CAF components are CP2 scope (deferred)${NC}"
else
    echo -e "${YELLOW}⚠️  CAF directories not found${NC}"
fi

# ==========================================
# 7. Proto/Contracts
# ==========================================
section "7. Proto/Contracts"

if [ -d "proto" ]; then
    cd proto
    
    if command -v buf &>/dev/null; then
        check "Proto lint" "buf lint"
        check "Proto breaking changes" "buf breaking --against .git#branch=main"
    else
        echo -e "${YELLOW}⚠️  buf not installed (skipping proto checks)${NC}"
    fi
    
    cd "$PROJECT_ROOT"
else
    echo -e "${YELLOW}⚠️  Proto directory not found${NC}"
fi

# ==========================================
# 8. Cross-Cutting Checks
# ==========================================
section "8. Cross-Cutting Checks"

check "CP1 contracts" "./scripts/check_cp1_contracts.sh"
check "State validation" "./scripts/validate_state.sh"

if [ -f "scripts/check_secret_leaks.sh" ]; then
    check "Secret leaks" "./scripts/check_secret_leaks.sh"
fi

# ==========================================
# Summary
# ==========================================
section "Summary"

echo ""
echo "Total Checks: $TOTAL_CHECKS"
echo -e "${GREEN}Passed: $PASSED_CHECKS${NC}"
echo -e "${RED}Failed: $FAILED_CHECKS${NC}"

echo ""

if [ "$FAILED_CHECKS" -eq 0 ]; then
    echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${GREEN}✅ ALL CHECKS PASSED${NC}"
    echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    exit 0
else
    echo -e "${RED}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${RED}❌ SOME CHECKS FAILED${NC}"
    echo -e "${RED}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    exit 1
fi
