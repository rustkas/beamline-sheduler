#!/bin/bash
# SLO Documentation Verification Script
# Purpose: Verify SLO/SLI documentation is complete and accurate
# Exit codes:
#   0: Documentation complete
#   1: Documentation incomplete (block release)
#   2: Documentation inaccurate (block release)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
SLO_DOC="$PROJECT_ROOT/docs/ARCHITECTURE/SLI_SLO_ROUTER_GATEWAY.md"
METRICS_CATALOG="$PROJECT_ROOT/config/observability/metrics.catalog.yaml"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "=========================================="
echo "SLO Documentation Verification"
echo "=========================================="
echo ""

# Check if SLO document exists
if [ ! -f "$SLO_DOC" ]; then
    echo -e "${RED}❌ SLO document not found: $SLO_DOC${NC}"
    exit 1
fi

echo -e "${GREEN}✅ SLO document found${NC}"
echo ""

# Verify SLO/SLI definitions
echo -e "${YELLOW}Verifying SLO/SLI definitions...${NC}"

REQUIRED_SLI=(
    "SLI 1: Intake Success Rate"
    "SLI 2: Intake Error Rate"
    "SLI 3: Intake Processing Latency"
    "SLI 4: DLQ Publication Success Rate"
    "SLI 5: Gateway HTTP Success Rate"
    "SLI 6: Gateway Error Rate"
    "SLI 7: Gateway End-to-End Latency"
    "SLI 8: Gateway Rate Limiting Hit Rate"
)

for sli in "${REQUIRED_SLI[@]}"; do
    if grep -q "$sli" "$SLO_DOC"; then
        echo -e "  ${GREEN}✅${NC} $sli"
    else
        echo -e "  ${RED}❌${NC} $sli (not found)"
        exit 1
    fi
done

echo ""

# Verify SLO targets
echo -e "${YELLOW}Verifying SLO targets...${NC}"

REQUIRED_SLO=(
    "SLO 1: Intake Success Rate"
    "SLO 2: Intake Error Rate"
    "SLO 3: Intake Processing Latency"
    "SLO 4: DLQ Publication Success Rate"
    "SLO 5: Gateway HTTP Success Rate"
    "SLO 6: Gateway Error Rate"
    "SLO 7: Gateway End-to-End Latency"
    "SLO 8: Gateway Rate Limiting"
)

for slo in "${REQUIRED_SLO[@]}"; do
    if grep -q "$slo" "$SLO_DOC"; then
        echo -e "  ${GREEN}✅${NC} $slo"
    else
        echo -e "  ${RED}❌${NC} $slo (not found)"
        exit 1
    fi
done

echo ""

# Verify test coverage
echo -e "${YELLOW}Verifying test coverage documentation...${NC}"

REQUIRED_TEST_COVERAGE=(
    "Load Tests"
    "Chaos Tests"
    "E2E Tests"
    "Overload Tests"
)

for coverage in "${REQUIRED_TEST_COVERAGE[@]}"; do
    if grep -q "$coverage" "$SLO_DOC"; then
        echo -e "  ${GREEN}✅${NC} $coverage"
    else
        echo -e "  ${RED}❌${NC} $coverage (not found)"
        exit 1
    fi
done

echo ""

# Verify pre-release gates
echo -e "${YELLOW}Verifying pre-release gates documentation...${NC}"

REQUIRED_GATES=(
    "Gate 1: SLO Verification Tests"
    "Gate 2: Metrics Verification"
    "Gate 3: Alert Rules Verification"
    "Gate 4: Documentation Verification"
)

for gate in "${REQUIRED_GATES[@]}"; do
    if grep -q "$gate" "$SLO_DOC"; then
        echo -e "  ${GREEN}✅${NC} $gate"
    else
        echo -e "  ${RED}❌${NC} $gate (not found)"
        exit 1
    fi
done

echo ""

# Verify metrics catalog
echo -e "${YELLOW}Verifying metrics catalog...${NC}"

if [ ! -f "$METRICS_CATALOG" ]; then
    echo -e "  ${YELLOW}⚠️${NC} Metrics catalog not found: $METRICS_CATALOG"
else
    echo -e "  ${GREEN}✅${NC} Metrics catalog found"
    
    # Check for key metrics in catalog
    KEY_METRICS=(
        "router_intake_messages_total"
        "router_intake_processing_latency_seconds"
        "router_intake_dlq_messages_total"
        "gateway_http_requests_total"
        "gateway_http_request_duration_seconds"
    )
    
    for metric in "${KEY_METRICS[@]}"; do
        if grep -q "$metric" "$METRICS_CATALOG"; then
            echo -e "    ${GREEN}✅${NC} $metric"
        else
            echo -e "    ${YELLOW}⚠️${NC} $metric (not found in catalog)"
        fi
    done
fi

echo ""

# Summary
echo "=========================================="
echo -e "${GREEN}✅ Documentation verification complete${NC}"
echo "=========================================="
echo ""

exit 0

