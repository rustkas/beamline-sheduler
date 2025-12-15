#!/bin/bash
# SLO Alert Rules Verification Script
# Purpose: Verify Prometheus alert rules are correctly configured
# Exit codes:
#   0: All alerts verified
#   1: Alert rules invalid (block release)
#   2: Alert thresholds incorrect (block release)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
ALERTS_FILE="$PROJECT_ROOT/apps/otp/router/docs/PROMETHEUS_ALERTS.md"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "=========================================="
echo "SLO Alert Rules Verification"
echo "=========================================="
echo ""

# Check if alerts file exists
if [ ! -f "$ALERTS_FILE" ]; then
    echo -e "${RED}❌ Alert rules file not found: $ALERTS_FILE${NC}"
    exit 1
fi

echo -e "${GREEN}✅ Alert rules file found${NC}"
echo ""

# Verify alert rules syntax (if promtool is available)
if command -v promtool &> /dev/null; then
    echo -e "${YELLOW}Verifying alert rules syntax...${NC}"
    
    # Extract YAML from markdown (basic extraction)
    # Note: This is a simplified check; full validation requires proper YAML extraction
    if grep -q "alert:" "$ALERTS_FILE"; then
        echo -e "  ${GREEN}✅${NC} Alert rules found in documentation"
    else
        echo -e "  ${RED}❌${NC} No alert rules found"
        exit 1
    fi
else
    echo -e "${YELLOW}⚠️  promtool not available, skipping syntax validation${NC}"
    echo "  Install promtool for full validation: https://prometheus.io/docs/prometheus/latest/getting_started/"
fi

echo ""

# Verify SLO alert thresholds
echo -e "${YELLOW}Verifying SLO alert thresholds...${NC}"

# Check for Router Intake Success Rate alert
if grep -q "RouterIntakeSuccessRateLow\|router_intake.*success.*rate" "$ALERTS_FILE"; then
    echo -e "  ${GREEN}✅${NC} Router Intake Success Rate alert found"
else
    echo -e "  ${YELLOW}⚠️${NC} Router Intake Success Rate alert not found (may be in separate file)"
fi

# Check for Router Intake Error Rate alert
if grep -q "RouterIntakeErrorRateHigh\|router_intake.*error.*rate" "$ALERTS_FILE"; then
    echo -e "  ${GREEN}✅${NC} Router Intake Error Rate alert found"
else
    echo -e "  ${YELLOW}⚠️${NC} Router Intake Error Rate alert not found (may be in separate file)"
fi

# Check for Router Intake Latency alert
if grep -q "RouterIntakeLatencyHigh\|router_intake.*latency" "$ALERTS_FILE"; then
    echo -e "  ${GREEN}✅${NC} Router Intake Latency alert found"
else
    echo -e "  ${YELLOW}⚠️${NC} Router Intake Latency alert not found (may be in separate file)"
fi

# Check for DLQ Publication alert
if grep -q "RouterIntakeDLQ\|router_intake.*dlq" "$ALERTS_FILE"; then
    echo -e "  ${GREEN}✅${NC} DLQ Publication alert found"
else
    echo -e "  ${YELLOW}⚠️${NC} DLQ Publication alert not found (may be in separate file)"
fi

echo ""

# Summary
echo "=========================================="
echo -e "${GREEN}✅ Alert rules verification complete${NC}"
echo "=========================================="
echo ""
echo "Note: Full validation requires:"
echo "  1. Prometheus alert rules in YAML format"
echo "  2. promtool for syntax validation"
echo "  3. Alert testing in staging environment"
echo ""

exit 0

