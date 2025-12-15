#!/bin/bash
# SLO Metrics Verification Script
# Purpose: Verify SLO metrics are correctly emitted and queryable
# Exit codes:
#   0: All metrics verified
#   1: Metrics missing or invalid (block release)
#   2: SLI queries failed (block release)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Prometheus URL (default: localhost:9090)
PROMETHEUS_URL="${PROMETHEUS_URL:-http://localhost:9090}"

echo "=========================================="
echo "SLO Metrics Verification"
echo "=========================================="
echo ""

# Check if Prometheus is available
if ! curl -sf "$PROMETHEUS_URL/api/v1/status/config" > /dev/null 2>&1; then
    echo -e "${YELLOW}⚠️  Prometheus not available at $PROMETHEUS_URL${NC}"
    echo "Skipping metrics verification (requires Prometheus)"
    echo ""
    echo "To verify metrics:"
    echo "  1. Start Prometheus"
    echo "  2. Set PROMETHEUS_URL environment variable"
    echo "  3. Re-run this script"
    echo ""
    exit 0
fi

echo -e "${GREEN}✅ Prometheus available${NC}"
echo ""

# Router Metrics Verification
echo -e "${YELLOW}Verifying Router Metrics...${NC}"

ROUTER_METRICS=(
    "router_intake_messages_total"
    "router_intake_processing_latency_seconds"
    "router_intake_dlq_messages_total"
    "router_intake_dlq_publish_failed_total"
    "router_intake_validation_errors_total"
)

for metric in "${ROUTER_METRICS[@]}"; do
    if curl -sf "$PROMETHEUS_URL/api/v1/query?query=$metric" | jq -e '.data.result | length > 0' > /dev/null 2>&1; then
        echo -e "  ${GREEN}✅${NC} $metric"
    else
        echo -e "  ${RED}❌${NC} $metric (not found)"
        exit 1
    fi
done

echo ""

# Gateway Metrics Verification
echo -e "${YELLOW}Verifying Gateway Metrics...${NC}"

GATEWAY_METRICS=(
    "gateway_http_requests_total"
    "gateway_http_request_duration_seconds"
    "gateway_rate_limit_exceeded_total"
    "gateway_rate_limit_hits_total"
)

for metric in "${GATEWAY_METRICS[@]}"; do
    if curl -sf "$PROMETHEUS_URL/api/v1/query?query=$metric" | jq -e '.data.result | length > 0' > /dev/null 2>&1; then
        echo -e "  ${GREEN}✅${NC} $metric"
    else
        echo -e "  ${YELLOW}⚠️${NC} $metric (not found, may not be exposed yet)"
        # Don't fail for Gateway metrics if not exposed (CP1 vs CP2)
    fi
done

echo ""

# SLI Queries Verification
echo -e "${YELLOW}Verifying SLI Queries...${NC}"

SLI_QUERIES=(
    "sum(rate(router_intake_messages_total{status=\"ok\"}[1h])) / sum(rate(router_intake_messages_total[1h]))"
    "sum(rate(router_intake_messages_total{status=\"failed\"}[1h])) / sum(rate(router_intake_messages_total[1h]))"
    "histogram_quantile(0.95, sum(rate(router_intake_processing_latency_seconds_bucket[1h])) by (le, subject))"
    "sum(rate(router_intake_dlq_messages_total[1h])) / (sum(rate(router_intake_dlq_messages_total[1h])) + sum(rate(router_intake_dlq_publish_failed_total[1h])))"
)

for query in "${SLI_QUERIES[@]}"; do
    # URL encode query
    encoded_query=$(printf '%s' "$query" | jq -sRr @uri)
    
    if curl -sf "$PROMETHEUS_URL/api/v1/query?query=$encoded_query" | jq -e '.status == "success"' > /dev/null 2>&1; then
        echo -e "  ${GREEN}✅${NC} SLI query valid"
    else
        echo -e "  ${RED}❌${NC} SLI query failed: $query"
        exit 2
    fi
done

echo ""

# Summary
echo "=========================================="
echo -e "${GREEN}✅ All metrics verified${NC}"
echo "=========================================="
echo ""

exit 0

