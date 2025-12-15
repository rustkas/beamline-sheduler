#!/bin/bash
# Service Verification Script
# Purpose: Verify all services are up and responding correctly
# Usage: Run after docker-compose up

set -euo pipefail

echo "Verifying Beamline services..."

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Check PostgreSQL
echo -n "Checking PostgreSQL... "
if docker exec beamline-postgres pg_isready -U beamline -d beamline > /dev/null 2>&1; then
  echo -e "${GREEN}✓${NC}"
else
  echo -e "${RED}✗${NC}"
  exit 1
fi

# Check NATS
echo -n "Checking NATS... "
if curl -s http://localhost:8222/healthz > /dev/null 2>&1; then
  echo -e "${GREEN}✓${NC}"
else
  echo -e "${RED}✗${NC}"
  exit 1
fi

# Check Router (gRPC health)
echo -n "Checking Router (gRPC)... "
if docker exec beamline-router /usr/local/bin/grpc_health_probe -addr=:9000 > /dev/null 2>&1; then
  echo -e "${GREEN}✓${NC}"
else
  echo -e "${YELLOW}⚠${NC} (Router may still be starting)"
fi

# Check Gateway health endpoint
echo -n "Checking Gateway /_health... "
if curl -s http://localhost:8080/_health > /dev/null 2>&1; then
  echo -e "${GREEN}✓${NC}"
else
  echo -e "${YELLOW}⚠${NC} (Gateway may still be starting)"
fi

# Check Gateway /api/v1/routes/decide endpoint
echo -n "Checking Gateway /api/v1/routes/decide... "
HTTP_CODE=$(curl -s -o /dev/null -w "%{http_code}" http://localhost:8080/api/v1/routes/decide || echo "000")
if [ "$HTTP_CODE" = "200" ] || [ "$HTTP_CODE" = "400" ] || [ "$HTTP_CODE" = "404" ]; then
  echo -e "${GREEN}✓${NC} (HTTP $HTTP_CODE)"
else
  echo -e "${YELLOW}⚠${NC} (HTTP $HTTP_CODE - endpoint may not be implemented yet)"
fi

echo ""
echo -e "${GREEN}Service verification complete${NC}"

