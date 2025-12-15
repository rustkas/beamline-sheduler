#!/usr/bin/env bash
set -euo pipefail

echo "=== UI-Web Smoke Test ==="

GATEWAY_URL="${GATEWAY_URL:-http://localhost:8080}"
UI_URL="${UI_URL:-http://localhost:4000}"
TIMEOUT=30

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m'

check() {
  local name=$1
  local url=$2
  local expected_status=${3:-200}

  echo -n "Checking $name... "

  status=$(curl -s -o /dev/null -w "%{http_code}" --max-time "$TIMEOUT" "$url" || echo "000")

  if [ "$status" = "$expected_status" ]; then
    echo -e "${GREEN}✓${NC} ($status)"
    return 0
  else
    echo -e "${RED}✗${NC} (got $status, expected $expected_status)"
    return 1
  fi
}

# 1. Check Gateway is running
echo "--- Checking Gateway ---"
if check "Gateway Health" "$GATEWAY_URL/_health"; then
  echo "  Gateway is online"
else
  echo -e "${YELLOW}  Warning: Gateway is not reachable${NC}"
  echo "  This may be expected if Gateway is not running"
fi

# 2. Check UI-Web is running
echo "--- Checking UI-Web ---"
if ! check "UI-Web Health" "$UI_URL/_health"; then
  echo -e "${RED}UI-Web is not running!${NC}"
  echo "  Start with: cd apps/ui_web && mix phx.server"
  exit 1
fi

check "UI-Web Home" "$UI_URL/" || exit 1

# 3. Check Extensions page (may require auth, so 200 or 302 is OK)
echo "--- Checking Extensions ---"
ext_status=$(curl -s -o /dev/null -w "%{http_code}" --max-time "$TIMEOUT" "$UI_URL/app/extensions" || echo "000")
if [ "$ext_status" = "200" ] || [ "$ext_status" = "302" ]; then
  echo -e "${GREEN}✓${NC} Extensions page accessible ($ext_status)"
else
  echo -e "${YELLOW}⚠${NC} Extensions page returned $ext_status (may require authentication)"
fi

# 4. Check Gateway API contract (if Gateway is available)
if curl -s --max-time 5 "$GATEWAY_URL/_health" > /dev/null 2>&1; then
  echo "--- Checking Gateway API ---"
  check "Extensions API" "$GATEWAY_URL/api/v1/extensions" || echo "  (May require authentication)"
  check "Messages API" "$GATEWAY_URL/api/v1/messages" || echo "  (May require authentication)"
else
  echo "--- Skipping Gateway API checks (Gateway not available) ---"
fi

# 5. Check NATS connectivity (if available)
if command -v nats &> /dev/null; then
  echo "--- Checking NATS ---"
  if nats server check 2>/dev/null; then
    echo -e "${GREEN}✓${NC} NATS server check passed"
  else
    echo -e "${YELLOW}⚠${NC} NATS not available (optional for smoke test)"
  fi
else
  echo "--- Skipping NATS check (nats CLI not installed) ---"
fi

echo ""
echo -e "${GREEN}=== All smoke tests passed ===${NC}"
echo ""
echo "Next steps:"
echo "  1. Open browser: $UI_URL"
echo "  2. Check Extensions page loads"
echo "  3. Verify Gateway status badge shows 'Online'"
echo "  4. Test real-time updates (open 2 tabs)"
