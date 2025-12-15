#!/bin/bash
# Start local development services
# Usage: ./scripts/local/up.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$PROJECT_ROOT"

echo "Starting Beamline services..."

# Check if .env exists
if [ ! -f .env ]; then
  echo "Warning: .env file not found. Using defaults from .env.example"
  if [ -f .env.example ]; then
    cp .env.example .env
    echo "Created .env from .env.example"
  fi
fi

# Start services
docker compose up -d

echo ""
echo "Services started. Checking status..."
docker compose ps

echo ""
echo "Services are available at:"
echo "  - C-Gateway API: http://localhost:${C_GATEWAY_HOST_PORT:-8081}"
echo "  - Router gRPC: localhost:${ROUTER_GRPC_PORT:-9000}"
echo "  - UI-Web: http://localhost:${UI_WEB_PORT:-4000}"
echo "  - NATS: nats://localhost:${NATS_PORT:-4222}"
echo "  - NATS Monitoring: http://localhost:${NATS_MONITOR_PORT:-8222}"
echo ""
echo "View logs: ./scripts/local/logs.sh"
echo "Stop services: ./scripts/local/down.sh"

