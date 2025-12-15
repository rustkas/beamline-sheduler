#!/bin/bash
# Run Router Intake Chaos Tests
# Usage: ./scripts/run_router_chaos_tests.sh [NATS_CONTAINER]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
ROUTER_DIR="$PROJECT_ROOT/apps/otp/router"

# Configuration
NATS_CONTAINER="${1:-nats}"

echo "=== Router Intake Chaos Tests ==="
echo "NATS Container: $NATS_CONTAINER"
echo ""

# Check if NATS container exists
if ! docker ps -a --format '{{.Names}}' | grep -q "^${NATS_CONTAINER}$"; then
    echo "Error: NATS container '${NATS_CONTAINER}' not found"
    echo "Please start NATS container first:"
    echo "  docker-compose up -d nats"
    echo "  or"
    echo "  docker run -d --name nats -p 4222:4222 nats:latest"
    exit 1
fi

# Ensure NATS is running
if ! docker ps --format '{{.Names}}' | grep -q "^${NATS_CONTAINER}$"; then
    echo "Starting NATS container..."
    docker start "$NATS_CONTAINER"
    sleep 3
fi

cd "$ROUTER_DIR"

# Export environment variables for tests
export NATS_CONTAINER="$NATS_CONTAINER"

# Run chaos tests
echo "Running chaos tests..."
rebar3 ct --suite router_intake_chaos_SUITE --verbose

echo ""
echo "=== Chaos Tests Complete ==="

# Ensure NATS is running at end
if ! docker ps --format '{{.Names}}' | grep -q "^${NATS_CONTAINER}$"; then
    echo "Warning: NATS container not running at end, starting..."
    docker start "$NATS_CONTAINER"
fi

