#!/bin/bash
# Run Router Intake Load Tests
# Usage: ./scripts/run_router_load_tests.sh [MESSAGE_COUNT] [PARALLEL_WORKERS]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
ROUTER_DIR="$PROJECT_ROOT/apps/otp/router"

# Configuration
MESSAGE_COUNT="${1:-2000}"  # Default: 2000 messages
PARALLEL_WORKERS="${2:-1}"  # Default: 1 (sequential)

echo "=== Router Intake Load Tests ==="
echo "Message Count: $MESSAGE_COUNT"
echo "Parallel Workers: $PARALLEL_WORKERS"
echo ""

cd "$ROUTER_DIR"

# Export environment variables for tests
export LOAD_TEST_MESSAGE_COUNT="$MESSAGE_COUNT"
export LOAD_TEST_PARALLEL_WORKERS="$PARALLEL_WORKERS"

# Run load tests
echo "Running load tests..."
rebar3 ct --suite router_performance_load_SUITE --verbose

echo ""
echo "=== Load Tests Complete ==="

