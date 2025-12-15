#!/bin/bash
# Start all extension services for E2E testing

set -e

NATS_URL=${NATS_URL:-nats://localhost:4222}
EXTENSIONS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "Starting Beamline Extensions..."
echo "NATS_URL: $NATS_URL"
echo "Extensions directory: $EXTENSIONS_DIR"

# Check if node_modules exists
if [ ! -d "$EXTENSIONS_DIR/node_modules" ]; then
    echo "Installing dependencies..."
    cd "$EXTENSIONS_DIR"
    npm install
fi

# Start extensions in background
echo "Starting normalize_text..."
NATS_URL="$NATS_URL" node "$EXTENSIONS_DIR/src/normalize_text.js" > /tmp/normalize_text.log 2>&1 &
NORMALIZE_PID=$!

echo "Starting pii_guard..."
NATS_URL="$NATS_URL" node "$EXTENSIONS_DIR/src/pii_guard.js" > /tmp/pii_guard.log 2>&1 &
PII_GUARD_PID=$!

echo "Starting mask_pii..."
NATS_URL="$NATS_URL" node "$EXTENSIONS_DIR/src/mask_pii.js" > /tmp/mask_pii.log 2>&1 &
MASK_PII_PID=$!

echo "Starting test_provider..."
NATS_URL="$NATS_URL" node "$EXTENSIONS_DIR/src/test_provider.js" > /tmp/test_provider.log 2>&1 &
TEST_PROVIDER_PID=$!

echo "Extensions started:"
echo "  normalize_text: PID $NORMALIZE_PID"
echo "  pii_guard: PID $PII_GUARD_PID"
echo "  mask_pii: PID $MASK_PII_PID"
echo "  test_provider: PID $TEST_PROVIDER_PID"

# Wait a bit for extensions to start
sleep 2

# Function to cleanup on exit
cleanup() {
    echo "Stopping extensions..."
    kill $NORMALIZE_PID $PII_GUARD_PID $MASK_PII_PID $TEST_PROVIDER_PID 2>/dev/null || true
    wait $NORMALIZE_PID $PII_GUARD_PID $MASK_PII_PID $TEST_PROVIDER_PID 2>/dev/null || true
    echo "Extensions stopped"
}

trap cleanup EXIT INT TERM

echo "Extensions running. Press Ctrl+C to stop."
echo "Logs:"
echo "  normalize_text: /tmp/normalize_text.log"
echo "  pii_guard: /tmp/pii_guard.log"
echo "  mask_pii: /tmp/mask_pii.log"
echo "  test_provider: /tmp/test_provider.log"

# Wait for all processes
wait

