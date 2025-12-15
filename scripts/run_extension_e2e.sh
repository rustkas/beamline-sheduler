#!/bin/bash
#
# Run Extension E2E Tests
#
# Usage:
#   ./scripts/run_extension_e2e.sh <extension_id> [options]
#
# Options:
#   --nats-url <url>        NATS server URL (default: nats://localhost:4222)
#   --router-url <url>      Router URL for E2E (optional)
#   --registry-config <file> Registry config file path
#
# Prerequisites:
#   - NATS server running
#   - Extension registered in Router registry
#   - Router running (optional, for full E2E)
#
# Examples:
#   ./scripts/run_extension_e2e.sh my_extension
#   ./scripts/run_extension_e2e.sh my_extension --nats-url nats://localhost:4222
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
EXTENSIONS_DIR="${PROJECT_ROOT}/tools/extensions"
ROUTER_FIXTURES_DIR="${PROJECT_ROOT}/apps/otp/router/priv/fixtures/extensions"

# Default values
NATS_URL="${NATS_URL:-nats://localhost:4222}"
ROUTER_URL=""
REGISTRY_CONFIG=""

# Parse arguments
if [ $# -lt 1 ]; then
    echo "Usage: $0 <extension_id> [options]"
    echo ""
    echo "Options:"
    echo "  --nats-url <url>        NATS server URL"
    echo "  --router-url <url>     Router URL for E2E"
    echo "  --registry-config <file> Registry config file path"
    exit 1
fi

EXTENSION_ID="$1"
shift

# Parse options
while [[ $# -gt 0 ]]; do
    case $1 in
        --nats-url)
            NATS_URL="$2"
            shift 2
            ;;
        --router-url)
            ROUTER_URL="$2"
            shift 2
            ;;
        --registry-config)
            REGISTRY_CONFIG="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

EXTENSION_DIR="${EXTENSIONS_DIR}/${EXTENSION_ID}"

# Check if extension exists
if [ ! -d "${EXTENSION_DIR}" ]; then
    echo "Error: Extension directory not found: ${EXTENSION_DIR}"
    echo "Generate extension first: ./scripts/generate_extension.sh ${EXTENSION_ID} <type>"
    exit 1
fi

# Check if registry config exists
if [ -z "${REGISTRY_CONFIG}" ]; then
    REGISTRY_CONFIG="${EXTENSION_DIR}/${EXTENSION_ID}.registry.json"
fi

if [ ! -f "${REGISTRY_CONFIG}" ]; then
    echo "Warning: Registry config not found: ${REGISTRY_CONFIG}"
    echo "Extension should be registered in Router registry for E2E tests"
fi

# Check NATS connection
echo "Checking NATS connection: ${NATS_URL}..."
if ! timeout 2 nc -z $(echo "${NATS_URL}" | sed 's|nats://||' | cut -d: -f1) $(echo "${NATS_URL}" | sed 's|nats://||' | cut -d: -f2) 2>/dev/null; then
    echo "Error: Cannot connect to NATS at ${NATS_URL}"
    echo "Start NATS: docker-compose up -d nats"
    exit 1
fi
echo "✓ NATS connection OK"

# Check if extension is registered
if [ -f "${REGISTRY_CONFIG}" ]; then
    REGISTRY_TARGET="${ROUTER_FIXTURES_DIR}/$(basename "${REGISTRY_CONFIG}")"
    if [ ! -f "${REGISTRY_TARGET}" ]; then
        echo "Warning: Extension not registered in Router fixtures"
        echo "Copy registry config: cp ${REGISTRY_CONFIG} ${REGISTRY_TARGET}"
        echo "Then reload Router registry"
    else
        echo "✓ Extension registered in Router fixtures"
    fi
fi

# Start extension in background
echo "Starting extension: ${EXTENSION_ID}"
cd "${EXTENSION_DIR}"

# Check if node_modules exists
if [ ! -d "node_modules" ]; then
    echo "Installing dependencies..."
    npm install
fi

# Start extension
export NATS_URL
EXTENSION_PID=""
(
    npm start > /tmp/extension_${EXTENSION_ID}.log 2>&1 &
    EXTENSION_PID=$!
    echo "Extension started (PID: ${EXTENSION_PID})"
    echo "Logs: /tmp/extension_${EXTENSION_ID}.log"
    
    # Wait a bit for extension to start
    sleep 2
    
    # Run E2E tests
    echo ""
    echo "Running E2E tests..."
    npm run test:e2e
    
    # Stop extension
    if [ -n "${EXTENSION_PID}" ]; then
        kill ${EXTENSION_PID} 2>/dev/null || true
        echo "Extension stopped"
    fi
) || {
    # Cleanup on error
    if [ -n "${EXTENSION_PID}" ]; then
        kill ${EXTENSION_PID} 2>/dev/null || true
    fi
    exit 1
}

echo ""
echo "✅ E2E tests completed"
echo "Extension logs: /tmp/extension_${EXTENSION_ID}.log"

