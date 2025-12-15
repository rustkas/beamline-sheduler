#!/bin/bash
#
# Run Extension Locally
#
# Usage:
#   ./scripts/run_extension_local.sh <extension_id> [options]
#
# Options:
#   --nats-url <url>    NATS server URL (default: nats://localhost:4222)
#   --timeout <ms>      Extension timeout (default: from package.json)
#
# Examples:
#   ./scripts/run_extension_local.sh my_extension
#   ./scripts/run_extension_local.sh my_extension --nats-url nats://localhost:4222
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
EXTENSIONS_DIR="${PROJECT_ROOT}/tools/extensions"

# Default values
NATS_URL="${NATS_URL:-nats://localhost:4222}"

# Parse arguments
if [ $# -lt 1 ]; then
    echo "Usage: $0 <extension_id> [options]"
    echo ""
    echo "Options:"
    echo "  --nats-url <url>    NATS server URL"
    echo ""
    echo "Examples:"
    echo "  $0 my_extension"
    echo "  $0 my_extension --nats-url nats://localhost:4222"
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

# Check if node_modules exists
if [ ! -d "${EXTENSION_DIR}/node_modules" ]; then
    echo "Installing dependencies..."
    cd "${EXTENSION_DIR}"
    npm install
fi

# Check NATS connection
echo "Checking NATS connection: ${NATS_URL}..."
if ! timeout 2 nc -z $(echo "${NATS_URL}" | sed 's|nats://||' | cut -d: -f1) $(echo "${NATS_URL}" | sed 's|nats://||' | cut -d: -f2) 2>/dev/null; then
    echo "Warning: Cannot connect to NATS at ${NATS_URL}"
    echo "Make sure NATS is running: docker-compose up -d nats"
    echo "Continuing anyway..."
fi

# Run extension
echo "Starting extension: ${EXTENSION_ID}"
echo "NATS URL: ${NATS_URL}"
echo ""

cd "${EXTENSION_DIR}"
export NATS_URL
npm start

