#!/bin/bash
# Stop local development services
# Usage: ./scripts/local/down.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$PROJECT_ROOT"

echo "Stopping Beamline services..."

docker compose down

echo "Services stopped."

