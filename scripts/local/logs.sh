#!/bin/bash
# View logs for local development services
# Usage: ./scripts/local/logs.sh [service_name]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$PROJECT_ROOT"

if [ $# -eq 0 ]; then
  # Show all logs
  docker compose logs -f
else
  # Show logs for specific service
  docker compose logs -f "$1"
fi

