#!/bin/sh
set -e
GRPC_PORT="${GRPC_PORT:-9000}"
# If real router release exists, prefer it; else run stub health server
if [ -x "/app/bin/beamline_router" ]; then
  exec /app/bin/beamline_router
else
  exec python3 /app/health_server.py
fi

