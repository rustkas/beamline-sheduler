#!/usr/bin/env bash
set -euo pipefail

export NATS_URL=${NATS_URL:-"nats://127.0.0.1:4222"}

echo "[smoke] starting NATS via docker-compose"
docker compose up -d nats

echo "[smoke] waiting for NATS to be ready at ${NATS_URL}"
for i in {1..30}; do
  if nc -z 127.0.0.1 4222; then
    break
  fi
  sleep 1
done

echo "[smoke] building worker"
pushd apps/worker >/dev/null
cargo build -q
echo "[smoke] starting worker (in background)"
RUST_LOG=info cargo run -q &
WORKER_PID=$!
popd >/dev/null

echo "[smoke] publishing demo assignment via nats-box"
docker run --rm --network host natsio/nats-box:latest nats pub caf.exec.assign.v1 \
  '{"version":"v1","kind":"exec_assign","data":{"version":"1.0","assignment_id":"a-smoke-1","request_id":"r-smoke-1","tenant_id":"t-smoke","job":{"type":"echo","payload":{"hello":"world","timeout_ms":2000}},"trace_id":"tr-smoke-1"}}'

echo "[smoke] wait 3s for processing"
sleep 3

echo "[smoke] fetch metrics"
curl -s http://127.0.0.1:9091/metrics | head -n 50

echo "[smoke] stopping worker"
kill ${WORKER_PID}
wait || true

echo "[smoke] done"
