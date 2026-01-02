# C-Gateway: Realtime (SSE) and Extensions API

This document describes the C-Gateway additions required for Phase 2:
- Realtime Messages stream over Server-Sent Events (SSE)
- Extensions Registry API (register/update/unregister)
- Build/Run instructions and smoke tests (port 8088)

## Overview
- Gateway binary: `apps/c-gateway/build/c-gateway`
- Default port is taken from env `GATEWAY_PORT`. For local runs we use `8088`.
- Added JSON metrics endpoint `GET /_metrics` for UI Dashboard.
- Added SSE endpoint `GET /api/v1/messages/stream?tenant_id={tenant}` for live updates.
- Added Extensions Registry endpoints:
  - `POST /api/v1/registry/blocks/:type/:version`
  - `PUT  /api/v1/registry/blocks/:type/:version`
  - `DELETE /api/v1/registry/blocks/:type/:version`
- JSON Schema validation for `{schema.input, schema.output}` is enforced in C (Draft‑07 subset, strict enough for CP1) with jansson; no Python at runtime required.

## Build & Run

Requirements:
- CMake >= 3.15
- C compiler with C11 support
- jansson library (libjansson)

Build:

```
make -C apps/c-gateway
```

Run on port 8088:

```
GATEWAY_PORT=8088 ./apps/c-gateway/build/c-gateway
```

## Health and Metrics

- `GET /_health` → `{ "status": "ok" }`
- `GET /_metrics` → JSON with:
  - `rps`: requests per second (since start)
  - `latency.p50 / latency.p95`: computed from a ring buffer
  - `error_rate`: errors / total
  - `nats`: "unknown" in CP1 (wire real status from `nats_client_real.c` next)
  - `ts`: epoch seconds

Example:

```
{"rps":0.333,"latency":{"p50":0,"p95":0},"error_rate":0.00000,"nats":"unknown","ts":1763683380}
```

## Realtime: SSE stream

Endpoint:

```
GET /api/v1/messages/stream?tenant_id={tenant}
```

Behavior:
- Responds with `Content-Type: text/event-stream`, `Cache-Control: no-cache`, `Connection: keep-alive`.
- Keeps the connection open (non‑blocking clients pool).
- Events emitted (line-separated per SSE spec):
  - `event: message_created` + `data: { ...message dto json... }`
  - (Optionally later) `message_updated` / `message_deleted` if message CRUD handlers are present.
- In CP1 events are published upon successful `POST /api/v1/routes/decide` (creation path).

UI integration:
- Phoenix UI uses a `MessagesSSE` hook which connects to this endpoint and updates the list live.
- If SSE fails, UI falls back to periodic polling automatically.

## Extensions Registry API

Paths:
- `POST /api/v1/registry/blocks/:type/:version`
- `PUT  /api/v1/registry/blocks/:type/:version`
- `DELETE /api/v1/registry/blocks/:type/:version`

Request body (POST/PUT) example:

```
{
  "type": "demo",
  "version": "1.0.0",
  "schema": {
    "input":  { "$schema": "http://json-schema.org/draft-07/schema#", "type": "object" },
    "output": { "$schema": "http://json-schema.org/draft-07/schema#", "type": "object" }
  },
  "capabilities": ["sync"],
  "metadata": {"desc": "demo block"}
}
```

Validation:
- Strict check (Draft‑07 subset) in C for `type/properties/required/items/anyOf/oneOf/allOf` and basic constraints.
- On invalid schemas: `400 { ok:false, error:{ code:"invalid_schema", ... } }`.

Responses:
- POST/PUT: `200/201` with JSON describing status (created/updated) and block identity.
- DELETE: `200` when unregistered; `404` if already absent.

## Smoke tests (port 8088)

Run Gateway:

```
GATEWAY_PORT=8088 ./apps/c-gateway/build/c-gateway
```

Health & Metrics:

```
curl -sS -i http://localhost:8088/_health
curl -sS http://localhost:8088/_metrics | jq .
```

Extensions:

```
# Register
curl -sS -X POST \
  http://localhost:8088/api/v1/registry/blocks/demo/1.0.0 \
  -H 'Content-Type: application/json' \
  -d '{
    "type":"demo",
    "version":"1.0.0",
    "schema":{
      "input":{"$schema":"http://json-schema.org/draft-07/schema#","type":"object"},
      "output":{"$schema":"http://json-schema.org/draft-07/schema#","type":"object"}
    },
    "capabilities":["sync"],
    "metadata":{"desc":"demo block"}
  }'

# Unregister
curl -sS -X DELETE http://localhost:8088/api/v1/registry/blocks/demo/1.0.0
```

SSE stream:

```
# Terminal A
curl -N http://localhost:8088/api/v1/messages/stream?tenant_id=tenant_dev

# Terminal B (emit event via decide)
curl -sS -X POST http://localhost:8088/api/v1/routes/decide \
  -H 'Content-Type: application/json' \
  -H 'X-Tenant-ID: tenant_dev' \
  -d '{"message":{"message_id":"m1","type":"chat","payload":{"text":"hi"}}}'
```

Expected in Terminal A:

```
event: message_created
data: { ... }
```

## Notes / Next steps
- Wire real NATS status in `/_metrics` from `nats_client_real.c`.
- Optionally emit `message_updated` / `message_deleted` from message PUT/DELETE handlers.
- Increase SSE pool size or switch to epoll/threaded model if needed beyond CP1.

---

## Verification Results (CP1 smoke on :8088)

- Health: `GET /_health` → 200 OK, `{ "status": "ok" }`.
- Metrics JSON: `GET /_metrics` → 200 OK, fields present (`rps`, `latency.p50/p95`, `error_rate`, `nats`, `ts`).
  - In stub build, `nats` shows `"unknown"`/`"stub"`; with real NATS build (`USE_NATS_LIB=ON`) switches to `"connected"/"disconnected"` based on last request.
- SSE stream: `GET /api/v1/messages/stream?tenant_id=tenant_dev` → 200 OK, `text/event-stream`, connection kept open; initial `: connected` comment is sent to flush proxies.
  - Events observed when triggering:
    - `POST /api/v1/routes/decide` (with `X-Tenant-ID`) → `event: message_created`.
    - `PUT /api/v1/messages/:id` (with `X-Tenant-ID`) → `event: message_updated`.
    - `DELETE /api/v1/messages/:id` (with `X-Tenant-ID`) → `event: message_deleted`.
- Extensions API:
  - `POST /api/v1/registry/blocks/demo/1.0.0` → 200/201 OK, strict schema validation (Draft‑07 subset) passes.
  - `DELETE /api/v1/registry/blocks/demo/1.0.0` → 200 OK; repeat delete → 404 (not found).

## Recommendations

- Build profiles
  - Provide make/cmake profile to enable real NATS client easily: `cmake -DUSE_NATS_LIB=ON` (ensure `libnats` present).
  - In stub mode, keep `nats` field set to `"stub"` for clarity.

- SSE robustness
  - Keep simple non‑blocking pool for CP1; for higher fan‑out migrate to `epoll`/select loop or threaded model.
  - Optionally add periodic heartbeat comments (e.g., every 15–30s) if proxies time out idle streams.

- Validation and contracts
  - Registry: already validates `type/version` vs path, `capabilities` (array of strings) and `metadata` (object); extend if new manifest fields appear.
  - Messages PUT: enforced `message_id` match with path when present in body.
  - Require `X-Tenant-ID` for message mutations (PUT/DELETE) — implemented.

- Auth and rate limiting
  - Minimal rate limiting exists for `/api/v1/routes/decide`; extend per‑tenant/per‑endpoint as needed.
  - Auth skeleton is present; integrate actual token/JWT verification when moving beyond CP1.

- UI integration
  - Phoenix hook already auto‑recovers to polling if SSE fails; no action required.
  - When building with real NATS, Dashboard will display `nats: "connected"/"disconnected"`.
