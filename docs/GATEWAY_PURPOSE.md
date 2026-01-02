# Gateway Purpose

A NestJS-based Gateway acts as the unified entry point between the UI (HTTP/S) and internal services (NATS, gRPC). It normalizes transport, enforces authN/Z, and provides standard REST+SSE endpoints for the application.

## Why it is needed
- Erlang/OTP and C++ CAF services do not expose convenient HTTP APIs out of the box
- The Gateway translates HTTP requests to NATS messages and gRPC calls
- Centralizes authentication, authorization, rate limiting, and observability (trace IDs, correlation IDs)

## Responsibilities
- Protocol bridging: `HTTP  NATS  gRPC`
- Stable REST API for UI and external clients
- Authentication and authorization (API keys, tokens)
- Server-Sent Events (SSE) for real-time updates
- Error normalization and consistent codes

## Integration with UI
- UI calls `POST/GET` REST endpoints under `/api/v1/...`
- Gateway maps endpoints to NATS subjects and/or gRPC methods
- SSE streams expose real-time status updates

## Transport Transformation
```
UI (HTTP)  Gateway (NestJS)  NATS (subjects)  Services (Erlang/CAF)
                                  gRPC where applicable
```

## Standard REST API
- Messages: `POST /api/v1/messages`  `beamline.router.v1.decide`
- Streaming: `GET /api/v1/stream/:message_id`
- Authentication: `POST /api/v1/auth/login`, token refresh

## References
- `docs/GATEWAY_ROUTES.md`  endpoints and subject mapping
- `docs/ROUTING_POLICY.md`  JSON-DSL for routing policies
