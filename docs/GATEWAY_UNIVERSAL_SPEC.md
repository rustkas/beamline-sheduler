# Gateway Universal Specification (CP1)

Purpose: define a provider-agnostic, universal Gateway API for routing, messages, policies, flow validation/dry-run, and registry access. No OpenAI-specific endpoints are required; the API must serve any model/provider via unified contracts.

## Principles
- Universal contracts: provider-agnostic DTOs with extensible `metadata`.
- Single source of truth: align with Router/Provider ABI, Flow DSL, and NATS subjects.
- Multi-tenant and audit-friendly: include `tenant_id`, `trace_id`, and idempotency keys.
- Observability-first: tracing and metrics required, SSE optional.

## Correlation & Identity
- `tenant_id`: multi-tenant isolation.
- `trace_id`: end-to-end trace correlation.
- `message_id`: idempotency key; returned by create/publish operations.

## Endpoints

### Messages
- `POST /api/v1/messages` — create and publish a message.
  - Request: `{ tenant_id, message_type, payload, trace_id?, metadata? }`
  - Response: `{ message_id, ack_timestamp_ms, status: 'published' }`
- `GET /api/v1/messages/:messageId` — get stored message.
- `GET /api/v1/messages` — list messages (optional/basic).

### Routing
- `POST /api/v1/routes/decide` — request a route decision for a message.
  - Request: `{ message: MessageDto, policy_id?, context? }`
  - Response: `{ provider_id, reason, priority, expected_latency_ms, expected_cost, metadata? }`
- `GET /api/v1/routes/decide/:messageId` — get cached decision for `message_id`.

### Policies
- `GET /api/v1/policies/:tenantId` — list policies for tenant.
- `GET /api/v1/policies/:tenantId/:policyId` — fetch a policy.
- `POST /api/v1/policies/:tenantId` — create policy.
- `PUT /api/v1/policies/:tenantId/:policyId` — update policy.
- `DELETE /api/v1/policies/:tenantId/:policyId` — delete policy.

### Flows (Low-code)
- `POST /api/v1/flows/validate` — validate Flow DSL with JSON Schema.
- `POST /api/v1/flows/dry-run` — compile and simulate latencies; uses block registry.

### Registry (Plugins/Blocks)
- `GET /api/v1/registry/blocks` — list available block types.
- `GET /api/v1/registry/blocks/:type` — get block manifest (schema and capabilities).
  - Backed by `RegistryService`; manifests include JSON-Schema for inputs/outputs.

### Tenants (Admin)
- `GET /api/v1/tenants` — list tenants (stub/minimal for CP1).
- `GET /api/v1/tenants/:tenantId` — tenant details (stub/minimal for CP1).

### Health & Observability
- `GET /health`, `GET /health/ready`, `GET /health/live` — probes.
- Tracing: inject/extract `trace_id` via headers; initialize OpenTelemetry.
- Metrics: request counters/latency histograms (Prometheus client).
- SSE (optional): generic streaming for async results, provider-agnostic.

## DTOs (Overview)
- `MessageDto`: `{ message_id?, tenant_id, trace_id?, message_type, payload, metadata?, timestamp_ms? }`
- `RouteRequestDto`: `{ message: MessageDto, policy_id?, context? }`
- `RouteDecisionDto`: `{ provider_id, reason, priority, expected_latency_ms, expected_cost, metadata? }`
- `PolicyDto`: universal policy fields: `defaults`, `weights`, `fallback`, `sticky`, `metadata`.
- Flow DTOs: `FlowValidateRequestDto`, `FlowValidateResponseDto`, `FlowDryRun*`.

## Security
- JWT: required in CP1 for basic protection; claims include `tenant_id` and `roles`.
- mTLS between internal services: planned for CP2+ (Envoy/gRPC-Web), not required in CP1.
- Rate limiting: per tenant and endpoint; `429` with `Retry-After`.

## NATS Subject Mapping (Conceptual)
- `messages.create` → publish asynchronous processing requests.
- `routes.decide` → synchronous/async routing decisions.
- Subjects prefixed by `tenant_id` where applicable; see `docs/NATS_SUBJECTS.md`.

## Acceptance Criteria (CP1 Gateway Universal)
1. Endpoints implemented: messages, routes, policies, flows; health.
2. Registry endpoints present (read-only) using `RegistryService`.
3. JWT guard stub and `TracingService` wired (OpenTelemetry init TODO).
4. REST API is defined by runtime controllers and DTOs (no OpenAPI file in CP1).
5. Unit tests pass for services and DTO validation.

## Out of Scope (CP1)
- OpenAI-specific endpoints (`/v1/chat/completions`) — not required.
- Full Envoy/mTLS and gRPC-Web proxy — targeted for CP2+.
- Persistent storage and real Router integration — can use mocks in CP1.
