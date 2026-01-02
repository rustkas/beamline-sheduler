---
version: 1.0
authors:
  - WORKER wrk-2: Architecture/Tech Lead
last_update: 2025-11-05T22:30:00Z
status: approved
rule_version: v10
message_protocol: v1
---

# CP1 Architecture Review Checklist

## Purpose

Brief checklist for architectural review of PR/changes within CP1-ROUTER. Helps prevent CP1 scope expansion and reject "golden" improvements.

> Note  
> This checklist describes the CP1-ROUTER architecture and boundaries as one checkpoint within the broader BeamLine roadmap.  
> The high-level product vision, component map and the CP0–CP8 lifecycle are defined in [docs/BEAMLINE_VISION_AND_ARCHITECTURE.md](cci:7://file:///home/rustkas/aigroup/docs/BEAMLINE_VISION_AND_ARCHITECTURE.md:0:0-0:0).

> Примечание  
> Этот чеклист описывает архитектуру и границы CP1-ROUTER как один из чекпоинтов внутри общей дорожной карты BeamLine.  
> Высокоуровневое видение продукта, карту компонентов и жизненный цикл CP0–CP8 см. в [docs/BEAMLINE_VISION_AND_ARCHITECTURE.md](cci:7://file:///home/rustkas/aigroup/docs/BEAMLINE_VISION_AND_ARCHITECTURE.md:0:0-0:0).

## General Principles

### ✅ Included in CP1

- Gateway REST API endpoints (`/api/v1/messages`, `/api/v1/routes/decide`)
- NATS integration (`beamline.router.v1.decide`)
- Router stub with compatible interface
- Protobuf definitions (`beamline.flow.v1`)
- Basic DTO structures (TypeScript, JSON)
- Minimal request/response validation

### ❌ NOT included in CP1

- Full Router implementation with policies (stub only)
- gRPC integration (optional, not required)
- Frontend implementation (optional)
- Observability (Prometheus/OTel) - **explicitly excluded**
- Complex routing logic (basic stub only)
- Rate limiting (optional for CP1)
- Sticky sessions (optional for CP1)
- Database integration (optional for CP1)
- Metrics collection (optional for CP1)

## Review Checklist

### Module Boundaries

- [ ] **Gateway does not know routing business rules**
  - Gateway does not contain provider selection logic
  - Gateway does not contain routing policies
  - Gateway only transforms HTTP ↔ NATS

- [ ] **Router does not know about HTTP**
  - Router does not handle HTTP requests
  - Router works only with NATS (and optionally gRPC)
  - Router does not contain HTTP-specific code

- [ ] **Frontend is optional**
  - Gateway works without Frontend
  - Frontend may be implemented later
  - No hard dependencies Gateway on Frontend

### Protocols

- [ ] **JSON/HTTP externally**
  - Gateway provides REST API with JSON
  - Clients work through HTTP/REST

- [ ] **Protobuf/NATS internally**
  - Router uses Protobuf for validation (internally)
  - NATS transmits JSON payload
  - gRPC optional for CP1

### Contracts

- [ ] **DTO for `/api/v1/messages`**
  - Request DTO matches specification
  - Response DTO matches specification
  - All required fields present

- [ ] **DTO for `/api/v1/routes/decide`** (if implemented)
  - Request DTO matches specification
  - Response DTO matches specification

- [ ] **Protobuf messages**
  - `Message`, `RouteRequest`, `RouteDecision` match `proto/beamline/flow/v1/flow.proto`
  - Version `v1` used

- [ ] **NATS subjects**
  - Subject `beamline.router.v1.decide` used
  - Message format matches specification
  - Request-Reply pattern applied

### Router as Stub

- [ ] **Interface compatible**
  - Router subscribes to `beamline.router.v1.decide`
  - Router returns correct `RouteDecision` format
  - All fields in response present

- [ ] **Minimal logic**
  - Router may return fixed values
  - Input validation present
  - Basic error handling

- [ ] **Future compatibility**
  - Interface does not change when stub is replaced
  - Gateway works without changes
  - All fields in `RouteDecision` present

### ADR Compliance

- [ ] **BeamLine vision alignment**
  - Architectural changes do not contradict overall product/architecture vision
  - See `docs/BEAMLINE_VISION_AND_ARCHITECTURE.md`

- [ ] **ADR-010 (Target Architecture)**
  - Module boundaries respected
  - Protocols match
  - Responsibilities correctly separated

- [ ] **ADR-004 (Erlang/OTP Router)**
  - Router may be stub
  - Interface compatible
  - gRPC optional

- [ ] **ADR-006 (NATS Inter-Service Communication)**
  - NATS subject used correctly
  - Request-Reply pattern applied
  - Message format matches

- [ ] **CP1_ROUTER_SPEC.md**
  - Minimal requirements met
  - Stub allowed
  - Interface ready for future implementation

### Versioning

- [ ] **ABI versions**
  - Protobuf: `v1` (package `beamline.flow.v1`)
  - NATS: `v1` (subject `beamline.router.v1.decide`)
  - REST API: `v1` (URL `/api/v1/*`)

- [ ] **Naming**
  - Consistent with existing conventions
  - Matches ADR and specifications

### CP1 Limitations

- [ ] **No Observability**
  - No Prometheus metrics
  - No OpenTelemetry tracing
  - No complex logs

- [ ] **No scope expansion**
  - No "golden" improvements
  - No features outside CP1
  - Focus on minimal contract

- [ ] **No references to .trae/rules**
  - Documentation does not reference `.trae/rules`
  - Only `docs/` and `proto/` used

## Violation Examples (Reject)

### ❌ Gateway contains routing logic

```typescript
// BAD: Gateway knows about providers
if (messageType === "chat") {
  provider = "openai";
}
```

**Correct**: Gateway delegates to Router via NATS

### ❌ Router handles HTTP

```erlang
% BAD: Router handles HTTP
handle_http_request(Req) ->
    ...
```

**Correct**: Router works only with NATS

### ❌ Adding Observability

```typescript
// BAD: Prometheus metrics in CP1
prometheus.inc('router_requests_total');
```

**Correct**: Observability excluded from CP1

### ❌ Scope expansion

```erlang
% BAD: Complex routing logic in CP1
apply_complex_routing_policy(Policy) ->
    % Multiple rules, optimizations, etc.
```

**Correct**: Stub with fixed values

## Correct Implementation Examples

### ✅ Gateway delegates to Router

```typescript
// Gateway transforms HTTP → NATS
async function routeMessage(req: MessageRequest): Promise<RouteDecisionResponse> {
  const natsRequest = {
    message: {
      message_id: req.message_id,
      tenant_id: extractTenantId(req),  // from headers
      trace_id: extractTraceId(req),    // from headers
      message_type: req.message_type,
      payload: req.payload,
      metadata: req.metadata
    },
    policy_id: req.policy_id
  };
  
  const response = await nats.request('beamline.router.v1.decide', natsRequest);
  return transformNatsToHttp(response);
}
```

### ✅ Router stub

```erlang
% Router returns fixed decision
handle_route_request(RouteRequest) ->
    #{
        provider_id => "openai",
        reason => "policy",
        priority => 80,
        expected_latency_ms => 500,
        expected_cost => 0.01,
        metadata => #{}
    }.
```

## Usage

When reviewing PR/changes:

1. Go through checklist
2. Mark all items
3. If violations found:
   - Reject changes
   - Point to specific checklist items
   - Suggest fixes

## References

- `docs/archive/dev/CP1_BOUNDARIES_AND_CONTRACTS.md`: Detailed boundaries and contracts
- `docs/ADR/ADR-010-target-architecture.md`: Target architecture
- `docs/ADR/ADR-004-erlang-otp-router.md`: Router decision
- `docs/ADR/ADR-006-nats-inter-service-communication.md`: NATS decision
- `docs/CP1_ROUTER_SPEC.md`: CP1 Router specification
