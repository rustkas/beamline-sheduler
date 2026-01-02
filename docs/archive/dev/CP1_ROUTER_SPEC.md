---
version: 2.0
authors:
  - WORKER wrk-9: Documentation & Developer Experience
last_update: 2025-01-27T00:00:00Z
status: approved
rule_version: v10
message_protocol: v1
---

# CP1 Router Specification

**Checkpoint**: CP1-LC  
**Component**: `apps/otp/router` (Erlang/OTP)  
**Status**: ✅ **COMPLETE**  
**Source of Truth**: This document is the authoritative specification for Router CP1 requirements and invariants.

## Purpose

This document serves as the **single source of truth** for Router requirements in CP1-LC (Baseline Router). It defines:

- What Router must do in CP1
- What is explicitly out of CP1 scope (deferred to CP2+)
- Architectural context and contracts
- Functional and non-functional requirements
- Test coverage and acceptance criteria
- Invariants that must not be broken in CP2+ changes

**All other Router CP1 documentation should reference this document, not duplicate it.**

---

## Introduction

### Router Role in System

Router is the central Erlang/OTP orchestrator for NATS message routing in the BeamLine system. In CP1, Router:

- Receives routing requests via NATS (subject `beamline.router.v1.decide`)
- Applies routing policies (JSON-DSL) to select providers
- Returns routing decisions via NATS reply
- Provides observability baseline (structured JSON logging, health checks)
- **Does NOT** expose HTTP endpoints directly (HTTP isolation: NATS/gRPC only)

### CP1 Scope vs CP2+ Scope

**CP1 Includes**:
- Core routing logic with policy enforcement
- NATS integration (request-reply pattern)
- Error handling and retry policies
- Structured JSON logging with PII filtering
- Health endpoint (gRPC health service on port 9000)
- Basic observability (logs, health checks)

**CP1 Explicitly Excludes** (CP2+ features):
- **Idempotency layer** (ETS-based deduplication) → CP2-LC
- **JetStream integration** (durable subscriptions, ACK/NAK) → CP2-LC
- **OpenTelemetry tracing** (distributed tracing with spans) → CP2-LC
- **Tenant validation/ACL** (tenant allowlist, policy registry) → CP2-LC
- **Advanced observability** (Prometheus metrics, Grafana dashboards, alerting) → CP2-LC
- **HTTP endpoints** (Router remains NATS/gRPC only in CP1)

**Reference**: See `docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY_OVERVIEW.md` for CP2+ features.

---

## Architectural Context

### NATS Subjects

Router handles the following NATS subjects in CP1:

- **Primary Subject**: `beamline.router.v1.decide`
  - **Pattern**: Request-Reply
  - **Request**: `RouteRequest` (Proto → NATS JSON)
  - **Response**: `RouteDecision` (Proto → NATS JSON)
  - **Timeout**: 5 seconds (configurable via policy)

**References**:
- `docs/NATS_SUBJECTS.md` - Complete NATS subject registry
- `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` - Proto to NATS mapping (two-level contract architecture)

### Architecture Decision Records (ADR)

Router implementation follows these ADRs:

- **ADR-004**: Erlang/OTP for Router Core (`docs/ADR/ADR-004-erlang-otp-router.md`)
- **ADR-006**: NATS Inter-Service Communication (`docs/ADR/ADR-006-nats-inter-service-communication.md`)
- **ADR-010**: Target Architecture (`docs/ADR/ADR-010-target-architecture.md`)
- **ADR-015**: Router ↔ DevState Integration (`docs/ADR/ADR-015-router-devstate-integration.md`)

### Two-Level Contract Architecture

Router uses a **two-level contract architecture**:

1. **Proto Wire Protocol (ABI)**: Binary protobuf format for gRPC
   - Source of Truth: Generated code (`apps/otp/router/src/flow_pb.erl`)
   - All fields are optional (protobuf v3 semantics)
   - Router enforces required fields at runtime

2. **NATS JSON Payload (Logical)**: JSON format for NATS messages
   - Source of Truth: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`
   - Includes Proto fields + NATS-specific fields (version, request_id, task, constraints, push_assignment)
   - Gateway transforms HTTP → NATS JSON → Router processes → NATS JSON → HTTP

**Reference**: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` for complete mapping.

---

## Functional Requirements CP1

### Message Types

Router accepts and processes the following message types (via `beamline.router.v1.decide`):

- **Flow messages**: Routing requests from Gateway
- **Provider messages**: Provider selection decisions
- **Usage messages**: Usage tracking (basic support in CP1)

**Proto Messages** (from `beamline.flow.v1`):
- `RouteRequest`: Contains `Message`, `policy_id`, `context`
- `RouteDecision`: Contains `provider_id`, `reason`, `priority`, `expected_latency_ms`, `expected_cost`, `metadata`

**Reference**: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` for complete message definitions.

### Message Processing Pipeline (CP1)

**Basic Flow** (CP1 minimum):

1. Router receives NATS request on `beamline.router.v1.decide`
2. Router validates request (required fields: `message`, `policy_id`)
3. Router loads routing policy (from cache or database)
4. Router applies routing algorithm (weighted round-robin, sticky sessions, fallback)
5. Router returns `RouteDecision` via NATS reply
6. Router logs decision (structured JSON with trace_id, run_id, flow_id)

**Error Handling** (CP1):
- Invalid request → Error response via NATS (error code + message)
- Policy not found → Fallback to default policy or error
- NATS unavailable → Error logged, no reply sent
- Internal error → Error logged, error response via NATS

**Retry Policy** (CP1):
- Basic retry on transient failures (NATS connection, policy load)
- No idempotency guarantees (CP2+ feature)
- No duplicate detection (CP2+ feature)

**Reference**: `apps/otp/router/src/router_core.erl` for core routing logic, `apps/otp/router/src/router_error.erl` for error handling.

### Idempotency (CP1 vs CP2+)

**CP1**: Idempotency is **explicitly NOT required**. Router may process duplicate requests multiple times.

**CP2+**: ETS-based idempotency layer with TTL will be added (see `docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY_OVERVIEW.md`).

**Reference**: `docs/archive/dev/ROUTER_CP1_IDEMPOTENCY_SCOPE.md` for explicit CP1 idempotency clarification.

---

## Non-Functional Requirements CP1

### HTTP Isolation

**CRITICAL**: Router does **NOT** expose HTTP endpoints in CP1.

- Router communicates via **NATS only** (subject `beamline.router.v1.decide`)
- Optional gRPC service (not required for CP1, may be used for admin operations)
- Gateway handles HTTP → NATS transformation

**Reference**: `docs/archive/dev/CP1_BOUNDARIES_AND_CONTRACTS.md` for Gateway ↔ Router boundary.

### Logging and Observability (CP1 Baseline)

**Structured JSON Logging**:

- **Format**: JSON with required fields:
  - `timestamp`: ISO 8601 format (UTC)
  - `level`: ERROR, WARN, INFO, DEBUG
  - `component`: "router"
  - `message`: Human-readable message
  - `trace_id`: Trace identifier (when available)
  - `run_id`: Run identifier (when available)
  - `flow_id`: Flow identifier (when available)
  - `context`: Additional structured context (JSON object)

- **PII/Secret Filtering**: Automatic filtering of sensitive data
  - Filtered fields: `password`, `api_key`, `secret`, `token`, `access_token`, `refresh_token`, `authorization`, `credit_card`, `ssn`, `email`, `phone`
  - Replacement: `[REDACTED]`

- **Implementation**: `apps/otp/router/src/router_logger.erl`
- **Test Suite**: `apps/otp/router/test/router_observability_SUITE.erl` (11 test cases)

**Health Endpoint**:

- **Type**: gRPC health service (NOT HTTP `/_health`)
- **Port**: 9000
- **Service**: `grpc.health.v1.Health`
- **Check**: `grpc_health_probe` or gRPC client
- **Status**: `SERVING` (healthy), `NOT_SERVING` (unhealthy)

**Reference**: `docs/OBSERVABILITY_CONVENTIONS.md` for observability requirements.

### Fault Tolerance (CP1)

**Basic Requirements**:

- **No Panic**: Router must not crash on invalid input or transient failures
- **Graceful Shutdown**: Router must handle shutdown signals and complete in-flight requests
- **Error Recovery**: Router must recover from transient failures (NATS reconnection, policy reload)
- **Basic Retry**: Retry on transient failures (NATS connection, policy load)

**Not Required in CP1**:
- Advanced retry policies (exponential backoff, circuit breakers) → CP2+
- Idempotency guarantees → CP2+
- Duplicate detection → CP2+

---

## Invariants and Verification

### What "Router Meets CP1" Means

Router is considered CP1-compliant when:

1. **Functionality**: All core routing logic implemented and tested
   - `router_core.erl`: Routing decisions with policy enforcement
   - `router_error.erl`: Error handling and mapping to gRPC status codes
   - `router_nats_subscriber.erl`: NATS message handling

2. **Contracts**: NATS/Proto contracts verified and consistent
   - All validation scripts passing (`check_proto.sh`, `check_proto_sync.sh`, `check_proto_nats_compatibility.sh`)
   - Two-level contract architecture documented
   - Proto/NATS consistency verified across all documentation

3. **Observability**: Baseline observability operational
   - Structured JSON logging with PII filtering (`router_logger.erl`)
   - Health endpoint (gRPC health service on port 9000)
   - All key CP1 scenarios logged correctly

4. **Test Coverage**: All CP1 test suites passing
   - `router_core_SUITE.erl`: 12 test cases (core routing, error handling, telemetry)
   - `router_error_SUITE.erl`: 10 test cases (error mapping, NATS unavailable, invalid payload, internal error)
   - `router_gateway_contract_smoke_SUITE.erl`: 7 test cases (Gateway↔Router contract validation)
   - `router_observability_SUITE.erl`: 11 test cases (log format, PII filtering, health endpoint)

**Reference**: `docs/CP1_ACCEPTANCE_REPORT.md#router-appsotprouter--cp1-status` for complete CP1 status.

### Key Test Suites

**CP1 Test Suites** (all must pass):

- `router_core_SUITE.erl`: Core routing logic, error handling, telemetry
- `router_error_SUITE.erl`: Error mapping, NATS unavailable, invalid payload, internal error
- `router_gateway_contract_smoke_SUITE.erl`: Gateway↔Router contract validation
- `router_observability_SUITE.erl`: Log format, PII filtering, health endpoint, logging scenarios

**Reference**: `apps/otp/router/test/` for all test suites.

### CP1 Acceptance Reports

**Main Reports**:

- `docs/CP1_ACCEPTANCE_REPORT.md` - Main CP1 acceptance report with Router status section
- `apps/otp/router/docs/archive/dev/CP1_ACCEPTANCE_REPORT.md` - Router-specific CP1 acceptance details
- `docs/archive/dev/ROUTER_CP1_COMPLETE_IMPLEMENTATION_REPORT.md` - Full implementation summary

**Reference**: See "Detailed Reports & Artifacts" section in `docs/CP1_ACCEPTANCE_REPORT.md#router-appsotprouter--cp1-status`.

### Invariants That Must Not Be Broken in CP2+

**ABI/DTO Invariants**:

- **Proto Contract**: Proto message definitions (`RouteRequest`, `RouteDecision`) must remain backward compatible
- **NATS Subject**: Subject `beamline.router.v1.decide` must remain stable (versioning via `v2`, `v3` if needed)
- **NATS JSON Format**: NATS JSON payload format must remain backward compatible (new fields optional)

**Behavioral Invariants**:

- **Error Handling**: Error codes and error response format must remain stable
- **Health Endpoint**: gRPC health service on port 9000 must remain available
- **Logging Format**: Structured JSON log format must remain backward compatible (new fields optional)

**Reference**: `docs/ARCHITECTURE/compatibility-rules.md` for compatibility policy.

---

## References

### Core Documentation

- `docs/NATS_SUBJECTS.md` - NATS subject registry
- `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` - Proto to NATS mapping (two-level contract architecture)
- `docs/ARCHITECTURE/api-registry.md` - API registry with Gateway ↔ Router contracts
- `docs/CP1_ACCEPTANCE_REPORT.md` - Main CP1 acceptance report
- `docs/archive/dev/CP1_BOUNDARIES_AND_CONTRACTS.md` - CP1 boundaries and contracts

### Architecture Decision Records

- `docs/ADR/ADR-004-erlang-otp-router.md` - Erlang/OTP for Router Core
- `docs/ADR/ADR-006-nats-inter-service-communication.md` - NATS Inter-Service Communication
- `docs/ADR/ADR-010-target-architecture.md` - Target Architecture
- `docs/ADR/ADR-015-router-devstate-integration.md` - Router ↔ DevState Integration

### Router-Specific Documentation

- `apps/otp/router/docs/archive/dev/CP1_ACCEPTANCE_REPORT.md` - Router-specific CP1 acceptance details
- `../../../apps/otp/router/docs/dev/ROUTER_CP1_SMOKE_CHECKLIST.md` - Operational checklist for Router CP1 readiness
- `apps/otp/router/docs/API_CONTRACTS.md` - Router API contracts
- `apps/otp/router/docs/OPERATIONAL_GUIDE.md` - Router operational guide

### Implementation References

- `apps/otp/router/src/router_core.erl` - Core routing logic
- `apps/otp/router/src/router_error.erl` - Error handling
- `apps/otp/router/src/router_nats_subscriber.erl` - NATS integration
- `apps/otp/router/src/router_logger.erl` - Structured JSON logging

### Test References

- `apps/otp/router/test/router_core_SUITE.erl` - Core routing tests
- `apps/otp/router/test/router_error_SUITE.erl` - Error handling tests
- `apps/otp/router/test/router_gateway_contract_smoke_SUITE.erl` - Gateway↔Router contract tests
- `apps/otp/router/test/router_observability_SUITE.erl` - Observability tests

---

## Change History

**v2.0 (2025-01-27)**:
- Complete rewrite as single source of truth for Router CP1
- Added architectural context, NATS subjects, ADR references
- Clarified CP1 scope vs CP2+ scope
- Added invariants and verification criteria
- Structured as contract/invariant document

**v1.0 (2025-11-05)**:
- Initial specification (implementation-focused)
