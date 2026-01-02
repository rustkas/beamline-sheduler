---
version: 1.0
authors:
  - WORKER wrk-9: Documentation & Developer Experience
last_update: 2025-01-27T00:00:00Z
status: approved
rule_version: v10
message_protocol: v1
---

# CP2 Readiness: Router + Gateway Overview

**Checkpoint**: CP2-LC  
**Components**: `apps/otp/router` (Erlang/OTP) + Gateway (NestJS/C++)  
**Status**: ✅ **COMPLETE**  
**Source of Truth**: This document is the authoritative specification for Router+Gateway CP2 readiness requirements.

## Purpose

This document serves as the **single source of truth** for Router+Gateway readiness to CP2-LC. It defines:

- What CP2 adds beyond CP1 baseline
- Key CP2 features: JetStream, idempotency, tenant validation, tracing, observability
- Router ↔ Gateway integration requirements for CP2
- Test coverage and acceptance criteria for CP2
- Compatibility guarantees between CP1 and CP2

**All other Router+Gateway CP2 documentation should reference this document, not duplicate it.**

---

## Introduction

### What is CP2 for Router+Gateway?

CP2-LC extends the CP1 baseline with:

- **JetStream Integration**: Durable subscriptions, ACK/NAK, redelivery tracking
- **Idempotency Layer**: ETS-based deduplication with TTL to prevent duplicate processing
- **OpenTelemetry Tracing**: Distributed tracing with span creation and trace context propagation
- **Tenant Validation/ACL**: Tenant allowlist and policy registry validation with audit events
- **NAK on Errors**: Automatic NAK on validation failures with controlled redelivery
- **Headers Support**: Headers in assignments and messages (trace_id, tenant_id, version)
- **Enhanced Observability**: Prometheus metrics, alerting rules, extended logging

### CP2 vs CP1: Key Differences

| Feature | CP1 | CP2 |
|---------|-----|-----|
| **NATS Integration** | Basic pub/sub (request-reply) | JetStream (durable subscriptions, ACK/NAK) |
| **Message Intake** | Basic JSON parsing, minimal validation | Unified validation layer (protobuf + JSON, format validation, tenant/idempotency checks) |
| **Error Handling** | Basic error responses | Deterministic error handling with DLQ, audit trail, standardized error codes |
| **Gateway Routing** | Basic HTTP → NATS mapping | Complete route specification, HTTP → NATS mapping, correlation fields propagation |
| **Rate Limiting** | Not available | Fixed-window rate limiting (CP1), per-endpoint limits, 429 responses with standard headers |
| **Idempotency** | Not required | ETS-based deduplication with TTL |
| **Tracing** | Basic trace_id in logs | OpenTelemetry spans with context propagation |
| **Tenant Validation** | Not required | Tenant allowlist + policy registry validation |
| **Observability** | JSON logs + health endpoint | Prometheus metrics + alerting + extended logs |
| **Headers** | Basic headers | Full header support (trace_id, tenant_id, version) |
| **DLQ Support** | Not available | Configurable DLQ with payload hash, best-effort publication |

**Reference**: `docs/archive/dev/CP1_ROUTER_SPEC.md` for CP1 baseline requirements.

---

## Functional Improvements CP2

### JetStream Integration

**What CP2 Adds**:

- **Durable Subscriptions**: Subscriptions survive Router restarts
- **ACK/NAK Semantics**: Explicit acknowledgment of message processing
- **Redelivery Tracking**: Automatic redelivery on NAK with MaxDeliver exhaustion detection
- **Consumer Groups**: Support for multiple Router instances (load balancing)

**Router Implementation**:

- **Module**: `apps/otp/router/src/router_nats.erl`
- **JetStream Client**: Real NATS/JetStream client with durable subscriptions
- **ACK/NAK Logic**: Automatic ACK on success, NAK on validation failures
- **Redelivery**: Configurable redelivery policy with MaxDeliver limit

**Gateway Integration**:

- Gateway publishes to JetStream stream (not basic NATS pub/sub)
- Gateway receives ACK/NAK responses from Router
- Gateway handles redelivery scenarios

**Test Coverage**:

- `router_jetstream_e2e_SUITE.erl` - JetStream E2E tests with durable subscriptions
- `router_jetstream_redelivery_SUITE.erl` - Redelivery tracking and MaxDeliver exhaustion

**Reference**: 
- `docs/ADR/ADR-011-jetstream-e2e.md` - JetStream architecture decision
- `docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY_JETSTREAM.md` - Detailed JetStream requirements

### Idempotency Layer

**What CP2 Adds**:

- **ETS-Based Deduplication**: In-memory deduplication table with TTL
- **Idempotency Key**: Support for `idempotency_key` in `RouteRequest` (from Gateway HTTP DTO)
- **TTL Management**: Automatic expiration of idempotency entries
- **Duplicate Detection**: Prevents duplicate processing of the same request

**Router Implementation**:

- **Module**: `apps/otp/router/src/router_idempotency.erl`
- **ETS Table**: `router_idempotency` table with TTL
- **Key Format**: `{tenant_id, idempotency_key}` or `{message_id}` (fallback)
- **TTL**: Configurable (default: 1 hour)

**Gateway Integration**:

- Gateway extracts `idempotency_key` from HTTP request header (`X-Idempotency-Key`)
- Gateway includes `idempotency_key` in NATS JSON payload
- Router checks idempotency before processing, returns cached response if duplicate

**Test Coverage**:

- `router_idempotency_SUITE.erl` - Idempotency layer tests
- `router_jetstream_e2e_idempotency_SUITE.erl` - E2E idempotency tests with JetStream

**Reference**: 
- `docs/ADR/ADR-012-idempotency-layer.md` - Idempotency architecture decision
- `docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY_IDEMPOTENCY.md` - Detailed idempotency requirements (if exists)

### Tenant Validation / ACL

**What CP2 Adds**:

- **Tenant Allowlist**: Validation against tenant allowlist (from policy registry)
- **Policy Registry**: Validation of policy_id against policy registry
- **Audit Events**: Audit logging for tenant validation failures
- **Security Invariants**: Tenant isolation and access control

**Router Implementation**:

- **Module**: `apps/otp/router/src/router_tenant_validator.erl`
- **Validation Logic**: Tenant allowlist check + policy registry validation
- **Audit Events**: Structured audit logs for validation failures
- **Error Response**: NAK on validation failure with error details

**Gateway Integration**:

- Gateway extracts `tenant_id` from HTTP request header (`X-Tenant-ID`)
- Gateway includes `tenant_id` in NATS JSON payload
- Router validates tenant before processing, returns NAK if invalid

**Test Coverage**:

- `router_tenant_validation_SUITE.erl` - Tenant validation tests
- `router_tenant_validation_e2e_SUITE.erl` - E2E tenant validation tests

**Reference**: 
- `docs/archive/dev/TENANT_VALIDATION_IMPLEMENTATION_REPORT.md` - Tenant validation implementation details

### NAK on Errors

**What CP2 Adds**:

- **Automatic NAK**: Router sends NAK on validation failures (tenant, policy, message format)
- **Controlled Redelivery**: Redelivery with exponential backoff and MaxDeliver limit
- **Error Details**: NAK includes error code and message for debugging

**Router Implementation**:

- **Module**: `apps/otp/router/src/router_nats.erl` (NAK logic)
- **NAK Conditions**: Validation failures, policy not found, invalid message format
- **Redelivery Policy**: Configurable redelivery with MaxDeliver exhaustion detection

**Gateway Integration**:

- Gateway receives NAK responses from Router
- Gateway handles redelivery scenarios (retry with backoff or fail fast)

**Test Coverage**:

- `router_nak_errors_SUITE.erl` - NAK on errors tests
- `router_jetstream_redelivery_SUITE.erl` - Redelivery tracking tests

### Headers Support

**What CP2 Adds**:

- **Full Header Support**: Headers in assignments and messages (trace_id, tenant_id, version)
- **Header Propagation**: Headers propagated through Router → Provider → Usage chain
- **Header Validation**: Header format validation and sanitization

**Router Implementation**:

- **Module**: `apps/otp/router/src/router_headers.erl`
- **Header Fields**: `trace_id`, `tenant_id`, `version`, `run_id`, `flow_id`, `step_id`
- **Header Propagation**: Headers included in NATS messages to Provider and Usage components

**Gateway Integration**:

- Gateway extracts headers from HTTP request (`X-Trace-ID`, `X-Tenant-ID`, etc.)
- Gateway includes headers in NATS JSON payload
- Router propagates headers to downstream components

**Test Coverage**:

- `router_headers_SUITE.erl` - Headers support tests
- `router_headers_propagation_SUITE.erl` - Header propagation tests

---

## Observability and Tracing in CP2

### OpenTelemetry Tracing

**What CP2 Adds**:

- **Distributed Tracing**: OpenTelemetry spans with trace context propagation
- **Span Creation**: Router creates spans for routing decisions, policy loading, provider selection
- **Trace Context**: Trace context propagated via headers (trace_id, span_id, parent_span_id)
- **Span Attributes**: Rich span attributes (tenant_id, provider_id, policy_id, latency_ms, cost)

**Router Implementation**:

- **Module**: `apps/otp/router/src/router_trace.erl`
- **Span Names**: `beamline.router.decide`, `beamline.router.policy.load`, `beamline.router.provider.select`
- **Trace Context**: Headers (`trace_id`, `span_id`, `parent_span_id`) propagated to Gateway and Provider

**Gateway Integration**:

- Gateway creates initial span for HTTP request
- Gateway propagates trace context to Router via NATS headers
- Router creates child spans for routing decisions
- Gateway receives trace context from Router and closes spans

**Test Coverage**:

- `router_tracing_SUITE.erl` - OpenTelemetry tracing tests
- `router_tracing_e2e_SUITE.erl` - E2E tracing tests with Gateway

**Reference**: 
- `docs/ADR/ADR-014-metrics-tracing.md` - Metrics and tracing architecture decision
- `docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY_OBSERVABILITY.md` - Detailed observability requirements

---

## Extensions – CI Coverage

### Test Suite Classification

#### CP2-LC Required Tests (Main CI)

These test suites are **mandatory** for CP2-LC and run in main CI pipelines:

1. **`router_extensions_pipeline_SUITE`** - Core pipeline functionality
   - Tests: pre/validators/post execution, error handling, fail-open/fail-closed
   - Status: ✅ Required for CP2-LC
   - CI: Runs in main, CP2 validation, Drone, GitLab CI

2. **`router_extensions_e2e_SUITE`** - End-to-end integration
   - Tests: Full pipeline with real NATS, extension services
   - Status: ✅ Required for CP2-LC
   - CI: Runs in main, CP2 validation, Drone, GitLab CI

3. **`router_extensions_security_SUITE`** - Security and abuse prevention
   - Tests: Authorization, payload validation, pipeline depth limits
   - Status: ✅ Required for CP2-LC
   - CI: Runs in main, CP2 validation, Drone, GitLab CI

4. **`router_extension_invoker_telemetry_SUITE`** - Observability
   - Tests: Telemetry events, structured logging, metrics
   - Status: ✅ Required for CP2-LC
   - CI: Runs in main, CP2 validation, Drone, GitLab CI

#### Pre-Release Tests (Extended CI)

These test suites are **optional** for CP2-LC but required for Pre-Release:

1. **`router_extensions_pipeline_load_SUITE`** - Performance and load testing
   - Tests: Latency, throughput, circuit breaker behavior, load profiles
   - Status: ⚠️ Pre-Release only (not blocking for CP2-LC)
   - CI: Runs in extended/load test pipelines, Pre-Release validation
   - Rationale: Load tests are resource-intensive and not required for basic functionality

### CI Configuration

**Main CI (CP2-LC)**:
- GitHub Actions: `router-full-test-suite.yml`, `validate-cp2.yml`
- Drone CI: `router-observability-tests` pipeline
- GitLab CI: `router-observability-tests` job

**Extended CI (Pre-Release)**:
- GitHub Actions: `router-load-tests.yml`
- Drone CI: `extensions-load-tests` step (non-blocking)
- GitLab CI: Load test jobs (non-blocking)

### Documentation Sync Check

**Script**: `scripts/check_extensions_docs_sync.sh`

**Validates**:
- All extension-related documentation files exist
- Documentation links are valid
- Test suites are referenced in CI configuration

**Runs in**:
- Pre-commit hooks (optional)
- CI validation pipelines
- Pre-Release validation

### CI Coverage Summary

| Test Suite | CP2-LC | Pre-Release | CI Location | Status |
|------------|--------|-------------|-------------|--------|
| `router_extensions_pipeline_SUITE` | ✅ Required | ✅ Required | Main CI | ✅ Integrated |
| `router_extensions_e2e_SUITE` | ✅ Required | ✅ Required | Main CI | ✅ Integrated |
| `router_extensions_security_SUITE` | ✅ Required | ✅ Required | Main CI | ✅ Integrated |
| `router_extension_invoker_telemetry_SUITE` | ✅ Required | ✅ Required | Main CI | ✅ Integrated |
| `router_extensions_pipeline_load_SUITE` | ⚠️ Optional | ✅ Required | Extended CI | ✅ Integrated |

**References**:
- `docs/archive/dev/CP2_EXTENSIONS_IMPLEMENTATION_PLAN.md` - Detailed implementation plan
- `docs/archive/dev/EXTENSIONS_CI_INTEGRATION_REPORT.md` - CI integration report
- `scripts/check_extensions_docs_sync.sh` - Documentation sync validation

### Extension Reports Mapping Matrix

**Mapping**: Extension implementation reports → CP2/Pre-Release steps

| CP2/Pre-Release Step | Description | Extension Reports | Status | CP Phase |
|---------------------|-------------|-------------------|--------|----------|
| **Step 4.1** | Extensions Pipeline Verification | `EXTENSIONS_PIPELINE_CHECK_REPORT.md` | ✅ **COMPLETED** (CP2-LC) | CP2-LC |
| **Step 4.2** | Core Pipeline Implementation | `EXTENSIONS_PIPELINE_IMPLEMENTATION_REPORT.md` | ✅ **COMPLETED** (CP2-LC) | CP2-LC |
| **Step 4.3** | Integration Tests | `EXTENSIONS_PIPELINE_IMPLEMENTATION_REPORT.md`<br>`EXTENSIONS_PIPELINE_TESTS_ENHANCEMENT_REPORT.md` | ✅ **COMPLETED** (CP2-LC) | CP2-LC |
| **Step 4.4** | Pipeline Enhancement & Error Format | `EXTENSIONS_PIPELINE_ENHANCEMENT_REPORT.md` | ✅ **COMPLETED** (CP2-LC) | CP2-LC |
| **Registry Implementation** | Extension Registry (PostgreSQL + Mnesia) | `EXTENSION_REGISTRY_IMPLEMENTATION_REPORT.md` | ✅ **COMPLETED** (CP2-LC) | CP2-LC |
| **E2E Integration** | E2E with real extension services | `EXTENSIONS_E2E_IMPLEMENTATION_REPORT.md` | ✅ **COMPLETED** (CP2-LC) | CP2-LC |
| **Contract E2E** | Gateway ↔ Router contract tests | `EXTENSIONS_CONTRACT_E2E_REPORT.md` | ✅ **COMPLETED** (CP2-LC) | CP2-LC |
| **Performance Testing** | Load/performance tests | `EXTENSIONS_PIPELINE_PERF_REPORT.md` | ✅ **COMPLETED** (Pre-Release) | Pre-Release |
| **Chaos/Resilience** | Chaos engineering scenarios | `EXTENSIONS_CHAOS_RESILIENCE_REPORT.md` | ✅ **COMPLETED** (Pre-Release) | Pre-Release |
| **UI Implementation** | Extension inspector UI | `EXTENSIONS_PIPELINE_UI_IMPLEMENTATION_REPORT.md` | ✅ **COMPLETED** (Pre-Release) | Pre-Release |
| **Production Integration** | Production-ready integration | `EXTENSIONS_PIPELINE_PRODUCTION_INTEGRATION_REPORT.md` | 🟡 **PARTIALLY COMPLETE** | Pre-Release |
| **Pipeline Complexity** | Complexity management | `PIPELINE_COMPLEXITY_MANAGEMENT_REPORT.md`<br>`PIPELINE_COMPLEXITY_MANAGEMENT_FINAL.md` | ✅ **COMPLETED** (Pre-Release) | Pre-Release |
| **CI/CD Integration** | CI/CD pipeline integration | `EXTENSIONS_CI_INTEGRATION_REPORT.md` | ✅ **COMPLETED** (CP2-LC) | CP2-LC |
| **Observability** | Telemetry and metrics | `EXTENSION_INVOKER_OBSERVABILITY_REPORT.md`<br>`EXTENSIONS_CP2_OBSERVABILITY_ALIGNMENT_REPORT.md` | ✅ **COMPLETED** (CP2-LC) | CP2-LC |
| **Advanced Features** | Circuit breaker, versioning, load balancing | `EXTENSION_ADVANCED_FEATURES_REPORT.md` | ✅ **COMPLETED** (Pre-Release) | Pre-Release |
| **Routing Strategy** | Multi-tenant routing | `EXTENSION_ROUTING_STRATEGY_IMPLEMENTATION_REPORT.md` | ✅ **COMPLETED** (Pre-Release) | Pre-Release |
| **Developer Experience** | DX tools and templates | `EXTENSION_TEMPLATE_IMPLEMENTATION_REPORT.md`<br>`EXTENSIONS_DEVELOPER_EXPERIENCE_REPORT.md` | ✅ **COMPLETED** (Pre-Release) | Pre-Release |
| **Documentation Sync** | Documentation synchronization | `EXTENSIONS_DOCUMENTATION_SYNC_REPORT.md` | ✅ **COMPLETED** (CP2-LC) | CP2-LC |

**Legend**:
- ✅ **COMPLETED**: Implementation complete and verified
- 🟡 **PARTIALLY COMPLETE**: Implementation in progress or partially complete
- ⚠️ **PRE-RELEASE ONLY**: Not required for CP2-LC, but required for Pre-Release

**CP Phase Classification**:
- **CP2-LC**: Required for CP2-LC checkpoint
- **Pre-Release**: Required for Pre-Release, optional for CP2-LC

**Complete Extension Reports List**:
- See `docs/archive/dev/CP2_EXTENSIONS_IMPLEMENTATION_PLAN.md` for complete list of all extension-related reports and documentation

---

### Metrics and Alerts

**What CP2 Adds**:

- **Prometheus Metrics**: Comprehensive metrics collection (latency, throughput, errors, redelivery)
- **Alert Rules**: Prometheus alert rules for Router health, errors, redelivery exhaustion
- **Metrics Endpoint**: `/metrics` endpoint for Prometheus scraping

**Router Implementation**:

- **Module**: `apps/otp/router/src/router_metrics.erl`
- **Metrics**: `router_decisions_total`, `router_decisions_latency_seconds`, `router_errors_total`, `router_redelivery_total`
- **Labels**: `tenant_id`, `provider_id`, `policy_id`, `error_code`

**Gateway Integration**:

- Gateway exposes `/metrics` endpoint for Prometheus scraping
- Gateway metrics include Router-related metrics (latency, errors, redelivery)

**Alert Rules**:

- **Router Unhealthy**: Router health check fails
- **High Error Rate**: Error rate exceeds threshold
- **Redelivery Exhaustion**: MaxDeliver exhausted for messages
- **High Latency**: P95 latency exceeds threshold

**Reference**: 
- `docs/observability/router-alert-rules.yaml` - Prometheus alert rules
- `docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY_OBSERVABILITY.md` - Detailed observability requirements

### Enhanced Logging

**What CP2 Adds**:

- **Extended Log Format**: Additional fields (run_id, flow_id, step_id, span_id, idempotency_key)
- **Correlation with Gateway**: Logs include Gateway request_id and correlation_id
- **Structured Context**: Rich structured context (tenant_id, provider_id, policy_id, latency_ms, cost)

**Router Implementation**:

- **Module**: `apps/otp/router/src/router_logger.erl` (extended for CP2)
- **Additional Fields**: `run_id`, `flow_id`, `step_id`, `span_id`, `idempotency_key`, `correlation_id`
- **Context Enrichment**: Context enriched with routing decision details (provider_id, policy_id, latency_ms, cost)

**Gateway Integration**:

- Gateway logs include Router correlation_id and request_id
- Gateway and Router logs can be correlated via trace_id and correlation_id

**Test Coverage**:

- `router_observability_SUITE.erl` - Extended logging tests (CP2 fields)
- `router_logging_correlation_SUITE.erl` - Log correlation tests with Gateway

**Reference**: 
- `docs/OBSERVABILITY_CONVENTIONS.md` - Observability conventions
- `docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY_OBSERVABILITY.md` - Detailed observability requirements

---

## Gateway Routing & Rate Limiting (CP2 Enhancement)

### Gateway Routing Specification

**What CP2 Adds**:

- **Complete Route Inventory**: All HTTP routes documented with METHOD + PATH, description, auth requirements, rate limiting
- **HTTP → NATS Mapping**: Formal mapping of HTTP routes to NATS subjects, DTO transformation, correlation fields
- **Error Code Alignment**: Complete error code chain Router Intake → Gateway Error Code → HTTP Status
- **Correlation Fields**: Documented field sources (headers vs body), validation rules, propagation flow

**Gateway Implementation**:

- **Route Specification**: `docs/GATEWAY_ROUTES_SPEC.md` - Complete route inventory and mapping
- **NATS Integration**: `apps/c-gateway/src/nats_client_real.c` - NATS request-reply implementation
- **Error Mapping**: `apps/c-gateway/src/http_server.c:map_router_error_status()` - Router error → HTTP status
- **Correlation Fields**: `apps/c-gateway/src/http_server.c:build_route_request_json()` - Field extraction and propagation

**Router Integration**:

- Router receives Gateway requests via NATS subject `beamline.router.v1.decide`
- Router validates intake messages using unified validator
- Router returns error responses with Gateway-compatible error codes
- Router includes `intake_error_code` in error response for debugging

**Test Coverage**:

- `tests/integration/gateway-router-error-handling.test.ts` - Gateway ↔ Router error handling integration tests
- `docs/archive/dev/GATEWAY_ROUTER_INTEGRATION_TESTS_REPORT.md` - Integration tests report

**Reference**: 
- `docs/GATEWAY_ROUTES_SPEC.md` - Complete Gateway routing specification
- `docs/ARCHITECTURE/api-registry.md` - REST API DTO specifications

### Rate Limiting (CP1 Level, CP2 Enhancement)

**What CP2 Adds**:

- **Fixed-Window Rate Limiting**: In-memory fixed-window algorithm (CP1 level)
- **Per-Endpoint Limits**: Configurable limits per endpoint (`/api/v1/routes/decide`, `/api/v1/messages`, registry endpoints)
- **429 Responses**: Standard 429 responses with rate limit headers (`X-RateLimit-*`, `Retry-After`)
- **Rate Limit Metrics**: Tracking of rate limit hits and exceeds per endpoint

**Gateway Implementation**:

- **Algorithm**: Fixed-window rate limiting (CP1 level, in-memory)
- **Configuration**: Environment variables (`GATEWAY_RATE_LIMIT_*`)
- **Headers**: `X-RateLimit-Limit`, `X-RateLimit-Remaining`, `X-RateLimit-Reset`, `Retry-After`
- **Metrics**: `rl_total_hits`, `rl_total_exceeded`, `rl_exceeded_by_endpoint[]`

**Limits** (defaults, configurable via environment variables):
- `/api/v1/routes/decide`: 50 req/min
- `/api/v1/messages`: 100 req/min
- `/api/v1/registry/blocks/*`: 200 req/min
- Global: 1000 req/min

**Future Enhancements** (CP2+):
- Per-tenant quotas with Redis-backed sliding window
- Admin introspection endpoints
- Extended metrics and trace annotations

**Scope Clarification**:
- Полный, распределённый rate limiting является **CP2 feature** и не входит в **CP1**. В CP1 действует минимальный фиксированный лимит в Gateway.
- CP2 добавляет пер‑tenant/пер‑policy слайдинговые окна (Redis), согласованные с Policy DSL.

**Reference**: 
- `docs/GATEWAY_RATE_LIMITING.md` - Rate limiting specification
- `docs/GATEWAY_ROUTES_SPEC.md` - Rate limiting enforcement points
 - `docs/archive/dev/CP2_ROUTER_PLAN.md` - Policy DSL rate limit (CP2) и общий бэклог

---

## Test Coverage and Acceptance Criteria CP2

### CP2 Test Suites

**Router CP2 Test Suites** (all must pass):

- `router_jetstream_e2e_SUITE.erl` - JetStream E2E tests with durable subscriptions
- `router_jetstream_redelivery_SUITE.erl` - Redelivery tracking and MaxDeliver exhaustion
- `router_idempotency_SUITE.erl` - Idempotency layer tests
- `router_jetstream_e2e_idempotency_SUITE.erl` - E2E idempotency tests with JetStream
- `router_tenant_validation_SUITE.erl` - Tenant validation tests
- `router_tenant_validation_e2e_SUITE.erl` - E2E tenant validation tests
- `router_nak_errors_SUITE.erl` - NAK on errors tests
- `router_headers_SUITE.erl` - Headers support tests
- `router_tracing_SUITE.erl` - OpenTelemetry tracing tests
- `router_tracing_e2e_SUITE.erl` - E2E tracing tests with Gateway
- `router_metrics_SUITE.erl` - Prometheus metrics tests
- `router_logging_correlation_SUITE.erl` - Log correlation tests with Gateway

**Gateway CP2 Test Suites** (all must pass):

- `gateway_jetstream_e2e_SUITE.erl` - Gateway ↔ Router JetStream E2E tests
- `gateway_idempotency_SUITE.erl` - Gateway idempotency key handling tests
- `gateway_tenant_validation_SUITE.erl` - Gateway tenant validation tests
- `gateway_tracing_SUITE.erl` - Gateway tracing tests
- `gateway_metrics_SUITE.erl` - Gateway metrics tests

**Reference**: `apps/otp/router/test/` and Gateway test directories for all test suites.

### CP2 Acceptance Reports

**Main Reports**:

- `docs/CP1_ACCEPTANCE_REPORT.md#next-cp-cp2-lc-router--planned-scope` - CP2-LC status section
- `../../../apps/otp/router/docs/archive/dev_reports/CP2_COMPLETE_IMPLEMENTATION_REPORT.md` - Router CP2 implementation details
- `docs/archive/dev/JETSTREAM_E2E_IDEMPOTENCY_TESTS_REPORT.md` - JetStream E2E idempotency tests report
- `docs/archive/dev/TENANT_VALIDATION_IMPLEMENTATION_REPORT.md` - Tenant validation implementation report

**Reference**: See "Detailed Reports & Artifacts" section in CP2 acceptance reports.

### E2E/Integration Tests

**CP2 E2E Test Scenarios**:

1. **NATS/JetStream E2E**: 
   - Durable subscriptions survive Router restarts
   - ACK/NAK semantics work correctly
   - Redelivery tracking and MaxDeliver exhaustion

2. **HTTP ↔ NATS E2E via Gateway**:
   - Gateway HTTP request → NATS JetStream → Router → NATS reply → Gateway HTTP response
   - Idempotency key handling end-to-end
   - Tenant validation end-to-end
   - Tracing context propagation end-to-end

3. **Observability / Tracing / Metrics E2E**:
   - OpenTelemetry spans created and propagated correctly
   - Prometheus metrics collected and exposed
   - Logs correlated via trace_id and correlation_id

**Reference**: `apps/otp/router/test/router_jetstream_e2e_SUITE.erl` and Gateway E2E test suites.

---

## Boundaries and Compatibility

### What is NOT in CP2 Scope

**Explicitly Excluded from CP2**:

- **Full UI Integration**: UI components and frontend integration (deferred to CP3+)
- **Advanced Observability**: Grafana dashboards, advanced alerting (deferred to Pre-Release phase)
- **Proto Source Files Restoration**: Restore `.proto` files (deferred to CP2+)
- **CP2+ Proto Fields**: Add CP2+ fields (run_id, flow_id, step_id, idempotency_key, span_id) to Proto (deferred to CP2+)

**Reference**: `docs/CP1_ACCEPTANCE_REPORT.md#next-cp-cp2-lc-router--planned-scope` for CP2+ deferred items.

### Backward Compatibility Guarantees

**ABI/DTO Compatibility**:

- **Proto Contract**: Proto message definitions remain backward compatible (new fields optional)
- **NATS Subject**: Subject `beamline.router.v1.decide` remains stable (no breaking changes)
- **NATS JSON Format**: NATS JSON payload format remains backward compatible (new fields optional)

**Behavioral Compatibility**:

- **Error Handling**: Error codes and error response format remain stable
- **Health Endpoint**: gRPC health service on port 9000 remains available
- **Logging Format**: Structured JSON log format remains backward compatible (new fields optional)

**CP1 → CP2 Migration**:

- CP1 Router can be upgraded to CP2 without breaking changes
- CP1 Gateway can work with CP2 Router (backward compatible)
- CP2 Router can work with CP1 Gateway (forward compatible, with CP2 features disabled)

**Reference**: `docs/ARCHITECTURE/compatibility-rules.md` for compatibility policy.

---

## References

### Core Documentation

- `docs/archive/dev/CP1_ROUTER_SPEC.md` - CP1 Router specification (baseline)
- `docs/CP1_ACCEPTANCE_REPORT.md` - CP1 acceptance report with CP2 status
- `docs/archive/dev/CP2_ROUTER_PLAN.md` - CP2 Router plan
- `docs/archive/dev/CP2_ROUTER_GATEWAY_SPEC.md` - CP2 Router/Gateway specification

### Architecture Decision Records

- `docs/ADR/ADR-011-jetstream-e2e.md` - JetStream E2E architecture decision
- `docs/ADR/ADR-012-idempotency-layer.md` - Idempotency layer architecture decision
- `docs/ADR/ADR-014-metrics-tracing.md` - Metrics and tracing architecture decision

### Detailed CP2 Readiness Documents

- `docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY_JETSTREAM.md` - Detailed JetStream requirements
- `docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY_OBSERVABILITY.md` - Detailed observability requirements

### Implementation Reports

- `../../../apps/otp/router/docs/archive/dev_reports/CP2_COMPLETE_IMPLEMENTATION_REPORT.md` - Router CP2 implementation details
- `docs/archive/dev/ROUTER_STAGE2_CP_SUMMARY.md` - Router Message Intake Stage 2 CP-level summary
- `docs/archive/dev/JETSTREAM_E2E_IDEMPOTENCY_TESTS_REPORT.md` - JetStream E2E idempotency tests report
- `docs/archive/dev/TENANT_VALIDATION_IMPLEMENTATION_REPORT.md` - Tenant validation implementation report

### Operational Guides

- `apps/otp/router/docs/OPERATIONAL_GUIDE.md` - Router operational guide (CP2-LC features)
- `docs/OPS_RUNBOOK_ROUTER_INTAKE.md` - Router intake operations runbook (DLQ, validation errors, NATS failures, backpressure)
- `docs/OPS_RUNBOOK_GATEWAY_RATE_LIMITING.md` - Gateway rate limiting operations runbook (configuration, mass 429, abuse detection)
- `apps/otp/router/docs/EXTENSIONS_RUNBOOK.md` - Extension Registry and Extensions operations runbook
- `docs/observability/router-alert-rules.yaml` - Prometheus alert rules

### Production Readiness (CP3+)

- `docs/ARCHITECTURE/SLI_SLO_ROUTER_GATEWAY.md` - **Formal SLO/SLI definitions** for Router intake and Gateway (CP3+/Release readiness)
  - 8 SLI definitions (Router intake: success rate, error rate, latency, DLQ; Gateway: HTTP success rate, error rate, latency, rate limiting)
  - 8 SLO targets with measurement windows and error budgets
  - Test coverage mapping (load, chaos, e2e, overload tests)
  - Pre-release gates (SLO verification, metrics verification, alert rules verification, documentation verification)
  - Acceptance criteria for release
  - Error budget management
  - Monitoring dashboard specifications

---

## Change History

**v1.0 (2025-01-27)**:
- Initial version as single source of truth for Router+Gateway CP2 readiness
- Comprehensive overview of CP2 features: JetStream, idempotency, tenant validation, tracing, observability
- Test coverage and acceptance criteria
- Compatibility guarantees
