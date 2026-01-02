# CP2-LC Router Plan

**Date**: 2025-01-27  
**Status**: 📋 Planning  
**Checkpoint**: CP2-LC (Baseline)  
**Component**: Router (apps/otp/router)  
**Version**: 1.0

## Executive Summary

This document defines the CP2-LC plan for Router, outlining the scope, acceptance criteria, and implementation status. CP2-LC builds upon CP1 foundation with enhanced reliability, observability, and operational capabilities.

## CP2-LC Scope for Router

### CP2-Core (Required for CP2-LC)

**Status**: ✅ **COMPLETE** - All CP2-core features are implemented and production-ready.

#### 1. JetStream Integration ✅

**Feature**: Real NATS/JetStream client with durable subscriptions

**Invariants**:
- Router guarantees at-least-once delivery for results and ACKs via JetStream durable subscriptions
- Router guarantees MaxDeliver exhaustion detection and metrics emission
- Router guarantees automatic reconnection on NATS connection failures
- Router guarantees ACK/NAK operations respect JetStream redelivery policy

**Requirements**:
- Durable consumer groups for results and ACKs
- ACK/NAK support with controlled redelivery
- Connection health monitoring and automatic reconnection
- MaxDeliver exhaustion tracking

**Implementation Status**: ✅ **COMPLETE**
- **Module**: `router_nats.erl` - Real NATS/JetStream client
- **Consumers**: `router_result_consumer.erl`, `router_ack_consumer.erl` - Durable subscriptions
- **Configuration**: `nats_js_enabled`, `nats_js_durable_group_*`, `nats_js_max_deliver`
- **ADR**: `docs/ADR/ADR-011-jetstream-e2e.md`
- **Reference**: `docs/archive/dev/NATS_JETSTREAM_IMPLEMENTATION_REPORT.md`

**Tests**:
- **E2E Tests**: `router_jetstream_e2e_SUITE.erl` - Full JetStream integration tests
- **Unit Tests**: `router_nats_SUITE.erl` - NATS client unit tests

**Acceptance Criteria**:
- ✅ Durable subscriptions work with consumer groups
- ✅ ACK/NAK operations work correctly
- ✅ MaxDeliver exhaustion is tracked and metrics emitted
- ✅ E2E tests pass (`router_jetstream_e2e_SUITE.erl`)

#### 2. Idempotency Layer ✅

**Feature**: ETS-based idempotency checks with TTL to prevent duplicate processing

**Invariants**:
- Router guarantees duplicate detection for results, ACKs, and usage events within TTL window
- Router guarantees fail-open behavior (idempotency errors do not block message processing)
- Router guarantees TTL-based expiration of idempotency keys
- Router guarantees metrics emission for all duplicate detections

**Requirements**:
- Idempotency checks for results, ACKs, and usage events
- Configurable TTL (default: 1 hour)
- Fail-open strategy (does not block on errors)
- Metrics for duplicate detection

**Implementation Status**: ✅ **COMPLETE**
- **Module**: `router_idempotency.erl` - Idempotency gen_server with ETS table
- **Usage**: `router_result_consumer.erl`, `router_ack_consumer.erl`
- **Configuration**: `idempotency_enabled`, `idempotency_ttl_seconds`
- **ADR**: `docs/ADR/ADR-012-idempotency-layer.md`
- **Reference**: `../../../apps/otp/router/docs/archive/dev_reports/CP2_COMPLETE_IMPLEMENTATION_REPORT.md`

**Tests**:
- **Unit Tests**: `router_idempotency_SUITE.erl` - Idempotency logic tests
- **Integration Tests**: `router_jetstream_e2e_SUITE.erl` - Idempotency under load tests

**Acceptance Criteria**:
- ✅ Idempotency checks prevent duplicate processing
- ✅ TTL-based expiration works correctly
- ✅ Metrics emitted for duplicates (`router_results_duplicate_total`, `router_acks_duplicate_total`)
- ✅ Fail-open strategy implemented (errors don't block processing)

#### 3. Tenant Validation/ACL ✅

**Feature**: Tenant allowlist and policy registry validation with audit events

**Invariants**:
- Router guarantees validation of `tenant_id` in `ExecResult` and `ACK` messages against policy registry
- Router guarantees audit event emission for all unauthorized access attempts
- Router guarantees NAK on validation failures for controlled redelivery
- Router guarantees metrics emission for all tenant rejections

**Requirements**:
- Validates `tenant_id` in `ExecResult` and `ACK` messages against policy registry
- Emits audit events for unauthorized access attempts
- NAK messages on validation failures for controlled redelivery
- Metrics for tenant rejections

**Implementation Status**: ✅ **COMPLETE**
- **Module**: `router_tenant_validator.erl` - Tenant validation logic
- **Usage**: `router_result_consumer.erl`, `router_ack_consumer.erl`
- **Configuration**: `tenant_validation_enabled`, `result_ack_allowed_tenants`
- **Reference**: `docs/archive/dev/TENANT_VALIDATION_IMPLEMENTATION_REPORT.md`

**Tests**:
- **Integration Tests**: `router_jetstream_e2e_SUITE.erl` - Tenant validation E2E tests
- **Unit Tests**: `router_tenant_validator_SUITE.erl` - Validation logic tests

**Acceptance Criteria**:
- ✅ Tenant validation works against policy registry
- ✅ Audit events emitted for unauthorized access
- ✅ NAK called on validation failures
- ✅ Metrics emitted (`router_results_tenant_rejected_total`, `router_acks_tenant_rejected_total`)

#### 4. OpenTelemetry Tracing ✅

**Feature**: Distributed tracing with span creation and trace context propagation

**Invariants**:
- Router guarantees span creation for all key operations (routing, NATS publish, result processing)
- Router guarantees trace context propagation via headers (`trace_id`, `tenant_id`, `version`)
- Router guarantees trace continuity across Router→Gateway→Provider boundaries
- Router guarantees span attributes include tenant_id, operation type, and error status

**Requirements**:
- Span creation for key operations (routing, NATS publish, result processing)
- Trace context propagation via headers (`trace_id`, `tenant_id`, `version`)
- Integration with OpenTelemetry SDK

**Implementation Status**: ✅ **COMPLETE**
- **Modules**: `router_core.erl`, `router_nats.erl`, `router_result_consumer.erl`
- **Configuration**: `telemetry_enabled`, OpenTelemetry exporter configuration
- **Reference**: `docs/OBSERVABILITY.md`

**Tests**:
- **Integration Tests**: `router_observability_SUITE.erl` - Tracing integration tests
- **E2E Tests**: `router_jetstream_e2e_SUITE.erl` - Trace context propagation tests

**Acceptance Criteria**:
- ✅ Spans created for key operations
- ✅ Trace context propagated via headers
- ✅ Traces visible in OpenTelemetry collector

#### 5. NAK on Errors ✅

**Feature**: Automatic NAK on validation failures with controlled redelivery

**Invariants**:
- Router guarantees NAK on all validation failures (tenant, schema, format)
- Router guarantees MaxDeliver exhaustion detection and metrics emission
- Router guarantees controlled redelivery via JetStream redelivery policy
- Router guarantees error metrics emission for all NAK operations

**Requirements**:
- NAK called on tenant validation failures
- NAK respects MaxDeliver configuration
- Metrics emitted for redelivery

**Implementation Status**: ✅ **COMPLETE**
- **Modules**: `router_result_consumer.erl`, `router_ack_consumer.erl`
- **Reference**: `../../../apps/otp/router/docs/archive/dev_reports/CP2_COMPLETE_IMPLEMENTATION_REPORT.md`

**Tests**:
- **Integration Tests**: `router_jetstream_e2e_SUITE.erl` - NAK and redelivery E2E tests
- **Unit Tests**: Error handling tests in consumer modules

**Acceptance Criteria**:
- ✅ NAK called on validation failures
- ✅ MaxDeliver exhaustion detected and metrics emitted
- ✅ Redelivery metrics emitted (`router_jetstream_redelivery_total`)

#### 6. Headers Support ✅

**Feature**: Headers in assignments and messages (trace_id, tenant_id, version)

**Invariants**:
- Router guarantees headers formatted as NATS/1.0 block in all published messages
- Router guarantees headers extraction from incoming messages and propagation to downstream
- Router guarantees headers priority over payload for `trace_id`, `tenant_id`, `version` fields
- Router guarantees header-based trace context propagation across Router→Provider boundaries

**Requirements**:
- Headers formatted as NATS/1.0 block
- Headers extracted from messages and propagated
- Headers priority over payload for trace_id, tenant_id, version

**Implementation Status**: ✅ **COMPLETE**
- **Modules**: `router_nats.erl`, `router_caf_adapter.erl`
- **Reference**: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`

**Tests**:
- **Integration Tests**: `router_jetstream_e2e_SUITE.erl` - Headers propagation E2E tests
- **Unit Tests**: Header extraction and formatting tests in NATS modules

**Acceptance Criteria**:
- ✅ Headers published with assignments
- ✅ Headers extracted from messages
- ✅ Headers priority over payload works correctly

### CP2+ / Optional Improvements (Deferred to CP3/Pre-Release)

**Status**: 📅 **PLANNED** - These features are documented but not required for CP2-LC.

#### 1. Advanced Observability (CP2+)

**Features**:
- Grafana dashboards
- Prometheus alerting rules
- k6 load testing scripts
- Runbook documentation

**Status**: 📅 **DEFERRED** to Pre-Release phase (see `docs/.trae/milestones/prerelease-observability.json`)

#### 2. Proto Source Files Restoration (CP2+)

**Feature**: Restore Proto source files (`proto/beamline/flow/v1/flow.proto`, `proto/beamline/provider/v1/provider.proto`)

**Status**: 📅 **DEFERRED** to CP2+ (see `docs/archive/dev/ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md`)

**Note**: Generated code (`flow_pb.erl`, `flow_pb.hrl`) is current source of truth.

#### 3. CP2+ Fields in Proto (CP2+)

**Feature**: Add CP2+ fields to Proto messages (e.g., `run_id`, `flow_id`, `step_id`, `idempotency_key`, `span_id`)

**Status**: 📅 **DEFERRED** to CP2+ (backward compatible additions)

**Reference**: `docs/archive/dev/ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md`

#### 4. Policy Admin/Ops Tooling (CP2+)

**Feature**: Admin API/CLI for policy management (view, validate, dry-run, safe updates with versioning and audit)

**Status**: 📅 **DEFERRED** to CP2+ (see `docs/archive/dev/POLICY_ADMIN_TOOLING_SPEC.md`)

**Implementation Tasks**: See `docs/archive/dev/CP2_POLICY_IMPLEMENTATION_TASKS.md` - 7 tasks

**Components**:
- **gRPC Admin API**: Extend existing `router_admin_grpc.erl` with policy management RPCs
- **HTTP Admin API**: REST endpoints for policy management (new or extend existing)
- **CLI Tool**: `router-policy` CLI for policy operations
- **Policy Versioning**: Versioned policy updates with rollback capability
- **Policy Validation**: Schema validation and business logic validation
- **Dry-Run Engine**: Test policy decisions without affecting production

**Reference**: `docs/archive/dev/POLICY_ADMIN_TOOLING_SPEC.md` - Complete specification

#### 5. Circuit Breaker (CP2+)

**Feature**: Per-provider circuit breaker to prevent cascading failures

**Status**: 🎯 **SELECTED AS FIRST CP2 FEATURE** (see `docs/archive/dev/CP2_FIRST_FEATURE_SELECTION.md`)

**Implementation Tasks**: See `docs/archive/dev/CP2_POLICY_IMPLEMENTATION_TASKS.md` - 7 tasks

**Implementation Checklist**: See `docs/archive/dev/CP2_FIRST_FEATURE_SELECTION.md` - Detailed checklist with module order and readiness criteria

**Rationale for Selection**:
- ✅ Lower complexity (state machine vs multi-level rate limiting)
- ✅ No Gateway dependencies (Router-internal, no coordination needed)
- ✅ Isolated implementation (can be implemented independently)
- ✅ Lower risk (fewer integration points, simpler testing)
- ✅ Foundation for Rate Limit (patterns can inform rate limit design)

**Components**:
- **State Machine**: `router_circuit_breaker.erl` - Circuit breaker state management
- **Metrics**: `router_circuit_breaker_metrics.erl` - Error tracking and rate calculation
- **Integration**: `router_policy_applier.erl` - Circuit breaker integration with policy applier
- **Configuration**: Policy JSON DSL with `circuit_breaker` block

**Estimated Timeline**: 2-3 weeks (10-14 days)

**Reference**: 
- `docs/archive/dev/CIRCUIT_BREAKER_DESIGN.md` - Complete design
- `docs/archive/dev/CP2_FIRST_FEATURE_SELECTION.md` - Feature selection and implementation checklist

#### 6. Rate Limit (CP2+)

**Feature**: Multi-level rate limiting (global, tenant, policy) with Token Bucket algorithm

**Status**: 📅 **DEFERRED** to CP2+ (see `docs/archive/dev/RATE_LIMIT_POLICY_DSL_DESIGN.md`)

**Implementation Tasks**: See `docs/archive/dev/CP2_POLICY_IMPLEMENTATION_TASKS.md` - 6 tasks

**Components**:
- **Storage**: `router_rate_limit_store.erl` - Token Bucket implementation
- **Integration**: `router_policy_applier.erl` - Rate limit check before provider selection
- **Gateway Coordination**: `router_rate_limit_gateway.erl` - Coordination with Gateway rate limiting
- **Configuration**: Policy JSON DSL with `rate_limit` block

**Reference**: 
- `docs/archive/dev/RATE_LIMIT_POLICY_DSL_DESIGN.md` - Complete design
- `docs/archive/dev/RATE_LIMIT_BOUNDARIES_ROUTER_VS_GATEWAY.md` - **Router vs Gateway boundaries** (defines responsibilities)

#### 7. Timeout and Health Check (CP2+)

**Feature**: Per-policy timeout and proactive health checks for providers

**Status**: 📅 **DEFERRED** to CP2+ (see `docs/archive/dev/TIMEOUT_HEALTH_CHECK_DESIGN.md`)

**Implementation Tasks**: See `docs/archive/dev/CP2_POLICY_IMPLEMENTATION_TASKS.md` - 7 tasks

**Components**:
- **Timeout Manager**: `router_timeout_manager.erl` - Timeout configuration and enforcement
- **Health Check**: `router_health_check.erl` - Health check state tracking and scheduling
- **Integration**: `router_policy_applier.erl` - Timeout and health check integration
- **Configuration**: Policy JSON DSL with `timeout_ms` and `health_check` blocks

**Reference**: `docs/archive/dev/TIMEOUT_HEALTH_CHECK_DESIGN.md` - Complete design

#### 8. Policy Performance Testing (CP2+)

**Feature**: Performance and load testing for policy engine

**Status**: 📅 **DEFERRED** to CP2+ (see `docs/archive/dev/POLICY_PERFORMANCE_PLAN.md`)

**Implementation Tasks**: See `docs/archive/dev/CP2_POLICY_IMPLEMENTATION_TASKS.md` - 4 tasks

**Components**:
- **Load Test Suites**: `router_policy_applier_load_SUITE.erl`, `router_policy_nats_load_SUITE.erl`
- **Metrics Collection**: `router_policy_metrics.erl` - Performance metrics
- **Test Scenarios**: 8 scenarios covering various policy configurations
- **Performance Targets**: Latency (P50/P95/P99), Throughput (QPS), Memory

**Reference**: `docs/archive/dev/POLICY_PERFORMANCE_PLAN.md` - Complete plan

## High-Level CP2-LC Criteria for Router

### Functional Criteria

1. **JetStream E2E**: ✅ All JetStream E2E tests pass (`router_jetstream_e2e_SUITE.erl`)
2. **Idempotent Delivery**: ✅ Idempotency layer prevents duplicate processing under load
3. **Tenant Validation**: ✅ Tenant validation works with policy registry and audit events
4. **Tracing**: ✅ Distributed tracing works across main routes (routing, NATS publish, result processing)
5. **Error Handling**: ✅ NAK on errors works with controlled redelivery and MaxDeliver exhaustion detection

### Operational Criteria

1. **Configuration**: ✅ All CP2 features configurable via application config
2. **Metrics**: ✅ All required metrics emitted (redelivery, duplicates, tenant rejections, MaxDeliver exhaustion)
3. **Health Checks**: ✅ Health endpoint works (gRPC health service on port 9000)
4. **Logging**: ✅ Structured JSON logging with PII filtering (CP1 baseline maintained)

### Test Coverage Criteria

1. **Unit Tests**: ✅ Core modules have unit tests
2. **Integration Tests**: ✅ E2E tests for JetStream, idempotency, tenant validation
3. **Load Tests**: ✅ Load tests verify idempotency under load
4. **Contract Tests**: ✅ Gateway↔Router contract tests pass

## ADR Alignment

### Existing ADRs

1. **ADR-004: Erlang/OTP for Router Core** ✅
   - **Status**: Accepted
   - **CP2 Alignment**: Router core architecture remains Erlang/OTP
   - **CP2 Updates**: None required (foundational ADR)

2. **ADR-011: JetStream E2E with Durable Subscriptions** ✅
   - **Status**: Accepted
   - **CP2 Alignment**: JetStream integration is CP2-core feature
   - **CP2 Updates**: ✅ Already includes CP2 rollout section (lines 173-274)

3. **ADR-012: Idempotency Layer** ✅
   - **Status**: Accepted
   - **CP2 Alignment**: Idempotency is CP2-core feature
   - **CP2 Updates**: ✅ Already states implementation in CP2 (line 115: "for CP1" should be "for CP2")

### ADR Updates Needed

**ADR-012**: Minor clarification needed:
- Line 115: "Why not chosen: ETS provides sufficient performance and simplicity for CP1"
- Should be: "Why not chosen: ETS provides sufficient performance and simplicity for CP2-LC"

**Action**: Update ADR-012 to clarify CP2-LC scope (idempotency is CP2 feature, not CP1).

## Roadmap Alignment

**Current ROADMAP**: `docs/archive/dev/BEAMLINE_ROADMAP_AND_MAPPINGS.md`

**CP2-LC Router Section**: Needs explicit block with:
- List of CP2-core features (JetStream, Idempotency, Tenant Validation, Tracing, NAK, Headers)
- Status: ✅ COMPLETE
- Links to ADRs and implementation reports

**Action**: Add "CP2-LC: Router" section to ROADMAP.

## Release Process Alignment

**Current RELEASE_PROCESS**: `docs/RELEASE_PROCESS.md`

**CP2-LC Router Release Criteria**: Needs explicit section with:
- Test requirements (JetStream E2E, idempotency, tenant validation)
- Documentation requirements (CP2 reports, ADR updates)
- Observability requirements (metrics, tracing, health checks)

**Action**: Add "CP2-LC Router release criteria" section to RELEASE_PROCESS.

## Implementation Status Summary

### CP2-Core Features

| Feature | Status | Module | ADR | Tests |
|---------|--------|--------|-----|-------|
| JetStream Integration | ✅ COMPLETE | `router_nats.erl` | ADR-011 | `router_jetstream_e2e_SUITE.erl` |
| Idempotency Layer | ✅ COMPLETE | `router_idempotency.erl` | ADR-012 | Unit + Integration |
| Tenant Validation/ACL | ✅ COMPLETE | `router_tenant_validator.erl` | - | Integration |
| OpenTelemetry Tracing | ✅ COMPLETE | `router_core.erl`, `router_nats.erl` | - | Integration |
| NAK on Errors | ✅ COMPLETE | `router_result_consumer.erl` | ADR-011 | Integration |
| Headers Support | ✅ COMPLETE | `router_nats.erl`, `router_caf_adapter.erl` | ADR-011 | Integration |

### CP2+ / Optional Features

| Feature | Status | Target Phase | Reference |
|---------|--------|--------------|-----------|
| Advanced Observability | 📅 DEFERRED | Pre-Release | `prerelease-observability.json` |
| Proto Source Files | 📅 DEFERRED | CP2+ | `ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md` |
| CP2+ Proto Fields | 📅 DEFERRED | CP2+ | `ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md` |

### Policy DSL Enhancements (CP2)

**Reference**: `docs/archive/dev/POLICY_DSL_SPEC_VS_IMPL_GAP.md`

**CP2 Enhancements** (from gap analysis):
1. **Future extension fields** - timeout_ms, retry в Policy (per-policy override)
   - Priority: Medium
   - Impact: Per-policy override для extension timeout/retry
   
2. **Circuit breaker в Policy DSL** - circuit breaker configuration
   - Priority: Medium
   - Impact: Улучшение reliability через circuit breaker
   
3. **Rate limit в Policy DSL** - per-policy rate limiting
   - Priority: Medium
   - Impact: Per-policy rate limiting (сейчас только per-tenant)
   
4. **Per-policy timeout** - timeout configuration в Policy DSL
   - Priority: Medium
   - Impact: Per-policy timeout override
   
5. **Provider priority (separate from weights)** - provider priority field
   - Priority: Low
   - Impact: Приоритет провайдеров независимо от weights
   
6. **Health check в Policy DSL** - health check configuration
 - Priority: Low
 - Impact: Per-policy health check configuration

### Backlog: Rate Limiting (CP2 Feature)

**Scope**:
- Per-tenant и per-policy rate limiting с Redis-backed sliding window.
- Gateway: распределённые счётчики, стандартные 429 заголовки и тело.
- Router: policy-aware ограничения (Policy DSL), маппинг в решения.
- Админ эндпоинты: просмотр использования и управление overrides.
- Наблюдаемость: метрики (`gateway_rate_limit_*`, `router_rate_limit_*`), трейс‑атрибуты.

**Acceptance Criteria**:
- Включено через фичефлаги, CP1 базис не ломается.
- Redis‑бэкенд и фолбэк в память при недоступности Redis.
- Заголовки `Retry-After`, `X-RateLimit-*` возвращаются детерминированно.
- Policy DSL поддерживает per-policy rate limit, валидируется и применяется.
- Метрики и трейс‑атрибуты экспонируются и покрыты тестами.

**References**:
- `docs/GATEWAY_RATE_LIMITING.md`
- `docs/GATEWAY_ROUTES_SPEC.md`
- `docs/archive/dev/RATE_LIMIT_BOUNDARIES_ROUTER_VS_GATEWAY.md` - **Router vs Gateway boundaries** (defines responsibilities)
- `docs/archive/dev/POLICY_DSL_SPEC_VS_IMPL_GAP.md`

## Next Steps

1. **Update ADR-012**: Clarify CP2-LC scope (idempotency is CP2 feature)
2. **Update ROADMAP**: Add explicit "CP2-LC: Router" section
3. **Update RELEASE_PROCESS**: Add "CP2-LC Router release criteria" section
4. **Update CP1_ACCEPTANCE_REPORT**: Add "Next CP: CP2-LC (Router) — planned scope" section
5. **Create CP2_ACCEPTANCE_REPORT**: Add Router CP2-LC section with completion status

## Proto Changes Policy

**CRITICAL**: Proto wire-level changes (fields X, Y, Z) are **only** executed after `current_cp` transitions to `CP2-LC` in `.trae/state.json`.

**Proto Changes Scope**:
- **CP2.1**: Restore Proto source files (`proto/beamline/flow/v1/flow.proto`, `proto/beamline/provider/v1/provider.proto`)
- **CP2.2**: Add CP2+ fields to Proto messages (e.g., `run_id`, `flow_id`, `step_id`, `idempotency_key`, `span_id`)
- **CP2.3**: Validate ABI compatibility and backward compatibility

**Detailed Instructions**: See `docs/archive/dev/ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md` for step-by-step procedures:
- Proto file restoration procedures
- CP2+ field addition with ABI compatibility checks
- Version gates and breaking change detection
- Migration procedures and backward compatibility verification

**Version Gate**: All CP2+ Proto actions require:
- ✅ `current_cp >= CP2-LC` in `.trae/state.json`
- ✅ `buf breaking` validation passes
- ✅ ABI compatibility verified

**Note**: CP2-core features (JetStream, Idempotency, Tenant Validation, Tracing, NAK, Headers) are implemented **without** Proto changes. Proto changes are deferred to CP2+ phase as documented in this plan.

## References

- **CP2 Specification**: `docs/archive/dev/CP2_ROUTER_GATEWAY_SPEC.md`
- **CP2 Implementation Report**: `../../../apps/otp/router/docs/archive/dev_reports/CP2_COMPLETE_IMPLEMENTATION_REPORT.md`
- **CP2 Improvements Summary**: `../../../apps/otp/router/docs/archive/dev_reports/CP2_IMPROVEMENTS_SUMMARY.md`
- **ADR-004**: `docs/ADR/ADR-004-erlang-otp-router.md`
- **ADR-011**: `docs/ADR/ADR-011-jetstream-e2e.md`
- **ADR-012**: `docs/ADR/ADR-012-idempotency-layer.md`
- **Operational Guide**: `apps/otp/router/docs/OPERATIONAL_GUIDE.md`
- **Proto/NATS CP2 Plan**: `docs/archive/dev/ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md` - Detailed step-by-step instructions for Proto changes
