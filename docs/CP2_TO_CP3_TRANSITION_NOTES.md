# CP2 → CP3 Transition Notes

**Version**: 1.0  
**Date**: 2025-01-27  
**Control Point**: CP2-LC → CP3  
**Purpose**: Document CP2 feature status and carry-over items for CP3

---

## Executive Summary

This document captures the transition state from CP2-LC to CP3, documenting:
- **Status in CP2**: What was completed, partially completed, or E2E-covered
- **Carry-over to CP3**: What will be extended or improved in CP3
- **Known constraints**: Limitations that need to be addressed in CP3

**Goal**: Avoid re-analysis and re-formulation of requirements in CP3 by documenting current state and future work.

---

## CP2 Feature Blocks

### 1. JetStream Durability & Redelivery

**Status in CP2**: ✅ **COMPLETE** - E2E covered

- ✅ Durable consumer configured
- ✅ ACK/NAK semantics verified
- ✅ MaxDeliver enforced with backoff schedule
- ✅ DLQ policy defined and implemented
- ✅ Metrics: `router_jetstream_ack_total`, `router_redelivery_total`, `router_dlq_total`
- ✅ E2E tests: `router_jetstream_e2e_SUITE.erl` (PASS)
- ✅ Integration: `router_cp2_features_e2e_SUITE.erl` (PASS)

**Carry-over to CP3**:
- ⚠️ **Parallel pipeline integration**: JetStream integration with parallel execution pipeline (see `docs/archive/dev/EXTENSIONS_PARALLEL_PIPELINE_ADR.md` if exists)
- ⚠️ **Advanced redelivery policies**: Per-subject redelivery configuration, custom backoff strategies
- ⚠️ **DLQ management UI/API**: Admin interface for DLQ inspection and replay

**Known constraints to address in CP3**:
- DLQ messages require manual inspection (no admin API for DLQ management)
- Redelivery backoff is fixed per subject (no per-message customization)
- No automatic DLQ replay mechanism

**Key Artifacts**:
- Module: `apps/otp/router/src/router_jetstream.erl`
- Tests: `apps/otp/router/test/router_jetstream_e2e_SUITE.erl`
- Documentation: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`

---

### 2. Idempotency Layer

**Status in CP2**: ✅ **COMPLETE** - E2E covered

- ✅ ETS cache with TTL
- ✅ Duplicate suppression by `idempotency_key`
- ✅ Keys propagation across Router → Gateway
- ✅ Eviction policy tested
- ✅ Metrics: `router_idem_hits_total`, `router_idem_miss_total`
- ✅ E2E tests: `router_idem_SUITE.erl` (PASS)
- ✅ Integration: `router_cp2_features_e2e_SUITE.erl` (PASS)

**Carry-over to CP3**:
- ⚠️ **Distributed idempotency**: Redis-backed idempotency for multi-instance deployments
- ⚠️ **Idempotency key generation**: Automatic key generation from request hash
- ⚠️ **Idempotency window configuration**: Per-tenant TTL configuration

**Known constraints to address in CP3**:
- ETS-based storage is single-instance only (no distributed idempotency)
- TTL is global (no per-tenant configuration)
- Manual key generation required (no automatic key derivation)

**Key Artifacts**:
- Module: `apps/otp/router/src/router_idem.erl` ⚠️ **Note**: Actual module name is `router_idem.erl` (not `router_idempotency.erl`)
- Tests: `apps/otp/router/test/router_idem_SUITE.erl`
- Documentation: `docs/archive/dev/IDEMPOTENCY_IMPLEMENTATION_REPORT.md`

---

### 3. ACL (Tenant Validation)

**Status in CP2**: ✅ **COMPLETE** - E2E covered

- ✅ Tenant allow/deny lists
- ✅ Role permissions enforced
- ✅ Audit entries created on deny
- ✅ Policy updates are atomic and logged
- ✅ Metrics: `router_acl_allowed_total`, `router_acl_denied_total`, `router_tenant_audit_total`
- ✅ E2E tests: `router_acl_SUITE.erl` (PASS)
- ✅ Integration: `router_cp2_features_e2e_SUITE.erl` (PASS)

**Carry-over to CP3**:
- ⚠️ **Global scope ACL**: Multi-tenant global ACL rules (deferred from CP2)
- ⚠️ **Multi-level checks**: Global → tenant → policy hierarchy
- ⚠️ **Dynamic policy updates**: Hot-reload of ACL policies without restart

**Known constraints to address in CP3**:
- ACL is per-tenant only (no global scope rules)
- Policy updates require restart (no hot-reload)
- No hierarchical permission model (global → tenant → policy)

**Key Artifacts**:
- Modules: `apps/otp/router/src/router_acl.erl`, `apps/otp/router/src/router_tenant_validator.erl`
- Tests: `apps/otp/router/test/router_acl_SUITE.erl`
- Documentation: `docs/CP2_CHECKLIST.md` (ACL section)

---

### 4. Circuit Breaker (Policy DSL)

**Status in CP2**: ✅ **COMPLETE** - E2E covered

- ✅ State machine (Closed/Open/Half-Open) with ETS storage
- ✅ CB check before provider selection (fail-fast when open)
- ✅ Integration with retry/backoff and fallbacks
- ✅ Configuration parsing from policy JSON
- ✅ Metrics: `router_circuit_breaker_events_total`, `router_circuit_breaker_state_transitions_total`
- ✅ E2E tests: `router_circuit_breaker_SUITE.erl` (PASS)
- ✅ Integration: `router_cp2_features_e2e_SUITE.erl` (PASS)

**Carry-over to CP3**:
- ⚠️ **Advanced CB policies**: Per-provider CB configuration, custom failure thresholds
- ⚠️ **CB metrics dashboard**: Real-time CB state visualization
- ⚠️ **CB manual override**: Admin API for manual CB state transitions

**Known constraints to address in CP3**:
- CB configuration is per-policy only (no per-provider override)
- No admin API for manual CB state management
- CB metrics are basic (no detailed breakdown by provider)

**Key Artifacts**:
- Module: `apps/otp/router/src/router_circuit_breaker.erl`
- Tests: `apps/otp/router/test/router_circuit_breaker_SUITE.erl`
- Documentation: `docs/ROUTING_POLICY.md`, `docs/archive/dev/CIRCUIT_BREAKER_DESIGN.md`

---

### 5. Rate Limiting (Policy DSL)

**Status in CP2**: ✅ **COMPLETE** - E2E covered

- ✅ Token bucket algorithm
- ✅ Per-tenant rate limiting
- ✅ Policy DSL integration
- ✅ Metrics: `router_rate_limit_allowed_total`, `router_rate_limit_rejected_total`
- ✅ E2E tests: `router_rate_limit_store_SUITE.erl` (PASS)
- ✅ Integration: `router_cp2_features_e2e_SUITE.erl` (PASS)

**Carry-over to CP3**:
- ⚠️ **Per-policy rate limiting**: Rate limits at policy level (currently only per-tenant)
- ⚠️ **Distributed rate limiting**: Redis-backed rate limiting for multi-instance deployments
- ⚠️ **Advanced rate limit policies**: Burst limits, sliding window, custom algorithms

**Known constraints to address in CP3**:
- Rate limiting is per-tenant only (no per-policy limits)
- ETS-based storage is single-instance (no distributed rate limiting)
- Token bucket algorithm is fixed (no custom algorithms)

**Key Artifacts**:
- Module: `apps/otp/router/src/router_rate_limit_store.erl`
- Tests: `apps/otp/router/test/router_rate_limit_store_SUITE.erl`
- Documentation: `docs/archive/dev/RATE_LIMIT_IMPLEMENTATION_AUDIT.md`

---

### 6. Admin gRPC

**Status in CP2**: ✅ **COMPLETE** - E2E covered

- ✅ gRPC RouterAdmin service
- ✅ Health, status, RBAC endpoints
- ✅ NATS-based admin API
- ✅ Integration tests: `router_admin_grpc_integration_SUITE.erl` (PASS)

**Carry-over to CP3**:
- ⚠️ **Admin UI**: Web-based admin interface for Router management
- ⚠️ **Advanced admin operations**: Policy hot-reload, CB manual override, DLQ management
- ⚠️ **Admin audit log**: Comprehensive audit trail for admin operations

**Known constraints to address in CP3**:
- Admin API is gRPC/NATS only (no web UI)
- Limited admin operations (no policy hot-reload, no CB override)
- No comprehensive admin audit log

**Key Artifacts**:
- Modules: `apps/otp/router/src/router_admin_grpc.erl`, `apps/otp/router/src/router_admin_nats.erl`
- Tests: `apps/otp/router/test/router_admin_grpc_integration_SUITE.erl`
- Documentation: `docs/GATEWAY_ADMIN_GRPC_STATUS.md`

---

### 7. Headers Propagation

**Status in CP2**: ✅ **COMPLETE** - Partially E2E covered

- ✅ Header extraction in Gateway (`X-Tenant-ID`, `X-Trace-ID`, `traceparent`)
- ✅ Headers propagation to Router via JSON
- ✅ Headers handling in Router consumers
- ✅ Integration tests: `router_headers_propagation_e2e_SUITE.erl` (PASS)

**Carry-over to CP3**:
- ⚠️ **Full E2E propagation tests**: REST → NATS → CAF → Usage (deferred from CP2)
- ⚠️ **Cross-component span linkage**: Full OpenTelemetry trace linkage across all components
- ⚠️ **Header validation**: Strict header format validation and error handling

**Known constraints to address in CP3**:
- Full E2E propagation not fully tested (REST → NATS → CAF → Usage)
- Cross-component span linkage deferred (partial implementation)
- Header validation is basic (no strict format checking)

**Key Artifacts**:
- Gateway: `apps/c-gateway/src/http_server.c` (header extraction, propagation)
- Router: `apps/otp/router/src/router_decide_consumer.erl`, `apps/otp/router/src/router_result_consumer.erl`
- Tests: `apps/otp/router/test/router_headers_propagation_e2e_SUITE.erl`
- Documentation: `docs/GATEWAY_HEADERS_PROPAGATION.md`

---

### 8. Observability Expansion

**Status in CP2**: ✅ **COMPLETE** (CP1 baseline + CP2 minimal OTel) - Partially E2E covered

- ✅ Minimal OpenTelemetry spans (CP2 wrk-3)
- ✅ Prometheus metrics implemented (Wave 1)
- ✅ Structured JSON logging (CP1 baseline)
- ✅ Metrics: All CP2 metrics exported via `/metrics` endpoint
- ✅ E2E tests: `router_observability_otel_spans_SUITE.erl` (PASS)

**Carry-over to CP3**:
- ⚠️ **Full OpenTelemetry integration**: Complete OTel spans for all operations, trace context propagation
- ⚠️ **Dashboards and alerts**: Grafana dashboards, Prometheus alert rules (deferred to release infra)
- ⚠️ **Advanced observability**: Custom metrics, detailed breakdowns, performance profiling

**Known constraints to address in CP3**:
- Dashboards and alerts deferred (metrics available but no dashboards)
- OTel spans are minimal (not all operations have spans)
- No performance profiling or detailed breakdowns

**Key Artifacts**:
- Modules: `apps/otp/router/src/router_tracing.erl`, `apps/otp/router/src/router_observability.erl`, `apps/otp/router/src/router_prometheus.erl`
- Tests: `apps/otp/router/test/router_observability_otel_spans_SUITE.erl`
- Documentation: `docs/OBSERVABILITY_METRICS_MONITORING_GUIDE.md`

---

### 9. CI DevState Gates

**Status in CP2**: ✅ **COMPLETE** (CP2) / ⚠️ **DEFERRED** (Pre-commit/pre-push hooks)

- ✅ DevState verify in CI workflows
- ✅ JSON schema validation
- ✅ Artifact checksums verification
- ✅ HMAC chain verification via DevState API
- ✅ CI workflows: `.github/workflows/ci.yml`, `.github/workflows/router-full-test-suite.yml`

**Carry-over to CP3**:
- ⚠️ **Pre-commit/pre-push hooks**: Local DevState verification before commit/push (documentation exists, manual setup required)
- ⚠️ **Enhanced DevState integration**: Automatic state updates, conflict resolution

**Known constraints to address in CP3**:
- Pre-commit/pre-push hooks not implemented (CI gates are active)
- Manual DevState setup required (no automatic IDE integration)

**Key Artifacts**:
- Scripts: `devstate-tools/devstate/scripts/devstate_verify.sh`, `devstate-tools/devstate/scripts/devstate_export.sh`
- CI: `.github/workflows/ci.yml` (DevState verify step)
- Documentation: `devstate-tools/devstate/docs/IDE_INTEGRATION.md`

---

## CP2 → CP3 Summary Matrix

| Feature | CP2 Status | CP3 Carry-over | Priority |
|---------|------------|----------------|----------|
| **JetStream** | ✅ COMPLETE | Parallel pipeline, advanced redelivery, DLQ management | Medium |
| **Idempotency** | ✅ COMPLETE | Distributed idempotency, auto key generation | High |
| **ACL** | ✅ COMPLETE | Global scope, multi-level checks, hot-reload | Medium |
| **Circuit Breaker** | ✅ COMPLETE | Advanced policies, CB dashboard, manual override | Medium |
| **Rate Limiting** | ✅ COMPLETE | Per-policy limits, distributed RL, advanced algorithms | Medium |
| **Admin gRPC** | ✅ COMPLETE | Admin UI, advanced operations, audit log | Low |
| **Headers Propagation** | ✅ COMPLETE | Full E2E tests, cross-component linkage | High |
| **Observability** | ✅ COMPLETE (minimal) | Full OTel, dashboards, alerts | High |
| **CI DevState** | ✅ COMPLETE (CI) | Pre-commit hooks, IDE integration | Low |

---

## CP3 Planning Notes

### High Priority (CP3 Core)

1. **Full OpenTelemetry Integration**: Complete OTel spans for all operations, trace context propagation
2. **Distributed Idempotency**: Redis-backed idempotency for multi-instance deployments
3. **Full E2E Headers Propagation**: REST → NATS → CAF → Usage with cross-component span linkage

### Medium Priority (CP3 Enhancements)

1. **Parallel Pipeline Integration**: JetStream with parallel execution pipeline
2. **Advanced ACL**: Global scope, multi-level checks, hot-reload
3. **Advanced CB/RateLimit**: Per-provider CB, per-policy RL, distributed RL

### Low Priority (CP3 Nice-to-Have)

1. **Admin UI**: Web-based admin interface
2. **Pre-commit Hooks**: Local DevState verification

---

## References

### CP2 Documentation

- `docs/CP2_CHECKLIST.md` - Complete CP2 checklist
- `docs/CP2_READINESS_SUMMARY.md` - CP2 readiness summary
- `docs/archive/dev/CP2_ACCEPTANCE_REPORT.md` - CP2 acceptance report
- `docs/archive/dev/CP2_CHECKLIST_GAP_REPORT.md` - Gap analysis

### CP3 Planning

- `docs/CP3_DELTA_CHECKLIST.md` - CP3 delta checklist (if exists)
- `docs/archive/dev/EXTENSIONS_PARALLEL_PIPELINE_ADR.md` - Parallel pipeline ADR (if exists)

---

**WORKER**: wrk-4 (Docs/Architecture)  
**Control Point**: CP2-LC → CP3  
**Status**: ✅ **COMPLETE** - Transition notes documented

