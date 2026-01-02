# CP2 Router/Gateway Specification

**Version**: CP2-LC (Baseline)  
**Date**: 2025-01-27  
**Status**: Production Ready  
**rule_version**: v10  
**message_protocol**: v1

## Purpose

This document provides the formal specification for CP2 baseline features in Router and Gateway components. It defines acceptance criteria, migration guide from CP1, and operational requirements.

## Scope

This specification covers:
- **Router (Erlang/OTP)**: CP2 baseline features and capabilities
- **Gateway (NestJS/TypeScript)**: CP2 baseline features and capabilities
- **Integration**: Router ↔ Gateway communication via CP2 features

## CP2 Baseline Definition

CP2 baseline includes all CP1 features plus the following enhancements:

### CP1 Foundation (Included in CP2)
- Router Core: NATS integration, schema validation, retry logic
- Policy Store: PostgreSQL + Mnesia/ETS caching with RBAC
- Rate Limiting: Per-tenant/user sliding window counters
- RBAC System: Role-based access control
- Policy Enforcement: Quota management and validation
- Audit Logging: Complete audit trail
- Sticky Sessions: ETS-based session affinity
- HMAC Chain: Audit trail and state validation

### CP2 New Features

1. **JetStream Integration** ✅
2. **Idempotency Layer** ✅
3. **OpenTelemetry Tracing** ✅
4. **Tenant Validation/ACL** ✅
5. **Admin gRPC Service** ✅
6. **NAK on Errors** ✅
7. **Headers Support** ✅
8. **JetStream Redelivery** ✅

## Router CP2 Specification

### 1. JetStream Integration

**Feature**: Real NATS/JetStream client with durable subscriptions

**Requirements**:
- Durable consumer groups for results and ACKs
- ACK/NAK support with controlled redelivery
- Connection health monitoring and automatic reconnection
- MaxDeliver exhaustion tracking

**Configuration**:
```erlang
{nats_js_enabled, true},
{nats_js_durable_group_results, <<"router-results">>},
{nats_js_durable_group_acks, <<"router-acks">>},
{nats_js_max_deliver, 5},
{nats_js_ack_wait_seconds, 30}
```

**Acceptance Criteria**:
- ✅ Durable subscriptions created on startup
- ✅ Messages acknowledged after successful processing
- ✅ Messages NAKed on validation failures
- ✅ Redelivery occurs within MaxDeliver limits
- ✅ MaxDeliver exhaustion tracked and metrics emitted
- ✅ Connection failures trigger automatic reconnection

**Reference**: `docs/archive/dev/NATS_JETSTREAM_IMPLEMENTATION_REPORT.md`

### 2. Idempotency Layer

**Feature**: ETS-based idempotency checks with TTL

**Requirements**:
- Prevent duplicate processing of results, ACKs, and usage events
- Configurable TTL for idempotency records
- Metrics for duplicate detection

**Configuration**:
```erlang
{idempotency_enabled, true},
{idempotency_ttl_seconds, 3600}  %% 1 hour default
```

**Acceptance Criteria**:
- ✅ Duplicate results detected and skipped
- ✅ Duplicate ACKs detected and skipped
- ✅ Duplicate usage events detected and skipped
- ✅ TTL-based expiration of idempotency records
- ✅ Metrics emitted for duplicates: `router_results_duplicate_total`, `router_acks_duplicate_total`

**Reference**: `docs/archive/dev/CP2_COMPLETE_IMPLEMENTATION_REPORT.md`

### 3. OpenTelemetry Tracing

**Feature**: Distributed tracing with span creation and trace context propagation

**Requirements**:
- Create spans for publish/consume/emit operations
- Propagate trace context across Router ↔ CAF boundaries
- Link trace_id to OpenTelemetry spans

**Configuration**:
```erlang
{tracing_enabled, true}
```

**Acceptance Criteria**:
- ✅ Spans created for all major operations
- ✅ Trace context propagated in headers
- ✅ Trace IDs linked across Router ↔ CAF boundaries
- ✅ OTLP export configured and working

**Reference**: `docs/archive/dev/OPENTELEMETRY_TRACING_IMPLEMENTATION_REPORT.md`

### 4. Tenant Validation/ACL

**Feature**: Tenant allowlist and policy registry validation with audit events

**Requirements**:
- Validate tenant_id in ExecResult and ACK messages
- Emit audit events for unauthorized access
- NAK messages on validation failures

**Configuration**:
```erlang
{tenant_validation_enabled, true}
```

**Acceptance Criteria**:
- ✅ Tenant validation performed on all incoming messages
- ✅ Unauthorized access attempts logged and audited
- ✅ NAK called on validation failures
- ✅ Metrics emitted: `router_results_tenant_rejected_total`, `router_acks_tenant_rejected_total`, `router_tenant_audit_total`

**Reference**: `docs/archive/dev/TENANT_VALIDATION_IMPLEMENTATION_REPORT.md`

### 5. Admin gRPC Service

**Feature**: RouterAdmin service for administrative operations

**Requirements**:
- RouterAdmin gRPC service enabled
- Administrative endpoints for Router management

**Configuration**:
```erlang
{admin_grpc_enabled, true}
```

**Acceptance Criteria**:
- ✅ RouterAdmin service started and accessible
- ✅ Administrative endpoints functional
- ✅ Proper authentication and authorization

**Reference**: `router_admin_grpc.erl`, `router_grpc_sup.erl`

### 6. NAK on Errors

**Feature**: Automatic NAK on validation failures with controlled redelivery

**Requirements**:
- NAK messages on tenant validation failures
- Respect MaxDeliver configuration for controlled redelivery
- Emit redelivery metrics

**Acceptance Criteria**:
- ✅ NAK called on validation failures
- ✅ Redelivery occurs within MaxDeliver limits
- ✅ Metrics emitted: `router_jetstream_redelivery_total`

**Reference**: `router_result_consumer.erl`, `router_ack_consumer.erl`

### 7. Headers Support

**Feature**: Headers in assignments and messages (trace_id, tenant_id, version)

**Requirements**:
- Headers included in all JetStream messages
- Headers formatted as NATS/1.0 block
- Headers extracted and used for trace context propagation

**Acceptance Criteria**:
- ✅ Headers included in ExecAssignment publications
- ✅ Headers extracted from incoming messages
- ✅ Headers used for trace context propagation

**Reference**: `router_nats.erl`, `router_caf_adapter.erl`

### 8. JetStream Redelivery

**Feature**: Redelivery tracking and metrics with MaxDeliver exhaustion detection

**Requirements**:
- Track delivery count per message
- Emit metric when MaxDeliver exhausted
- Support controlled redelivery

**Acceptance Criteria**:
- ✅ Delivery count tracked per message
- ✅ Metric emitted: `router_jetstream_maxdeliver_exhausted_total`
- ✅ Redelivery respects MaxDeliver limits

**Reference**: `router_result_consumer.erl`, `router_ack_consumer.erl`

## Gateway CP2 Specification

### 1. Idempotency Service

**Feature**: In-memory idempotency with TTL

**Requirements**:
- Prevent duplicate request processing
- TTL-based expiration of idempotency records
- Integration with RoutesController

**Acceptance Criteria**:
- ✅ Duplicate requests detected and skipped
- ✅ TTL-based expiration working
- ✅ Idempotency integrated in RoutesController

**Reference**: `apps/gateway/src/common/services/idempotency.service.ts`

### 2. Tracing Service

**Feature**: OpenTelemetry tracing with trace context propagation

**Requirements**:
- Create spans for HTTP requests
- Propagate trace context to Router
- Export traces via OTLP

**Acceptance Criteria**:
- ✅ Spans created for HTTP requests
- ✅ Trace context propagated to Router
- ✅ OTLP export configured (if implemented)

**Reference**: `apps/gateway/src/observability/tracing.service.ts`

### 3. Rate Limiting

**Feature**: Per-tenant rate limiting

**Requirements**:
- Rate limiting per tenant
- Integration with RoutesController

**Acceptance Criteria**:
- ✅ Rate limiting enforced per tenant
- ✅ Rate limit guard functional
- ✅ Rate limit metrics collected

**Reference**: `apps/gateway/src/common/guards/rate-limit.guard.ts`

### 4. Structured Logging

**Feature**: JSON logging with correlation IDs

**Requirements**:
- Structured JSON logs
- Correlation ID propagation
- Integration with all services

**Acceptance Criteria**:
- ✅ JSON logs emitted
- ✅ Correlation IDs included in logs
- ✅ Logs structured and parseable

**Reference**: `apps/gateway/src/common/services/logger.service.ts`

### 5. Metrics Service

**Feature**: Internal metrics collection

**Requirements**:
- Collect internal metrics
- Integration with interceptors

**Acceptance Criteria**:
- ✅ Metrics collected for all operations
- ✅ Metrics accessible via interceptors
- ✅ Prometheus export (if implemented)

**Reference**: `apps/gateway/src/common/services/metrics.service.ts`

## Integration CP2 Specification

### Router ↔ Gateway Communication

**Requirements**:
- Request-Reply pattern via NATS
- Trace context propagation
- Idempotency across boundaries
- Tenant isolation

**Acceptance Criteria**:
- ✅ Request-Reply working via NATS
- ✅ Trace context propagated end-to-end
- ✅ Idempotency maintained across boundaries
- ✅ Tenant isolation enforced

## Acceptance Criteria

### Functional Requirements

1. ✅ All CP2 features enabled by default
2. ✅ CP2 validation suite passes
3. ✅ All E2E tests pass
4. ✅ CP1 smoke tests still pass
5. ✅ No breaking changes from CP1

### Non-Functional Requirements

1. ✅ Documentation complete
2. ✅ Migration guide available
3. ✅ Performance benchmarks met
4. ✅ Security validation passed
5. ✅ Observability operational

### Operational Requirements

1. ✅ Configuration examples work
2. ✅ Troubleshooting guides updated
3. ✅ Monitoring dashboards configured
4. ✅ Alerting rules in place
5. ✅ Health checks functional

## Migration Guide from CP1

### Step 1: Review CP2 Features

1. Review `docs/CP2_ROUTER_GATEWAY_SPEC.md` (this document)
2. Review `docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY_UPDATED.md`
3. Review implementation reports for each CP2 feature

### Step 2: Update Configuration

**Update `apps/otp/router/src/beamline_router.app.src`**:

```erlang
{env, [
    %% CP2 Baseline Features (Enable by Default)
    {idempotency_enabled, true},      % Change from false
    {tracing_enabled, true},          % Change from false
    {tenant_validation_enabled, true}, % Change from false
    {admin_grpc_enabled, true},       % Change from false
    
    %% JetStream Configuration
    {nats_js_enabled, true},
    {nats_js_durable_group_results, <<"router-results">>},
    {nats_js_durable_group_acks, <<"router-acks">>},
    {nats_js_max_deliver, 5},
    {nats_js_ack_wait_seconds, 30},
    
    %% Idempotency Configuration
    {idempotency_ttl_seconds, 3600}  % 1 hour default
]}
```

### Step 3: Verify NATS JetStream

1. Ensure NATS JetStream is enabled and configured
2. Verify durable consumer groups are created
3. Test ACK/NAK functionality
4. Verify redelivery behavior

### Step 4: Configure OpenTelemetry

1. Configure OTLP collector endpoint
2. Verify trace export is working
3. Test trace context propagation

### Step 5: Configure Tenant Validation

1. Populate policy registry with tenant configurations
2. Verify tenant validation is working
3. Test audit event emission

### Step 6: Run Validation Suite

1. Run CP2 validation suite: `scripts/validate_cp2.sh`
2. Run CP1 smoke tests: Verify backward compatibility
3. Run CP2 E2E tests: Verify CP2 features

### Step 7: Update Documentation

1. Review `apps/otp/router/docs/OPERATIONAL_GUIDE.md`
2. Review `docs/CP2_PROVIDER_PREPARATION.md`
3. Update team documentation

### Step 8: Deploy to Staging

1. Deploy CP2-enabled Router/Gateway to staging
2. Monitor metrics and logs
3. Verify all CP2 features operational
4. Run integration tests

### Step 9: Production Deployment

1. Deploy to production with gradual rollout
2. Monitor metrics and alerts
3. Verify performance and reliability
4. Document any issues

## Breaking Changes

**None**: CP2 is backward compatible with CP1. All CP1 features continue to work.

## Deprecations

**None**: No CP1 features are deprecated in CP2.

## Performance Considerations

### Idempotency TTL

- Default TTL: 1 hour (3600 seconds)
- Adjust based on message processing latency
- Monitor `router_results_duplicate_total` for optimal TTL

### JetStream MaxDeliver

- Default: 5 redeliveries
- Adjust based on error recovery requirements
- Monitor `router_jetstream_maxdeliver_exhausted_total` for tuning

### Tracing Overhead

- Tracing adds minimal overhead
- Monitor trace export success rates
- Adjust sampling if needed

## Security Considerations

### Tenant Validation

- Tenant validation is enabled by default in CP2
- Ensure policy registry is properly configured
- Monitor audit events for unauthorized access

### Admin gRPC Service

- Secure admin endpoints with proper authentication
- Monitor admin API usage
- Restrict access to authorized personnel

## Monitoring and Observability

### Key Metrics

**Router**:
- `router_jetstream_redelivery_total`
- `router_jetstream_maxdeliver_exhausted_total`
- `router_results_duplicate_total`
- `router_acks_duplicate_total`
- `router_results_tenant_rejected_total`
- `router_acks_tenant_rejected_total`
- `router_tenant_audit_total`

**Gateway**:
- Internal metrics via MetricsService
- Prometheus export (if implemented)

### Alerting

See `apps/otp/router/docs/PROMETHEUS_ALERTS.md` for complete alerting rules.

### Tracing

- OpenTelemetry spans for all major operations
- Trace context propagation across boundaries
- OTLP export for trace collection

## References

### Implementation Reports

- `docs/archive/dev/CP2_COMPLETE_IMPLEMENTATION_REPORT.md` - Complete CP2 implementation
- `docs/archive/dev/NATS_JETSTREAM_IMPLEMENTATION_REPORT.md` - JetStream implementation
- `docs/archive/dev/TENANT_VALIDATION_IMPLEMENTATION_REPORT.md` - Tenant validation
- `docs/archive/dev/OPENTELEMETRY_TRACING_IMPLEMENTATION_REPORT.md` - Tracing implementation

### Operational Guides

- `apps/otp/router/docs/OPERATIONAL_GUIDE.md` - CP2 operational guide
- `docs/CP2_PROVIDER_PREPARATION.md` - CP2 provider preparation

### Readiness Assessment

- `docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY_UPDATED.md` - CP2 readiness assessment

## Version History

- **v1.0** (2025-01-27): Initial CP2-LC specification

---

**Prepared by**: WORKER wrk-9 (Documentation & Developer Experience)  
**Reviewed by**: Architecture Team  
**Status**: Production Ready

