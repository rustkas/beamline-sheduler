# CP1 Baseline vs CP2+ Features

This document defines the minimal CP1 baseline components and CP2+ features with explicit feature flags.

## CP1 Baseline (Required Components)

The following components are **mandatory** for CP1 and always started:

### Core Infrastructure

1. **router_nats** (`apps/otp/router/src/router_nats.erl`)
   - NATS connection and messaging interface
   - Basic publish/subscribe operations
   - **Status**: Always started (CP1 baseline)

2. **router_nats_subscriber** (`apps/otp/router/src/router_nats_subscriber.erl`)
   - Subscribes to `beamline.router.v1.decide` for routing requests
   - Handles DecideRequest/DecideResponse
   - **Status**: Always started (CP1 baseline)

3. **router_result_consumer** (`apps/otp/router/src/router_result_consumer.erl`)
   - Subscribes to `caf.exec.result.v1` for execution results
   - Processes ExecResult messages
   - **Status**: Always started (CP1 baseline)

4. **router_core** / **router_decider** (`apps/otp/router/src/router_core.erl`, `router_decider.erl`)
   - Basic routing decision logic
   - Minimal algorithm stub (weights/sticky/fallback)
   - **Status**: Always available (CP1 baseline)

5. **router_telemetry_handler** (`apps/otp/router/src/router_telemetry_handler.erl`)
   - Basic telemetry event handling
   - Simple in-memory aggregation (no Prometheus/OTel)
   - **Status**: Always started (CP1 baseline)

6. **router_logger** (`apps/otp/router/src/router_logger.erl`)
   - Basic structured JSON logging
   - Log levels: ERROR, WARN, INFO, DEBUG
   - **Status**: Always available (CP1 baseline)

### Policy Enforcement (CP1 Phase 3)

7. **router_rbac** (`apps/otp/router/src/router_rbac.erl`)
   - Role-Based Access Control (admin, operator, viewer)
   - **Status**: Always started (CP1 baseline, can be disabled via `rbac_enabled`)

8. **router_rate_limiter** (`apps/otp/router/src/router_rate_limiter.erl`)
   - Per-tenant/user rate limiting
   - **Status**: Always started (CP1 baseline)

9. **router_grpc** (`apps/otp/router/src/router_grpc.erl`)
   - Basic gRPC Router.Decide service
   - **Status**: Started if `grpc_enabled = true` (CP1 baseline)

### Configuration

**Default CP1 Configuration** (`apps/otp/router/src/beamline_router.app.src`):
- `grpc_enabled`: `true` (can be disabled for testing)
- `rbac_enabled`: `true` (can be disabled)
- `telemetry_enabled`: `true` (basic telemetry)

## CP2+ Features (Now Baseline)

**Status Update**: As of CP2 baseline, the following CP2 features are **enabled by default** and are part of the baseline:

- ✅ **Idempotency Layer** (`idempotency_enabled: true`)
- ✅ **Distributed Tracing** (`tracing_enabled: true`)
- ✅ **Tenant Validation** (`tenant_validation_enabled: true`)
- ✅ **Admin gRPC Service** (`admin_grpc_enabled: true`)

These features were previously optional and controlled by feature flags, but are now part of the CP2 baseline.

### CP2+ Features (Previously Optional, Now Baseline)

### 1. Idempotency Layer

**Component**: `router_idempotency.erl`
**Feature Flag**: `idempotency_enabled` (default: `true` - CP2 baseline)
**Purpose**: Prevents duplicate processing of messages (results, ACKs, usage events)
**Status**: ✅ **Enabled by default in CP2 baseline**

**Usage**:
```erlang
IdempotencyEnabled = application:get_env(beamline_router, idempotency_enabled, true),  % Default: true (CP2 baseline)
case IdempotencyEnabled of
    true ->
        case router_idempotency:check_and_mark(KeyType, MessageId, AdditionalData) of
            {ok, seen} -> skip_processing();
            {ok, not_seen} -> process_message();
            {error, _} -> process_message()  % Fail open
        end;
    false ->
        process_message()  % No idempotency check (can be disabled)
end
```

**Supervisor**: Started conditionally in `beamline_router_sup.erl`

**References**:
- ADR-012: Idempotency Layer for Message Processing
- `apps/otp/router/src/router_idempotency.erl`

### 2. Distributed Tracing

**Component**: `router_tracing.erl`
**Feature Flag**: `tracing_enabled` (default: `true` - CP2 baseline)
**Purpose**: OpenTelemetry distributed tracing with context propagation
**Status**: ✅ **Enabled by default in CP2 baseline**

**Usage**:
```erlang
TracingEnabled = application:get_env(beamline_router, tracing_enabled, true),  % Default: true (CP2 baseline)
case TracingEnabled of
    true ->
        router_tracing:with_span(SpanName, Attributes, ParentContext, fun() ->
            process_message()
        end);
    false ->
        process_message()  % No tracing (can be disabled)
end
```

**References**:
- ADR-014: Metrics and Distributed Tracing
- `apps/otp/router/src/router_tracing.erl`

### 3. Tenant Validation

**Component**: `router_tenant_validator.erl`
**Feature Flag**: `tenant_validation_enabled` (default: `true` - CP2 baseline)
**Purpose**: Validates tenant IDs against allowlist and policy registry
**Status**: ✅ **Enabled by default in CP2 baseline**

**Usage**:
```erlang
TenantValidationEnabled = application:get_env(beamline_router, tenant_validation_enabled, true),  % Default: true (CP2 baseline)
case TenantValidationEnabled of
    true ->
        case router_tenant_validator:validate_tenant(TenantId, Context) of
            {ok, ValidatedTenantId} -> process_message(ValidatedTenantId);
            {error, Reason, ErrorContext} -> handle_validation_error(Reason, ErrorContext)
        end;
    false ->
        process_message(TenantId)  % No validation (can be disabled)
end
```

**References**:
- ADR-013: Tenant Validation for Multi-Tenancy
- `apps/otp/router/src/router_tenant_validator.erl`

### 4. Admin gRPC Service

**Component**: `router_admin_grpc.erl`
**Feature Flag**: `admin_grpc_enabled` (default: `true` - CP2 baseline)
**Purpose**: Admin gRPC service (RouterAdmin) for policy management
**Status**: ✅ **Enabled by default in CP2 baseline**

**Configuration**:
- Service registered in `router_grpc_sup.erl` only if `admin_grpc_enabled = true`
- Services: `UpsertPolicy`, `DeletePolicy`, `GetPolicy`, `ListPolicies`

**References**:
- `apps/otp/router/src/router_admin_grpc.erl`
- `apps/otp/router/src/router_grpc_sup.erl`

### 5. HEIR Policy Store

**Component**: `router_policy_store_heir.erl`
**Feature Flag**: `heir_enabled` (default: `false`)
**Purpose**: Advanced policy store with HEIR (Hot Erlang In-memory Replication)

**Status**: Not started in CP1 baseline (uses static/ETS policy store)

**References**:
- `apps/otp/router/src/router_policy_store_heir.erl`

### 6. Advanced Metrics

**Feature Flag**: `advanced_metrics_enabled` (default: `false`)
**Purpose**: Advanced metrics beyond basic telemetry (Prometheus export, detailed histograms)

**Status**: CP1 baseline uses basic telemetry only

**References**:
- ADR-014: Metrics and Distributed Tracing
- `apps/otp/router/src/router_metrics.erl`

### 7. ACK Consumer

**Component**: `router_ack_consumer.erl`
**Feature Flag**: `ack_enabled` (default: `false`)
**Purpose**: Consumes assignment acknowledgments from CAF

**Status**: Optional, controlled by `ack_enabled` flag

**References**:
- `apps/otp/router/src/router_ack_consumer.erl`

## Feature Flag Configuration

All CP2+ feature flags are defined in `apps/otp/router/src/beamline_router.app.src`:

```erlang
{env, [
    %% CP2+ Feature Flags (now baseline - enabled by default)
    {idempotency_enabled, true},  %% CP2+: Enable idempotency layer (CP2 baseline)
    {tracing_enabled, true},  %% CP2+: Enable OpenTelemetry distributed tracing (CP2 baseline)
    {tenant_validation_enabled, true},  %% CP2+: Enable tenant validation (CP2 baseline)
    {admin_grpc_enabled, true},  %% CP2+: Enable admin gRPC service (RouterAdmin) (CP2 baseline)
    {heir_enabled, false},  %% CP2+: Enable HEIR policy store (still optional)
    {advanced_metrics_enabled, false},  %% CP2+: Enable advanced metrics (still optional)
    {ack_enabled, false}  %% CP2+: Enable ACK consumer (still optional)
]}
```

## Enabling CP2+ Features

**CRITICAL**: CP2+ features require BOTH:
1. Feature flag enabled (`application:get_env` or environment variable)
2. `current_cp >= CP2-LC` in DevState (`.trae/state.json`)

Even if feature flags are set, CP2+ features will **NOT** start unless `current_cp >= CP2-LC`. This is enforced by `router_state:is_cp2_plus_allowed()`.

**See**: `docs/ADR/ADR-015-router-devstate-integration.md` for CP1 Minimal Mode Enforcement details.

To enable CP2+ features:

1. **Set CP via DevState**:
   ```bash
   make devstate-set-cp CP=CP2-LC
   ```

2. **Enable feature flags** in `beamline_router.app.src` or via environment variables:
   ```erlang
   {env, [
       {idempotency_enabled, true},
       {tracing_enabled, true},
       {tenant_validation_enabled, true},
       {admin_grpc_enabled, true}
   ]}
   ```

   Or via environment variables:
   ```bash
   export BEAMLINE_ROUTER_IDEMPOTENCY_ENABLED=true
   export BEAMLINE_ROUTER_TRACING_ENABLED=true
   export BEAMLINE_ROUTER_TENANT_VALIDATION_ENABLED=true
   export BEAMLINE_ROUTER_ADMIN_GRPC_ENABLED=true
   ```

3. **Restart Router** to apply changes.

## Supervisor Tree

**CP1 Baseline** (`beamline_router_sup.erl`):
- Always started: `router_telemetry_handler`, `router_nats`, `router_rbac`, `router_rate_limiter`, `router_grpc_sup`, `router_nats_subscriber`, `router_result_consumer`
- Conditionally started: `router_ack_consumer` (if `ack_enabled = true`), `router_idempotency` (if `idempotency_enabled = true`)

## CP vs Features (Quick Scan)
- CP1 (baseline, always on):
  - `router_telemetry_handler` — always
  - `router_nats` — always
  - `router_rbac` — `rbac_enabled` (default true)
  - `router_rate_limiter` — always
  - `router_grpc_sup` — `grpc_enabled` (default true)
  - `router_nats_subscriber` — always
  - `router_result_consumer` — always
- CP2+ (gated via env/config):
  - `router_ack_consumer` — `ack_enabled` (default false)
  - `router_idempotency` — `idempotency_enabled` (default false)
  - RouterAdmin in `router_grpc_sup` — `admin_grpc_enabled` (default false)
  - Tracing paths in consumers/adapters — `tracing_enabled` (default false)
  - JetStream tuning — `nats_js_*` keys
  - HEIR policy store — `heir_enabled`/`disable_heir`

**CP2+ Components**:
- Started via feature flags or used as libraries (tracing, tenant validation)

## Migration from CP1 to CP2+

To migrate from CP1 baseline to CP2+ features:

1. **Enable feature flags** in `beamline_router.app.src` or via environment variables
2. **Verify dependencies**: Ensure all CP2+ dependencies are available (e.g., OpenTelemetry for tracing)
3. **Update configuration**: Configure CP2+ specific settings (e.g., idempotency TTL, tracing endpoints)
4. **Test**: Verify all enabled features work correctly

## References

- `apps/otp/router/src/beamline_router_sup.erl`: Supervisor tree
- `apps/otp/router/src/beamline_router.app.src`: Feature flag configuration
- `docs/CP1_CHECKLIST.md`: CP1 acceptance criteria
- ADR-011: JetStream E2E with Durable Subscriptions
- ADR-012: Idempotency Layer for Message Processing
- ADR-013: Tenant Validation for Multi-Tenancy
- ADR-014: Metrics and Distributed Tracing

