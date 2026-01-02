# BeamLine Constructor: State Management Technical Specification

> **CRITICAL SCOPE CLARIFICATION**: This document defines the **product state management architecture** for BeamLine Constructor runtime operations. 
> Checkpoint (CP) transitions and HMAC-chain audit are **development process mechanisms** managed by TRAE IDE tooling and are **NOT part of the product functionality**.

## Executive Summary

**Product State Management (Runtime):**
- ✅ **ETS-based runtime state** - Implemented and operational
- ✅ **In-memory audit logging** - Implemented with telemetry  
- ✅ **CP-based feature gating** - Implemented (read-only from DevState)
- ✅ **NATS event sourcing** - Implemented via message flows
- ❌ **PostgreSQL persistence** - Not required for MVP (future enhancement)
- ❌ **CP transition mechanism** - Not part of product (development process only)

**Development Process Management (External):**
- ✅ **DevState HMAC-chain audit** - TRAE tooling for development workflow
- ✅ **CP transition gates** - Development process validation
- ✅ **No-Drift enforcement** - CI/CD pipeline validation

## 1. Product State Management Scope (MVP)

### 1.1 Runtime State (ETS-Based)

**Implemented Components:**
```erlang
%% Current implementation in router_policy_store.erl
-policy_cache: ETS table for compiled routing policies
-sticky_sessions: ETS table for provider-to-session mappings  
-rate_counters: ETS table for sliding window rate limiting
-idempotency: ETS table for duplicate detection (24h TTL)
-delivery_tracker: ETS table for retry attempt counting
```

**Key Features:**
- ✅ **Fault tolerance**: ETS table ownership transfer with heir processes
- ✅ **Automatic recovery**: Table recreation on process crashes
- ✅ **Tenant isolation**: Scoped operations per tenant
- ✅ **Telemetry integration**: Metrics emission for monitoring
- ✅ **TTL expiration**: Automatic cleanup of expired entries

### 1.2 In-Memory Audit System

**Implemented in router_audit.erl:**
```erlang
%% Audit record structure (ETS-based)
-record(audit_entry, {
    ts :: integer(),              % Timestamp (microseconds)
    tenant_id :: binary(),        % Tenant scope
    user_id :: binary(),          % Actor identification
    action :: binary(),           % Action performed
    resource_type :: binary(),    % Resource category
    resource_id :: binary() | undefined, % Specific resource
    details :: map(),           % Structured details
    ip_address :: binary() | undefined,   % Client IP
    trace_id :: binary() | undefined      % Distributed trace
}).
```

**Current Capabilities:**
- ✅ **Real-time logging**: Immediate audit event capture
- ✅ **Tenant-scoped queries**: Filtered audit log retrieval
- ✅ **Structured logging**: JSON-formatted audit events
- ✅ **Telemetry correlation**: Trace ID propagation
- ✅ **Automatic cleanup**: Retention-based cleanup policies

### 1.3 Checkpoint-Based Feature Gating

**Implemented in router_state.erl:**
```erlang
%% CP detection and feature gating
is_cp2_plus_allowed() ->
    case get_current_cp() of
        {ok, Cp} -> check_cp_requirement(Cp);
        {error, missing} -> cp1_baseline_mode()
    end.

%% Graceful fallback to CP1 baseline
cp1_baseline_mode() ->
    #{
        routing => basic,
        audit => ets_only,
        observability => basic_metrics,
        multi_tenant => false
    }.
```

**Implementation Details:**
- ✅ **CP reading**: Reads current checkpoint from DevState state file
- ✅ **Feature gating**: Enables CP2+ features only when CP >= CP2-LC
- ✅ **Graceful degradation**: Falls back to CP1 baseline mode
- ✅ **Fallback telemetry**: Logs and metrics for fallback scenarios
- ✅ **No CP mutation**: Product code only reads, never modifies CP

### 1.4 Event Sourcing via NATS

**Implemented through protobuf messages:**
```erlang
%% Message correlation fields (flow.proto)
message Message {
  string run_id = 8;      % Multi-step workflow correlation
  string flow_id = 9;     % Process definition reference
  string step_id = 10;    % Step execution tracking
  string idempotency_key = 11; % Safe retry support
  string trace_id = 12;   % Distributed tracing
  string tenant_id = 13;  % Multi-tenant isolation
}
```

**Current Event Streams:**
- ✅ `beamline.usage.v1.metered` - Usage events for metering
- ✅ `caf.exec.assigned.v1` - Execution assignment events
- ✅ `caf.exec.result.v1` - Execution result events
- ✅ `beamline.alert.v1.*` - Alert events (critical/warning/info)

## 2. Development Process Management (External to Product)

### 2.1 Checkpoint Transitions (Development Process Only)

**Mechanism:** CP transitions are managed entirely through TRAE IDE tooling:
- ✅ **DevState service**: Node.js HTTP server for state management
- ✅ **HMAC-chain verification**: Cryptographic audit trail
- ✅ **CI/CD gates**: Automated validation before transitions
- ✅ **Manual approval**: Human-in-the-loop for CP changes
- ❌ **Not in product**: No CP transition APIs in BeamLine Constructor

**Process Flow:**
```
Developer → TRAE IDE → DevState Service → .trae/state.json → Router (read-only)
```

### 2.2 HMAC-Chain Audit (Development Workflow Only)

**Purpose:** Ensures development process integrity:
- ✅ **Immutable history**: Append-only change log
- ✅ **Cryptographic verification**: HMAC signatures
- ✅ **Multi-IDE support**: Export/import capabilities
- ✅ **CI integration**: Pre-commit/pre-push validation
- ❌ **Not runtime audit**: Separate from product audit system

**Reference sample files (for documentation and tooling only):**
- `data/state.sample.json` — example of BeamLine state matching `STATE.schema.json`
- `data/history.sample.json` — example of BeamLine history matching `HISTORY.schema.json`

## 3. Out of Scope for MVP (Future Enhancements)

### 3.1 PostgreSQL Persistence

**Rationale:** ETS-based audit is sufficient for MVP requirements
- **Performance**: ETS provides microsecond-level latency
- **Capacity**: Memory-based storage adequate for operational data
- **Recovery**: ETS table transfers provide fault tolerance
- **Complexity**: Avoids database operational overhead

**Future Implementation (Post-MVP):**
```erlang
%% Planned PostgreSQL persistence layer
persist_audit_entry(Entry) ->
    %% Async write to PostgreSQL
    gen_server:cast(audit_persister, {persist, Entry}),
    %% Continue with ETS operations
    ets:insert(audit_log, Entry).
```

**When to Implement:**
- When audit retention requirements exceed memory capacity
- When cross-instance audit correlation is required
- When compliance mandates persistent audit trails
- When operational analytics require SQL querying

### 3.2 CP Transition APIs

**Rationale:** CP transitions are development lifecycle events, not runtime operations
- **Separation of concerns**: Development vs. runtime responsibilities
- **Safety**: Prevents accidental CP changes in production
- **Process**: Enforces proper review and approval workflows
- **Tooling**: Leverages existing DevState infrastructure

**Future Considerations:**
- Read-only CP APIs for monitoring and reporting
- CP transition requests (with approval workflows)
- Integration with release management systems

## 4. Architecture Validation

### 4.1 Current Implementation Status

**✅ Fully Operational:**
- ETS-based runtime state management
- In-memory audit logging with telemetry
- CP-based feature gating with fallbacks
- NATS event sourcing via message flows
- Multi-tenant state isolation

**✅ Development Tooling:**
- DevState HMAC-chain audit
- CP transition management
- No-Drift enforcement
- CI/CD integration

**❌ Not Required for MVP:**
- PostgreSQL audit persistence
- Product-level CP transition mechanisms
- Cross-instance state synchronization
- Advanced audit analytics

### 4.2 Design Principles Maintained

1. **Separation of Concerns**: Product runtime vs. development process clearly separated
2. **Fault Tolerance**: Graceful degradation when DevState unavailable
3. **Performance**: ETS-based operations for low-latency requirements
4. **Observability**: Comprehensive telemetry and logging
5. **Multi-Tenancy**: Complete tenant isolation at all layers

## 5. Operational Guidelines

### 5.1 Monitoring and Alerting

**Key Metrics:**
- ETS table sizes and memory usage
- Audit log generation rate
- CP fallback frequency
- NATS message processing latency
- Tenant isolation violations

**Alert Conditions:**
- ETS table memory threshold exceeded
- Audit log query performance degradation
- Frequent CP fallback events
- DevState connectivity issues

### 5.2 Capacity Planning

**ETS Memory Requirements:**
- Audit logs: ~1KB per entry, 10K entries/second peak = ~10MB/second
- Policy cache: ~100KB per tenant, 1000 tenants = ~100MB
- Idempotency cache: ~200 bytes per key, 1M keys = ~200MB
- Total estimated: 500MB-1GB per node for operational data

**Retention Policies:**
- Audit logs: 7 days in ETS (configurable)
- Idempotency keys: 24 hours (fixed)
- Policy cache: 5 minutes TTL (configurable)
- Delivery tracking: Per-message lifecycle

## 6. Summary

**MVP State Management Architecture:**
- **Product**: ETS-based runtime state + in-memory audit + CP gating
- **Development**: DevState HMAC-chain + CP transitions (external)
- **Future**: PostgreSQL persistence (post-MVP enhancement)

**Key Benefits:**
- ✅ **Performance**: Microsecond-level state operations
- ✅ **Reliability**: Fault-tolerant with automatic recovery
- ✅ **Simplicity**: No external database dependencies for MVP
- ✅ **Observability**: Comprehensive telemetry and logging
- ✅ **Scalability**: Multi-tenant with proper isolation

This specification clarifies that **PostgreSQL audit persistence and CP transition mechanisms are not required for MVP**, while ensuring that the **ETS-based runtime state management** provides sufficient functionality for production deployment.