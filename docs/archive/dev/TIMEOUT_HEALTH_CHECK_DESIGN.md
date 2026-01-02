# Timeout and Health Check Design for Router Policy DSL

## Purpose

This document defines the design and specification for Timeout and Health Check functionality in Router Policy DSL. Timeouts control request duration limits, while health checks monitor provider availability and correlate with fallbacks and circuit breakers.

## Status

📅 **CP2 Enhancement** (Design Document)

**Current State**: Design and specification only. Implementation deferred to CP2-LC.

## Overview

### Problem

Without timeout and health check configuration:
- No per-policy timeout control (all policies use same default timeout)
- No per-provider timeout customization (different providers may need different timeouts)
- No proactive health monitoring (providers marked unhealthy only after failures)
- No correlation between health status and routing decisions (fallbacks, circuit breakers)

### Solution

Multi-level timeout and health check:
1. **Per-Policy Timeout**: Default timeout for all providers in policy
2. **Per-Provider Timeout**: Override timeout for specific providers
3. **Health Check**: Proactive monitoring of provider health
4. **Health-Based Routing**: Exclude unhealthy providers from selection, trigger fallbacks

### Integration with Fallbacks and Circuit Breaker

**Health Check** works **before** provider selection:
1. **Health Check** (first): If provider unhealthy → skip provider, trigger fallback
2. **Circuit Breaker Check** (second): If circuit open → fail fast, trigger fallback
3. **Provider Selection**: Only healthy providers with closed circuits
4. **Timeout**: Applied during provider call (if exceeded → trigger fallback)
5. **Fallback**: Triggered if provider unhealthy, circuit open, or timeout

## Timeout Configuration

### Per-Policy Timeout

**Location**: Top-level `timeout_ms` in policy

**Format**:
```json
{
  "version": "1.0",
  "timeout_ms": 30000,
  "providers": [
    { "name": "provider_a", "weight": 70 },
    { "name": "provider_b", "weight": 30 }
  ]
}
```

**Behavior**:
- Default timeout for all providers in policy
- Applied to provider HTTP/gRPC calls
- If timeout exceeded → request fails with `timeout` status
- Triggers fallback rules matching `{"status": ["timeout"]}`

### Per-Provider Timeout Override

**Location**: `providers[].timeout_ms` (optional override)

**Format**:
```json
{
  "version": "1.0",
  "timeout_ms": 30000,  // Default for all providers
  "providers": [
    {
      "name": "provider_a",
      "weight": 70,
      "timeout_ms": 10000  // Override: faster timeout for provider_a
    },
    {
      "name": "provider_b",
      "weight": 30,
      "timeout_ms": 60000  // Override: slower timeout for provider_b
    }
  ]
}
```

**Priority**: Per-provider timeout takes precedence over policy-level timeout

**Behavior**:
- `provider_a`: Uses 10s timeout (override)
- `provider_b`: Uses 60s timeout (override)
- Other providers: Use 30s timeout (policy default)

### Timeout Configuration Fields

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `timeout_ms` (policy-level) | integer | `30000` | Default timeout for all providers in policy (milliseconds) |
| `timeout_ms` (provider-level) | integer | `inherit` | Override timeout for specific provider (milliseconds) |

**Constraints**:
- Minimum: `100` ms
- Maximum: `300000` ms (5 minutes)
- Policy-level default: `30000` ms (30 seconds)

### Timeout Behavior

**When Timeout Exceeded**:
1. Provider call is **cancelled** (if possible) or **abandoned**
2. Request fails with status: `timeout`
3. **Fallback rules** are evaluated:
   - If fallback rule matches `{"status": ["timeout"]}` → use fallback provider
   - If no matching fallback → request fails with `timeout` error
4. **Circuit Breaker** tracks timeout as failure:
   - Increment `failure_count`
   - Update `error_rate`
   - If threshold exceeded → open circuit

**Timeout vs Retry**:
- Timeout occurs **during** provider call (request in progress)
- Retry occurs **after** provider call fails (retry with backoff)
- Timeout can trigger retry if retry configured for timeout condition

**Example**:
```json
{
  "timeout_ms": 30000,
  "providers": [
    { "name": "provider_a", "weight": 100 }
  ],
  "fallbacks": [
    {
      "when": {"status": ["timeout"]},
      "retry": 2,
      "backoff": {
        "strategy": "exponential",
        "base_ms": 100
      },
      "to": "provider_b"
    }
  ]
}
```

**Behavior**:
1. Request to `provider_a` with 30s timeout
2. If timeout exceeded → status: `timeout`
3. Retry `provider_a` 2 times with exponential backoff
4. If all retries timeout → fallback to `provider_b`

## Health Check Configuration

### Per-Policy Health Check

**Location**: Top-level `health_check` object in policy

**Format**:
```json
{
  "version": "1.0",
  "providers": [
    { "name": "provider_a", "weight": 70 },
    { "name": "provider_b", "weight": 30 }
  ],
  "health_check": {
    "enabled": true,
    "url": "https://provider_a.example.com/health",
    "interval_ms": 10000,
    "timeout_ms": 5000,
    "unhealthy_threshold": 3,
    "healthy_threshold": 2
  }
}
```

### Per-Provider Health Check Override

**Location**: `providers[].health_check` (optional override)

**Format**:
```json
{
  "version": "1.0",
  "health_check": {
    "enabled": true,
    "interval_ms": 10000,
    "timeout_ms": 5000,
    "unhealthy_threshold": 3,
    "healthy_threshold": 2
  },
  "providers": [
    {
      "name": "provider_a",
      "weight": 70,
      "health_check": {
        "url": "https://provider_a.example.com/health",
        "interval_ms": 5000  // Override: more frequent checks
      }
    },
    {
      "name": "provider_b",
      "weight": 30,
      "health_check": {
        "url": "https://provider_b.example.com/health",
        "enabled": false  // Override: disable health check for provider_b
      }
    }
  ]
}
```

**Priority**: Per-provider health check config takes precedence over policy-level config

### Health Check Configuration Fields

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `enabled` | boolean | `false` | Enable health checks for providers |
| `url` | string | `undefined` | Health check endpoint URL (required if enabled) |
| `interval_ms` | integer | `10000` | Health check interval in milliseconds |
| `timeout_ms` | integer | `5000` | Health check request timeout in milliseconds |
| `unhealthy_threshold` | integer | `3` | Number of consecutive failures to mark provider unhealthy |
| `healthy_threshold` | integer | `2` | Number of consecutive successes to mark provider healthy |

**Constraints**:
- `interval_ms`: Minimum `1000` ms, Maximum `60000` ms
- `timeout_ms`: Minimum `100` ms, Maximum `30000` ms
- `unhealthy_threshold`: Minimum `1`, Maximum `10`
- `healthy_threshold`: Minimum `1`, Maximum `10`

### Health Check Behavior

**Health Check States**:
- **Healthy**: Provider responds with 2xx status code within timeout
- **Unhealthy**: Provider fails health check (timeout, 4xx, 5xx, connection error)

**Health Check Process**:
1. **Periodic Checks**: Health check runs every `interval_ms` milliseconds
2. **HTTP GET Request**: `GET {url}` with timeout `timeout_ms`
3. **Response Evaluation**:
   - **2xx Status**: Success → increment healthy counter
   - **Non-2xx or Timeout**: Failure → increment unhealthy counter
4. **State Transition**:
   - If `unhealthy_count >= unhealthy_threshold` → mark provider **unhealthy**
   - If `healthy_count >= healthy_threshold` → mark provider **healthy**

**Health Check Storage**:
- ETS table: `router_provider_health`
- Key: `{TenantId, PolicyId, ProviderId}`
- Value: Map with `state`, `unhealthy_count`, `healthy_count`, `last_check`, `last_success`, `last_failure`

### Health Check vs Circuit Breaker

**Health Check** (Proactive):
- **Purpose**: Monitor provider availability **before** requests
- **Trigger**: Periodic checks (every `interval_ms`)
- **Scope**: Per-provider health status
- **Action**: Exclude unhealthy providers from selection

**Circuit Breaker** (Reactive):
- **Purpose**: Protect against cascading failures **during** requests
- **Trigger**: Request failures (timeout, 5xx, connection errors)
- **Scope**: Per-provider failure tracking
- **Action**: Fail fast if circuit open

**Integration**:
- **Health Check** → marks provider unhealthy → excluded from selection
- **Circuit Breaker** → tracks failures → opens circuit → fail fast
- Both can trigger fallbacks independently

## Integration with Fallbacks and Circuit Breaker

### Execution Order

**Decision Flow**:
```
1. Health Check (if enabled)
   ├─ If provider unhealthy → skip provider, proceed to step 2
   └─ If provider healthy → proceed to step 3

2. Circuit Breaker Check
   ├─ If circuit open → fail fast, trigger fallback
   └─ If circuit closed/half-open → proceed to step 3

3. Provider Selection (sticky, weighted)
   └─ Only healthy providers with closed circuits

4. Provider Call (with timeout)
   ├─ If timeout exceeded → status: timeout, trigger fallback
   └─ If success → record success, update health/circuit

5. Fallback (if provider unhealthy, circuit open, or timeout)
   └─ Select fallback provider
```

### Health Check → Fallback

**Behavior**:
- If provider marked **unhealthy** → excluded from provider selection
- If **no healthy providers** available → trigger fallback immediately
- Fallback rule can match on `{"status": ["unhealthy"]}` condition

**Example**:
```json
{
  "health_check": {
    "enabled": true,
    "url": "https://provider_a.example.com/health",
    "interval_ms": 10000,
    "unhealthy_threshold": 3
  },
  "providers": [
    { "name": "provider_a", "weight": 100 }
  ],
  "fallbacks": [
    {
      "when": {"status": ["unhealthy"]},
      "to": "provider_b"
    }
  ]
}
```

**Behavior**:
1. Health check runs every 10s
2. If `provider_a` fails 3 consecutive health checks → marked **unhealthy**
3. Provider selection → `provider_a` excluded (unhealthy)
4. No healthy providers → trigger fallback to `provider_b`

### Health Check → Circuit Breaker

**Behavior**:
- Health check failures **do NOT** directly open circuit breaker
- Health check and circuit breaker are **independent** mechanisms
- Health check failures can **influence** circuit breaker indirectly:
  - If provider unhealthy → excluded from selection → no requests → circuit stays closed
  - If provider becomes healthy → requests resume → circuit breaker tracks new failures

**Rationale**:
- Health check: Proactive monitoring (periodic checks)
- Circuit breaker: Reactive protection (request-based failures)
- Separation of concerns: Different purposes, different triggers

### Timeout → Fallback

**Behavior**:
- If timeout exceeded during provider call → status: `timeout`
- Fallback rules evaluated for `{"status": ["timeout"]}` condition
- If matching fallback rule → use fallback provider

**Example**:
```json
{
  "timeout_ms": 30000,
  "providers": [
    { "name": "provider_a", "weight": 100 }
  ],
  "fallbacks": [
    {
      "when": {"status": ["timeout"]},
      "retry": 1,
      "to": "provider_b"
    }
  ]
}
```

**Behavior**:
1. Request to `provider_a` with 30s timeout
2. If timeout exceeded → status: `timeout`
3. Retry `provider_a` 1 time
4. If retry also times out → fallback to `provider_b`

### Timeout → Circuit Breaker

**Behavior**:
- Timeout counts as **failure** for circuit breaker
- Increment `failure_count`
- Update `error_rate`
- If threshold exceeded → open circuit

**Example**:
```json
{
  "timeout_ms": 30000,
  "circuit_breaker": {
    "enabled": true,
    "failure_threshold": 5
  },
  "providers": [
    { "name": "provider_a", "weight": 100 }
  ]
}
```

**Behavior**:
1. Request to `provider_a` with 30s timeout
2. If timeout exceeded → increment circuit breaker `failure_count`
3. If `failure_count >= 5` → open circuit
4. Future requests → circuit open → fail fast → trigger fallback

## Health Check Criteria

### When Provider is Considered Unhealthy

**Health Check Failures**:
1. **Timeout**: Health check request exceeds `timeout_ms`
2. **4xx Status**: Provider returns 4xx status code (e.g., 404, 401)
3. **5xx Status**: Provider returns 5xx status code (e.g., 500, 503)
4. **Connection Error**: Network connection failure (DNS, TCP, TLS)
5. **Invalid Response**: Non-HTTP response or malformed response

**Unhealthy Threshold**:
- Provider marked unhealthy after `unhealthy_threshold` consecutive failures
- Default: `3` consecutive failures
- Configurable per-policy or per-provider

### When Provider is Considered Healthy

**Health Check Successes**:
1. **2xx Status**: Provider returns 2xx status code (e.g., 200, 204)
2. **Response Time**: Response received within `timeout_ms`

**Healthy Threshold**:
- Provider marked healthy after `healthy_threshold` consecutive successes
- Default: `2` consecutive successes
- Configurable per-policy or per-provider

### Health Check Response Format

**Expected Response**:
- **Status Code**: `200 OK` (or any 2xx)
- **Content-Type**: `application/json` (optional)
- **Body**: Optional JSON with health status (not required)

**Example Health Check Response**:
```json
{
  "status": "healthy",
  "timestamp": "2025-01-27T12:00:00Z"
}
```

**Router Behavior**:
- Router only checks **HTTP status code** (2xx = healthy, non-2xx = unhealthy)
- Response body is **ignored** (not parsed or validated)
- Health check is **lightweight** (minimal processing)

## Examples

### Example 1: Per-Policy Timeout

```json
{
  "version": "1.0",
  "timeout_ms": 30000,
  "providers": [
    { "name": "provider_a", "weight": 70 },
    { "name": "provider_b", "weight": 30 }
  ],
  "fallbacks": [
    {
      "when": {"status": ["timeout"]},
      "to": "provider_b"
    }
  ]
}
```

**Behavior**:
- All providers use 30s timeout
- If timeout exceeded → fallback to `provider_b`

### Example 2: Per-Provider Timeout Override

```json
{
  "version": "1.0",
  "timeout_ms": 30000,  // Default
  "providers": [
    {
      "name": "provider_a",
      "weight": 70,
      "timeout_ms": 10000  // Override: faster timeout
    },
    {
      "name": "provider_b",
      "weight": 30,
      "timeout_ms": 60000  // Override: slower timeout
    }
  ]
}
```

**Behavior**:
- `provider_a`: 10s timeout
- `provider_b`: 60s timeout

### Example 3: Health Check with Fallback

```json
{
  "version": "1.0",
  "health_check": {
    "enabled": true,
    "url": "https://provider_a.example.com/health",
    "interval_ms": 10000,
    "timeout_ms": 5000,
    "unhealthy_threshold": 3,
    "healthy_threshold": 2
  },
  "providers": [
    { "name": "provider_a", "weight": 100 }
  ],
  "fallbacks": [
    {
      "when": {"status": ["unhealthy"]},
      "to": "provider_b"
    }
  ]
}
```

**Behavior**:
1. Health check runs every 10s
2. If `provider_a` fails 3 consecutive checks → marked unhealthy
3. Provider selection → `provider_a` excluded
4. No healthy providers → fallback to `provider_b`

### Example 4: Timeout + Health Check + Circuit Breaker

```json
{
  "version": "1.0",
  "timeout_ms": 30000,
  "health_check": {
    "enabled": true,
    "url": "https://provider_a.example.com/health",
    "interval_ms": 10000,
    "unhealthy_threshold": 3
  },
  "circuit_breaker": {
    "enabled": true,
    "failure_threshold": 5
  },
  "providers": [
    { "name": "provider_a", "weight": 100 }
  ],
  "fallbacks": [
    {
      "when": {"status": ["timeout", "unhealthy", "circuit_breaker_open"]},
      "to": "provider_b"
    }
  ]
}
```

**Behavior**:
1. **Health Check**: If `provider_a` unhealthy → excluded from selection → fallback
2. **Circuit Breaker**: If circuit open → fail fast → fallback
3. **Timeout**: If timeout exceeded → status: timeout → fallback
4. All mechanisms work together to ensure reliability

## Implementation Considerations

### Health Check Implementation

**Module**: `router_provider_health_check.erl` (new module)

**Functions**:
- `start_health_check/3`: Start health check for provider
- `stop_health_check/2`: Stop health check for provider
- `get_health_status/2`: Get current health status
- `is_healthy/2`: Check if provider is healthy

**Storage**: ETS table `router_provider_health`

**Scheduler**: GenServer with timer-based periodic checks

### Timeout Implementation

**Location**: `router_core.erl` or `router_provider_client.erl`

**Behavior**:
- Apply timeout to HTTP/gRPC client calls
- Use Erlang `receive ... after TimeoutMs -> timeout end`
- Cancel request if timeout exceeded
- Return `{error, timeout}` status

### Integration Points

**Provider Selection** (`router_decider.erl`):
- Filter out unhealthy providers before selection
- Skip providers with open circuit breakers
- Apply per-provider timeout during call

**Fallback Evaluation** (`router_decider.erl`):
- Match `{"status": ["unhealthy"]}` condition
- Match `{"status": ["timeout"]}` condition
- Match `{"status": ["circuit_breaker_open"]}` condition

## Metrics and Observability

### Prometheus Metrics

**Health Check Metrics**:
```
router_provider_health_check_total{tenant_id, policy_id, provider_id, result}
router_provider_health_status{tenant_id, policy_id, provider_id, status}
```

**Timeout Metrics**:
```
router_provider_timeout_total{tenant_id, policy_id, provider_id}
router_provider_request_duration_ms{tenant_id, policy_id, provider_id, quantile}
```

### Logging

**Health Check Status Change**:
```json
{
  "timestamp": "2025-01-27T12:00:00Z",
  "level": "WARN",
  "component": "router",
  "message": "Provider marked unhealthy",
  "context": {
    "tenant_id": "tenant_123",
    "policy_id": "default",
    "provider_id": "provider_a",
    "health_status": "unhealthy",
    "unhealthy_count": 3,
    "last_failure": "2025-01-27T11:59:50Z"
  }
}
```

**Timeout Event**:
```json
{
  "timestamp": "2025-01-27T12:00:00Z",
  "level": "WARN",
  "component": "router",
  "message": "Provider request timeout",
  "context": {
    "tenant_id": "tenant_123",
    "policy_id": "default",
    "provider_id": "provider_a",
    "timeout_ms": 30000,
    "elapsed_ms": 30001
  }
}
```

## Future Enhancements (CP2+)

### Advanced Health Check

- **Custom Health Check Logic**: User-defined health check scripts
- **Health Check Aggregation**: Multiple health check endpoints
- **Health Check Dependencies**: Health check based on dependent services

### Dynamic Timeout Adjustment

- **Adaptive Timeout**: Adjust timeout based on provider latency
- **Timeout Prediction**: ML-based timeout prediction
- **Per-Request Timeout**: Timeout based on request size/complexity

### Health Check Dashboard

- Visualize provider health status over time
- Track health check failures and recoveries
- Monitor timeout rates per provider

## Risks and Non-Goals

### Risks

#### 1. Impact on CP1 Invariants

**Risk**: Timeout and health check implementation must not break CP1 policy engine invariants.

**CP1 Invariants to Preserve**:
- ✅ **Provider Selection Logic**: Sticky sessions, weighted routing, fallbacks must continue to work
- ✅ **Explanation Format**: Decision explanations must remain compatible with CP1 format
- ✅ **Extension Pipeline**: Pre/validators/post extensions must execute regardless of timeout/health status
- ✅ **Backward Compatibility**: Legacy policy formats must continue to work (timeout/health check is optional)

**Mitigation**:
- Timeout and health check are **additive** - they only add checks, don't modify existing logic
- Timeout enforcement happens **during** provider call, doesn't change selection algorithm
- Health check is checked **before** provider selection, but doesn't change selection algorithm
- Fallback logic already handles provider failures - timeout/health check just add more failure conditions

#### 2. Infrastructure Dependencies

**Risk**: Timeout and health check require additional infrastructure components.

**Affected Components**:
- **NATS**: No direct impact (timeout/health check are Router-internal)
- **Extensions**: No impact (extensions execute independently)
- **Metrics**: Requires metrics infrastructure (Prometheus) for timeout/health check metrics
- **Storage**: Requires ETS or external store for health check state
- **Scheduling**: Requires timer/scheduler for periodic health checks

**Mitigation**:
- Use ETS for health check state (no external dependencies)
- Use Erlang timers for health check scheduling (built-in)
- Metrics are optional (timeout/health check work without metrics)
- Health check state is per-provider, per-tenant (scalable with ETS)

#### 3. Performance Impact

**Risk**: Timeout and health check add latency and overhead.

**Impact Areas**:
- **Health Check Scheduling**: Periodic health checks consume resources
- **Health Check State Lookup**: ETS lookup before provider selection
- **Timeout Enforcement**: Process monitoring and kill operations
- **Metrics Collection**: Additional overhead for timeout/health check tracking

**Mitigation**:
- Health checks are scheduled (not per-request, low overhead)
- ETS lookups are O(1) and very fast (< 1ms)
- Timeout enforcement is async (doesn't block request path)
- Metrics collection is async (doesn't block request path)

#### 4. Health Check Accuracy

**Risk**: Health check state may not accurately reflect provider health.

**Affected Scenarios**:
- **Stale Health Check**: Health check runs periodically, may miss transient failures
- **False Positives**: Provider may be healthy but health check fails (network issues)
- **False Negatives**: Provider may be unhealthy but health check passes (timing)

**Mitigation**:
- Health check is **best effort** - not guaranteed to catch all failures
- Fallback logic handles failures even if health check passes
- Circuit breaker provides additional protection (complements health check)
- Health check interval is configurable (balance between accuracy and overhead)

#### 5. Timeout vs Health Check Overlap

**Risk**: Timeout and health check may have overlapping responsibilities.

**Affected Areas**:
- **Provider Unavailability**: Both timeout and health check can detect this
- **Failure Handling**: Both can trigger fallbacks

**Mitigation**:
- Timeout is **reactive** (detects failures during request)
- Health check is **proactive** (detects failures before request)
- They complement each other (health check prevents requests, timeout handles failures)
- Clear separation of concerns (timeout = request-level, health check = provider-level)

### Non-Goals

#### 1. Distributed Health Check State

**Non-Goal**: Shared health check state across Router instances.

**Rationale**:
- Per-instance health check is sufficient for CP2
- Distributed state adds complexity (consensus, synchronization)
- Eventual consistency is acceptable (each instance tracks provider health independently)

**Future**: May be added in CP3+ if needed.

#### 2. Health Check for Extensions

**Non-Goal**: Health check for extension execution.

**Rationale**:
- Extensions are already handled by extension pipeline
- Extension health is separate concern (not provider health)
- Health check is for provider availability, not extension availability

**Future**: Extension health checks may be added separately.

#### 3. Dynamic Timeout/Health Check Configuration

**Non-Goal**: Runtime changes to timeout/health check configuration without policy update.

**Rationale**:
- Timeout/health check configuration is part of policy (versioned, audited)
- Dynamic configuration adds complexity (hot reload, validation)
- Policy updates are sufficient for configuration changes

**Future**: May be added in CP3+ for operational flexibility.

#### 4. Advanced Health Check Protocols

**Non-Goal**: Complex health check protocols (HTTP health endpoints, gRPC health service).

**Rationale**:
- Simple health check (provider call success/failure) is sufficient for CP2
- Complex protocols add unnecessary complexity
- Provider call is the actual health check (no separate endpoint needed)

**Future**: Advanced health check protocols may be added if needed.

#### 5. Timeout/Health Check Metrics Dashboard

**Non-Goal**: Built-in dashboard for timeout/health check metrics.

**Rationale**:
- Metrics are exported to Prometheus (external dashboard can be used)
- Dashboard is operational tooling, not core functionality
- CP2 focuses on core timeout/health check functionality

**Future**: Dashboard may be added in CP3+ or as separate tooling.

## References

- `docs/ROUTING_POLICY.md` - Main routing policy specification
- `docs/archive/dev/CIRCUIT_BREAKER_DESIGN.md` - Circuit breaker design
- `apps/otp/router/src/router_extension_circuit_breaker.erl` - Extension circuit breaker (reference)
- `docs/archive/dev/POLICY_DSL_GAPS_CP_CLASSIFICATION.md` - CP classification
- `docs/OBSERVABILITY_CONVENTIONS.md` - Logging conventions

## Change History

**v1.0 (2025-01-27)**:
- Initial design document
- Per-policy and per-provider timeout configuration
- Health check configuration and behavior
- Integration with fallbacks and circuit breaker
- Examples and implementation considerations

