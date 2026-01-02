# Gateway + Router Abuse Scenarios Specification

**Date**: 2025-11-26  
**Status**: 📋 **Design Document**  
**Scope**: ⏭ **CP3/Pre-Release** (specification and baseline implementation in CP2)  
**Purpose**: Define abuse scenarios and protection mechanisms for Gateway and Router  
**Related**: `docs/OPS_RUNBOOK_ROUTER_INTAKE.md`, `docs/OPS_RUNBOOK_GATEWAY_RATE_LIMITING.md`, `docs/SECURITY_GUIDE.md`

## Scope: CP2 Baseline vs CP3/Pre-Release

**CP2 Baseline**:
- ✅ Specification: Abuse scenarios defined
- ✅ Metrics: Gateway and Router abuse metrics implemented
- ✅ Alerts: Prometheus alert rules defined
- ✅ Tests: Integration tests for abuse scenarios

**CP3/Pre-Release Requirements**:
- Production alerting integration
- Dashboard definitions
- Runbook completion
- Full observability integration

**Reference**: `docs/archive/dev/ABUSE_DETECTION_PHASE2_4_COMPLETE_REPORT.md`, `docs/archive/dev/CP2_TECH_DEBT_SUMMARY.md`

## Executive Summary

This document specifies abuse scenarios that Gateway and Router must protect against, including:
- Flood attacks with valid but "empty" requests
- Targeted attacks on specific tenants
- Rate limit evasion attempts (multiple API keys, IP rotation)
- Heavy payload attacks (large payload_ref / blobs)

For each scenario, we define:
- **Detection Layer**: Which component should catch it (Gateway rate limiting / Router intake validation)
- **Logging**: What to log for audit and forensics
- **Alerts**: Which alerts should fire
- **Response**: How the system should respond

## Current Protection Mechanisms

### Gateway Rate Limiting (CP1/CP2)

**Fixed-Window Rate Limiting**:
- Per-endpoint limits (50/100/200 req/min)
- Per-tenant tracking (if tenant_id available)
- HTTP 429 responses with standard headers

**Limitations**:
- In-memory mode (CP1): Not consistent across instances
- Distributed mode (CP2 PoC): Redis-backed, but not fully production-ready

### Router Intake Validation (CP2)

**Validation Layers**:
- Schema validation (protobuf/JSON decode)
- Version validation (schema version check)
- Correlation fields validation (run_id, flow_id, step_id, idempotency_key)
- Tenant validation (allowlist check)
- Payload size validation (1MB default limit)

**Error Handling**:
- DLQ support (configurable DLQ subject)
- Audit logging (structured JSON logs)
- Metrics (error counters, DLQ events)

**Limitations**:
- Payload size limit is global (not per-tenant)
- No per-tenant rate limiting at Router level
- No detection of "empty" but valid requests

## Abuse Scenarios

### Scenario 1: Flood with Valid but "Empty" Requests

**Description**: Attacker sends many valid requests with minimal/empty payloads to consume resources.

**Example**:
```json
{
  "version": "1",
  "tenant_id": "attacker_tenant",
  "request_id": "req-001",
  "task": {
    "type": "text.generate",
    "payload": {}  // Empty payload
  }
}
```

**Attack Vector**:
- Valid schema, valid version, valid correlation fields
- Passes all validation checks
- But payload is empty or minimal
- Goal: Consume Router processing resources

**Detection Layer**:
- **Primary**: Router intake validation (payload content validation)
- **Secondary**: Gateway rate limiting (if per-tenant limits configured)

**Detection Logic**:
```erlang
%% In router_intake_validator.erl
validate_payload_content(ValidatedMessage) ->
    Task = maps:get(<<"task">>, ValidatedMessage, #{}),
    Payload = maps:get(<<"payload">>, Task, #{}),
    
    %% Check if payload is empty or too small
    PayloadSize = byte_size(jx:encode(Payload)),
    MinPayloadSize = application:get_env(beamline_router, min_payload_size, 10),  %% 10 bytes minimum
    
    case PayloadSize < MinPayloadSize of
        true ->
            {error, {payload_too_small, <<"Payload is too small or empty">>}};
        false ->
            {ok, ValidatedMessage}
    end.
```

**Logging**:
```json
{
  "timestamp": "2025-11-26T12:00:00Z",
  "level": "WARN",
  "component": "router",
  "message": "Abuse detected: empty payload request",
  "event_type": "abuse.empty_payload",
  "tenant_id": "attacker_tenant",
  "request_id": "req-001",
  "trace_id": "trace-001",
  "context": {
    "payload_size": 0,
    "min_payload_size": 10,
    "subject": "beamline.router.v1.decide"
  }
}
```

**Alerts**:
- `RouterAbuseEmptyPayloadHigh` - Empty payload requests > 10/min for 5 minutes (warning)
- `RouterAbuseEmptyPayloadCritical` - Empty payload requests > 50/min for 1 minute (critical)

**Response**:
- Reject request with error code: `PAYLOAD_TOO_SMALL`
- Send to DLQ (if enabled)
- Increment metric: `router_abuse_empty_payload_total`
- **Do NOT** process request (save resources)

### Scenario 2: Targeted Attack on Specific Tenant

**Description**: Attacker targets a specific tenant with high-volume requests to disrupt their service.

**Example**:
```json
{
  "version": "1",
  "tenant_id": "target_tenant",  // Specific tenant being attacked
  "request_id": "req-001",
  "task": {
    "type": "text.generate",
    "payload": {"prompt": "test"}
  }
}
```

**Attack Vector**:
- Valid requests (pass validation)
- High volume (1000+ req/min)
- Single tenant targeted
- Goal: Exhaust tenant's quota or disrupt service

**Detection Layer**:
- **Primary**: Gateway rate limiting (per-tenant limits)
- **Secondary**: Router intake validation (tenant validation, quota checks)

**Detection Logic**:
```c
// In http_server.c
// Per-tenant rate limiting (if implemented)
static int check_tenant_rate_limit(const char *tenant_id, rl_endpoint_id_t endpoint) {
    char key[256];
    snprintf(key, sizeof(key), "rl:tenant:%s:endpoint:%d", tenant_id, endpoint);
    
    // Check per-tenant limit (e.g., 200 req/min per tenant)
    int tenant_limit = get_tenant_rate_limit(tenant_id, endpoint);
    int current_count = get_rate_limit_count(key);
    
    if (current_count >= tenant_limit) {
        return 0;  // Rate limit exceeded
    }
    
    increment_rate_limit_count(key);
    return 1;  // Allowed
}
```

**Logging**:
```json
{
  "timestamp": "2025-11-26T12:00:00Z",
  "level": "WARN",
  "component": "gateway",
  "message": "Abuse detected: targeted tenant attack",
  "event_type": "abuse.targeted_tenant",
  "tenant_id": "target_tenant",
  "context": {
    "endpoint": "/api/v1/routes/decide",
    "request_rate_per_min": 1200,
    "tenant_limit_per_min": 200,
    "client_ip": "192.168.1.100"
  }
}
```

**Alerts**:
- `GatewayAbuseTargetedTenantHigh` - Single tenant > 500 req/min for 5 minutes (warning)
- `GatewayAbuseTargetedTenantCritical` - Single tenant > 1000 req/min for 1 minute (critical)

**Response**:
- Apply per-tenant rate limiting (HTTP 429)
- Log abuse event
- Increment metric: `gateway_abuse_targeted_tenant_total`
- **Optionally**: Temporarily block tenant (if abuse continues)

### Scenario 3: Rate Limit Evasion (Multiple API Keys / IP Rotation)

**Description**: Attacker uses multiple API keys or rotates IPs to evade rate limits.

**Example**:
```
# Request 1: API key 1, IP 1.1.1.1
POST /api/v1/routes/decide
X-API-Key: key-001
X-Tenant-ID: tenant-001

# Request 2: API key 2, IP 1.1.1.2
POST /api/v1/routes/decide
X-API-Key: key-002
X-Tenant-ID: tenant-001

# Request 3: API key 3, IP 1.1.1.3
POST /api/v1/routes/decide
X-API-Key: key-003
X-Tenant-ID: tenant-001
```

**Attack Vector**:
- Multiple API keys for same tenant
- IP rotation (different source IPs)
- Goal: Evade per-tenant/per-IP rate limits

**Detection Layer**:
- **Primary**: Gateway rate limiting (per-tenant aggregation)
- **Secondary**: Router intake validation (idempotency checks, pattern detection)

**Detection Logic**:
```c
// In http_server.c
// Aggregate rate limiting by tenant (not just API key)
static int check_tenant_aggregate_rate_limit(const char *tenant_id, rl_endpoint_id_t endpoint) {
    // Check aggregate rate for tenant across all API keys/IPs
    char tenant_key[256];
    snprintf(tenant_key, sizeof(tenant_key), "rl:tenant:%s:endpoint:%d:aggregate", tenant_id, endpoint);
    
    int tenant_limit = get_tenant_aggregate_limit(tenant_id, endpoint);
    int current_count = get_rate_limit_count(tenant_key);
    
    if (current_count >= tenant_limit) {
        // Detect evasion pattern
        log_abuse_evasion(tenant_id, endpoint);
        return 0;  // Rate limit exceeded
    }
    
    increment_rate_limit_count(tenant_key);
    return 1;  // Allowed
}
```

**Logging**:
```json
{
  "timestamp": "2025-11-26T12:00:00Z",
  "level": "WARN",
  "component": "gateway",
  "message": "Abuse detected: rate limit evasion attempt",
  "event_type": "abuse.rate_limit_evasion",
  "tenant_id": "tenant-001",
  "context": {
    "endpoint": "/api/v1/routes/decide",
    "api_keys_used": ["key-001", "key-002", "key-003"],
    "ips_used": ["1.1.1.1", "1.1.1.2", "1.1.1.3"],
    "aggregate_rate_per_min": 150,
    "tenant_limit_per_min": 200
  }
}
```

**Alerts**:
- `GatewayAbuseRateLimitEvasion` - Multiple API keys/IPs for same tenant > 10 in 1 minute (warning)
- `GatewayAbuseRateLimitEvasionCritical` - Evasion pattern detected + aggregate rate > limit (critical)

**Response**:
- Apply aggregate per-tenant rate limiting (HTTP 429)
- Log evasion pattern
- Increment metric: `gateway_abuse_rate_limit_evasion_total`
- **Optionally**: Temporarily block tenant (if pattern continues)

### Scenario 4: Heavy Payload Attacks (Large payload_ref / blobs)

**Description**: Attacker sends requests with very large payloads to consume bandwidth and processing resources.

**Example**:
```json
{
  "version": "1",
  "tenant_id": "attacker_tenant",
  "request_id": "req-001",
  "task": {
    "type": "text.generate",
    "payload": {
      "prompt": "A" * 1000000  // 1MB of 'A' characters
    }
  }
}
```

**Attack Vector**:
- Valid schema, valid version
- Payload size at or near limit (1MB)
- Goal: Consume bandwidth, memory, processing resources

**Detection Layer**:
- **Primary**: Router intake validation (payload size check)
- **Secondary**: Gateway (request size check before forwarding)

**Detection Logic**:
```erlang
%% In router_decide_consumer.erl (already implemented)
PayloadSize = byte_size(Payload),
MaxPayloadSize = application:get_env(beamline_router, nats_max_payload_size, 1048576),  %% 1MB

case PayloadSize > MaxPayloadSize of
    true ->
        %% Reject immediately (already implemented)
        ErrorCode = schema_validation_failed,
        ErrorMessage = <<"Payload size exceeds limit">>,
        router_intake_error_handler:handle_intake_error(...);
    false ->
        %% Additional check: payload size distribution
        check_payload_size_distribution(PayloadSize, TenantId)
end.
```

**Additional Check** (new):
```erlang
%% Check if tenant is sending consistently large payloads (abuse pattern)
check_payload_size_distribution(PayloadSize, TenantId) ->
    %% Track payload sizes per tenant
    Table = router_payload_size_tracking,
    ets:update_counter(Table, {TenantId, large_payloads}, 1, {{TenantId, large_payloads}, 0}),
    
    %% Check if tenant sends > 80% of requests with payloads > 500KB
    LargePayloadThreshold = 524288,  %% 500KB
    case PayloadSize > LargePayloadThreshold of
        true ->
            LargePayloads = ets:lookup_element(Table, {TenantId, large_payloads}, 2),
            TotalRequests = ets:lookup_element(Table, {TenantId, total_requests}, 2),
            
            case TotalRequests > 10 andalso (LargePayloads / TotalRequests) > 0.8 of
                true ->
                    %% Abuse pattern detected
                    log_abuse_heavy_payload(TenantId, PayloadSize),
                    emit_abuse_metric(heavy_payload, TenantId);
                false ->
                    ok
            end;
        false ->
            ok
    end.
```

**Logging**:
```json
{
  "timestamp": "2025-11-26T12:00:00Z",
  "level": "WARN",
  "component": "router",
  "message": "Abuse detected: heavy payload pattern",
  "event_type": "abuse.heavy_payload",
  "tenant_id": "attacker_tenant",
  "request_id": "req-001",
  "context": {
    "payload_size": 1048576,
    "max_payload_size": 1048576,
    "large_payload_ratio": 0.85,
    "subject": "beamline.router.v1.decide"
  }
}
```

**Alerts**:
- `RouterAbuseHeavyPayloadHigh` - Tenant sending > 80% large payloads (> 500KB) for 5 minutes (warning)
- `RouterAbuseHeavyPayloadCritical` - Multiple tenants sending max-size payloads simultaneously (critical)

**Response**:
- Reject if payload > max size (already implemented)
- Log abuse pattern if consistent large payloads
- Increment metric: `router_abuse_heavy_payload_total`
- **Optionally**: Apply per-tenant payload size limits (future enhancement)

### Scenario 5: Multi-Tenant Flood (Distributed Attack)

**Description**: Attacker uses multiple tenants to flood the system, avoiding per-tenant limits.

**Example**:
```
# Request 1: tenant-001
POST /api/v1/routes/decide
X-Tenant-ID: tenant-001

# Request 2: tenant-002
POST /api/v1/routes/decide
X-Tenant-ID: tenant-002

# Request 3: tenant-003
POST /api/v1/routes/decide
X-Tenant-ID: tenant-003
```

**Attack Vector**:
- Multiple tenants (each below per-tenant limit)
- High aggregate volume (total > global limit)
- Goal: Overwhelm system while avoiding per-tenant limits

**Detection Layer**:
- **Primary**: Gateway rate limiting (global limit)
- **Secondary**: Router intake validation (aggregate metrics)

**Detection Logic**:
```c
// In http_server.c
// Global rate limiting (already implemented)
static int check_global_rate_limit(rl_endpoint_id_t endpoint) {
    int global_limit = rl_config.global_limit;  // e.g., 1000 req/min
    int current_global_count = get_global_rate_limit_count();
    
    if (current_global_count >= global_limit) {
        return 0;  // Global rate limit exceeded
    }
    
    increment_global_rate_limit_count();
    return 1;  // Allowed
}
```

**Logging**:
```json
{
  "timestamp": "2025-11-26T12:00:00Z",
  "level": "WARN",
  "component": "gateway",
  "message": "Abuse detected: multi-tenant flood",
  "event_type": "abuse.multi_tenant_flood",
  "context": {
    "endpoint": "/api/v1/routes/decide",
    "global_rate_per_min": 1200,
    "global_limit_per_min": 1000,
    "active_tenants": 50,
    "avg_rate_per_tenant": 24
  }
}
```

**Alerts**:
- `GatewayAbuseMultiTenantFlood` - Global rate limit exceeded with > 20 active tenants (warning)
- `GatewayAbuseMultiTenantFloodCritical` - Global rate limit exceeded + > 50 active tenants (critical)

**Response**:
- Apply global rate limiting (HTTP 429)
- Log multi-tenant flood pattern
- Increment metric: `gateway_abuse_multi_tenant_flood_total`
- **Optionally**: Temporarily reduce global limit (emergency)

## Protection Mechanisms by Layer

### Gateway Layer

**Rate Limiting**:
- Per-endpoint limits (50/100/200 req/min)
- Per-tenant limits (if configured)
- Global limits (1000 req/min default)
- Aggregate per-tenant tracking (for evasion detection)

**Request Size Validation**:
- HTTP request size limit (before forwarding to Router)
- Early rejection of oversized requests

**IP/API Key Tracking**:
- Track API keys per tenant (for evasion detection)
- Track IPs per tenant (for evasion detection)

### Router Layer

**Intake Validation**:
- Payload size validation (1MB limit)
- Payload content validation (minimum size, empty payload detection)
- Tenant validation (allowlist check)
- Idempotency checks (duplicate detection)

**Payload Size Distribution Tracking**:
- Track payload sizes per tenant
- Detect patterns (consistent large payloads)
- Alert on abuse patterns

## Logging Requirements

### Abuse Event Logging

**Required Fields**:
- `event_type`: Abuse event type (`abuse.empty_payload`, `abuse.targeted_tenant`, etc.)
- `tenant_id`: Tenant ID (if available)
- `request_id`: Request ID (if available)
- `trace_id`: Trace ID (if available)
- `client_ip`: Client IP address (if available)
- `endpoint`: HTTP endpoint or NATS subject
- `context`: Abuse-specific context (payload size, rate, etc.)

**Log Format**:
```json
{
  "timestamp": "ISO-8601",
  "level": "WARN",
  "component": "router|gateway",
  "message": "Abuse detected: <type>",
  "event_type": "abuse.<type>",
  "tenant_id": "...",
  "request_id": "...",
  "trace_id": "...",
  "context": {
    "abuse_type": "...",
    "detection_metrics": {...},
    "response_action": "..."
  }
}
```

### Audit Trail

**All abuse events must be logged to audit trail**:
- Structured JSON logs
- Correlation fields (tenant_id, trace_id, request_id)
- Timestamp and node ID
- Abuse-specific metrics

## Alert Rules

### Gateway Alerts

**`GatewayAbuseTargetedTenantHigh`**:
```yaml
alert: GatewayAbuseTargetedTenantHigh
expr: rate(gateway_abuse_targeted_tenant_total[5m]) > 10
for: 5m
labels:
  severity: warning
annotations:
  summary: "Targeted tenant attack detected"
  description: "Single tenant exceeding rate limits: {{ $value }} events/min"
```

**`GatewayAbuseRateLimitEvasion`**:
```yaml
alert: GatewayAbuseRateLimitEvasion
expr: rate(gateway_abuse_rate_limit_evasion_total[1m]) > 5
for: 1m
labels:
  severity: warning
annotations:
  summary: "Rate limit evasion attempt detected"
  description: "Multiple API keys/IPs used for same tenant: {{ $value }} events/min"
```

**`GatewayAbuseMultiTenantFlood`**:
```yaml
alert: GatewayAbuseMultiTenantFlood
expr: rate(gateway_abuse_multi_tenant_flood_total[5m]) > 0
for: 5m
labels:
  severity: warning
annotations:
  summary: "Multi-tenant flood detected"
  description: "Global rate limit exceeded with multiple tenants: {{ $value }} events/min"
```

### Router Alerts

**`RouterAbuseEmptyPayloadHigh`**:
```yaml
alert: RouterAbuseEmptyPayloadHigh
expr: rate(router_abuse_empty_payload_total[5m]) > 10
for: 5m
labels:
  severity: warning
annotations:
  summary: "Empty payload abuse detected"
  description: "High rate of empty payload requests: {{ $value }} events/min"
```

**`RouterAbuseHeavyPayloadHigh`**:
```yaml
alert: RouterAbuseHeavyPayloadHigh
expr: rate(router_abuse_heavy_payload_total[5m]) > 5
for: 5m
labels:
  severity: warning
annotations:
  summary: "Heavy payload abuse detected"
  description: "Tenant sending consistently large payloads: {{ $value }} events/min"
```

## Metrics

### New Abuse Metrics

**Gateway**:
- `gateway_abuse_targeted_tenant_total` (counter) - Targeted tenant attacks
- `gateway_abuse_rate_limit_evasion_total` (counter) - Rate limit evasion attempts
- `gateway_abuse_multi_tenant_flood_total` (counter) - Multi-tenant floods

**Router**:
- `router_abuse_empty_payload_total` (counter) - Empty payload requests
- `router_abuse_heavy_payload_total` (counter) - Heavy payload patterns
- `router_abuse_payload_size_distribution` (histogram) - Payload size distribution per tenant

## Implementation Plan

### Phase 1: Detection Logic

**Tasks**:
1. Add payload content validation (empty payload detection)
2. Add per-tenant aggregate rate limiting (Gateway)
3. Add payload size distribution tracking (Router)
4. Add evasion pattern detection (Gateway)

**Deliverables**:
- Updated `router_intake_validator.erl` - Payload content validation
- Updated `http_server.c` - Aggregate rate limiting, evasion detection
- New `router_payload_tracker.erl` - Payload size distribution tracking

### Phase 2: Logging and Metrics

**Tasks**:
1. Add abuse event logging
2. Add abuse metrics
3. Update metrics catalog

**Deliverables**:
- Updated logging modules - Abuse event logging
- Updated `router_metrics.erl` - Abuse metrics
- Updated `metrics.catalog.yaml` - Abuse metrics catalog

### Phase 3: Alerts

**Tasks**:
1. Add Prometheus alert rules
2. Update alert documentation

**Deliverables**:
- Updated `PROMETHEUS_ALERTS.md` - Abuse alert rules
- Alert runbook updates

### Phase 4: Testing

**Tasks**:
1. Create abuse scenario tests
2. Integration tests for abuse detection
3. Load tests with abuse patterns

**Deliverables**:
- New test suite: `router_abuse_SUITE.erl`
- Integration tests: `gateway-router-abuse.test.ts`
- Test documentation

## References

- **Router Intake Runbook**: `docs/OPS_RUNBOOK_ROUTER_INTAKE.md`
- **Gateway Rate Limiting Runbook**: `docs/OPS_RUNBOOK_GATEWAY_RATE_LIMITING.md`
- **Security Guide**: `docs/SECURITY_GUIDE.md`
- **Rate Limiting Spec**: `docs/GATEWAY_RATE_LIMITING.md`
- **Backpressure Policy**: `docs/ARCHITECTURE/router-intake-backpressure-policy.md`

