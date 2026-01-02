# Abuse Scenarios Implementation Report

**Date**: 2025-11-26  
**Status**: ✅ **Specification Complete**  
**Purpose**: Specification and test scenarios for Gateway + Router abuse protection  
**Related**: `docs/ARCHITECTURE/gateway-router-abuse-scenarios.md`, `tests/integration/gateway-router-abuse.test.ts`, `apps/otp/router/test/router_abuse_SUITE.erl`

## Executive Summary

Abuse scenarios specification and test scenarios have been created for Gateway and Router, providing comprehensive coverage of abuse patterns including empty payload floods, targeted tenant attacks, rate limit evasion, heavy payload attacks, and multi-tenant floods. The specification defines detection layers, logging requirements, alert rules, and response mechanisms for each scenario.

## Implementation

### D.1. Спецификация Abuse-Сценариев

**Document Created**: `docs/ARCHITECTURE/gateway-router-abuse-scenarios.md`

**Key Sections**:
1. **Current Protection Mechanisms**:
   - Gateway Rate Limiting (CP1/CP2)
   - Router Intake Validation (CP2)
   - Limitations and gaps

2. **5 Abuse Scenarios**:
   - **Scenario 1: Flood with Valid but "Empty" Requests**
     - Detection: Router intake validation (payload content validation)
     - Logging: Abuse event with payload size context
     - Alerts: `RouterAbuseEmptyPayloadHigh`, `RouterAbuseEmptyPayloadCritical`
     - Response: Reject with `PAYLOAD_TOO_SMALL`, send to DLQ, increment metric
   
   - **Scenario 2: Targeted Attack on Specific Tenant**
     - Detection: Gateway rate limiting (per-tenant limits)
     - Logging: Abuse event with tenant, endpoint, request rate
     - Alerts: `GatewayAbuseTargetedTenantHigh`, `GatewayAbuseTargetedTenantCritical`
     - Response: Apply per-tenant rate limiting (HTTP 429), log abuse event
   
   - **Scenario 3: Rate Limit Evasion (Multiple API Keys / IP Rotation)**
     - Detection: Gateway rate limiting (per-tenant aggregation)
     - Logging: Abuse event with API keys/IPs used, aggregate rate
     - Alerts: `GatewayAbuseRateLimitEvasion`, `GatewayAbuseRateLimitEvasionCritical`
     - Response: Apply aggregate per-tenant rate limiting, log evasion pattern
   
   - **Scenario 4: Heavy Payload Attacks (Large payload_ref / blobs)**
     - Detection: Router intake validation (payload size check + distribution tracking)
     - Logging: Abuse event with payload size, large payload ratio
     - Alerts: `RouterAbuseHeavyPayloadHigh`, `RouterAbuseHeavyPayloadCritical`
     - Response: Reject if > max size, log abuse pattern if consistent large payloads
   
   - **Scenario 5: Multi-Tenant Flood (Distributed Attack)**
     - Detection: Gateway rate limiting (global limit)
     - Logging: Abuse event with global rate, active tenants
     - Alerts: `GatewayAbuseMultiTenantFlood`, `GatewayAbuseMultiTenantFloodCritical`
     - Response: Apply global rate limiting (HTTP 429), log multi-tenant flood pattern

3. **Protection Mechanisms by Layer**:
   - Gateway Layer: Rate limiting, request size validation, IP/API key tracking
   - Router Layer: Intake validation, payload size distribution tracking

4. **Logging Requirements**:
   - Abuse event logging format
   - Required fields (event_type, tenant_id, request_id, trace_id, client_ip, endpoint, context)
   - Audit trail requirements

5. **Alert Rules**:
   - Gateway alerts (targeted tenant, rate limit evasion, multi-tenant flood)
   - Router alerts (empty payload, heavy payload)
   - Prometheus alert definitions

6. **Metrics**:
   - New abuse metrics (Gateway and Router)
   - Metrics catalog updates

7. **Implementation Plan**:
   - Phase 1: Detection Logic
   - Phase 2: Logging and Metrics
   - Phase 3: Alerts
   - Phase 4: Testing

### D.2. Тестовые Сценарии

**Integration Tests Created**: `tests/integration/gateway-router-abuse.test.ts`

**Test Scenarios**:
1. **Empty Payload Flood**:
   - Test: Detect and reject empty payload requests
   - Test: Log abuse events for empty payloads
   - Verifies: 400/422 responses, error messages, abuse logging

2. **Targeted Tenant Attack**:
   - Test: Detect high-volume requests to single tenant
   - Test: Log abuse events for targeted tenant attacks
   - Verifies: Rate limiting (429), abuse detection, metrics

3. **Rate Limit Evasion (Multiple API Keys)**:
   - Test: Detect rate limit evasion with multiple API keys
   - Test: Log abuse events for rate limit evasion
   - Verifies: Aggregate rate limiting, evasion pattern detection

4. **Heavy Payload Attacks**:
   - Test: Reject requests with payload exceeding size limit
   - Test: Detect heavy payload pattern (consistent large payloads)
   - Verifies: 400/413/422 responses, abuse pattern detection

5. **Multi-Tenant Flood**:
   - Test: Detect multi-tenant flood (distributed attack)
   - Test: Log abuse events for multi-tenant flood
   - Verifies: Global rate limiting (429), multi-tenant flood detection

6. **Combined Attack (Multiple Vectors)**:
   - Test: Detect combined abuse patterns
   - Verifies: Multiple abuse vectors detected simultaneously

**Erlang Test Suite Created**: `apps/otp/router/test/router_abuse_SUITE.erl`

**Test Scenarios**:
1. **test_abuse_empty_payload_flood**: Empty payload flood detection
2. **test_abuse_heavy_payload_attack**: Heavy payload attack detection
3. **test_abuse_targeted_tenant**: Targeted tenant attack detection
4. **test_abuse_multi_tenant_flood**: Multi-tenant flood detection
5. **test_abuse_payload_size_distribution**: Payload size distribution tracking

### D.3. Связь с SECURITY_GUIDE

**Document Updated**: `docs/SECURITY_GUIDE.md`

**Added Section**: "Gateway + Router Abuse Scenarios"

**Content**:
- Reference to abuse scenarios specification
- Key protection mechanisms summary
- Links to operational runbooks
- Updated references section

## Files Created/Modified

### New Files

1. **`docs/ARCHITECTURE/gateway-router-abuse-scenarios.md`** - Abuse scenarios specification
2. **`tests/integration/gateway-router-abuse.test.ts`** - Integration tests for abuse scenarios
3. **`apps/otp/router/test/router_abuse_SUITE.erl`** - Erlang test suite for Router abuse scenarios
4. **`docs/archive/dev/ABUSE_SCENARIOS_IMPLEMENTATION_REPORT.md`** - This report

### Modified Files

1. **`docs/SECURITY_GUIDE.md`** - Added "Gateway + Router Abuse Scenarios" section

## Key Features

### Abuse Scenarios Specification

**Comprehensive Coverage**:
- ✅ 5 abuse scenarios (empty payload, targeted tenant, rate limit evasion, heavy payload, multi-tenant flood)
- ✅ Detection layers (Gateway vs Router)
- ✅ Logging requirements (structured JSON, audit trail)
- ✅ Alert rules (Prometheus alerts)
- ✅ Response mechanisms (rejection, rate limiting, metrics)
- ✅ Implementation plan (4 phases)

**Practical Examples**:
- Code examples for detection logic
- Logging format examples
- Alert rule definitions
- Metrics definitions

### Test Scenarios

**Integration Tests (TypeScript)**:
- ✅ 6 test groups (empty payload, targeted tenant, rate limit evasion, heavy payload, multi-tenant flood, combined)
- ✅ HTTP request/response validation
- ✅ Abuse event logging verification (TODO: requires log aggregation)
- ✅ Metrics verification (TODO: requires metrics endpoint)

**Erlang Test Suite**:
- ✅ 5 test cases (empty payload, heavy payload, targeted tenant, multi-tenant flood, payload distribution)
- ✅ Mock setup for validation and telemetry
- ✅ ETS tables for abuse tracking
- ✅ Payload size distribution tracking

## Integration Points

### Security Guide

**Location**: `docs/SECURITY_GUIDE.md`

**Added Section**:
```markdown
## Gateway + Router Abuse Scenarios

**For detailed abuse scenarios and protection mechanisms, see**:
- **`docs/ARCHITECTURE/gateway-router-abuse-scenarios.md`** - Complete abuse scenarios specification
- **Operational Runbooks**: Links to intake and rate limiting runbooks
```

## Next Steps

### Immediate (Phase 1: Detection Logic)

1. **Implement Payload Content Validation**:
   - Add `validate_payload_content/1` to `router_intake_validator.erl`
   - Check minimum payload size (10 bytes default)
   - Reject empty payloads with `PAYLOAD_TOO_SMALL` error

2. **Implement Aggregate Rate Limiting**:
   - Add per-tenant aggregate tracking in Gateway
   - Track API keys/IPs per tenant
   - Detect evasion patterns

3. **Implement Payload Size Distribution Tracking**:
   - Create `router_payload_tracker.erl` module
   - Track payload sizes per tenant
   - Detect patterns (80% large payloads)

### Short-Term (Phase 2: Logging and Metrics)

1. **Add Abuse Event Logging**:
   - Update logging modules to emit abuse events
   - Structured JSON format with required fields
   - Audit trail integration

2. **Add Abuse Metrics**:
   - Update `router_metrics.erl` with abuse metrics
   - Update `metrics.catalog.yaml` with abuse metrics
   - Gateway metrics for abuse events

### Medium-Term (Phase 3: Alerts)

1. **Add Prometheus Alert Rules**:
   - Update `PROMETHEUS_ALERTS.md` with abuse alerts
   - Configure alert thresholds
   - Test alert firing

### Long-Term (Phase 4: Testing)

1. **Complete Test Implementation**:
   - Implement log aggregation for test verification
   - Implement metrics endpoint for test verification
   - Run full test suite
   - Document test results

## Testing Status

### Integration Tests

**Status**: ✅ **Created** (requires implementation to pass)

**Test File**: `tests/integration/gateway-router-abuse.test.ts`

**Coverage**:
- Empty payload flood: ✅
- Targeted tenant attack: ✅
- Rate limit evasion: ✅
- Heavy payload attacks: ✅
- Multi-tenant flood: ✅
- Combined attack: ✅

**TODO**:
- Implement log aggregation endpoint for abuse event verification
- Implement metrics endpoint for abuse metric verification
- Run tests and verify behavior

### Erlang Test Suite

**Status**: ✅ **Created** (requires implementation to pass)

**Test File**: `apps/otp/router/test/router_abuse_SUITE.erl`

**Coverage**:
- Empty payload flood: ✅
- Heavy payload attack: ✅
- Targeted tenant: ✅
- Multi-tenant flood: ✅
- Payload size distribution: ✅

**TODO**:
- Implement payload content validation
- Implement payload size distribution tracking
- Run tests and verify behavior

## References

- **Abuse Scenarios Spec**: `docs/ARCHITECTURE/gateway-router-abuse-scenarios.md`
- **Integration Tests**: `tests/integration/gateway-router-abuse.test.ts`
- **Erlang Test Suite**: `apps/otp/router/test/router_abuse_SUITE.erl`
- **Security Guide**: `docs/SECURITY_GUIDE.md`
- **Router Intake Runbook**: `docs/OPS_RUNBOOK_ROUTER_INTAKE.md`
- **Gateway Rate Limiting Runbook**: `docs/OPS_RUNBOOK_GATEWAY_RATE_LIMITING.md`

