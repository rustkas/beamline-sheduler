# Tech Debt: Router + Gateway Intake/Rate-Limit

**Date**: 2025-01-27  
**Status**: 📋 **Tech Debt Inventory**  
**Scope**: Router intake validation, Gateway rate limiting, distributed systems, observability  
**Target**: CP3/Release cleanup

## Purpose

This document lists technical debt items that need to be addressed before CP3/Release. Items are categorized by component and priority.

## Categories

- **🔴 Critical**: Blocks production readiness
- **🟡 High**: Important for production but not blocking
- **🟢 Medium**: Nice to have, can be deferred
- **⚪ Low**: Minor improvements

## Router Intake

### 🔴 Critical

#### 1. Backpressure: Real-time JetStream Consumer Info

**File**: `apps/otp/router/src/router_intake_backpressure.erl`

**Current State**:
- Uses cached ETS values for JetStream pending messages
- Uses cached ETS values for processing latency P95
- Marked as `⚠️ EXPERIMENTAL / PoC CODE`

**Issue**:
- Not real-time: relies on cached values instead of querying NATS API
- May miss backpressure triggers if cache is stale

**Required Changes**:
- Implement NATS API queries for JetStream consumer info
- Calculate P95 from histogram metrics in real-time
- Remove ETS cache dependency for critical metrics

**Priority**: **Critical** (affects backpressure accuracy)

---

#### 2. Observability Stub Replacement

**File**: `apps/otp/router/src/router_observability_stub.erl`

**Current State**:
- Stub implementation for Prometheus/OpenTelemetry
- Returns empty metrics/spans
- Marked as stub for CP1

**Issue**:
- No actual observability data emitted
- Metrics/spans not available for monitoring

**Required Changes**:
- Replace with `prometheus.erl` integration
- Replace with `opentelemetry_erlang` integration
- Remove stub module

**Priority**: **Critical** (blocks production monitoring)

---

### 🟡 High

#### 3. Admin NATS: Track Executed Extensions

**File**: `apps/otp/router/src/router_admin_nats.erl`

**Current State**:
- `executed_extensions => []` (TODO comment)

**Issue**:
- Cannot track which extensions were executed
- No audit trail for extension execution

**Required Changes**:
- Track executed extensions in response
- Add to audit logs

**Priority**: **High** (important for debugging/audit)

---

#### 4. Policy Validator: JSON Schema Validation

**File**: `apps/otp/router/src/router_policy_validator.erl`

**Current State**:
- Stub for JSON Schema validation (CP1)

**Issue**:
- No actual schema validation
- Invalid policies may be accepted

**Required Changes**:
- Implement JSON Schema validation
- Add schema files
- Validate policies against schema

**Priority**: **High** (important for data integrity)

---

### 🟢 Medium

#### 5. Rate Limiter: Tenant Tier Stub

**File**: `apps/otp/router/src/router_rate_limiter.erl`

**Current State**:
- `get_tenant_tier/1` returns stub value

**Issue**:
- No actual tenant tier lookup
- All tenants treated equally

**Required Changes**:
- Implement tenant tier lookup (from RouterAdmin or config)
- Apply tier-based rate limits

**Priority**: **Medium** (can be deferred if not needed for CP3)

---

## Gateway Rate Limiting

### 🔴 Critical

#### 6. Distributed Rate Limiting: Redis PoC → Production

**File**: `apps/c-gateway/src/rate_limiter_redis.c`

**Current State**:
- PoC implementation using Redis
- Fixed-window algorithm
- Basic error handling
- Marked as `⚠️ EXPERIMENTAL / PoC CODE`

**Issues**:
- No connection pooling
- No retry logic with exponential backoff
- No metrics for Redis operations
- No circuit breaker for Redis failures
- Fixed-window algorithm (may have burst issues)

**Required Changes**:
- Add connection pooling for Redis
- Implement retry logic with exponential backoff
- Add metrics for Redis operations (queries, errors, latency)
- Add circuit breaker for Redis failures
- Consider token bucket or sliding window algorithm
- Add comprehensive error handling
- Consider alternative backends (NATS JetStream, SQL)

**Priority**: **Critical** (blocks distributed rate limiting in production)

---

#### 7. Backpressure Client: HTTP Polling → Real-time

**File**: `apps/c-gateway/src/backpressure_client.c`

**Current State**:
- HTTP polling of Router metrics endpoint
- Simple socket-based HTTP client (no curl)
- Caching with periodic updates
- Marked as `⚠️ EXPERIMENTAL / PoC CODE`

**Issues**:
- Polling-based (not real-time)
- Simple HTTP client (no proper error handling)
- No connection pooling
- No retry logic

**Required Changes**:
- Replace HTTP polling with NATS pub/sub for real-time updates
- Or use Router's gRPC health service
- Add proper HTTP client library or gRPC client
- Add connection pooling
- Add retry logic with exponential backoff
- Add metrics for backpressure client operations

**Priority**: **Critical** (affects backpressure responsiveness)

---

### 🟡 High

#### 8. API Key Extraction from Authorization Header

**File**: `apps/c-gateway/src/http_server.c`

**Current State**:
- `const char *api_key = NULL; /* TODO: Extract from Authorization header */`
- Two locations (lines 648, 2017)

**Issue**:
- API key not extracted from Authorization header
- Rate limiting may not work correctly for authenticated requests

**Required Changes**:
- Implement Authorization header parsing
- Extract API key from Bearer token or API key header
- Use extracted API key for rate limiting

**Priority**: **High** (important for proper rate limiting)

---

#### 9. NATS Client Stub Replacement

**File**: `apps/c-gateway/src/nats_client_stub.h`

**Current State**:
- Stub implementation for NATS client

**Issue**:
- No actual NATS communication
- Gateway cannot publish to NATS

**Required Changes**:
- Replace with actual NATS client (nats.c or similar)
- Implement NATS publishing
- Add error handling and retry logic

**Priority**: **High** (blocks NATS integration)

---

## Test Infrastructure

### 🟡 High

#### 10. Distributed Rate Limiting Test Script

**File**: `scripts/test_distributed_rate_limiting.sh`

**Current State**:
- PoC test script for multi-instance rate limiting
- Marked as `⚠️ EXPERIMENTAL / PoC SCRIPT`

**Issue**:
- Not integrated into proper test suite
- Manual execution only

**Required Changes**:
- Integrate into CTest or proper test framework
- Or remove if replaced by unit/integration tests
- Add to CI/CD pipeline

**Priority**: **High** (important for test coverage)

---

## API/Interface Cleanup

### 🟡 High

#### 11. Rate Limiter Interface: Standardize Backend Selection

**File**: `apps/c-gateway/src/rate_limiter.h`

**Current State**:
- Abstract interface with backend selection
- Configuration via environment variables

**Issues**:
- Backend selection logic scattered
- No clear migration path from memory → distributed

**Required Changes**:
- Standardize backend selection API
- Add feature flags for gradual migration
- Document migration path
- Add metrics for backend usage

**Priority**: **High** (important for production deployment)

---

#### 12. Backpressure Interface: Standardize Status Format

**File**: `apps/c-gateway/src/backpressure_client.h`

**Current State**:
- Simple status enum (ACTIVE/INACTIVE)
- HTTP-based polling

**Issues**:
- Status format may not be sufficient for production
- No detailed backpressure metrics

**Required Changes**:
- Standardize backpressure status format
- Add detailed metrics (queue depth, latency, etc.)
- Document status interpretation

**Priority**: **High** (important for production monitoring)

---

## Documentation

### 🟢 Medium

#### 13. PoC Documentation Cleanup

**Files**:
- `docs/archive/dev/GATEWAY_DISTRIBUTED_RATE_LIMITING_POC_REPORT.md`
- `apps/c-gateway/README_RATE_LIMITING_POC.md`

**Current State**:
- PoC documentation exists
- May be outdated

**Required Changes**:
- Update or remove PoC documentation
- Mark as "superseded by production implementation" if replaced
- Or update to reflect current state

**Priority**: **Medium** (documentation cleanup)

---

## Summary

### By Priority

**🔴 Critical (3 items)**:
1. Backpressure: Real-time JetStream Consumer Info
2. Observability Stub Replacement
3. Distributed Rate Limiting: Redis PoC → Production
4. Backpressure Client: HTTP Polling → Real-time

**🟡 High (7 items)**:
5. Admin NATS: Track Executed Extensions
6. Policy Validator: JSON Schema Validation
7. API Key Extraction from Authorization Header
8. NATS Client Stub Replacement
9. Distributed Rate Limiting Test Script
10. Rate Limiter Interface: Standardize Backend Selection
11. Backpressure Interface: Standardize Status Format

**🟢 Medium (2 items)**:
12. Rate Limiter: Tenant Tier Stub
13. PoC Documentation Cleanup

### By Component

**Router (5 items)**:
- Backpressure: Real-time JetStream Consumer Info (🔴)
- Observability Stub Replacement (🔴)
- Admin NATS: Track Executed Extensions (🟡)
- Policy Validator: JSON Schema Validation (🟡)
- Rate Limiter: Tenant Tier Stub (🟢)

**Gateway (6 items)**:
- Distributed Rate Limiting: Redis PoC → Production (🔴)
- Backpressure Client: HTTP Polling → Real-time (🔴)
- API Key Extraction from Authorization Header (🟡)
- NATS Client Stub Replacement (🟡)
- Rate Limiter Interface: Standardize Backend Selection (🟡)
- Backpressure Interface: Standardize Status Format (🟡)

**Test Infrastructure (1 item)**:
- Distributed Rate Limiting Test Script (🟡)

**Documentation (1 item)**:
- PoC Documentation Cleanup (🟢)

## Migration Path

### Phase 1: Critical Items (CP3)

1. **Observability Stub Replacement** (Router)
   - Replace `router_observability_stub.erl` with `prometheus.erl` and `opentelemetry_erlang`
   - Add metrics/spans to all critical paths
   - Test metrics export

2. **Backpressure: Real-time JetStream Consumer Info** (Router)
   - Implement NATS API queries for JetStream consumer info
   - Calculate P95 from histogram metrics
   - Remove ETS cache dependency

3. **Distributed Rate Limiting: Redis PoC → Production** (Gateway)
   - Add connection pooling
   - Add retry logic with exponential backoff
   - Add metrics and circuit breaker
   - Consider algorithm improvements

4. **Backpressure Client: HTTP Polling → Real-time** (Gateway)
   - Replace HTTP polling with NATS pub/sub or gRPC
   - Add proper client library
   - Add connection pooling and retry logic

### Phase 2: High Priority Items (CP3+)

5. **API Key Extraction** (Gateway)
6. **NATS Client Stub Replacement** (Gateway)
7. **Policy Validator: JSON Schema Validation** (Router)
8. **Admin NATS: Track Executed Extensions** (Router)
9. **Test Script Integration** (Test Infrastructure)
10. **Interface Standardization** (Gateway)

### Phase 3: Medium Priority Items (Post-Release)

11. **Rate Limiter: Tenant Tier Stub** (Router)
12. **PoC Documentation Cleanup** (Documentation)

## References

- `apps/c-gateway/src/rate_limiter_redis.c`: Redis PoC implementation
- `apps/c-gateway/src/backpressure_client.c`: Backpressure client PoC
- `apps/otp/router/src/router_intake_backpressure.erl`: Backpressure implementation
- `apps/otp/router/src/router_observability_stub.erl`: Observability stub
- `scripts/test_distributed_rate_limiting.sh`: PoC test script
- `docs/ARCHITECTURE/gateway-distributed-rate-limiting.md`: Distributed rate limiting architecture
- `docs/archive/dev/GATEWAY_DISTRIBUTED_RATE_LIMITING_POC_REPORT.md`: PoC report

