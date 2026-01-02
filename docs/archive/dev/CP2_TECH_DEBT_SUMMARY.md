# CP2 Tech Debt Summary

**Date**: 2025-01-27  
**Status**: 📋 **Tech Debt Inventory**  
**Purpose**: Brief list of PoC/experimental elements that should be either productionized or removed by CP3/Pre-Release  
**Related**: `docs/archive/dev/TECH_DEBT_ROUTER_GATEWAY_INTAKE_RATE_LIMIT.md`

## Executive Summary

This document provides a concise list of PoC/experimental elements identified during CP2 that require action by CP3/Pre-Release:
- **Productionize**: Convert PoC to production-ready code
- **Remove**: Delete if no longer needed
- **Defer**: Move to CP3+ if not critical for release

## PoC/Experimental Elements

### Gateway: Distributed Rate Limiting (Redis Backend)

**File**: `apps/c-gateway/src/rate_limiter_redis.c`

**Status**: ⚠️ **EXPERIMENTAL / PoC CODE**

**Current State**:
- PoC implementation with Redis backend
- Basic connection handling
- Minimal error handling
- No connection pooling

**Action Required** (CP3/Pre-Release):
- ✅ **Productionize**: Add connection pooling, retry logic, circuit breaker
- ✅ **Productionize**: Improve error handling and logging
- ✅ **Productionize**: Add comprehensive tests
- ✅ **Productionize**: Performance optimization

**Acceptance Criteria** (CP3/Pre-Release):
- ✅ **Observability**: Metrics for Redis operations (queries, errors, latency, connection pool stats)
- ✅ **Tests**: Unit tests (connection handling, retry logic), integration tests (Redis failure scenarios), load tests (multi-instance rate limiting)
- ✅ **Fault Tolerance**: Circuit breaker for Redis failures, graceful fallback to in-memory mode, connection pool health checks
- ✅ **Performance**: Connection pooling (min 5, max 20 connections), retry with exponential backoff (max 3 retries), latency < 5ms p95 for Redis operations
- ✅ **Documentation**: Production deployment guide, Redis configuration guide, monitoring and alerting guide

**Milestone**: **CP3/Pre-Release** (production-ready distributed rate limiting)

**Reference**: `docs/archive/dev/GATEWAY_DISTRIBUTED_RATE_LIMITING_STAGING_ROLLOUT.md`

**Priority**: **High** (needed for production scale-out)

---

### Gateway: Backpressure Client

**File**: `apps/c-gateway/src/backpressure_client.c`

**Status**: ⚠️ **EXPERIMENTAL / PoC CODE**

**Current State**:
- Simple HTTP client implementation (PoC)
- Basic metrics fetching from Router
- Minimal error handling

**Action Required** (CP3/Pre-Release):
- ✅ **Productionize**: Add proper HTTP client library (curl or similar)
- ✅ **Productionize**: Add retry logic and timeout handling
- ✅ **Productionize**: Add connection pooling
- ✅ **Productionize**: Add comprehensive tests

**Acceptance Criteria** (CP3/Pre-Release):
- ✅ **Observability**: Metrics for backpressure client operations (requests, errors, latency, cache hits/misses)
- ✅ **Tests**: Unit tests (HTTP client, caching), integration tests (Router backpressure detection), E2E tests (Gateway → Router backpressure flow)
- ✅ **Fault Tolerance**: Graceful handling of Router unavailability, cache fallback for stale data, timeout handling (5s default)
- ✅ **Performance**: Real-time updates (polling interval < 1s or NATS pub/sub), cache TTL < 5s, latency < 10ms p95 for status checks
- ✅ **Documentation**: Backpressure integration guide, monitoring and alerting guide, troubleshooting guide

**Milestone**: **CP3/Pre-Release** (complete backpressure integration)

**Reference**: `docs/archive/dev/TECH_DEBT_ROUTER_GATEWAY_INTAKE_RATE_LIMIT.md` (Backpressure section)

**Priority**: **Medium** (needed for complete backpressure integration)

---

### Router: Backpressure Logic

**File**: `apps/otp/router/src/router_intake_backpressure.erl`

**Status**: ⚠️ **EXPERIMENTAL** (partial implementation)

**Current State**:
- Backpressure detection implemented
- Metrics and thresholds defined
- Gateway integration incomplete (PoC)

**Action Required** (CP3/Pre-Release):
- ✅ **Productionize**: Complete Gateway → Router backpressure integration
- ✅ **Productionize**: End-to-end overload scenarios testing
- ✅ **Productionize**: Production-ready backpressure policies
- ✅ **Productionize**: Full observability integration

**Acceptance Criteria** (CP3/Pre-Release):
- ✅ **Observability**: Metrics for backpressure state (active/warning/inactive), queue depth, processing latency, in-flight messages, backpressure events
- ✅ **Tests**: Unit tests (backpressure detection logic), integration tests (Gateway → Router integration), E2E tests (overload scenarios, recovery)
- ✅ **Fault Tolerance**: Real-time JetStream consumer info queries (not cached), P95 calculation from histogram metrics, graceful degradation under overload
- ✅ **Performance**: Backpressure detection latency < 100ms, policy evaluation < 10ms, queue depth monitoring < 50ms
- ✅ **Documentation**: Backpressure policy guide, monitoring and alerting guide, troubleshooting guide, runbook for overload scenarios

**Milestone**: **CP3/Pre-Release** (production-ready backpressure management)

**Reference**: `docs/archive/dev/TECH_DEBT_ROUTER_GATEWAY_INTAKE_RATE_LIMIT.md` (Backpressure section)

**Priority**: **Medium** (needed for production overload management)

---

### Test Scripts: Distributed Rate Limiting

**File**: `scripts/test_distributed_rate_limiting.sh`

**Status**: ⚠️ **EXPERIMENTAL / PoC SCRIPT**

**Current State**:
- PoC script for testing distributed rate limiting
- Basic test scenarios
- Not integrated into CI

**Action Required** (CP3/Pre-Release):
- ✅ **Productionize**: Integrate into CI/CD pipeline
- ✅ **Productionize**: Add comprehensive test scenarios
- ✅ **Remove**: If replaced by proper test suite

**Acceptance Criteria** (CP3/Pre-Release):
- ✅ **Integration**: Script integrated into CI/CD pipeline (GitHub Actions, Drone CI, GitLab CI)
- ✅ **Test Coverage**: Comprehensive test scenarios (multi-instance rate limiting, Redis failure, fallback behavior)
- ✅ **Documentation**: Test script usage guide, CI integration guide
- ✅ **Alternative**: If replaced by proper test suite, remove script and update documentation

**Milestone**: **CP3/Pre-Release** (integrated or removed)

**Reference**: `docs/archive/dev/GATEWAY_DISTRIBUTED_RATE_LIMITING_STAGING_ROLLOUT.md`

**Priority**: **Low** (can be replaced by proper test suite)

---

### Deprecated Modules

**Files**:
- `apps/otp/router/src/router_acl.erl` - **DEPRECATED** (not used)
- `apps/otp/router/test/router_acl_SUITE.erl` - **DEPRECATED** (tests deprecated module)

**Status**: ⚠️ **DEPRECATED / TECH DEBT**

**Current State**:
- Marked as deprecated
- Not used in production code paths
- Kept for backward compatibility

**Action Required** (CP3/Pre-Release):
- ✅ **Remove**: Delete `router_acl.erl` after ensuring no external dependencies
- ✅ **Remove**: Delete `router_acl_SUITE.erl` after ensuring no test dependencies
- ✅ **Update**: Remove references from documentation

**Reference**: `apps/otp/router/docs/ACL_MODEL.md`

**Priority**: **Low** (cleanup, not blocking)

---

## Summary by Priority

### High Priority (CP3/Pre-Release Required)

1. **Gateway: Distributed Rate Limiting (Redis Backend)**
   - Productionize connection pooling, retry logic, circuit breaker
   - Needed for production scale-out

### Medium Priority (CP3/Pre-Release Recommended)

2. **Gateway: Backpressure Client**
   - Productionize HTTP client, retry logic, connection pooling
   - Needed for complete backpressure integration

3. **Router: Backpressure Logic**
   - Complete Gateway → Router integration
   - Needed for production overload management

### Low Priority (CP3/Pre-Release Optional)

4. **Test Scripts: Distributed Rate Limiting**
   - Integrate into CI or remove
   - Can be replaced by proper test suite

5. **Deprecated Modules**
   - Remove `router_acl.erl` and `router_acl_SUITE.erl`
   - Cleanup, not blocking

---

## Action Items for CP3/Pre-Release

### Must Complete (High Priority)

- [ ] Productionize `rate_limiter_redis.c`:
  - [ ] Add connection pooling
  - [ ] Add retry logic with exponential backoff
  - [ ] Add circuit breaker
  - [ ] Add comprehensive tests
  - [ ] Performance optimization

### Should Complete (Medium Priority)

- [ ] Productionize `backpressure_client.c`:
  - [ ] Add proper HTTP client library (curl or similar)
  - [ ] Add retry logic and timeout handling (5s default)
  - [ ] Add connection pooling
  - [ ] Add comprehensive tests (unit, integration, E2E)
  - [ ] Add observability (metrics for backpressure client operations)
  - [ ] Real-time updates (polling < 1s or NATS pub/sub)
  - [ ] Documentation (backpressure integration guide)

- [ ] Complete Router backpressure integration:
  - [ ] Complete Gateway → Router integration
  - [ ] End-to-end overload scenarios testing
  - [ ] Production-ready backpressure policies
  - [ ] Real-time JetStream consumer info queries (not cached)
  - [ ] P95 calculation from histogram metrics
  - [ ] Observability (metrics for backpressure state, queue depth, latency)
  - [ ] Documentation (backpressure policy guide, runbook)

### Nice to Have (Low Priority)

- [ ] Integrate or remove `test_distributed_rate_limiting.sh`
- [ ] Remove deprecated `router_acl.erl` and `router_acl_SUITE.erl`

---

## References

- `docs/archive/dev/TECH_DEBT_ROUTER_GATEWAY_INTAKE_RATE_LIMIT.md` - Detailed tech debt list
- `docs/archive/dev/GATEWAY_DISTRIBUTED_RATE_LIMITING_STAGING_ROLLOUT.md` - Distributed RL rollout plan
- `apps/otp/router/docs/ACL_MODEL.md` - ACL model (deprecation notes)

