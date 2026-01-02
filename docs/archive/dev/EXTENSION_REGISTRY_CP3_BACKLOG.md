# Extension Registry CP3 Backlog

⚠️ **APPENDIX**: Future backlog document. See `docs/archive/dev/EXTENSION_REGISTRY_IMPLEMENTATION_REPORT.md` for current implementation.

**Status**: ⚠️ **APPENDIX** (Future Backlog)

**Date**: 2025-01-27  
**Status**: Backlog (CP3)  
**Target**: CP3

---

## Executive Summary

Advanced features for Extension Registry: circuit breaker, health monitoring, and automatic management.

**Features**:
- Circuit breaker for frequently failing extensions
- Real-time health monitoring
- Automatic disable/enable based on health
- Health-based routing

---

## Circuit Breaker Design

### State Machine

```
Closed (Normal)
  ↓ (failure threshold exceeded)
Open (Failing)
  ↓ (timeout elapsed)
Half-Open (Testing)
  ↓ (success)          ↓ (failure)
Closed                Open
```

### States

**Closed** (Normal):
- Extension working normally
- All requests pass through
- Track success/failure counts
- Update health metrics

**Open** (Failing):
- Extension failing repeatedly
- Requests fail fast (no invocation)
- Health check periodically attempts recovery
- Automatic disable (optional)

**Half-Open** (Testing):
- Testing if extension recovered
- Limited requests allowed (e.g., 3)
- On success → Closed
- On failure → Open

### Configuration

```erlang
{circuit_breaker_enabled, true},
{circuit_breaker_failure_threshold, 5},  % Open after 5 failures
{circuit_breaker_timeout_seconds, 60},  % Attempt recovery after 60s
{circuit_breaker_half_open_max_requests, 3}  % Max requests in half-open
```

### Implementation

**Module**: `apps/otp/router/src/router_extension_circuit_breaker.erl`

**Functions**:
- `check_circuit/1` - Check if circuit is open
- `record_success/1` - Record successful invocation
- `record_failure/1` - Record failed invocation
- `update_state/1` - Update circuit breaker state
- `should_attempt_recovery/1` - Check if recovery should be attempted

**Database Integration**:
- Update `extension_health` table
- Track circuit breaker state
- Store failure timestamps

---

## Health Monitoring

### Metrics Tracked

**Per Extension**:
- Success count
- Failure count
- Success rate (success / total)
- Average latency (ms)
- Last success timestamp
- Last failure timestamp
- Circuit breaker state

**Aggregated**:
- Total extensions
- Enabled extensions
- Healthy extensions (circuit closed)
- Unhealthy extensions (circuit open)
- Average success rate
- Average latency

### Update Frequency

**Real-time**:
- On every extension invocation
- Async update to database
- Cached in Mnesia/ETS

**Periodic**:
- Aggregate metrics every 60 seconds
- Update dashboard
- Generate alerts

### Health Check API

**Endpoint**: `GET /admin/extensions/health`

**Response**:
```json
{
  "extensions": [
    {
      "id": "normalize_text",
      "type": "pre",
      "health": {
        "success_rate": 0.95,
        "avg_latency_ms": 45.2,
        "circuit_breaker_state": "closed",
        "last_success": "2025-01-27T12:00:00Z",
        "last_failure": "2025-01-27T11:55:00Z"
      }
    }
  ],
  "summary": {
    "total": 10,
    "healthy": 9,
    "unhealthy": 1,
    "avg_success_rate": 0.92
  }
}
```

---

## Automatic Management

### Auto-Disable on Failure

**Trigger**: Circuit breaker opens

**Action**: Set `enabled = false` in database

**Configuration**:
```erlang
{auto_disable_on_circuit_open, true}
```

**Workflow**:
1. Extension fails repeatedly
2. Circuit breaker opens
3. Auto-disable extension
4. Log audit event
5. Notify administrators

### Auto-Enable on Recovery

**Trigger**: Circuit breaker closes after recovery

**Action**: Set `enabled = true` in database

**Configuration**:
```erlang
{auto_enable_on_circuit_close, true}
```

**Workflow**:
1. Extension recovers (circuit closes)
2. Auto-enable extension
3. Log audit event
4. Notify administrators

### Health-Based Routing

**Purpose**: Route away from unhealthy extensions

**Implementation**:
- Check extension health before invocation
- Skip unhealthy extensions (circuit open)
- Prefer healthy extensions
- Fallback to alternative extensions

**Configuration**:
```erlang
{health_based_routing_enabled, true},
{health_based_routing_min_success_rate, 0.8}  % Minimum success rate
```

---

## Implementation Tasks

### Task 1: Circuit Breaker Module

**File**: `apps/otp/router/src/router_extension_circuit_breaker.erl`

**Functions**:
- State machine implementation
- Failure threshold tracking
- Recovery logic
- Database integration

**Estimated Time**: 6-8 hours

**Dependencies**: Extension Registry database integration (CP2-LC)

### Task 2: Health Monitoring

**File**: `apps/otp/router/src/router_extension_health.erl`

**Functions**:
- Health metrics collection
- Database updates
- Health check API
- Dashboard integration

**Estimated Time**: 4-6 hours

**Dependencies**: Task 1

### Task 3: Automatic Management

**File**: `apps/otp/router/src/router_extension_auto_manager.erl`

**Functions**:
- Auto-disable on failure
- Auto-enable on recovery
- Health-based routing
- Notification system

**Estimated Time**: 4-6 hours

**Dependencies**: Task 1, Task 2

### Task 4: Admin API Integration

**File**: `apps/otp/router/src/router_admin_grpc.erl` (update)

**Functions**:
- Health check endpoint
- Circuit breaker status
- Manual enable/disable
- Health metrics

**Estimated Time**: 2-3 hours

**Dependencies**: Task 2

---

## Database Schema Updates

### Extension Health Table (Already Created)

**Table**: `extension_health`

**Fields**:
- `circuit_breaker_state` - State: closed, open, half_open
- `circuit_breaker_opened_at` - When circuit opened
- `circuit_breaker_failure_threshold` - Failure threshold

**Updates Required**:
- Add `half_open_request_count` field
- Add `half_open_max_requests` field
- Add `auto_disabled` field

### Migration

```sql
ALTER TABLE extension_health
ADD COLUMN IF NOT EXISTS half_open_request_count INTEGER DEFAULT 0,
ADD COLUMN IF NOT EXISTS half_open_max_requests INTEGER DEFAULT 3,
ADD COLUMN IF NOT EXISTS auto_disabled BOOLEAN DEFAULT FALSE;
```

---

## Configuration

### Circuit Breaker

```erlang
{extension_registry, [
  {circuit_breaker_enabled, true},
  {circuit_breaker_failure_threshold, 5},
  {circuit_breaker_timeout_seconds, 60},
  {circuit_breaker_half_open_max_requests, 3}
]}
```

### Automatic Management

```erlang
{extension_registry, [
  {auto_disable_on_circuit_open, true},
  {auto_enable_on_circuit_close, true},
  {health_based_routing_enabled, true},
  {health_based_routing_min_success_rate, 0.8}
]}
```

---

## Monitoring and Alerts

### Metrics

**Circuit Breaker**:
- `extension_circuit_breaker_state{extension_id}` - Circuit state (0=closed, 1=open, 2=half_open)
- `extension_circuit_breaker_failures_total{extension_id}` - Total failures
- `extension_circuit_breaker_recoveries_total{extension_id}` - Total recoveries

**Health**:
- `extension_health_success_rate{extension_id}` - Success rate (0.0-1.0)
- `extension_health_avg_latency_ms{extension_id}` - Average latency
- `extension_health_auto_disabled_total` - Total auto-disabled extensions

### Alerts

**Circuit Breaker Opened**:
- Alert when circuit opens
- Include extension ID, failure count, last failure time

**Auto-Disable**:
- Alert when extension auto-disabled
- Include extension ID, reason, health metrics

**High Failure Rate**:
- Alert when success rate < threshold
- Include extension ID, current success rate

---

## Testing Strategy

### Unit Tests

- Circuit breaker state machine
- Health metrics collection
- Auto-disable/enable logic

### Integration Tests

- Circuit breaker with real extensions
- Health monitoring end-to-end
- Auto-management workflows

### Load Tests

- Circuit breaker performance
- Health metrics update performance
- Concurrent circuit state updates

---

## Success Criteria

- ✅ Circuit breaker functional
- ✅ Health monitoring operational
- ✅ Automatic disable/enable works
- ✅ Health-based routing implemented
- ✅ Admin API endpoints available
- ✅ Metrics and alerts configured

---

## References

- `docs/archive/dev/EXTENSION_REGISTRY_PRODUCTION_DESIGN.md` - Production design
- `docs/archive/dev/EXTENSION_REGISTRY_MIGRATION_PLAN.md` - Migration plan
- `sql/011_extensions_registry.sql` - Database schema
- `apps/otp/router/src/router_extension_registry.erl` - Current implementation

