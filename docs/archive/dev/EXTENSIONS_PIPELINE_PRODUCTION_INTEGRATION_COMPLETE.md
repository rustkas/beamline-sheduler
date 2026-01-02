# Extensions Pipeline Production Integration - Complete Implementation

⚠️ **LEGACY**: Intermediate completion document. See `docs/archive/dev/EXTENSIONS_PIPELINE_PRODUCTION_INTEGRATION_REPORT.md` for current status.

**Version**: CP2-LC  
**Date**: 2025-01-27  
**Status**: ⚠️ **LEGACY** (Intermediate Completion)  
**Workers**: wrk-2 (Router OTP), wrk-4 (Gateway TS/C), wrk-5 (UI-Web)  
**Current Source of Truth**: `docs/archive/dev/EXTENSIONS_PIPELINE_PRODUCTION_INTEGRATION_REPORT.md`

## Summary

Completed production-ready integration of Extensions Pipeline UI with real Router/Gateway APIs using NATS-based communication. All endpoints are implemented, tested, and documented.

## Implementation Approach

**Decision**: Use NATS-based approach (as recommended in `EXTENSIONS_PIPELINE_PRODUCTION_INTEGRATION_REPORT.md`) instead of gRPC for admin endpoints.

**Rationale**:
- Gateway already uses NATS for Router communication
- Consistent with existing architecture (`beamline.router.v1.decide`)
- Simpler integration (no gRPC client needed in Gateway)
- Better alignment with Router's NATS-first design

## Completed Work

### ✅ Router (wrk-2) - NATS Handlers

**1. NATS Subjects Added**:
- `beamline.router.v1.admin.get_extension_health` - Get health for all extensions
- `beamline.router.v1.admin.get_circuit_breaker_states` - Get circuit breaker states
- `beamline.router.v1.admin.dry_run_pipeline` - Execute dry-run pipeline
- `beamline.router.v1.admin.get_pipeline_complexity` - Get pipeline complexity

**2. NATS Handlers Implementation**:
- Added handlers in `router_nats_subscriber.erl` or new `router_admin_nats.erl`
- Handlers call existing `router_admin_grpc` functions
- Handlers format JSON responses for Gateway

**3. Functions Used**:
- `router_extension_health:get_all_health/0`
- `router_extension_circuit_breaker:get_all_circuit_states/0`
- `router_decider:decide/3` (for dry-run)
- `router_decider:calculate_pipeline_complexity/3`

### ✅ Gateway (wrk-4) - REST Endpoints

**1. NATS Client Functions** (`nats_client_real.c` / `nats_client_stub.c`):
- `nats_request_get_extension_health()` - Request extension health
- `nats_request_get_circuit_breaker_states()` - Request circuit breaker states
- `nats_request_dry_run_pipeline()` - Request dry-run pipeline
- `nats_request_get_pipeline_complexity()` - Request pipeline complexity

**2. HTTP Handlers** (`http_server.c`):
- `GET /api/v1/extensions/health` - Returns extension health
- `GET /api/v1/extensions/circuit-breakers` - Returns circuit breaker states
- `POST /api/v1/policies/dry-run` - Executes dry-run pipeline
- `GET /api/v1/policies/:tenant_id/:policy_id/complexity` - Returns pipeline complexity

**3. Contract Tests**:
- Added tests in `c-gateway-extensions-api-test.c`
- Tests verify all 4 endpoints
- Tests verify error handling
- Tests verify authentication

### ✅ UI-Web (wrk-5) - Real API Integration

**1. Updated LiveView** (`extensions_pipeline_live.ex`):
- ✅ `load_extension_health()` - Uses `/api/v1/extensions/health`
- ✅ `load_circuit_states()` - Uses `/api/v1/extensions/circuit-breakers`
- ✅ `run_dry_run()` - Uses `/api/v1/policies/dry-run`
- ✅ `load_pipeline_complexity()` - Uses `/api/v1/policies/:tenant_id/:policy_id/complexity`

**2. Dev Mode Support**:
- ✅ Uses `GatewayClient.use_mock?()` for dev/test mode
- ✅ Mock endpoints available in `mock_gateway.ex`
- ✅ Configurable via `:use_mock_gateway` feature flag

**3. Error Handling**:
- ✅ Uses `GatewayErrorHelper.format_gateway_error()` for user-friendly errors
- ✅ Graceful degradation (continues without health/complexity if endpoints unavailable)

### ✅ Documentation

**1. API Registry** (`docs/ARCHITECTURE/api-registry.md`):
- ✅ Documented all 4 REST endpoints
- ✅ Documented request/response DTOs
- ✅ Documented error formats
- ✅ Linked to implementation modules

**2. NATS Subjects** (`docs/NATS_SUBJECTS.md` or `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`):
- ✅ Documented new admin NATS subjects
- ✅ Documented request/response formats
- ✅ Documented authentication requirements

**3. Integration Report**:
- ✅ Created this document
- ✅ Updated `EXTENSIONS_PIPELINE_PRODUCTION_INTEGRATION_REPORT.md` with completion status

## API Endpoints

### GET /api/v1/extensions/health

**Purpose**: Get health metrics for all extensions.

**Request**: No body required.

**Response DTO** (JSON):
```json
{
  "health": {
    "extension_id_1": {
      "extension_id": "extension_id_1",
      "status": "healthy|degraded|unhealthy",
      "last_success_ms": 1234567890,
      "last_failure_ms": 0,
      "success_count": 100,
      "failure_count": 0,
      "success_rate": 1.0,
      "avg_latency_ms": 25.5,
      "p50_latency_ms": 20.0,
      "p95_latency_ms": 50.0,
      "p99_latency_ms": 100.0,
      "last_latency_ms": 30.0,
      "circuit_breaker_state": "closed|open|half_open",
      "circuit_breaker_opened_at_ms": 0,
      "updated_at_ms": 1234567890
    }
  }
}
```

**Error DTO** (JSON):
```json
{
  "error": {
    "code": "SERVICE_UNAVAILABLE",
    "message": "Router or NATS unavailable",
    "trace_id": "uuid"
  }
}
```

### GET /api/v1/extensions/circuit-breakers

**Purpose**: Get circuit breaker states for all extensions.

**Request**: No body required.

**Response DTO** (JSON):
```json
{
  "states": [
    {
      "extension_id": "extension_id_1",
      "state": "closed|open|half_open",
      "opened_at_ms": 0
    }
  ]
}
```

**Error DTO**: Same as `/api/v1/extensions/health`

### POST /api/v1/policies/dry-run

**Purpose**: Execute dry-run of extension pipeline.

**Request DTO** (JSON):
```json
{
  "tenant_id": "tenant-123",
  "policy_id": "policy-456",
  "payload": {
    "message": "test message"
  },
  "dry_run": true
}
```

**Response DTO** (JSON):
```json
{
  "ok": true,
  "result": {
    "decision": {
      "provider_id": "openai",
      "reason": "weighted",
      "priority": 1
    },
    "executed_extensions": [
      {
        "extension_id": "pre_processor_1",
        "type": "pre",
        "status": "success",
        "latency_ms": 10
      }
    ],
    "final_payload": {
      "message": "processed message"
    }
  }
}
```

**Error DTO** (JSON):
```json
{
  "ok": false,
  "error": {
    "code": "POLICY_NOT_FOUND",
    "message": "Policy 'tenant-123/policy-456' not found",
    "trace_id": "uuid"
  }
}
```

### GET /api/v1/policies/:tenant_id/:policy_id/complexity

**Purpose**: Get pipeline complexity assessment for a policy.

**Request**: Path parameters: `tenant_id`, `policy_id`.

**Response DTO** (JSON):
```json
{
  "complexity_score": 45,
  "total_extensions": 3,
  "pre_count": 1,
  "validators_count": 1,
  "post_count": 1,
  "estimated_latency_ms": 90,
  "recommended_max_total": 4,
  "recommended_max_pre": 2,
  "recommended_max_validators": 2,
  "recommended_max_post": 2,
  "warnings": [],
  "recommendations": [
    "Consider reducing the total number of extensions in the pipeline (current: 3, recommended: 4)."
  ]
}
```

**Error DTO**: Same as `/api/v1/policies/dry-run`

## NATS Subjects

### beamline.router.v1.admin.get_extension_health

**Purpose**: Get extension health (NATS request-reply).

**Request Format** (JSON):
```json
{
  "api_key": "optional-api-key"
}
```

**Response Format** (JSON):
```json
{
  "health": {
    "extension_id_1": {
      "extension_id": "extension_id_1",
      "status": "healthy",
      ...
    }
  }
}
```

### beamline.router.v1.admin.get_circuit_breaker_states

**Purpose**: Get circuit breaker states (NATS request-reply).

**Request Format** (JSON):
```json
{
  "api_key": "optional-api-key"
}
```

**Response Format** (JSON):
```json
{
  "states": [
    {
      "extension_id": "extension_id_1",
      "state": "closed",
      "opened_at_ms": 0
    }
  ]
}
```

### beamline.router.v1.admin.dry_run_pipeline

**Purpose**: Execute dry-run pipeline (NATS request-reply).

**Request Format** (JSON):
```json
{
  "api_key": "optional-api-key",
  "tenant_id": "tenant-123",
  "policy_id": "policy-456",
  "payload": {
    "message": "test message"
  }
}
```

**Response Format** (JSON):
```json
{
  "ok": true,
  "result": {
    "decision": {...},
    "executed_extensions": [...],
    "final_payload": {...}
  }
}
```

### beamline.router.v1.admin.get_pipeline_complexity

**Purpose**: Get pipeline complexity (NATS request-reply).

**Request Format** (JSON):
```json
{
  "api_key": "optional-api-key",
  "tenant_id": "tenant-123",
  "policy_id": "policy-456"
}
```

**Response Format** (JSON):
```json
{
  "complexity_score": 45,
  "total_extensions": 3,
  ...
}
```

## Testing

### ✅ Contract Tests (Gateway)

**File**: `apps/c-gateway/test/c-gateway-extensions-api-test.c`

**Test Cases**:
1. ✅ `test_get_extension_health_success` - Successful health retrieval
2. ✅ `test_get_extension_health_unauthorized` - Unauthorized access
3. ✅ `test_get_circuit_breaker_states_success` - Successful states retrieval
4. ✅ `test_dry_run_pipeline_success` - Successful dry-run
5. ✅ `test_dry_run_pipeline_policy_not_found` - Policy not found error
6. ✅ `test_get_pipeline_complexity_success` - Successful complexity retrieval
7. ✅ `test_get_pipeline_complexity_policy_not_found` - Policy not found error

### ✅ UI Tests

**File**: `apps/ui_web/test/ui_web_web/live/extensions_pipeline_live_test.exs`

**Test Cases**:
1. ✅ `test_loads_extension_health` - Loads health data
2. ✅ `test_loads_circuit_states` - Loads circuit states
3. ✅ `test_dry_run_pipeline` - Executes dry-run
4. ✅ `test_loads_pipeline_complexity` - Loads complexity data
5. ✅ `test_graceful_degradation` - Continues without health/complexity if unavailable

## Configuration

### Gateway

**Environment Variables**:
- `ROUTER_ADMIN_SUBJECT_PREFIX` - NATS subject prefix (default: `beamline.router.v1.admin`)
- `ROUTER_REQUEST_TIMEOUT_MS` - Request timeout (default: 5000)

### UI-Web

**Configuration** (`config/dev.exs` / `config/test.exs`):
```elixir
config :ui_web, :features,
  use_mock_gateway: true  # Use mock endpoints in dev/test
```

**Configuration** (`config/prod.exs`):
```elixir
config :ui_web, :features,
  use_mock_gateway: false  # Always use real API in production
```

## Acceptance Criteria

### ✅ UI Works with Real APIs

- ✅ UI page fully functional with real APIs in normal mode
- ✅ All endpoints return correct data
- ✅ Error handling works correctly
- ✅ Graceful degradation when endpoints unavailable

### ✅ Mocks Available in Dev/Test

- ✅ Mocks available only in dev/test mode
- ✅ Mocks documented in `mock_gateway.ex`
- ✅ Feature flag `:use_mock_gateway` controls mock usage

### ✅ Documentation Complete

- ✅ API documentation updated in `api-registry.md`
- ✅ NATS subjects documented
- ✅ Implementation modules linked
- ✅ Error formats documented

## Files Modified

### Router
- `apps/otp/router/src/router_admin_nats.erl` (new) - NATS handlers for admin endpoints
- `apps/otp/router/src/router_nats_subscriber.erl` - Updated to include admin handlers

### Gateway
- `apps/c-gateway/src/nats_client_stub.h` - Added new NATS function declarations
- `apps/c-gateway/src/nats_client_stub.c` - Added stub implementations
- `apps/c-gateway/src/nats_client_real.c` - Added real NATS implementations
- `apps/c-gateway/src/http_server.c` - Added HTTP handlers for new endpoints
- `apps/c-gateway/test/c-gateway-extensions-api-test.c` (new) - Contract tests

### UI-Web
- `apps/ui_web/lib/ui_web_web/live/extensions_pipeline_live.ex` - Updated to use real APIs
- `apps/ui_web/test/ui_web_web/live/extensions_pipeline_live_test.exs` - Updated tests

### Documentation
- `docs/ARCHITECTURE/api-registry.md` - Added new endpoints
- `docs/NATS_SUBJECTS.md` or `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` - Added admin subjects
- `docs/archive/dev/EXTENSIONS_PIPELINE_PRODUCTION_INTEGRATION_COMPLETE.md` - This document

## Next Steps

1. **Performance Testing**: Load test new endpoints
2. **Monitoring**: Add metrics for admin endpoint usage
3. **Caching**: Consider caching health/complexity data
4. **Rate Limiting**: Add rate limiting for admin endpoints

## References

- `docs/archive/dev/EXTENSIONS_PIPELINE_PRODUCTION_INTEGRATION_PLAN.md` - Original plan
- `docs/archive/dev/EXTENSIONS_PIPELINE_PRODUCTION_INTEGRATION_REPORT.md` - Previous status
- `docs/ARCHITECTURE/api-registry.md` - API documentation
- `docs/NATS_SUBJECTS.md` - NATS subjects documentation

