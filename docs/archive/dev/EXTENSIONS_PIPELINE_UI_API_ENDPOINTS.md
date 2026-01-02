# Extensions Pipeline UI API Endpoints

**Version**: CP2-LC  
**Date**: 2025-01-27  
**Status**: Proposed  
**Worker**: wrk-5 (UI-Web)

## Purpose

This document describes the API endpoints required for the Extensions Pipeline Inspector UI. These endpoints provide access to extension registry, health metrics, circuit breaker states, and dry-run functionality.

## Required Endpoints

### 1. List Extensions

**Endpoint**: `GET /api/v1/extensions`

**Description**: Returns list of all registered extensions with their metadata.

**Query Parameters**:
- `type` (optional): Filter by extension type (`pre`, `validator`, `post`, `provider`)

**Response**:
```json
{
  "items": [
    {
      "id": "normalize_text",
      "type": "pre",
      "subject": "beamline.ext.pre.normalize_text.v1",
      "version": "1.0.0",
      "timeout_ms": 100,
      "retry": 0,
      "enabled": true,
      "instances": 2
    }
  ]
}
```

**Status**: ✅ Already exists (via Gateway)

### 2. Get Extension Health

**Endpoint**: `GET /api/v1/extensions/health`

**Description**: Returns health metrics for all extensions.

**Response**:
```json
{
  "normalize_text": {
    "extension_id": "normalize_text",
    "status": "healthy",
    "success_rate": 0.995,
    "avg_latency_ms": 15.5,
    "p50_latency_ms": 12.0,
    "p95_latency_ms": 25.0,
    "p99_latency_ms": 35.0,
    "last_success": "2025-01-27T12:00:00Z",
    "last_failure": null
  }
}
```

**Status**: ❌ **Needs Implementation** (Router gRPC or Gateway REST)

**Implementation Options**:
1. **Option A**: Add to Router gRPC Admin API (`GetExtensionHealth`)
2. **Option B**: Add to Gateway REST API (proxy to Router)
3. **Option C**: Direct database query (if UI has DB access)

**Recommendation**: Option B (Gateway REST API) for consistency with existing endpoints.

### 3. Get Circuit Breaker States

**Endpoint**: `GET /api/v1/extensions/circuit-breakers`

**Description**: Returns circuit breaker state for all extensions.

**Response**:
```json
{
  "normalize_text": {
    "extension_id": "normalize_text",
    "state": "closed",
    "opened_at": null,
    "failure_count": 0,
    "error_rate": 0.0
  }
}
```

**Status**: ❌ **Needs Implementation** (Router gRPC or Gateway REST)

**Implementation Options**: Same as Extension Health endpoint.

### 4. Dry Run Pipeline

**Endpoint**: `POST /api/v1/policies/dry-run`

**Description**: Executes a dry-run of the extension pipeline for a given policy and payload.

**Request**:
```json
{
  "tenant_id": "tenant_dev",
  "policy_id": "default",
  "payload": {
    "message": "test"
  },
  "dry_run": true
}
```

**Response**:
```json
{
  "ok": true,
  "result": {
    "executed_extensions": [
      {
        "extension_id": "normalize_text",
        "type": "pre",
        "status": "success",
        "latency_ms": 12.5,
        "output": {
          "payload": "TEST"
        }
      }
    ],
    "blocked_by": null,
    "final_payload": {
      "message": "TEST"
    },
    "provider_selected": "openai",
    "post_processors_executed": [
      {
        "extension_id": "mask_pii",
        "type": "post",
        "status": "success",
        "latency_ms": 8.3
      }
    ]
  }
}
```

**Status**: ❌ **Needs Implementation** (Router gRPC or Gateway REST)

**Implementation Options**:
1. **Option A**: Add to Router gRPC Admin API (`DryRunPipeline`)
2. **Option B**: Add to Gateway REST API (proxy to Router)
3. **Option C**: Implement in Gateway (requires Router integration)

**Recommendation**: Option A (Router gRPC Admin API) for consistency with other admin operations.

## Implementation Plan

### Phase 1: Mock Endpoints (UI Development)

**For UI development and testing**, create mock endpoints in Gateway:

1. `GET /api/v1/extensions/health` → Returns mock health data
2. `GET /api/v1/extensions/circuit-breakers` → Returns mock circuit breaker states
3. `POST /api/v1/policies/dry-run` → Returns mock dry-run result

**Status**: ✅ Can be implemented in Gateway mock server for UI testing

### Phase 2: Real Implementation (Production)

**For production**, implement real endpoints:

1. **Router gRPC Admin API**:
   - `GetExtensionHealth` RPC
   - `GetCircuitBreakerStates` RPC
   - `DryRunPipeline` RPC

2. **Gateway REST API** (proxy to Router):
   - `GET /api/v1/extensions/health` → Calls Router `GetExtensionHealth`
   - `GET /api/v1/extensions/circuit-breakers` → Calls Router `GetCircuitBreakerStates`
   - `POST /api/v1/policies/dry-run` → Calls Router `DryRunPipeline`

## ADR Reference

**Decision**: Use Gateway REST API as proxy to Router gRPC Admin API for consistency with existing UI endpoints.

**Rationale**:
- UI already uses Gateway REST API
- Gateway provides authentication/authorization layer
- Consistent error handling and response format
- Easier to test and mock

## References

- `apps/otp/router/src/router_extension_health.erl` - Health metrics module
- `apps/otp/router/src/router_extension_circuit_breaker.erl` - Circuit breaker module
- `apps/otp/router/src/router_decider.erl` - Pipeline execution logic
- `apps/ui_web/lib/ui_web_web/live/extensions_pipeline_live.ex` - UI implementation

