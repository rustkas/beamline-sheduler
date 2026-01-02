# Extensions Pipeline Production Integration Plan

⚠️ **LEGACY**: Planning document. See `docs/archive/dev/EXTENSIONS_PIPELINE_PRODUCTION_INTEGRATION_REPORT.md` for current status.

**Version**: CP2-LC  
**Date**: 2025-01-27  
**Status**: ⚠️ **LEGACY** (Planning)  
**Workers**: wrk-2 (Router OTP), wrk-4 (Gateway TS/C), wrk-5 (UI-Web)  
**Current Source of Truth**: `docs/archive/dev/EXTENSIONS_PIPELINE_PRODUCTION_INTEGRATION_REPORT.md`

## Problem Statement

UI page currently uses mock endpoints. There is a risk of desynchronization between UI and real APIs.

## Solution Overview

Implement production-ready integration:
1. **Router**: Add real gRPC/HTTP endpoints for extension health, circuit breakers, and dry-run
2. **Gateway**: Proxy these Router methods to REST API
3. **UI-Web**: Switch from mock endpoints to real REST API (with dev/test fallback)

## Implementation Status

### ✅ Router (wrk-2) - Partially Complete

**Completed**:
- ✅ Added `get_all_circuit_states/0` function in `router_extension_circuit_breaker.erl`
- ✅ Added `get_extension_health/2`, `get_circuit_breaker_states/2`, `dry_run_pipeline/2` RPC methods in `router_admin_grpc.erl`
- ✅ Functions use existing `router_extension_health:get_all_health()` and new `router_extension_circuit_breaker:get_all_circuit_states()`

**Remaining**:
- ❌ Update proto file to include new RPC methods (requires proto regeneration)
- ❌ Register new RPC methods in `router_grpc_sup.erl` (if needed)
- ❌ Add NATS subjects for these methods (alternative to gRPC)
- ❌ Update documentation in `api-registry.md` and `PROTO_NATS_MAPPING.md`

### ❌ Gateway (wrk-4) - Not Started

**Required**:
- ❌ Add REST endpoints:
  - `GET /api/v1/extensions/health`
  - `GET /api/v1/extensions/circuit-breakers`
  - `POST /api/v1/policies/dry-run`
- ❌ Implement NATS/gRPC client calls to Router
- ❌ Add contract tests for these endpoints

**Options**:
1. **Option A**: Use NATS request-reply (like existing `/api/v1/routes/decide`)
2. **Option B**: Use gRPC directly (requires gRPC client library in C-Gateway)

**Recommendation**: Option A (NATS) for consistency with existing Gateway implementation.

### ❌ UI-Web (wrk-5) - Partially Complete

**Completed**:
- ✅ UI page created with mock endpoints
- ✅ Mock endpoints in `mock_gateway.ex` for testing

**Remaining**:
- ❌ Switch to real API endpoints (remove mock dependency)
- ❌ Add feature flag/config for dev/test mode (keep mocks available)
- ❌ Update error handling for real API responses

## Detailed Implementation Plan

### Phase 1: Router NATS Subjects (Alternative to gRPC)

**Add NATS subjects in Router**:
- `beamline.router.v1.admin.get_extension_health`
- `beamline.router.v1.admin.get_circuit_breaker_states`
- `beamline.router.v1.admin.dry_run_pipeline`

**Add NATS handlers in Router**:
- `router_nats_admin_subscriber.erl` (new module)
- Handlers call existing `router_admin_grpc` functions

**Benefits**:
- Consistent with existing Gateway → Router communication
- No need for gRPC client in Gateway
- Easier to test and debug

### Phase 2: Gateway REST Endpoints

**Add to `http_server.c`**:
```c
// GET /api/v1/extensions/health
static void handle_extensions_health(int client_fd, const request_context_t *ctx)

// GET /api/v1/extensions/circuit-breakers
static void handle_circuit_breakers(int client_fd, const request_context_t *ctx)

// POST /api/v1/policies/dry-run
static void handle_dry_run_pipeline(int client_fd, const char *body, const request_context_t *ctx)
```

**Add NATS client functions in `nats_client_real.c`**:
```c
int nats_request_get_extension_health(char *resp_buf, size_t resp_size)
int nats_request_get_circuit_breaker_states(char *resp_buf, size_t resp_size)
int nats_request_dry_run_pipeline(const char *req_json, char *resp_buf, size_t resp_size)
```

### Phase 3: UI-Web Integration

**Update `extensions_pipeline_live.ex`**:
- Remove dependency on mock endpoints
- Add config check for dev/test mode
- Use real Gateway endpoints in production

**Add config in `config/dev.exs`**:
```elixir
config :ui_web, :use_mock_gateway, true  # Only in dev/test
```

**Update `extensions_pipeline_live.ex`**:
```elixir
defp load_extension_health(socket) do
  if Application.get_env(:ui_web, :use_mock_gateway, false) do
    # Use mock
    load_extension_health_mock(socket)
  else
    # Use real API
    case GatewayClient.get_json("/api/v1/extensions/health") do
      {:ok, health_map} -> assign(socket, extension_health: health_map, error: nil)
      {:error, reason} -> socket
    end
  end
end
```

## Testing Strategy

### Router Tests
- Unit tests for new RPC methods
- Integration tests for NATS handlers (if using NATS)

### Gateway Tests
- Contract tests for new REST endpoints
- Integration tests with Router (via NATS)

### UI-Web Tests
- Update existing tests to use real API (when available)
- Keep mock tests for dev/test mode

## Documentation Updates

### Required Updates
1. **`docs/ARCHITECTURE/api-registry.md`**:
   - Add new REST endpoints
   - Document request/response DTOs
   - Document error codes

2. **`docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`**:
   - Add NATS subjects for admin operations
   - Document JSON message formats

3. **`apps/otp/router/docs/API_CONTRACTS.md`**:
   - Add new gRPC RPC methods (if using gRPC)
   - Document request/response structures

## Acceptance Criteria

### ✅ Router
- [ ] New RPC methods implemented and tested
- [ ] Proto file updated (if using gRPC)
- [ ] NATS subjects added (if using NATS)
- [ ] Documentation updated

### ✅ Gateway
- [ ] REST endpoints implemented
- [ ] NATS/gRPC integration working
- [ ] Contract tests added
- [ ] Error handling implemented

### ✅ UI-Web
- [ ] Uses real API in production
- [ ] Mocks available in dev/test mode
- [ ] Feature flag/config documented
- [ ] Tests updated

## Next Steps

1. **Immediate**: Complete Router implementation (NATS subjects or gRPC registration)
2. **Next**: Implement Gateway REST endpoints
3. **Then**: Update UI-Web to use real API
4. **Finally**: Update documentation and tests

## References

- `apps/otp/router/src/router_admin_grpc.erl` - Admin gRPC service
- `apps/otp/router/src/router_extension_health.erl` - Health metrics
- `apps/otp/router/src/router_extension_circuit_breaker.erl` - Circuit breaker
- `apps/c-gateway/src/http_server.c` - HTTP server
- `apps/c-gateway/src/nats_client_real.c` - NATS client
- `apps/ui_web/lib/ui_web_web/live/extensions_pipeline_live.ex` - UI page

