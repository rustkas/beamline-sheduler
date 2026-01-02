# Extensions Contract E2E Implementation Report

**Version**: CP2-LC  
**Date**: 2025-01-27  
**Status**: ✅ Complete  
**Workers**: wrk-2 (Router OTP), wrk-4 (Gateway TS)

## Summary

Implemented contract-based E2E testing for Gateway ↔ Router extensions errors, ensuring stable error mapping and documented error DTOs.

## Tasks Completed

### 1. Router Error Mapping (wrk-2)

**Created**: `apps/otp/router/src/router_extension_error_mapper.erl`

- **Purpose**: Maps extension-related errors to Gateway-compatible error codes and messages
- **Functions**:
  - `map_extension_error/1`: Main mapping function
  - `map_extension_error_to_code/1`: Maps error reason to Gateway error code
  - `map_extension_error_to_message/2`: Maps error reason to human-readable message
  - `build_error_details/2`: Builds error details map with metadata

**Error Codes Mapped**:
- `extension_not_found` → `extension_not_found` (404)
- `extension_timeout` → `extension_timeout` (504)
- `validator_blocked` → `validator_blocked` (403)
- `post_processor_failed` → `post_processor_failed` (500)
- `extension_circuit_open` → `extension_unavailable` (503)
- `extension_invocation_error` → `extension_error` (500)
- `extension_max_retries_exceeded` → `extension_timeout` (504)
- `extension_registry_error` → `extension_error` (500)
- `extension_load_balancer_error` → `extension_error` (500)
- `pipeline_too_deep` → `invalid_request` (400)
- `too_many_pre_processors` → `invalid_request` (400)
- `too_many_validators` → `invalid_request` (400)
- `too_many_post_processors` → `invalid_request` (400)

**Updated Modules**:
- `router_decide_consumer.erl`: Integrated extension error mapper
- `router_nats_subscriber.erl`: Integrated extension error mapper
- Both modules now detect extension errors and use the mapper for consistent error formatting

### 2. Gateway Contract Tests (wrk-4)

**Created**: `apps/c-gateway/tests/c-gateway-router-extension-errors-test.c`

**Test Cases**:
1. `test_status_extension_not_found_404`: Verifies `extension_not_found` maps to HTTP 404
2. `test_status_extension_timeout_504`: Verifies `extension_timeout` maps to HTTP 504
3. `test_status_validator_blocked_403`: Verifies `validator_blocked` maps to HTTP 403
4. `test_status_post_processor_failed_500`: Verifies `post_processor_failed` maps to HTTP 500
5. `test_status_extension_unavailable_503`: Verifies `extension_unavailable` maps to HTTP 503
6. `test_status_extension_error_500`: Verifies `extension_error` maps to HTTP 500
7. `test_error_code_mapping`: Verifies error code mapping via JSON responses
8. `test_error_response_structure`: Validates error response structure

**Updated**: `apps/c-gateway/src/http_server.c`

- Updated `map_router_error_status()` to handle extension error codes:
  - `extension_not_found` → 404
  - `extension_timeout` → 504
  - `validator_blocked` → 403
  - `post_processor_failed` → 500
  - `extension_unavailable` → 503
  - `extension_error` → 500

**CMakeLists.txt**: Added new test target `c-gateway-router-extension-errors-test`

### 3. Documentation Updates

**Updated**: `docs/ARCHITECTURE/api-registry.md`

- Added **Extension Error Codes** section with all extension error codes and HTTP status mappings
- Added **Extension Error Response Format** with complete JSON example
- Documented error details structure including:
  - `extension_id`
  - `error_type`
  - `timestamp`
  - `policy_id`
  - `tenant_id`
  - Additional context fields

## Error Response Format

### Standard Extension Error Response

```json
{
  "ok": false,
  "error": {
    "code": "extension_not_found",
    "message": "Extension not found: normalize_text",
    "details": {
      "extension_id": "normalize_text",
      "error_type": "extension_not_found",
      "timestamp": "2025-01-27T12:00:00Z",
      "policy_id": "default",
      "tenant_id": "tenant_abc"
    }
  },
  "context": {
    "request_id": "req_123",
    "trace_id": "trace_xyz"
  }
}
```

### HTTP Status Code Mappings

| Error Code | HTTP Status | Description |
|------------|-------------|-------------|
| `extension_not_found` | 404 | Extension not found in registry |
| `extension_timeout` | 504 | Extension call timed out |
| `validator_blocked` | 403 | Validator blocked the request |
| `post_processor_failed` | 500 | Post-processor failed |
| `extension_unavailable` | 503 | Extension unavailable (circuit breaker open) |
| `extension_error` | 500 | Generic extension invocation error |

## Testing

### Running Contract Tests

**Router Tests** (Erlang):
```bash
cd apps/otp/router
rebar3 ct --suite test/router_extension_error_mapper_SUITE.erl
```

**Gateway Tests** (C):
```bash
cd apps/c-gateway
mkdir -p build
cd build
cmake ..
make c-gateway-router-extension-errors-test
./c-gateway-router-extension-errors-test
```

### Test Coverage

**Router**:
- ✅ All extension error types mapped correctly
- ✅ Error details include all required metadata
- ✅ Timestamp formatting (ISO 8601)
- ✅ Integration with `router_decide_consumer` and `router_nats_subscriber`

**Gateway**:
- ✅ All extension error codes map to correct HTTP status codes
- ✅ Error response structure validation
- ✅ JSON parsing and error extraction
- ✅ Edge cases (missing fields, invalid JSON)

## Acceptance Criteria

### ✅ E2E Contract Tests Green

- All Gateway contract tests pass
- All Router error mapping tests pass
- Error responses are stable and documented

### ✅ Error Visibility

- Extension errors are visible to clients in stable, documented format
- Error codes are consistent across Router and Gateway
- Error details provide sufficient context for debugging

### ✅ Documentation Complete

- `api-registry.md` includes all extension error codes
- Error response format documented with examples
- HTTP status code mappings documented

## Files Created/Modified

### Created
- `apps/otp/router/src/router_extension_error_mapper.erl`
- `apps/c-gateway/tests/c-gateway-router-extension-errors-test.c`
- `docs/archive/dev/EXTENSIONS_CONTRACT_E2E_REPORT.md`

### Modified
- `apps/otp/router/src/router_decide_consumer.erl`
- `apps/otp/router/src/router_nats_subscriber.erl`
- `apps/c-gateway/src/http_server.c`
- `apps/c-gateway/CMakeLists.txt`
- `apps/c-gateway/tests/c-gateway-router-test.c`
- `docs/ARCHITECTURE/api-registry.md`

## Next Steps

1. **E2E Integration Tests**: Add full E2E tests with real Router and Gateway
2. **Error Metrics**: Add telemetry for extension error rates by type
3. **Error Retry Logic**: Document retry behavior for different error types
4. **Client SDK Updates**: Update client SDKs to handle extension error codes

## References

- `docs/ARCHITECTURE/api-registry.md` - API registry with extension error codes
- `apps/otp/router/src/router_extension_error_mapper.erl` - Error mapper implementation
- `apps/c-gateway/tests/c-gateway-router-extension-errors-test.c` - Contract tests
- `docs/EXTENSIONS_API.md` - Extensions API specification

