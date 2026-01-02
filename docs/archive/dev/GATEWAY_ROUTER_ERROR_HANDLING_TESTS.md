# Gateway ↔ Router Error Handling Integration Tests

**Date**: 2025-01-27  
**Status**: ✅ **Tests Created**  
**Location**: `tests/integration/gateway-router-error-handling.test.ts`

## Purpose

Integration tests for complete error handling flow: **Gateway HTTP request → Router validation → DLQ/audit → HTTP error response**.

These tests verify:
1. **Error Code Mapping**: Router intake error codes → Gateway HTTP status codes
2. **DLQ Publication**: Invalid messages are published to DLQ by Router
3. **Audit Logging**: Error events are logged for audit trail
4. **HTTP Response Structure**: Gateway returns proper error responses with context

## Error Code Mapping

Tests use the existing error code mapping from `router_intake_error_handler.erl`:

| Intake Error Code | Gateway Error Code | HTTP Status | Description |
|-------------------|-------------------|-------------|-------------|
| `SCHEMA_VALIDATION_FAILED` | `invalid_request` | 400 | Schema validation failed |
| `VERSION_UNSUPPORTED` | `invalid_request` | 400 | Unsupported schema version |
| `CORRELATION_FIELDS_INVALID` | `invalid_request` | 400 | Invalid correlation fields |
| `TENANT_FORBIDDEN` | `unauthorized` | 401 | Tenant validation failed |
| `IDEMPOTENCY_VIOLATION` | `invalid_request` | 400 | Idempotency violation |
| `INTERNAL_VALIDATION_ERROR` | `internal` | 500 | Internal validation error |

**Implementation**:
- Router: `router_intake_error_handler.erl` → `map_intake_error_to_gateway_code/1`
- Gateway: `apps/c-gateway/src/http_server.c` → `map_router_error_status()`

## Test Structure

### 1. Error Code Mapping & HTTP Status Tests

**File**: `tests/integration/gateway-router-error-handling.test.ts`

**Test Groups**:
- `SCHEMA_VALIDATION_FAILED → 400 Bad Request`
- `VERSION_UNSUPPORTED → 400 Bad Request`
- `CORRELATION_FIELDS_INVALID → 400 Bad Request`
- `TENANT_FORBIDDEN → 401 Unauthorized`
- `IDEMPOTENCY_VIOLATION → 400 Bad Request`
- `INTERNAL_VALIDATION_ERROR → 500 Internal Server Error`
- `Error Response Structure`

**What They Test**:
- HTTP status codes match expected values
- Error response includes Gateway-compatible error code
- Error response includes original intake error code (if Router provides it)
- Error message indicates validation failure
- Context includes `request_id` and `trace_id` for audit

### 2. DLQ Publication Verification

**Test Group**: `Gateway ↔ Router: DLQ Publication Verification`

**What They Test**:
- Error responses are returned correctly (DLQ publication happens in Router)
- Router doesn't block on DLQ publication failures
- Response time is reasonable (not blocked by DLQ)

**Note**: Actual DLQ publication is verified in Router e2e tests (`router_intake_e2e_SUITE.erl`).

### 3. Audit Logging Verification

**Test Group**: `Gateway ↔ Router: Audit Logging Verification`

**What They Test**:
- Error responses include `trace_id` for audit correlation
- Error responses include `tenant_id` for audit
- Error responses include `error_code` for audit logging

**Note**: Actual audit log entries are verified in Router e2e tests.

### 4. End-to-End Error Flow

**Test Group**: `Gateway ↔ Router: End-to-End Error Flow`

**What They Test**:
- Complete flow: HTTP request → Router validation → DLQ/audit → HTTP error response
- Multiple validation errors are handled correctly
- First validation error is returned

## Running Tests

### Prerequisites

1. **Gateway (C-Gateway)** running on `http://localhost:8081` (or set `C_GATEWAY_URL`)
2. **Router** running and accessible via Gateway
3. **NATS** running (optional, for full integration)

### Run All Tests

```bash
cd tests/integration
npm test gateway-router-error-handling.test.ts
```

### Run Specific Test Group

```bash
# Run only error code mapping tests
npm test gateway-router-error-handling.test.ts -t "Error Code Mapping"

# Run only DLQ tests
npm test gateway-router-error-handling.test.ts -t "DLQ Publication"

# Run only audit tests
npm test gateway-router-error-handling.test.ts -t "Audit Logging"
```

### Environment Variables

```bash
# Gateway URL (default: http://localhost:8081)
export C_GATEWAY_URL=http://localhost:8081

# Router URL (for direct Router testing, optional)
export ROUTER_URL=http://localhost:3081

# NATS URL (for full integration, optional)
export NATS_URL=nats://localhost:4222
```

## Test Coverage

### Error Scenarios Covered

1. ✅ **Schema Validation Errors**:
   - Missing `tenant_id`
   - Invalid JSON payload
   - Missing required fields

2. ✅ **Version Errors**:
   - Unsupported version in request

3. ✅ **Correlation Field Errors**:
   - Invalid UUID format
   - Invalid field dependencies (e.g., `flow_id` without `run_id`)

4. ✅ **Tenant Validation Errors**:
   - Tenant not in allowlist
   - Missing tenant header

5. ✅ **Idempotency Errors**:
   - Duplicate request with conflicting data

6. ✅ **Internal Errors**:
   - Internal validation errors (if triggerable)

### Response Structure Verification

1. ✅ **Error Response Format**:
   - `ok: false`
   - `error.code`: Gateway-compatible code
   - `error.message`: Human-readable message
   - `error.intake_error_code`: Original intake code (if provided)
   - `context.request_id`: Request ID for correlation
   - `context.trace_id`: Trace ID for audit

2. ✅ **HTTP Status Codes**:
   - 400 for client errors (schema, version, correlation, idempotency)
   - 401 for tenant validation failures
   - 500 for internal errors

## Integration with Router E2E Tests

These Gateway integration tests complement Router e2e tests:

**Router E2E Tests** (`router_intake_e2e_SUITE.erl`):
- Verify DLQ publication
- Verify audit log entries
- Verify metrics emission
- Test Router internals

**Gateway Integration Tests** (`gateway-router-error-handling.test.ts`):
- Verify HTTP status codes
- Verify error response structure
- Verify Gateway → Router → Gateway flow
- Test Gateway error handling

**Together**, they provide complete coverage of the error handling flow.

## Known Limitations

1. **DLQ Verification**: Gateway tests verify error responses, but actual DLQ publication is verified in Router e2e tests (requires NATS access).

2. **Audit Log Verification**: Gateway tests verify error response structure, but actual audit log entries are verified in Router e2e tests (requires log access).

3. **Metrics Verification**: Gateway tests don't verify metrics emission (verified in Router e2e tests).

4. **Internal Error Triggering**: Some internal errors may be difficult to trigger without mocking Router internals.

## Future Enhancements

1. **Direct DLQ Verification**: Add NATS subscription to verify DLQ messages directly.

2. **Audit Log Verification**: Add log parsing to verify audit entries.

3. **Metrics Verification**: Add metrics scraping to verify error metrics.

4. **Performance Tests**: Add tests for error handling under load.

5. **Circuit Breaker Tests**: Add tests for Gateway circuit breaker behavior.

## References

- `apps/otp/router/src/router_intake_error_codes.erl` - Error code definitions
- `apps/otp/router/src/router_intake_error_handler.erl` - Error handling and mapping
- `apps/c-gateway/src/http_server.c` - Gateway error status mapping
- `apps/otp/router/test/router_intake_e2e_SUITE.erl` - Router e2e tests
- `docs/ARCHITECTURE/api-registry.md` - Error code mapping documentation
- `docs/archive/dev/ROUTER_INTAKE_E2E_GATEWAY_INTEGRATION_REPORT.md` - Gateway integration report

