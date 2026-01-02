# Gateway Observability Test

**Date**: 2025-01-27  
**Purpose**: Test Gateway observability with real HTTP requests  
**Script**: `scripts/observability/test_gateway_observability.sh`

## Overview

This script tests Gateway observability endpoints and API endpoints with real HTTP requests, validating:

- Health endpoints (`GET /health`, `GET /_health`)
- Metrics endpoints (`GET /metrics`, `GET /_metrics`)
- API endpoints (`POST /api/v1/routes/decide`, `POST /api/v1/messages`)
- Observability fields (trace_id, tenant_id, run_id)
- Error handling and validation

## Prerequisites

1. **Gateway (C-Gateway) must be running** on port 8081 (default) or set `GATEWAY_URL` environment variable
2. **Required tools**:
   - `curl` - for HTTP requests
   - `jq` - for JSON parsing and validation

## Usage

### Basic Usage

```bash
# Test with default Gateway URL (http://localhost:8081)
bash scripts/observability/test_gateway_observability.sh
```

### Custom Gateway URL

```bash
# Test with custom Gateway URL
GATEWAY_URL=http://localhost:8081 bash scripts/observability/test_gateway_observability.sh
```

### Custom Test Parameters

```bash
# Test with custom tenant, trace, and run IDs
GATEWAY_URL=http://localhost:8081 \
TEST_TENANT_ID=tenant-custom \
TEST_TRACE_ID=trace-custom \
TEST_RUN_ID=run-custom \
bash scripts/observability/test_gateway_observability.sh
```

## Test Scenarios

### Test 1: Health Endpoint (GET /health)

**Purpose**: Verify health endpoint returns valid JSON with required fields.

**Expected Response**:
```json
{
  "status": "healthy" | "degraded" | "unhealthy" | "ok",
  "timestamp": "2025-01-27T12:00:00Z",
  "checks": {
    "nats": {
      "status": "ok",
      "message": "NATS connection active"
    }
  }
}
```

**Validation**:
- ✅ HTTP 200 OK
- ✅ Valid JSON response
- ✅ Required fields: `status`, `timestamp`
- ✅ Status value is one of: `healthy`, `degraded`, `unhealthy`, `ok`

### Test 2: Health Endpoint Alternative (GET /_health)

**Purpose**: Verify alternative health endpoint path.

**Expected Response**: Same as Test 1.

**Validation**:
- ✅ HTTP 200 OK
- ✅ Valid JSON response
- ✅ Same structure as `/health`

### Test 3: Metrics Endpoint (GET /metrics)

**Purpose**: Verify Prometheus metrics endpoint.

**Expected Response**: Prometheus text format metrics.

**Validation**:
- ✅ HTTP 200 OK
- ✅ Non-empty response
- ✅ Prometheus format (optional validation)

### Test 4: Metrics JSON Endpoint (GET /_metrics)

**Purpose**: Verify JSON metrics endpoint.

**Expected Response**: JSON metrics format.

**Validation**:
- ✅ HTTP 200 OK
- ✅ Valid JSON response

### Test 5: POST /api/v1/routes/decide (Happy Path)

**Purpose**: Verify routing decision endpoint with observability fields.

**Request**:
```json
{
  "version": "1",
  "tenant_id": "tenant-test-...",
  "request_id": "req-...",
  "message_id": "msg-...",
  "message_type": "chat",
  "payload": "dGVzdCBwYXlsb2Fk",
  "metadata": {},
  "policy_id": "default-policy",
  "context": {
    "user_id": "user_001"
  }
}
```

**Headers**:
- `X-Tenant-ID: tenant-test-...`
- `X-Trace-ID: trace-test-...`
- `X-Run-ID: run-test-...`

**Expected Response**:
```json
{
  "ok": true,
  "decision": {
    "provider_id": "provider-...",
    "reason": "weighted" | "sticky" | "fallback" | "policy",
    "priority": 100,
    "expected_latency_ms": 200,
    "expected_cost": 0.001
  },
  "context": {
    "request_id": "req-...",
    "trace_id": "trace-test-..."
  }
}
```

**Validation**:
- ✅ HTTP 200 OK
- ✅ Valid JSON response
- ✅ Response contains `trace_id` matching request
- ✅ Response contains `request_id`

### Test 6: POST /api/v1/messages (Happy Path)

**Purpose**: Verify message creation endpoint with observability fields.

**Request**:
```json
{
  "tenant_id": "tenant-test-...",
  "message_type": "chat",
  "payload": "{\"text\": \"Hello, world!\"}",
  "trace_id": "trace-test-...",
  "metadata": {
    "source": "test",
    "run_id": "run-test-..."
  }
}
```

**Headers**:
- `X-Tenant-ID: tenant-test-...`
- `X-Trace-ID: trace-test-...`
- `X-Run-ID: run-test-...`

**Expected Response**:
```json
{
  "message_id": "msg-...",
  "ack_timestamp_ms": 1704067200000,
  "status": "published"
}
```

**Validation**:
- ✅ HTTP 200 OK (if implemented)
- ✅ Valid JSON response
- ✅ Response contains `message_id`

### Test 7: Validation Error (400 Bad Request)

**Purpose**: Verify error handling for invalid requests.

**Request**:
```json
{
  "invalid": "request"
}
```

**Expected Response**:
```json
{
  "error": {
    "code": "invalid_request" | "INVALID_REQUEST",
    "message": "string",
    "trace_id": "trace-test-...",
    "timestamp": "2025-01-27T12:00:00Z"
  }
}
```

**Validation**:
- ✅ HTTP 400 Bad Request
- ✅ Valid JSON error response
- ✅ Error response contains `trace_id`
- ✅ Error code is `invalid_request` or `INVALID_REQUEST`

### Test 8: Missing Tenant ID Header (400 Bad Request)

**Purpose**: Verify validation for missing required headers.

**Request**:
```json
{
  "version": "1",
  "tenant_id": "test",
  "request_id": "req-123"
}
```

**Headers**: Only `X-Trace-ID` (no `X-Tenant-ID`)

**Expected Response**: HTTP 400 Bad Request with error message about missing `X-Tenant-ID` header.

**Validation**:
- ✅ HTTP 400 Bad Request
- ✅ Error message indicates missing header

## Exit Codes

- `0` - All tests passed (may have warnings)
- `1` - Some tests failed
- `2` - Gateway service not running (skip tests)

## Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `GATEWAY_URL` | `http://localhost:3000` | Gateway base URL |
| `TIMEOUT_SEC` | `10` | HTTP request timeout in seconds |
| `TEST_TENANT_ID` | `tenant-test-<timestamp>` | Test tenant ID |
| `TEST_TRACE_ID` | `trace-test-<timestamp>` | Test trace ID |
| `TEST_RUN_ID` | `run-test-<timestamp>` | Test run ID |

## Output

The script provides colored output:

- **Green [INFO]**: Informational messages
- **Blue [TEST]**: Test scenario names
- **Yellow [WARN]**: Warnings (non-critical issues)
- **Red [ERROR]**: Errors (test failures)

At the end, a summary is displayed:

```
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Test Summary
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

[INFO] Passed: 8
[WARN] Warnings: 2
[ERROR] Failed: 0
```

## Troubleshooting

### Gateway Not Reachable

**Error**: `Gateway not reachable at http://localhost:3000/health`

**Solution**:
1. Check if Gateway is running:
   ```bash
   curl http://localhost:3000/health
   ```

2. Check Gateway port (default is 3000, but may be configured differently):
   ```bash
   GATEWAY_URL=http://localhost:8080 bash scripts/observability/test_gateway_observability.sh
   ```

3. Start Gateway if not running:
   ```bash
   # Check Gateway startup instructions in apps/c-gateway/README.md
   ```

### Invalid JSON Response

**Error**: `Health endpoint response is not valid JSON`

**Solution**:
1. Check Gateway logs for errors
2. Verify Gateway is configured correctly
3. Check if Gateway is returning HTML error page instead of JSON

### Missing Required Fields

**Error**: `Health endpoint missing required 'status' field`

**Solution**:
1. Verify Gateway health endpoint implementation matches spec
2. Check `docs/OBSERVABILITY_HEALTH_ENDPOINTS.md` for expected format
3. Review Gateway code in `apps/c-gateway/src/http_server.c`

### Trace ID Mismatch

**Warning**: `Trace ID mismatch: expected trace-test-..., got trace-...`

**Solution**:
- This is a warning, not an error
- Gateway may generate its own trace_id if request trace_id is invalid
- Verify Gateway trace_id handling logic

## Integration with CI/CD

This script can be integrated into CI/CD pipelines:

```yaml
# Example GitHub Actions workflow
- name: Test Gateway Observability
  run: |
    bash scripts/observability/test_gateway_observability.sh
  env:
    GATEWAY_URL: http://localhost:3000
```

## Related Documentation

- `docs/OBSERVABILITY.md` - General observability requirements
- `docs/OBSERVABILITY_HEALTH_ENDPOINTS.md` - Health endpoint specifications
- `docs/OBSERVABILITY_CP1_INVARIANTS.md` - CP1 observability invariants
- `docs/ARCHITECTURE/api-registry.md` - API endpoint specifications
- `scripts/smoke_c_gateway.sh` - Basic Gateway smoke test
- `scripts/gateway_router_cp1_smoke.sh` - Gateway-Router CP1 smoke test

## References

- Gateway implementation: `apps/c-gateway/src/http_server.c`
- Health endpoint handler: `handle_health()` function
- Metrics endpoint handler: `handle_metrics_request()`, `handle_metrics_json()` functions
- API endpoint handlers: `handle_decide()`, message handlers

