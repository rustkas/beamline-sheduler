# Worker Observability Test

**Date**: 2025-01-27  
**Purpose**: Test Worker observability with real HTTP requests  
**Script**: `scripts/observability/test_worker_observability.sh`

## Overview

This script tests Worker observability health endpoint with real HTTP requests, validating:

- Health endpoint (`GET /_health`)
- CP1-compliant response format
- ISO 8601 timestamp format (with microseconds)
- HTTP status codes and Content-Type headers

## Prerequisites

1. **Worker must be running** on port 9091 (default) or set `WORKER_URL` environment variable
2. **Required tools**:
   - `curl` - for HTTP requests
   - `jq` - for JSON parsing and validation

## Configuration

**Default Port**: 9091 (derived from `prometheus_endpoint + 1`)

- Default `prometheus_endpoint`: `0.0.0.0:9090` → health port: `9091`
- Port is configurable via `prometheus_endpoint` configuration
- Health endpoint address: `0.0.0.0` (all interfaces)

## Usage

### Basic Usage

```bash
# Test with default Worker URL (http://localhost:9091)
bash scripts/observability/test_worker_observability.sh
```

### Custom Worker URL

```bash
# Test with custom Worker URL
WORKER_URL=http://localhost:9091 bash scripts/observability/test_worker_observability.sh
```

### Custom Timeout

```bash
# Test with custom timeout (default: 10 seconds)
WORKER_URL=http://localhost:9091 TIMEOUT_SEC=5 bash scripts/observability/test_worker_observability.sh
```

## Test Scenarios

### Test 1: Health Endpoint (GET /_health) - CP1 Format Validation

**Purpose**: Verify health endpoint returns valid JSON with required fields and CP1 compliance.

**Expected Response**:
```json
{
  "status": "healthy",
  "timestamp": "2025-01-27T12:00:00.123456Z",
  "checks": {
    "processor": {
      "status": "ok",
      "message": "Processor pool operational"
    },
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
- ✅ Status value is `healthy` (CP1 compliant, not `ok`)
- ✅ Timestamp is ISO 8601 format with microseconds (6 digits)
- ✅ Optional `checks` field structure (if present)

**CP1 Compliance Checks**:
- Status must be `healthy` (not `ok`)
- Timestamp must be ISO 8601 format: `YYYY-MM-DDTHH:MM:SS[.ssssss]Z`
- Timestamp should have 6 digits for microseconds (CP1 recommendation)

### Test 2: HTTP Status Code Validation

**Purpose**: Verify health endpoint returns correct HTTP status code.

**Validation**:
- ✅ HTTP 200 OK (required for CP1 compliance)
- ❌ Any other status code is an error

### Test 3: Response Content-Type Validation

**Purpose**: Verify health endpoint returns correct Content-Type header.

**Validation**:
- ✅ Content-Type: `application/json` (CP1 compliant)
- ⚠️ Other content types are warnings (may still be valid JSON)

## Exit Codes

- `0` - All tests passed
- `1` - Tests failed (validation errors)
- `2` - Service not running (skip tests)

## Example Output

```
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Worker Observability Test
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

[INFO] Worker URL: http://localhost:9091
[INFO] Default port: 9091 (prometheus_port + 1)

[TEST] Test 1: Health Endpoint (GET /_health) - CP1 Format Validation
[INFO] ✓ Health endpoint status is 'healthy' (CP1 compliant)
[INFO] ✓ Health endpoint timestamp is valid ISO 8601 format: 2025-01-27T12:00:00.123456Z
[INFO] ✓ Health endpoint timestamp has microseconds (6 digits) - CP1 compliant
[INFO] ✓ Health endpoint contains optional 'checks' field
[INFO]   Checks count: 2
[INFO] Health endpoint response (CP1 compliant):
{
  "status": "healthy",
  "timestamp": "2025-01-27T12:00:00.123456Z",
  "checks": {
    "processor": {
      "status": "ok",
      "message": "Processor pool operational"
    },
    "nats": {
      "status": "ok",
      "message": "NATS connection active"
    }
  }
}

[TEST] Test 2: HTTP Status Code Validation
[INFO] ✓ Health endpoint returned HTTP 200 OK (CP1 compliant)

[TEST] Test 3: Response Content-Type Validation
[INFO] ✓ Health endpoint Content-Type is 'application/json' (CP1 compliant)

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Test Summary
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

[INFO] Passed: 3
[WARN] Warnings: 0
[ERROR] Failed: 0

[INFO] All tests passed successfully!
```

## Troubleshooting

### Service Not Running

**Error**: `Worker not reachable at http://localhost:9091/_health, skipping (service not running)`

**Solution**:
1. Ensure Worker is running
2. Check if Worker is listening on port 9091:
   ```bash
   curl http://localhost:9091/_health
   ```
3. Verify Worker configuration (check `prometheus_endpoint` setting)

### Invalid JSON Response

**Error**: `Health endpoint response is not valid JSON`

**Solution**:
1. Check Worker logs for errors
2. Verify health endpoint implementation
3. Test manually:
   ```bash
   curl -v http://localhost:9091/_health
   ```

### Missing Required Fields

**Error**: `Health endpoint missing required 'status' field` or `Health endpoint missing required 'timestamp' field`

**Solution**:
1. Verify Worker health endpoint implementation returns both `status` and `timestamp`
2. Check health endpoint response format matches CP1 requirements

### Status Value Not "healthy"

**Error**: `Health endpoint status is 'ok' (should be 'healthy' for CP1 compliance)`

**Solution**:
1. Update Worker health endpoint to return `status: "healthy"` instead of `status: "ok"`
2. CP1 compliance requires `healthy`, `degraded`, or `unhealthy` (not `ok`)

### Timestamp Format Invalid

**Error**: `Health endpoint timestamp format invalid: '...' (expected ISO 8601 format)`

**Solution**:
1. Verify timestamp is ISO 8601 format: `YYYY-MM-DDTHH:MM:SS[.ssssss]Z`
2. Ensure timestamp has 6 digits for microseconds (CP1 recommendation)
3. Check Worker observability implementation for timestamp generation

## Integration with CI/CD

This script can be integrated into CI/CD pipelines:

```yaml
# Example GitHub Actions workflow
- name: Test Worker Observability
  run: |
    bash scripts/observability/test_worker_observability.sh
  env:
    WORKER_URL: http://localhost:9091
    TIMEOUT_SEC: 10
```

## References

- `docs/OBSERVABILITY_CP1_INVARIANTS.md` - CP1 observability invariants specification
- `docs/OBSERVABILITY_HEALTH_ENDPOINTS.md` - Health endpoint requirements
- `apps/caf/processor/src/main.cpp` - Worker health endpoint implementation
- `apps/caf/processor/src/observability.cpp` - Observability implementation

