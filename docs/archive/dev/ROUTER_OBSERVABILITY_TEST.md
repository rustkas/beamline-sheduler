# Router Observability Test

**Date**: 2025-01-27  
**Purpose**: Test Router observability with real gRPC health check requests  
**Script**: `scripts/observability/test_router_observability.sh`

## Overview

This script tests Router observability health endpoint with real gRPC requests, validating:

- gRPC health endpoint (`grpc.health.v1.Health/Check`)
- Health status values (SERVING, UNKNOWN, NOT_SERVING)
- gRPC health protocol compliance
- Service availability and response format

**Note**: Router uses **gRPC health check** (not HTTP), which differs from Gateway and Worker. This script uses `grpc_health_probe` or `grpcurl` to test the health endpoint.

## Prerequisites

1. **Router must be running** on port 9000 (default) or set `ROUTER_PORT` environment variable
2. **Required tools** (at least one):
   - `grpc_health_probe` - preferred tool for gRPC health checks
     - Download: https://github.com/grpc-ecosystem/grpc-health-probe
     - Installation: `go install github.com/grpc-ecosystem/grpc-health-probe@latest`
   - `grpcurl` - alternative tool for gRPC requests
     - Download: https://github.com/fullstorydev/grpcurl
     - Installation: `go install github.com/fullstorydev/grpcurl/cmd/grpcurl@latest`
3. **Optional tools**:
   - `jq` - for JSON parsing and validation (recommended when using grpcurl)
   - `timeout` - for timeout handling (usually pre-installed on Linux)

## Configuration

**Default Port**: 9000 (gRPC port)

- Default gRPC port: `9000` (configurable via `beamline_router.grpc_port`)
- Health service: `grpc.health.v1.Health`
- Health method: `Check`
- Health endpoint address: `localhost:9000` (default)

## Usage

### Basic Usage

```bash
# Test with default Router address (localhost:9000)
bash scripts/observability/test_router_observability.sh
```

### Custom Router Address

```bash
# Test with custom host and port
ROUTER_HOST=localhost ROUTER_PORT=9000 bash scripts/observability/test_router_observability.sh

# Or use ROUTER_ADDR directly
ROUTER_ADDR=localhost:9000 bash scripts/observability/test_router_observability.sh
```

### Custom Timeout

```bash
# Test with custom timeout (default: 10 seconds)
ROUTER_ADDR=localhost:9000 TIMEOUT_SEC=5 bash scripts/observability/test_router_observability.sh
```

## Test Scenarios

### Test 1: gRPC Health Check via grpc_health_probe (Preferred Method)

**Purpose**: Verify health endpoint using `grpc_health_probe` tool (standard Kubernetes health probe tool).

**Command**:
```bash
grpc_health_probe -addr=localhost:9000 -service=grpc.health.v1.Health
```

**Expected Behavior**:
- ✅ Exit code 0: Service is healthy (SERVING status)
- ❌ Exit code 1: Service is unhealthy (NOT_SERVING status)
- ⚠️ Timeout: Service not responding (may indicate Router is not running)

**Validation**:
- ✅ Health probe exits with code 0 (service healthy)
- ✅ No error messages in output
- ✅ Response received within timeout period

**Note**: `grpc_health_probe` is the preferred method as it's the standard tool used in Kubernetes for gRPC health checks.

### Test 2: gRPC Health Check via grpcurl (Alternative Method)

**Purpose**: Verify health endpoint using `grpcurl` tool (alternative gRPC client).

**Command**:
```bash
grpcurl -plaintext localhost:9000 grpc.health.v1.Health/Check
```

**Expected Response** (JSON format):
```json
{
  "status": "SERVING"
}
```

**Validation**:
- ✅ Valid JSON response
- ✅ Status field present
- ✅ Status value is one of: `SERVING`, `UNKNOWN`, `NOT_SERVING`
- ✅ Status is `SERVING` (healthy) or `UNKNOWN` (acceptable)
- ❌ Status is `NOT_SERVING` (unhealthy)

**Status Values**:
- **SERVING**: Service is healthy and ready to accept requests
- **UNKNOWN**: Health status is unknown (acceptable, but not ideal)
- **NOT_SERVING**: Service is unhealthy and should not receive traffic

**Note**: `grpcurl` may require authentication (API key) if Router has authentication enabled. The script tries without authentication first, which may work if authentication is disabled.

### Test 3: Validate Health Status Values

**Purpose**: Verify health status enum values comply with gRPC health protocol.

**Validation**:
- ✅ Status is valid enum value: `SERVING`, `UNKNOWN`, or `NOT_SERVING`
- ✅ Status is `SERVING` (preferred) or `UNKNOWN` (acceptable)
- ❌ Status is `NOT_SERVING` (unhealthy)
- ❌ Status is invalid or missing

**gRPC Health Protocol**:
- Standard gRPC health checking protocol: `grpc.health.v1.Health`
- Defined in: https://github.com/grpc/grpc/blob/master/doc/health-checking.md
- Status enum: `SERVING`, `UNKNOWN`, `NOT_SERVING`

## Expected Results

### Successful Test Run

```
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Router Observability Test
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

[INFO] Router Address: localhost:9000
[INFO] Default port: 9000 (gRPC)
[INFO] ✓ grpc_health_probe found
[INFO] ✓ grpcurl found
[INFO] ✓ jq found (for JSON parsing)

[TEST] Test 1: gRPC Health Check via grpc_health_probe
[INFO] ✓ Health check passed (grpc_health_probe exit code: 0)

[TEST] Test 2: gRPC Health Check via grpcurl
[INFO] ✓ Health check status is 'SERVING' (healthy)
[INFO] Health check response:
{
  "status": "SERVING"
}

[TEST] Test 3: Validate Health Status Values
[INFO] ✓ Health status is 'SERVING' (service is healthy and ready)

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Test Summary
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

[INFO] Passed: 3
[WARN] Warnings: 0
[ERROR] Failed: 0

[INFO] All tests passed successfully!
```

### Service Not Running

```
[TEST] Test 1: gRPC Health Check via grpc_health_probe
[WARN] Health check timed out after 10 seconds
[WARN] Router may not be running or not responding
```

**Exit Code**: `2` (service not running - skip)

### Service Unhealthy

```
[TEST] Test 1: gRPC Health Check via grpc_health_probe
[ERROR] Health check failed (grpc_health_probe exit code: 1)
[ERROR] Output:
[ERROR] service unhealthy
```

**Exit Code**: `1` (test failed)

## Troubleshooting

### Issue: Neither grpc_health_probe nor grpcurl is available

**Error**:
```
[ERROR] Neither grpc_health_probe nor grpcurl is available.
[ERROR] Please install one of the following:
```

**Solution**:
1. Install `grpc_health_probe`:
   ```bash
   # Using Go
   go install github.com/grpc-ecosystem/grpc-health-probe@latest
   
   # Or download pre-built binary
   wget https://github.com/grpc-ecosystem/grpc-health-probe/releases/latest/download/grpc_health_probe-linux-amd64
   chmod +x grpc_health_probe-linux-amd64
   sudo mv grpc_health_probe-linux-amd64 /usr/local/bin/grpc_health_probe
   ```

2. Or install `grpcurl`:
   ```bash
   # Using Go
   go install github.com/fullstorydev/grpcurl/cmd/grpcurl@latest
   ```

### Issue: Health check timed out

**Error**:
```
[WARN] Health check timed out after 10 seconds
[WARN] Router may not be running or not responding
```

**Solutions**:
1. **Check if Router is running**:
   ```bash
   # Check if port 9000 is listening
   netstat -tuln | grep 9000
   # Or
   ss -tuln | grep 9000
   ```

2. **Start Router**:
   ```bash
   # In Router directory
   rebar3 shell
   # Or start via release
   ```

3. **Check Router logs** for errors

4. **Verify Router configuration**:
   - Check `grpc_enabled` is `true`
   - Check `grpc_port` is `9000` (or your configured port)

### Issue: Health check failed (service unhealthy)

**Error**:
```
[ERROR] Health check failed (grpc_health_probe exit code: 1)
[ERROR] service unhealthy
```

**Solutions**:
1. **Check Router status**:
   - Review Router logs for errors
   - Check if Router is fully initialized
   - Verify NATS connection (if required)

2. **Check health service configuration**:
   - Verify `grpcbox_health_service` is enabled
   - Check gRPC server is running

3. **Test manually**:
   ```bash
   grpc_health_probe -addr=localhost:9000 -service=grpc.health.v1.Health
   ```

### Issue: grpcurl requires authentication

**Error**:
```
[WARN] Health check failed with exit code 1
[WARN] Router may not be running or requires authentication
```

**Solutions**:
1. **Use grpc_health_probe** (preferred, doesn't require auth):
   ```bash
   grpc_health_probe -addr=localhost:9000
   ```

2. **Or provide API key to grpcurl**:
   ```bash
   grpcurl -plaintext -H "x-api-key: YOUR_API_KEY" \
     localhost:9000 grpc.health.v1.Health/Check
   ```

3. **Check Router authentication configuration**:
   - Verify if authentication is required for health endpoint
   - Check if health endpoint should be public (no auth)

### Issue: jq not found (optional)

**Warning**:
```
[WARN] jq not found (optional, for JSON parsing)
```

**Solution**:
- Install `jq` for better JSON parsing (optional but recommended):
  ```bash
  # Ubuntu/Debian
  sudo apt-get install jq
  
  # macOS
  brew install jq
  
  # Or download from https://stedolan.github.io/jq/download/
  ```

**Note**: `jq` is optional. The script will work without it, but JSON validation will be limited.

## Exit Codes

| Exit Code | Meaning | Description |
|-----------|---------|-------------|
| `0` | Success | All tests passed |
| `1` | Failed | One or more tests failed |
| `2` | Service Not Running | Router is not running or not responding (skip) |

## CI/CD Integration

### GitHub Actions

```yaml
- name: Test Router Observability
  run: |
    # Start Router (example)
    # ... start Router ...
    
    # Wait for Router to be ready
    sleep 5
    
    # Run observability tests
    bash scripts/observability/test_router_observability.sh
  env:
    ROUTER_ADDR: localhost:9000
    TIMEOUT_SEC: 10
```

### GitLab CI

```yaml
test_router_observability:
  script:
    - # Start Router
    - sleep 5
    - bash scripts/observability/test_router_observability.sh
  variables:
    ROUTER_ADDR: "localhost:9000"
    TIMEOUT_SEC: "10"
```

### Drone CI

```yaml
- name: test-router-observability
  image: alpine:latest
  commands:
    - apk add --no-cache bash curl jq
    - # Install grpc_health_probe or grpcurl
    - # Start Router
    - sleep 5
    - bash scripts/observability/test_router_observability.sh
  environment:
    ROUTER_ADDR: localhost:9000
    TIMEOUT_SEC: 10
```

## Differences from Gateway/Worker Tests

| Feature | Gateway/Worker | Router |
|---------|----------------|--------|
| **Protocol** | HTTP | gRPC |
| **Port** | 3000 (Gateway), 9091 (Worker) | 9000 |
| **Health Endpoint** | `GET /_health` | `grpc.health.v1.Health/Check` |
| **Response Format** | JSON | Protobuf (JSON via grpcurl) |
| **Tools** | `curl`, `jq` | `grpc_health_probe`, `grpcurl`, `jq` |
| **Status Values** | `healthy`, `degraded`, `unhealthy` | `SERVING`, `UNKNOWN`, `NOT_SERVING` |

## References

- `apps/otp/router/docs/OBSERVABILITY.md` - Router observability documentation
- `scripts/observability/test_router_observability.sh` - Test script
- `scripts/observability/test_gateway_observability.sh` - Gateway test script (reference)
- `scripts/observability/test_worker_observability.sh` - Worker test script (reference)
- [gRPC Health Checking Protocol](https://github.com/grpc/grpc/blob/master/doc/health-checking.md)
- [grpc-health-probe](https://github.com/grpc-ecosystem/grpc-health-probe)
- [grpcurl](https://github.com/fullstorydev/grpcurl)

