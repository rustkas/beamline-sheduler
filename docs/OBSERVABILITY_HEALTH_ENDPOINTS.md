# Health Endpoints for CP1 Components

**Version**: 1.0  
**Last Updated**: 2025-01-27  
**Purpose**: Define health endpoint requirements for all CP1 core components

## Overview

All CP1 core components must provide health indicators for monitoring and orchestration. Health endpoints are **cross-cutting invariants** that must be implemented consistently across all components.

## Component Health Endpoints

### 1. Router (Erlang/OTP)

**Protocol**: gRPC (not HTTP)  
**Port**: 9000  
**Service**: `grpc.health.v1.Health`  
**Method**: `Check`

**Note**: Router uses **gRPC health service** instead of HTTP `/_health` endpoint. This is acceptable because Router uses gRPC as its primary protocol. The gRPC health service follows the standard gRPC health checking protocol and is compatible with Kubernetes health checks via `grpc_health_probe`.

**Implementation**:
- Uses `grpcbox_health_service` for gRPC health protocol
- Implements standard gRPC health checking service
- Follows gRPC health checking specification

**Verification**:
```bash
# Using grpc_health_probe (recommended for Kubernetes)
grpc_health_probe -addr=localhost:9000

# Expected: exit code 0 if healthy

# Using grpc_health_probe with service name
grpc_health_probe -addr=localhost:9000 -service=grpc.health.v1.Health

# Using grpcurl
grpcurl -plaintext localhost:9000 grpc.health.v1.Health/Check
```

**Response Format**:
- Protocol: Protobuf (not JSON)
- Status values: `SERVING`, `UNKNOWN`, `NOT_SERVING`
- Empty service name returns `SERVING` (overall health)
- Specific service names return `UNKNOWN` (not implemented for specific services)

**Docker Healthcheck**:
```dockerfile
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD grpc_health_probe -addr=:9000 || exit 1
```

**Documentation**: `../apps/otp/router/docs/dev/HEALTH_ENDPOINT_VERIFICATION.md`

---

### 2. Gateway (C-Gateway)

**Protocol**: HTTP  
**Path**: `GET /_health` or `GET /health`  
**Port**: 3000 (default, configurable)

**Implementation**:
- HTTP endpoint in `apps/c-gateway/src/http_server.c`
- Handler: `handle_health()` function
- Returns JSON response

**Verification**:
```bash
# Basic health check
curl http://localhost:3000/_health

# Expected: HTTP 200 OK with JSON response
```

**Response Format**:
```json
{
  "status": "healthy",
  "timestamp": "2025-01-27T12:00:00Z",
  "checks": {
    "nats": {
      "status": "ok",
      "message": "NATS connection active"
    }
  }
}
```

**Required Fields**:
- `status` (string): `healthy`, `degraded`, or `unhealthy`
- `timestamp` (string): ISO 8601 timestamp

**Optional Fields**:
- `checks` (object): Individual component health checks

**Docker Healthcheck**:
```dockerfile
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:3000/_health || exit 1
```

---

### 3. Worker (Rust)

**Protocol**: HTTP  
**Path**: `GET /_health`  
**Port**: 9091 (default, configurable via `prometheus_endpoint`)

**Configuration**:
- Default port: **9091** (Prometheus port 9090 + 1)
- Port is derived from `prometheus_endpoint` configuration: `health_port = prometheus_port + 1`
- Default `prometheus_endpoint`: `0.0.0.0:9090` → health port: `9091`
- Address: `0.0.0.0` (all interfaces)

**Implementation**:
- HTTP endpoint in Worker component (`apps/worker`)
- Returns JSON response with status and timestamp
- Health endpoint started by the component’s observability module

**Verification**:
```bash
# Basic health check (default port 9091)
curl http://localhost:9091/_health

# Expected: HTTP 200 OK with JSON response
```

**Response Format**:
```json
{
  "status": "healthy",
  "timestamp": "2025-01-27T12:00:00Z",
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

**Required Fields**:
- `status` (string): `healthy`, `degraded`, or `unhealthy`
- `timestamp` (string): ISO 8601 timestamp

**Optional Fields**:
- `checks` (object): Individual component health checks (processor, NATS, etc.)

**Alternative for Worker without HTTP Server**:
- Separate health check utility/command
- File-based health indicator (e.g., `/tmp/worker_health.json`)
- Exit code for docker healthcheck

**Docker Healthcheck** (if HTTP available):
```dockerfile
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:9091/_health || exit 1
```

**Docker Healthcheck** (if file-based):
```dockerfile
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD test -f /tmp/worker_health.json && jq -e '.status == "healthy"' /tmp/worker_health.json || exit 1
```

**Documentation**: `apps/worker/docs/ARCHITECTURE_ROLE.md`

---

## Verification Examples
```bash
# Liveness check
curl http://localhost:<port>/_health

# Readiness check
curl http://localhost:<port>/_readyz
```

**Response Format**:

**Liveness (`/_health`)**:
- HTTP Status: `200 OK`
- Content-Type: `text/plain`
- Body: `ok`

**Readiness (`/_readyz`)**:
- HTTP Status: `200 OK` (if ready) or `503 Service Unavailable` (if not ready)
- Content-Type: `text/plain`
- Body: `ready` (if ready) or `unready` (if not ready)

**Docker Healthcheck**:
```dockerfile
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:<port>/_health || exit 1
```

**Readiness Check** (Kubernetes):
```yaml
readinessProbe:
  httpGet:
    path: /_readyz
    port: <port>
  initialDelaySeconds: 5
  periodSeconds: 10
```

---

## Health Status Values

### Overall Status

- **healthy**: Service is operating normally
- **degraded**: Service is operating with limitations (e.g., some dependencies unavailable)
- **unhealthy**: Service is unavailable or critically failing

### Individual Check Status

- **ok**: Component check passed
- **fail**: Component check failed

---

## Validation

### Manual Verification

**Router (gRPC)**:
```bash
grpc_health_probe -addr=localhost:9000
```

**Gateway (HTTP)**:
```bash
curl -f http://localhost:3000/_health | jq .
```

**Worker (HTTP)**:
```bash
curl -f http://localhost:<port>/_health | jq .
```

### Automated Validation

**Script**: `scripts/observability/validate_observability.sh`

The script checks:
- Health endpoint availability (HTTP 200 OK)
- JSON response format (for HTTP endpoints)
- Required fields (`status`, `timestamp`)
- gRPC health service (for Router)

**Exit Codes**:
- `0` - Success (may have warnings)
- `2` - External endpoints unavailable (services not running - expected in local validation)
- `3` - Local configs missing or invalid

---

## Integration with Observability

Health endpoints are part of the **cross-cutting observability invariants** for CP1:

1. **Structured JSON Logging** - All components log in unified format
2. **Health Endpoints** - All components provide health indicators
3. **Trace ID Propagation** - All events linked to trace_id/run_id
4. **Minimal Metrics** - Components log metrics in structured format (even if Prometheus not deployed)

**Documentation**:
- `docs/OBSERVABILITY.md` - General observability requirements
- `docs/OBSERVABILITY_CONVENTIONS.md` - Detailed conventions
- `docs/CORE_COMPONENTS.md` - Cross-cutting observability invariants section

---

## References

- `config/observability/logging.json` - Logging format schema (includes health endpoint definition)
- `docs/OBSERVABILITY.md` - Observability requirements
- `../apps/otp/router/docs/dev/HEALTH_ENDPOINT_VERIFICATION.md` - Router health endpoint verification
- `apps/caf/processor/docs/ARCHITECTURE_ROLE.md` - Worker CAF architecture and health requirements
