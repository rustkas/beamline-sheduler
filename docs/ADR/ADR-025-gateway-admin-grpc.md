---
version: 1.0
status: accepted
date: 2025-01-27
deciders:
  - CP2-GATEWAY Implementation Team
related_adrs:
  - ADR-014: Metrics and Distributed Tracing (Gateway metrics)
  - ADR-016: C-Gateway Migration (Gateway architecture)
supersedes: []
superseded_by: []
---

# ADR-025: Gateway Admin gRPC API

## Status

accepted

## Context

Gateway needs administrative API for:
- Health monitoring and status checks
- Operational control and configuration
- Metrics export for monitoring
- Role-based access control (RBAC) for admin operations
- Integration with monitoring and orchestration systems

**Requirements**:
- Health endpoint for Kubernetes/Docker health checks
- Status endpoint for operational monitoring
- Metrics endpoint for Prometheus scraping
- RBAC-protected operations (admin, operator, viewer roles)
- API key authentication
- Trace context support for distributed tracing

## Decision

Implement **Gateway Admin gRPC API** as a set of C functions (`admin_grpc.c`) providing:
1. **Health Check**: `admin_health()` - Returns Gateway health status (200 = healthy, 503 = unhealthy)
2. **Status Check**: `admin_status()` - Returns Gateway status information (JSON format)
3. **Authorization**: `admin_authorize()` - Role-based access control (admin, operator, viewer)
4. **Metrics Export**: `admin_get_metrics()` - Prometheus metrics in text format
5. **Trace Context**: `admin_trace_ctx()` - Create trace context for admin operations

**Key Components**:

1. **Health Check**:
   - Checks NATS connection status (must be "connected")
   - Checks HTTP server status (assumed healthy if function is callable)
   - Returns 200 if healthy, 503 if unhealthy
   - Synchronized with HTTP `GET /_health` endpoint

2. **Status Information**:
   - Returns JSON string with Gateway status
   - Includes NATS connection status, timestamp
   - Format: `{"status":"operational","nats":"connected","timestamp":...}`

3. **Role-Based Access Control**:
   - **Admin role**: Can perform all actions (health, status, metrics, config, restart)
   - **Operator role**: Can perform read-only actions (health, status, metrics)
   - **Viewer role**: Can only read metrics
   - API key validation via `GATEWAY_ADMIN_API_KEY` environment variable

4. **Metrics Export**:
   - Prometheus text format (RFC 4180)
   - Metrics: `gateway_admin_requests_total`, `gateway_admin_errors_total`
   - Synchronized with HTTP `GET /metrics` endpoint

5. **Integration Points**:
   - HTTP endpoints: `GET /_health`, `GET /metrics` use admin_grpc functions
   - NATS client: Health check uses `nats_get_status_string()`
   - Metrics registry: Uses `prometheus_counter_inc()` for metric recording

**Implementation**:
- `apps/c-gateway/src/admin_grpc.c`: Admin API implementation
- `apps/c-gateway/src/admin_grpc.h`: Header file with function declarations
- `apps/c-gateway/tests/admin_grpc_test.c`: Comprehensive test suite (positive and negative scenarios)
- `apps/c-gateway/docs/ADMIN_GRPC_API.md`: API contract documentation

**Relationship with Router Admin API**:
- Gateway Admin API is **local to Gateway** (health, status, auth, metrics for Gateway)
- Router Admin API is **for Router** (GetValidatorsHealth, GetCheckpointStatus, policy management)
- Both APIs follow similar patterns but serve different components
- Gateway Admin API does not use NATS (unlike Router Admin API which uses NATS for some operations)

## Consequences

### Positive

- **Operational Control**: Enables health monitoring and status checks
- **RBAC**: Role-based access control provides security
- **Standards Compliance**: Prometheus metrics format is industry standard
- **Integration**: Synchronized with HTTP endpoints for consistency
- **Trace Support**: Trace context enables distributed tracing
- **Testability**: Comprehensive test suite ensures reliability

### Negative

- **Complexity**: Multiple roles and permissions to manage
- **Configuration**: Requires API key configuration
- **Maintenance**: Need to keep RBAC matrix updated

### Neutral

- **Future Enhancement**: Could add gRPC server for admin API (currently C functions)
- **Router Integration**: Could call Router Admin API via gRPC client (future enhancement)

## Alternatives Considered

### Alternative 1: HTTP-Only Admin API

**Description**: Use only HTTP endpoints for admin operations

**Pros**:
- Simpler implementation
- No need for separate admin module

**Cons**:
- Less structured than gRPC
- Harder to extend with complex operations

**Why not chosen**: Admin gRPC API provides better structure and extensibility for future operations

### Alternative 2: Router Admin API Proxy

**Description**: Gateway Admin API proxies all requests to Router Admin API

**Pros**:
- Single source of truth (Router)
- Less code duplication

**Cons**:
- Gateway-specific health/status not available
- Dependency on Router availability
- Network overhead

**Why not chosen**: Gateway needs local health/status checks independent of Router

## References

- `apps/c-gateway/src/admin_grpc.c`: Implementation
- `apps/c-gateway/src/admin_grpc.h`: Header file
- `apps/c-gateway/tests/admin_grpc_test.c`: Test suite
- `apps/c-gateway/docs/ADMIN_GRPC_API.md`: API contract documentation
- `docs/CP2_CHECKLIST.md`: CP2 checklist (Admin gRPC task)
- `apps/otp/router/src/router_admin_grpc.erl`: Router Admin API (reference)

