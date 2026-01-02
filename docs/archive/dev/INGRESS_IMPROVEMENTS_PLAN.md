# Ingress Improvements Plan

**Date**: 2025-01-27  
**Status**: 📋 Planning  
**Component**: Ingress  
**Current Status**: Stub (CP3 planned)  
**Target**: CP1 Observability Compliance + Basic Functionality

## Current State

### Implementation Status

**Current**: Stub implementation
- Location: `infra/docker/ingress/ingress_health.py`
- Functionality: Basic health check stub
- Technology: Python (minimal)
- Status: Not production-ready

### CP1 Observability Requirements

According to `docs/OBSERVABILITY_CP1_INVARIANTS.md`:

**Required CP1 Fields** (when context available):
- ✅ `tenant_id` - Required when tenant context is available
- ✅ `trace_id` - Required when trace context is available
- ❌ `run_id` - Not required (Ingress does not process runs)
- ❌ `flow_id` - Not required (Ingress does not process flows)
- ❌ `step_id` - Not required (Ingress does not process steps)

**Health Endpoints**:
- `GET /_health` - Liveness probe (HTTP 200 OK, body: `ok`)
- `GET /_readyz` - Readiness probe (HTTP 200 OK, body: `ready`)

**Logging**:
- Structured JSON logs with required fields (`timestamp`, `level`, `component`, `message`)
- CP1 fields (`tenant_id`, `trace_id`) when context available
- PII/secret filtering

---

## Improvement Plan

### Phase 1: CP1 Observability Compliance (High Priority)

#### 1.1. Structured JSON Logging

**Goal**: Implement structured JSON logging with CP1 fields support.

**Tasks**:
1. Create logging module (`apps/ingress/src/logging.py` or similar):
   - JSON formatter with required fields
   - CP1 fields extraction from context
   - PII filtering (passwords, tokens, secrets)
   - Log levels: ERROR, WARN, INFO, DEBUG

2. Integration points:
   - HTTP request handlers
   - Error handlers
   - Health check handlers

**Example Log Entry**:
```json
{
  "timestamp": "2025-01-27T12:00:00.123456Z",
  "level": "INFO",
  "component": "ingress",
  "message": "Request processed",
  "tenant_id": "tenant_123",
  "trace_id": "trace_abc123",
  "context": {
    "method": "POST",
    "path": "/api/v1/ingress",
    "status_code": 200,
    "latency_ms": 45
  }
}
```

**Files to Create/Update**:
- `apps/ingress/src/logging.py` - Logging module
- `apps/ingress/src/config.py` - Logging configuration
- Update all handlers to use structured logging

#### 1.2. Health Endpoints Enhancement

**Goal**: Implement proper health endpoints with JSON responses.

**Current**: Stub with basic text responses

**Tasks**:
1. Enhance `/_health` endpoint:
   - Return JSON: `{"status": "healthy", "timestamp": "ISO-8601"}`
   - Check internal dependencies (if any)
   - Return appropriate HTTP status codes

2. Enhance `/_readyz` endpoint:
   - Return JSON: `{"status": "ready", "timestamp": "ISO-8601"}`
   - Check external dependencies (Router gRPC, NATS, etc.)
   - Return appropriate HTTP status codes

**Example Response**:
```json
{
  "status": "healthy",
  "timestamp": "2025-01-27T12:00:00Z",
  "checks": {
    "router": {
      "status": "ok",
      "message": "Connection successful"
    },
    "nats": {
      "status": "ok",
      "message": "Connection successful"
    }
  }
}
```

**Files to Update**:
- `infra/docker/ingress/ingress_health.py` → `apps/ingress/src/health.py`
- Add proper health check logic

#### 1.3. CP1 Fields Extraction

**Goal**: Extract and log CP1 fields (`tenant_id`, `trace_id`) from incoming requests.

**Tasks**:
1. Extract `tenant_id` from:
   - HTTP header: `X-Tenant-ID`
   - Request context (if available)
   - JWT claims (future: CP2+)

2. Extract `trace_id` from:
   - HTTP header: `Traceparent` (W3C Trace Context)
   - HTTP header: `X-Trace-ID` (fallback)
   - Request context (if available)

3. Propagate CP1 fields:
   - Include in logs
   - Forward to downstream services (Router, NATS)
   - Include in tracing spans (if OpenTelemetry integrated)

**Files to Create/Update**:
- `apps/ingress/src/context.py` - Context extraction utilities
- `apps/ingress/src/middleware.py` - Middleware for CP1 fields extraction
- Update request handlers to use extracted context

---

### Phase 2: Basic Functionality (Medium Priority)

#### 2.1. Request Processing

**Goal**: Implement basic request processing logic.

**Tasks**:
1. HTTP request handling:
   - Parse incoming requests
   - Validate required headers/fields
   - Extract CP1 fields
   - Forward to Router (gRPC) or NATS

2. Error handling:
   - Structured error responses
   - Error logging with CP1 fields
   - Proper HTTP status codes

**Files to Create**:
- `apps/ingress/src/handlers.py` - Request handlers
- `apps/ingress/src/errors.py` - Error definitions
- `apps/ingress/src/router_client.py` - Router gRPC client

#### 2.2. Router Integration

**Goal**: Integrate with Router via gRPC.

**Tasks**:
1. gRPC client setup:
   - Connection to Router (port 9000)
   - Health check integration
   - Error handling and retries

2. Request forwarding:
   - Convert HTTP requests to gRPC `RouteRequest`
   - Include CP1 fields in requests
   - Handle gRPC responses

**Files to Create**:
- `apps/ingress/src/router_client.py` - gRPC client
- `apps/ingress/proto/` - Protobuf definitions (if needed)

#### 2.3. NATS Integration (Optional)

**Goal**: Support NATS messaging for async operations.

**Tasks**:
1. NATS client setup:
   - Connection to NATS server
   - Subject subscriptions
   - Message publishing

2. Message handling:
   - Parse NATS messages
   - Extract CP1 fields
   - Forward to appropriate handlers

**Files to Create**:
- `apps/ingress/src/nats_client.py` - NATS client
- `apps/ingress/src/nats_handler.py` - NATS message handlers

---

### Phase 3: Advanced Features (Low Priority)

#### 3.1. OpenTelemetry Integration

**Goal**: Add distributed tracing support.

**Tasks**:
1. OpenTelemetry setup:
   - Trace context extraction (W3C Trace Context)
   - Span creation and propagation
   - OTLP exporter configuration

2. Integration:
   - HTTP request spans
   - gRPC call spans
   - NATS message spans

**Files to Create**:
- `apps/ingress/src/tracing.py` - OpenTelemetry integration

#### 3.2. Metrics Collection

**Goal**: Add Prometheus metrics.

**Tasks**:
1. Metrics definition:
   - Request count
   - Request latency
   - Error count
   - Health check status

2. Export endpoint:
   - `GET /metrics` - Prometheus metrics

**Files to Create**:
- `apps/ingress/src/metrics.py` - Metrics collection

#### 3.3. Configuration Management

**Goal**: Centralized configuration.

**Tasks**:
1. Configuration file:
   - Environment variables support
   - Default values
   - Validation

2. Configuration schema:
   - Router gRPC URL
   - NATS connection string
   - Logging level
   - Health check intervals

**Files to Create**:
- `apps/ingress/src/config.py` - Configuration management
- `apps/ingress/config/config.yaml` - Configuration file template

---

## Implementation Roadmap

### Sprint 1: CP1 Observability Compliance

**Duration**: 1-2 weeks

**Deliverables**:
- ✅ Structured JSON logging with CP1 fields
- ✅ Enhanced health endpoints (`/_health`, `/_readyz`)
- ✅ CP1 fields extraction (`tenant_id`, `trace_id`)
- ✅ PII filtering in logs

**Acceptance Criteria**:
- All logs are structured JSON
- CP1 fields present when context available
- Health endpoints return proper JSON responses
- Validation script passes for Ingress

### Sprint 2: Basic Functionality

**Duration**: 2-3 weeks

**Deliverables**:
- ✅ HTTP request handling
- ✅ Router gRPC integration
- ✅ Error handling
- ✅ Request forwarding

**Acceptance Criteria**:
- Ingress accepts HTTP requests
- Requests forwarded to Router via gRPC
- CP1 fields propagated correctly
- Error responses are structured

### Sprint 3: Advanced Features (Optional)

**Duration**: 2-3 weeks

**Deliverables**:
- ✅ OpenTelemetry integration
- ✅ Prometheus metrics
- ✅ Configuration management
- ✅ NATS integration (if needed)

**Acceptance Criteria**:
- Traces visible in tracing system
- Metrics available at `/metrics`
- Configuration validated and loaded
- NATS messages processed (if implemented)

---

## File Structure

```
apps/ingress/
├── src/
│   ├── __init__.py
│   ├── main.py              # Application entry point
│   ├── config.py            # Configuration management
│   ├── logging.py           # Structured JSON logging
│   ├── health.py            # Health endpoints
│   ├── context.py           # CP1 fields extraction
│   ├── middleware.py        # Request middleware
│   ├── handlers.py          # HTTP request handlers
│   ├── router_client.py     # Router gRPC client
│   ├── nats_client.py       # NATS client (optional)
│   ├── errors.py            # Error definitions
│   ├── tracing.py           # OpenTelemetry integration (optional)
│   └── metrics.py           # Prometheus metrics (optional)
├── config/
│   └── config.yaml          # Configuration template
├── tests/
│   ├── test_logging.py
│   ├── test_health.py
│   ├── test_handlers.py
│   └── test_context.py
├── proto/                   # Protobuf definitions (if needed)
├── requirements.txt         # Python dependencies
└── README.md               # Documentation
```

---

## Dependencies

### Required

- **Python**: 3.9+ (or latest stable)
- **Framework**: FastAPI or Flask (recommended: FastAPI for async support)
- **gRPC**: `grpcio`, `grpcio-tools` for Router integration
- **JSON**: Built-in `json` module
- **Logging**: `structlog` or custom JSON formatter

### Optional

- **NATS**: `nats-py` for NATS integration
- **OpenTelemetry**: `opentelemetry-api`, `opentelemetry-sdk`, `opentelemetry-instrumentation-fastapi`
- **Prometheus**: `prometheus-client` for metrics
- **HTTP Client**: `httpx` or `requests` for external calls

---

## Testing Strategy

### Unit Tests

1. **Logging Tests**:
   - JSON format validation
   - CP1 fields inclusion
   - PII filtering

2. **Health Tests**:
   - Endpoint responses
   - Status codes
   - JSON structure

3. **Context Tests**:
   - CP1 fields extraction
   - Header parsing
   - Context propagation

### Integration Tests

1. **Router Integration**:
   - gRPC connection
   - Request forwarding
   - CP1 fields propagation

2. **End-to-End Tests**:
   - Full request flow
   - Error handling
   - Health checks

### Validation

1. **Observability Validation**:
   - Run `validate_observability_e2e.sh`
   - Verify CP1 fields in logs
   - Check health endpoints

2. **Manual Testing**:
   - Send test requests
   - Verify logs
   - Check health endpoints

---

## Migration from Stub

### Step 1: Replace Stub Health Check

**Current**: `infra/docker/ingress/ingress_health.py`

**Action**: 
- Create proper health endpoints in `apps/ingress/src/health.py`
- Update Dockerfile to use new entry point
- Test health endpoints

### Step 2: Add Logging

**Action**:
- Create `apps/ingress/src/logging.py`
- Integrate with health endpoints
- Test log format

### Step 3: Add CP1 Fields

**Action**:
- Create `apps/ingress/src/context.py`
- Extract CP1 fields from headers
- Include in logs

### Step 4: Add Request Handling

**Action**:
- Create `apps/ingress/src/handlers.py`
- Add Router gRPC client
- Forward requests

---

## Success Criteria

### CP1 Observability Compliance

- ✅ All logs are structured JSON
- ✅ CP1 fields (`tenant_id`, `trace_id`) present when context available
- ✅ Health endpoints (`/_health`, `/_readyz`) return proper JSON
- ✅ PII filtering works correctly
- ✅ Validation script passes

### Basic Functionality

- ✅ Ingress accepts HTTP requests
- ✅ Requests forwarded to Router
- ✅ CP1 fields propagated correctly
- ✅ Error handling works

### Code Quality

- ✅ Unit tests coverage > 80%
- ✅ Integration tests pass
- ✅ Linter passes
- ✅ Documentation complete

---

## References

- `docs/OBSERVABILITY_CP1_INVARIANTS.md` - CP1 observability requirements
- `docs/OBSERVABILITY.md` - General observability requirements
- `config/observability/logging.json` - Log format schema
- `scripts/observability/validate_observability_e2e.sh` - Validation script
- `infra/docker/ingress/ingress_health.py` - Current stub implementation

---

## Next Steps

1. **Review and Approve Plan**: Get stakeholder approval
2. **Create Project Structure**: Set up `apps/ingress/` directory
3. **Implement Phase 1**: CP1 observability compliance
4. **Validate**: Run validation scripts
5. **Document**: Update documentation
6. **Iterate**: Continue with Phase 2 and 3

---

## Notes

- Ingress is currently planned for CP3, but CP1 observability compliance can be achieved earlier
- Focus on observability first, then add functionality
- Keep implementation minimal but compliant
- Use existing patterns from Router and Gateway where applicable

