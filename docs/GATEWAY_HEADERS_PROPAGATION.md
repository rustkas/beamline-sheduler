# Gateway Headers Propagation - Current Status

**Date**: 2025-01-27  
**Status**: ✅ Current Implementation Status  
**Component**: C-Gateway (`apps/c-gateway/`)  
**Purpose**: Document actual header propagation behavior in Gateway (not planned, but current implementation)

## 1. Source of Truth

### 1.1. Implementation Location

**Primary Implementation**: `apps/c-gateway/src/http_server.c`

- **Header Extraction**: Lines 2618-2652 (`handle_client` function)
- **Context Building**: Lines 1719-1868 (`build_route_request_json` function)
- **Tracing Integration**: Lines 2654-2691 (OpenTelemetry traceparent extraction)

**Configuration**:
- No separate config file for header rules
- Header processing logic is hardcoded in `http_server.c`
- Environment variables: `GATEWAY_AUTH_REQUIRED` (affects Authorization header handling)

### 1.2. Header Processing Flow

```
HTTP Request
    ↓
Header Extraction (http_server.c:2618-2652)
    ↓
Context Population (request_context_t)
    ↓
RouteRequest JSON Building (http_server.c:1719-1868)
    ↓
NATS Request (nats_request_decide)
    ↓
Router (via NATS JSON payload)
```

## 2. Header Categories

### 2.1. Tracing Headers (Mandatory/Optional)

| Header | Source | Extraction | Propagation | Destination | Status |
|--------|--------|------------|-------------|-------------|--------|
| `X-Trace-ID` | HTTP Header | ✅ Extracted to `ctx.trace_id` | ✅ Passed to Router | RouteRequest JSON `trace_id` field | ✅ Implemented |
| `traceparent` | HTTP Header (W3C) | ✅ Extracted and parsed | ✅ Used for OpenTelemetry spans | OpenTelemetry context (not in Router JSON) | ✅ Implemented |
| `span_id` | Derived from `traceparent` | ✅ Extracted from traceparent | ✅ Used for OpenTelemetry | OpenTelemetry spans only | ✅ Implemented |

**Implementation Details**:
- `X-Trace-ID`: Extracted at line 2637-2643, stored in `request_context_t.trace_id`
- `traceparent`: Extracted at line 2644-2648, parsed via `otel_extract_trace_context()` (line 2659)
- Priority: `X-Trace-ID` header takes precedence over `trace_id` from request body
- Fallback: If header missing, `trace_id` from request body is used (line 1796-1802)

**Propagation to Router**:
- `trace_id` is included in RouteRequest JSON (line 1784-1802)
- Priority: Context `trace_id` (from header) > Body `trace_id`
- OpenTelemetry spans are created with trace context but not passed to Router

### 2.2. Authentication/Authorization Headers

| Header | Source | Extraction | Propagation | Destination | Status |
|--------|--------|------------|-------------|-------------|--------|
| `Authorization` | HTTP Header | ✅ Detected (presence only) | ❌ **NOT passed to Router** | Gateway auth check only | ⚠️ Partially implemented |
| `X-Tenant-ID` | HTTP Header | ✅ Extracted to `ctx.tenant_id` | ✅ Passed to Router | RouteRequest JSON `tenant_id` field | ✅ Implemented |

**Implementation Details**:
- `Authorization`: Only presence is checked (line 2649-2651), value is **NOT extracted or stored**
  - Used for: Gateway-level auth check if `GATEWAY_AUTH_REQUIRED=true` (line 2750-2758)
  - **Gap**: Authorization token is not passed to Router/NATS
- `X-Tenant-ID`: Extracted at line 2629-2636, stored in `request_context_t.tenant_id`
  - **Required** for API calls (line 2759-2768)
  - Priority: Header `X-Tenant-ID` > Body `tenant_id` (line 1756-1775)

**Security Note**:
- `Authorization` header is **dropped** and not propagated to Router
- This is intentional for security (Gateway handles auth, Router doesn't need token)

### 2.3. Technical Headers

| Header | Source | Extraction | Propagation | Destination | Status |
|--------|--------|------------|-------------|-------------|--------|
| `Content-Type` | HTTP Header | ✅ Validated | ❌ **NOT passed to Router** | Gateway validation only | ✅ Implemented |
| `Accept` | HTTP Header | ❌ Not extracted | ❌ Not used | N/A | ❌ Not implemented |
| `User-Agent` | HTTP Header | ❌ Not extracted | ❌ Not used | N/A | ❌ Not implemented |
| `X-Forwarded-For` | HTTP Header | ❌ Not extracted | ❌ Not used | N/A | ❌ Not implemented |
| `X-Forwarded-Proto` | HTTP Header | ❌ Not extracted | ❌ Not used | N/A | ❌ Not implemented |

**Implementation Details**:
- `Content-Type`: Validated for JSON requests (implicitly via JSON parsing)
- Other technical headers: **Not extracted or used** in current implementation
- Client IP: Extracted from socket (`getpeername`) at line 2316-2322, stored in `client_ip` variable
  - Used for: Rate limiting, abuse detection, logging
  - **NOT passed to Router** in RouteRequest JSON

### 2.4. Request Context Headers

| Header | Source | Extraction | Propagation | Destination | Status |
|--------|--------|------------|-------------|-------------|--------|
| `X-Request-ID` | HTTP Header | ❌ Not extracted | ❌ Not used | N/A | ❌ Not implemented |
| `correlation-id` | HTTP Header | ❌ Not extracted | ❌ Not used | N/A | ❌ Not implemented |

**Implementation Details**:
- `request_id` comes from **request body** (JSON field), not HTTP header
- Extracted in `validate_decide_request()` at line 1695-1699
- Stored in `request_context_t.request_id`
- Passed to Router in RouteRequest JSON `request_id` field (line 1777-1782)

## 3. Propagation Rules by Category

### 3.1. Tracing Headers

**Rule**: `passthrough` (with priority)

- `X-Trace-ID`: Header → Context → RouteRequest JSON
- `traceparent`: Header → OpenTelemetry context (not in Router JSON)
- Priority: HTTP header > Request body field

**Code Reference**:
```c
// Line 2637-2643: Extract X-Trace-ID header
else if (strncmp(line, trace_header_name, strlen(trace_header_name)) == 0) {
    const char *value = line + strlen(trace_header_name);
    // ... trim whitespace ...
    strncpy(ctx.trace_id, value, sizeof(ctx.trace_id) - 1U);
}

// Line 1784-1802: Propagate to Router (prefer context over body)
const char *trace_src = (ctx != NULL && ctx->trace_id[0] != '\0')
                            ? ctx->trace_id : NULL;
if (trace_src != NULL) {
    json_object_set_new(route, "trace_id", json_string(trace_src));
} else {
    // Fallback to body
    json_t *trace_body = json_object_get(in_root, "trace_id");
    if (json_is_string(trace_body)) {
        json_object_set(route, "trace_id", trace_body);
    }
}
```

### 3.2. Authentication Headers

**Rule**: `drop` (security-sensitive)

- `Authorization`: Checked for presence, **NOT extracted or propagated**
- `X-Tenant-ID`: Extracted and propagated to Router

**Code Reference**:
```c
// Line 2649-2651: Authorization header detection (presence only)
else if (strncmp(line, auth_header_name, strlen(auth_header_name)) == 0) {
    has_auth_header = 1;  // Only flag, value not stored
}

// Line 2629-2636: X-Tenant-ID extraction and propagation
if (strncmp(line, tenant_header_name, strlen(tenant_header_name)) == 0) {
    has_tenant_header = 1;
    const char *value = line + strlen(tenant_header_name);
    // ... trim whitespace ...
    strncpy(ctx.tenant_id, value, sizeof(ctx.tenant_id) - 1U);
}

// Line 1756-1775: Propagate tenant_id to Router (prefer header over body)
const char *tenant_src = (ctx != NULL && ctx->tenant_id[0] != '\0')
                             ? ctx->tenant_id : NULL;
if (tenant_src != NULL) {
    json_object_set_new(route, "tenant_id", json_string(tenant_src));
} else {
    // Fallback to body
    json_t *tenant_body = json_object_get(in_root, "tenant_id");
    if (json_is_string(tenant_body)) {
        json_object_set(route, "tenant_id", tenant_body);
    }
}
```

### 3.3. Request Body Fields (Not Headers, but Propagated)

**Fields extracted from request body and propagated to Router**:

| Field | Source | Propagation | Destination |
|-------|--------|-------------|-------------|
| `version` | Request body JSON | ✅ Passed as-is | RouteRequest JSON `version` |
| `request_id` | Request body JSON | ✅ Passed as-is | RouteRequest JSON `request_id` |
| `run_id` | Request body JSON | ✅ Passed as-is (optional) | RouteRequest JSON `run_id` |
| `message.*` | Request body JSON | ✅ Passed as nested object | RouteRequest JSON `message` |
| `policy_id` | Request body JSON | ✅ Passed as-is | RouteRequest JSON `policy_id` |
| `context` | Request body JSON | ✅ Passed as-is (transparent) | RouteRequest JSON `context` |

**Note**: These are **not HTTP headers**, but JSON fields in request body. Included here for completeness.

## 4. Propagation to Router/NATS

### 4.1. Current Implementation

**Method**: JSON payload via NATS (not HTTP headers)

- Gateway builds RouteRequest JSON object
- JSON is sent via `nats_request_decide()` (line 2422)
- Router receives JSON payload, not HTTP headers

**RouteRequest JSON Structure** (as built by `build_route_request_json`):
```json
{
  "version": "1",
  "tenant_id": "...",      // From X-Tenant-ID header (preferred) or body
  "request_id": "...",     // From request body
  "trace_id": "...",        // From X-Trace-ID header (preferred) or body
  "run_id": "...",          // From request body (optional)
  "message": {
    "message_id": "...",
    "message_type": "...",
    "payload": {...},
    "metadata": {...}
  },
  "policy_id": "...",
  "context": {...}          // Transparent passthrough
}
```

### 4.2. What is NOT Propagated

**Dropped/Not Extracted**:
- `Authorization` header (security-sensitive)
- `Content-Type` header (validated but not passed)
- `Accept` header (not used)
- `User-Agent` header (not used)
- `X-Forwarded-*` headers (not used)
- `X-Request-ID` header (not extracted, `request_id` comes from body)
- `correlation-id` header (not extracted)
- Client IP (extracted but not passed to Router)

**Note**: Client IP is used locally for rate limiting and abuse detection, but not sent to Router.

## 5. Known Gaps / TODO

### 5.1. Missing Header Extraction

**Not Currently Extracted**:
- `X-Request-ID` / `correlation-id` headers (request_id comes from body only)
- `User-Agent` (could be useful for analytics)
- `X-Forwarded-For` / `X-Forwarded-Proto` (useful behind proxies)
- `Accept` header (content negotiation not implemented)

### 5.2. Propagation Gaps

**Not Propagated to Router**:
- `Authorization` token (intentional for security)
- Client IP address (used locally but not sent to Router)
- `Content-Type` / `Accept` headers
- OpenTelemetry trace context (spans created but not in Router JSON)

### 5.3. Alignment with Router/Observability

**Deviations from Planned Spec**:
- **Planned**: Headers passed via NATS message headers/metadata
- **Actual**: Headers embedded in JSON payload only
- **Impact**: Router cannot access HTTP headers directly, only JSON fields

**Observability Integration**:
- ✅ OpenTelemetry spans created with trace context
- ✅ `trace_id` propagated in JSON (for Router logs)
- ⚠️ `span_id` not propagated to Router (only in OpenTelemetry)
- ❌ No correlation-id header support

### 5.4. Future Enhancements

**Planned (from specs/ADRs)**:
1. **NATS Headers Support**: Pass headers via NATS message headers (not just JSON)
2. **Correlation ID**: Extract `X-Request-ID` or `correlation-id` header
3. **Client IP Propagation**: Include client IP in RouteRequest (if needed by Router)
4. **User-Agent Extraction**: Extract for analytics/abuse detection
5. **Forwarded Headers**: Extract `X-Forwarded-*` for proxy-aware routing

**References**:
- `docs/ARCHITECTURE/api-registry.md` - Planned header propagation
- `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` - NATS message structure
- `docs/CP2_CHECKLIST.md` - Headers propagation task (marked COMPLETE)

## 6. Examples

### 6.1. Request with Tracing Headers

**HTTP Request**:
```http
POST /api/v1/routes/decide HTTP/1.1
X-Tenant-ID: tenant-123
X-Trace-ID: trace-456
traceparent: 00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01
Content-Type: application/json

{
  "version": "1",
  "tenant_id": "tenant-123",
  "request_id": "req-001",
  "task": {"type": "text.generate", "payload": {}}
}
```

**RouteRequest JSON (sent to Router)**:
```json
{
  "version": "1",
  "tenant_id": "tenant-123",      // From X-Tenant-ID header
  "request_id": "req-001",         // From request body
  "trace_id": "trace-456",         // From X-Trace-ID header (preferred over body)
  "message": {
    "task": {"type": "text.generate", "payload": {}}
  }
}
```

**Note**: `traceparent` is used for OpenTelemetry spans but not included in Router JSON.

### 6.2. Request with Authorization Header

**HTTP Request**:
```http
POST /api/v1/routes/decide HTTP/1.1
Authorization: Bearer token-abc123
X-Tenant-ID: tenant-123
Content-Type: application/json

{...}
```

**RouteRequest JSON (sent to Router)**:
```json
{
  "version": "1",
  "tenant_id": "tenant-123",
  "request_id": "req-002",
  "message": {...}
}
```

**Note**: `Authorization` header is **NOT** included in RouteRequest JSON (dropped for security).

## 7. Testing

### 7.1. Current Test Coverage

**Unit Tests**: None specifically for header propagation

**Integration Tests**: 
- Header extraction tested indirectly via request processing tests
- No dedicated tests for header propagation to Router

### 7.2. Recommended Tests

**Missing Test Coverage**:
1. **Header Priority Tests**: Verify header takes precedence over body field
2. **Propagation Tests**: Verify headers appear in RouteRequest JSON
3. **Security Tests**: Verify Authorization header is NOT propagated
4. **Tracing Tests**: Verify traceparent extraction and OpenTelemetry integration

## 8. References

- `apps/c-gateway/src/http_server.c` - Main implementation
- `apps/c-gateway/src/tracing/otel.c` - OpenTelemetry traceparent parsing
- `docs/ARCHITECTURE/api-registry.md` - Planned header propagation spec
- `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` - NATS message structure
- `docs/CP2_CHECKLIST.md` - Headers propagation task status

