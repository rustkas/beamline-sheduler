---
version: 1.0
status: accepted
date: 2025-11-05
deciders:
  - Agent 2: Architecture/Tech Lead
related_adrs:
  - ADR-01-Architecture.md
  - ADR-010-target-architecture.md
supersedes: []
superseded_by: []
---

# ADR-02: API Contracts v0.1

## Status

accepted

## Context

BeamLine Constructor requires minimal API contracts (v0.1) for local development, CI/CD validation, and integration with STATE/HISTORY. The API must:

- Provide health and status endpoints
- Expose STATE/HISTORY information (read-only)
- Support local validation gates
- Comply with `.trae/manifest.json` constraints
- Use consistent error models
- Follow security policies (HMAC masking, no secrets)

## Decision

Define **minimal API contracts v0.1** with three core endpoints:

### Endpoints

#### 1. GET /health

**Purpose**: Basic service health check  
**Security**: No authentication required  
**Response**: Simple health status  
**No PII/Secrets**: Returns only service status

#### 2. GET /status

**Purpose**: Summary of gate statuses and versions  
**Security**: No authentication required (public status)  
**Response**: State versions, history versions, artifact checksums format, gate statuses  
**No PII/Secrets**: Returns only public status information

#### 3. GET /history

**Purpose**: Read HISTORY with pagination and filtering  
**Security**: No authentication required (read-only, public audit trail)  
**Response**: Paginated history entries  
**No PII/Secrets**: HMAC values masked (`16+...` format)

### Data Transfer Objects (DTO)

#### HealthResponse

```typescript
interface HealthResponse {
  status: "healthy" | "degraded" | "unhealthy";
  timestamp: string;  // ISO 8601
}
```

#### StatusResponse

```typescript
interface StatusResponse {
  state_version: string;              // "1.0.0" from manifest
  history_version: string;            // "1.0.0" from manifest
  artifact_checksums_format: string;  // "array_object_v1" from manifest
  gates: Array<{
    name: string;                     // Gate name (e.g., "state_validation")
    status: "pass" | "fail" | "unknown";
    last_run_at?: string;             // ISO 8601 timestamp
    script?: string;                   // Script path from manifest
  }>;
  timestamp: string;                  // ISO 8601
}
```

#### HistoryItem

**Aligned with `docs/HISTORY.schema.json`**:

- Schema $id: `https://beamline.example.com/schemas/history/v1.0.0`
- Schema version: `1.0.0`

```typescript
interface HistoryItem {
  ts: string;                         // ISO 8601, matches schema
  actor: string;                      // Matches schema pattern: ^AGENT_\d+_[A-Z_]+$
  action: string;                     // Matches schema pattern: ^[a-z_]+$
  cp_from: string | null;             // Matches schema pattern: ^CP\d+-[A-Z]+$ or null
  cp_to: string;                      // Matches schema pattern: ^CP\d+-[A-Z]+$
  state_checksum: string;             // SHA256 hex (64 chars)
  hmac_prev: string;                  // HMAC-SHA256 (masked: first 16 hex + "...")
  hmac: string;                       // HMAC-SHA256 (masked: first 16 hex + "...")
  metadata?: Record<string, any>;     // Optional metadata
}
```

#### HistoryResponse

```typescript
interface HistoryResponse {
  items: HistoryItem[];
  pagination: {
    page: number;
    page_size: number;
    total: number;
    total_pages: number;
  };
  schema: {
    $id: string;                      // "https://beamline.example.com/schemas/history/v1.0.0"
    version: string;                  // "1.0.0"
  };
}
```

### Error Model

**Unified error response** for all endpoints:

```typescript
interface ErrorResponse {
  error: {
    code: string;                     // Error code (documented code space)
    message: string;                  // Human-readable message
    details?: Record<string, any>;    // Optional additional details
    trace_id?: string;                // Optional trace ID for correlation
  };
}
```

#### Error Code Space

**v0.1 Error Codes**:

- `INVALID_REQUEST`: Invalid request parameters (400)
- `NOT_FOUND`: Resource not found (404)
- `VALIDATION_ERROR`: Schema validation failed (422)
- `INTERNAL_ERROR`: Internal server error (500)
- `SERVICE_UNAVAILABLE`: Service temporarily unavailable (503)

**Future Error Codes** (reserved for v0.2+):
- `UNAUTHORIZED`: Missing or invalid authentication (401)
- `FORBIDDEN`: Insufficient permissions (403)
- `RATE_LIMIT_EXCEEDED`: Rate limit exceeded (429)

### Query Parameters

#### GET /history

- `page`: Page number (default: 1, min: 1)
- `page_size`: Items per page (default: 20, min: 1, max: 100)
- `from_ts`: Filter from timestamp (ISO 8601, optional)
- `to_ts`: Filter to timestamp (ISO 8601, optional)

**Example**:
```
GET /api/v1/history?page=1&page_size=20&from_ts=2025-01-01T00:00:00Z&to_ts=2025-01-31T23:59:59Z
```

## Constraints from Manifest

### Schema Versions

From `.trae/manifest.json`:

- **STATE schema**: version `1.0.0`, $id: `https://beamline.example.com/schemas/state/v1.0.0`
- **HISTORY schema**: version `1.0.0`, $id: `https://beamline.example.com/schemas/history/v1.0.0`
- **Artifact checksums format**: `array_object_v1` (version `1.0.0`)

### Compatibility Policy

From `.trae/manifest.json` compatibility_policy:

- **Versioning**: Semver (MAJOR.MINOR.PATCH)
- **Minor changes**: Backward-compatible additions (new optional fields, new endpoints)
- **Major changes**: Breaking changes (removing fields, changing types, changing structure)

### Security Constraints

From `.trae/manifest.json` security:

- **HMAC masking**: All HMAC values in responses must be masked (`16+...` format)
- **No secrets**: No real secrets in API responses or examples
- **Secret storage**: CI/CD secrets/variables only

### STATE/HISTORY Read-Only

**v0.1 Constraint**: API provides **read-only** access to STATE/HISTORY:

- ✅ Read `.trae/state.json` for status information
- ✅ Read `.trae/history.json` for history entries
- ❌ No write operations (Agent 1/8 responsibility)
- ❌ No schema modifications

## Versioning and Compatibility

### API Versioning

- **URL-based**: `/api/v1/*` for v0.1
- **Future versions**: `/api/v2/*` for breaking changes

### Compatibility Rules

**Minor changes** (backward-compatible):
- Adding optional fields to DTOs
- Adding new endpoints
- Extending error code space
- Adding query parameters (optional)

**Major changes** (breaking):
- Removing required fields
- Changing field types
- Changing response structure
- Removing endpoints

**Migration**:
- Old versions supported for 6 months after new version release
- Migration guides in `docs/MIGRATION.md`
- Deprecation warnings in API responses

## Security and Masking

### HMAC Masking

All HMAC values in API responses must be masked:

- **Format**: `16+...` (first 16 hex characters + `...`)
- **Example**: `abc123def4567890...` (instead of full 64-char HMAC)
- **Validation**: `scripts/check_hmac_masking.sh` (from manifest)

### No Secrets

- ❌ No real secrets in API responses
- ❌ No real secrets in examples
- ❌ No API keys or tokens in responses
- ✅ Use placeholders: `YOUR_API_KEY_HERE`
- ✅ Mask HMAC values: `16+...`

### Example Response (Masked)

```json
{
  "items": [
    {
      "ts": "2025-11-05T12:00:00Z",
      "actor": "AGENT_1_REPO_STATE",
      "action": "init_cp0",
      "cp_from": null,
      "cp_to": "CP0-LC",
      "state_checksum": "abc123def4567890abcdef1234567890abcdef1234567890abcdef1234567890",
      "hmac_prev": "0000000000000000...",
      "hmac": "abc123def4567890..."
    }
  ]
}
```

## Integration with STATE/HISTORY

### Read Operations

API endpoints read from STATE/HISTORY:

1. **GET /status**:
   - Reads `.trae/manifest.json` for schema versions
   - Reads `.trae/state.json` for current state
   - Validates against `docs/STATE.schema.json`

2. **GET /history**:
   - Reads `.trae/history.json`
   - Validates against `docs/HISTORY.schema.json`
   - Masks HMAC values in responses

### Validation

All reads must:
- Validate against schemas from `.trae/manifest.json`
- Check schema versions match manifest
- Verify artifact checksums format (`array_object_v1`)
- Mask HMAC values before returning

### No Write Operations

**v0.1**: API does **not** provide write operations:
- ❌ No `POST /state` endpoint
- ❌ No `POST /history` endpoint
- ❌ No schema modifications

**Future** (v0.2+): Write operations may be added via Agent 1/8 coordination

## Consequences

### Positive

- **Minimal API**: Simple, focused endpoints for v0.1
- **Read-only STATE/HISTORY**: Prevents accidental modifications
- **Consistent error model**: Unified error handling
- **Security**: HMAC masking and no secrets enforced
- **Manifest compliance**: All versions and formats from manifest

### Negative

- **Limited functionality**: Only read operations at v0.1
- **No authentication**: Endpoints are public (acceptable for v0.1 status/history)
- **Validation overhead**: All reads must validate against schemas

### Neutral

- **Future extensibility**: Write operations can be added in v0.2+
- **Versioning**: API versioning strategy defined for future changes

## Alternatives Considered

### Alternative 1: Full CRUD API

**Description**: Complete CRUD operations for STATE/HISTORY

**Pros**:
- More flexible
- Complete functionality

**Cons**:
- Complex for v0.1
- Risk of breaking HMAC chain
- Requires Agent 1/8 coordination

**Why not chosen**: Overkill for v0.1, read-only sufficient for local/CI validation

### Alternative 2: GraphQL API

**Description**: GraphQL for flexible queries

**Pros**:
- Flexible querying
- Single endpoint

**Cons**:
- Additional complexity
- Not needed for v0.1 simple queries

**Why not chosen**: REST is sufficient for v0.1, simpler to implement

## Implementation Notes

- API contracts align with `.trae/manifest.json` schema versions
- All DTOs reference schema $id and version
- HMAC masking enforced in all responses
- No real secrets in examples or responses
- Read-only STATE/HISTORY access at v0.1

## References

- `.trae/manifest.json`: Single source of truth for schema versions
- `docs/STATE.schema.json`: State validation schema (version 1.0.0, $id: https://beamline.example.com/schemas/state/v1.0.0)
- `docs/HISTORY.schema.json`: History validation schema (version 1.0.0, $id: https://beamline.example.com/schemas/history/v1.0.0)
- `docs/CI_VALIDATION.md`: CI validation guide
- `docs/SCHEMA_VERSIONING.md`: Schema versioning policy
- `docs/archive/dev/AGENTS_CONTRACT.md`: Agent contract

## Compliance

- ✅ Aligns with `.trae/manifest.json`
- ✅ Follows compatibility policy (semver)
- ✅ Respects security constraints (HMAC masking `16+...`, no secrets)
- ✅ Integrates with STATE/HISTORY (read-only at v0.1)
- ✅ No schema modifications (STATE/HISTORY versions 1.0.0)
- ✅ DTOs aligned with schema $id and version

