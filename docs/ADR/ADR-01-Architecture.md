---
version: 1.0
status: accepted
date: 2025-11-05
deciders:
  - Agent 2: Architecture/Tech Lead
related_adrs:
  - ADR-001-monorepo-structure.md
  - ADR-002-state-management.md
  - ADR-010-target-architecture.md
supersedes: []
superseded_by: []
---

# ADR-01: Architecture and Module Boundaries

## Status

accepted

## Context

BeamLine Constructor requires a clear architecture definition with module boundaries, responsibilities, and integration points. The system must integrate with STATE/HISTORY management (read-only at v0.1) and comply with `.trae/manifest.json` as the single source of truth.

The architecture must support:
- Local development and testing
- CI/CD validation gates
- Security constraints (HMAC masking, secret storage)
- Schema versioning (STATE 1.0.0, HISTORY 1.0.0)
- Artifact checksums format (array_object_v1)

## Decision

Adopt a **component-based architecture** with clear module boundaries:

### Module Components

#### 1. Backend (`apps/otp/`, `apps/caf/`, `apps/gateway/`)

**Responsibilities**:
- Business logic implementation
- API endpoints (REST, gRPC)
- Data processing and routing
- Integration with external services

**Integration with STATE/HISTORY**:
- **Read-only** access to `.trae/state.json` and `.trae/history.json`
- Read schema versions from `.trae/manifest.json`
- Validate against schemas before operations
- **No direct writes** to STATE/HISTORY (Agent 1/8 responsibility)

**Dependencies**:
- PostgreSQL (via epgsql/pgo or native drivers)
- NATS (for inter-service communication)
- gRPC (for service-to-service calls)

#### 2. Frontend (`apps/ui/`)

**Responsibilities**:
- User interface (SvelteKit)
- API client for backend services
- State management (local)
- Real-time updates (SSE)

**Integration with STATE/HISTORY**:
- **Read-only** access via API endpoints (`GET /status`, `GET /history`)
- No direct file system access
- Display schema versions and gate statuses

**Dependencies**:
- Backend API (HTTP/REST, SSE)
- No direct database access

#### 3. Scripts (`scripts/`)

**Responsibilities**:
- CI/CD validation utilities
- Local development checks
- Schema validation
- HMAC chain verification
- Secret leak detection

**Integration with STATE/HISTORY**:
- **Read and validate** `.trae/state.json` and `.trae/history.json`
- Verify against schemas from `.trae/manifest.json`
- Check artifact checksums
- Verify HMAC chain integrity
- **No writes** (validation only)

**Key Scripts** (from `.trae/manifest.json`):
- `scripts/validate_state.sh`: State validation
- `scripts/verify_hmac_chain.py`: HMAC chain verification
- `scripts/check_schema_changes.sh`: Schema changes validation
- `scripts/check_hmac_masking.sh`: HMAC masking verification
- `scripts/check_secret_leaks.sh`: Secret leak detection

#### 4. Security (`docs/archive/dev/CI_SECRETS_SETUP.md`, Agent 6)

**Responsibilities**:
- Secret management policy
- HMAC masking rules
- Secret storage guidelines
- Security validation

**Integration with STATE/HISTORY**:
- Validate HMAC masking in documentation
- Check for secret leaks
- Verify secret compatibility with HMAC chain
- **No direct STATE/HISTORY writes**

#### 5. Observability (`docs/OBSERVABILITY.md`, Agent 10)

**Responsibilities**:
- Metrics collection
- Distributed tracing
- Logging conventions
- Alerting rules

**Integration with STATE/HISTORY**:
- Log trace_id for correlation
- Include schema versions in metrics
- **No direct STATE/HISTORY writes**

### Module Boundaries

#### Boundary 1: Backend ↔ STATE/HISTORY

**Protocol**: File system (read-only)  
**Access Pattern**: Read `.trae/state.json`, `.trae/history.json` via file I/O  
**Validation**: JSON-Schema validation against `docs/STATE.schema.json`, `docs/HISTORY.schema.json`  
**Constraints**:
- Read-only access (no writes)
- Schema versions from `.trae/manifest.json`
- Artifact checksums format: `array_object_v1`

**Example**:
```typescript
// Backend reads state.json
const state = JSON.parse(fs.readFileSync('.trae/state.json', 'utf8'));
const manifest = JSON.parse(fs.readFileSync('.trae/manifest.json', 'utf8'));

// Validate against schema
const stateSchema = JSON.parse(fs.readFileSync('docs/STATE.schema.json', 'utf8'));
validate(state, stateSchema);

// Check schema version matches manifest
assert(stateSchema.version === manifest.schema_versions.state.version);
```

#### Boundary 2: Frontend ↔ Backend

**Protocol**: HTTP/REST, Server-Sent Events  
**Endpoints**: `/api/v1/health`, `/api/v1/status`, `/api/v1/history`  
**Data Format**: JSON  
**Constraints**:
- Frontend has no direct access to STATE/HISTORY files
- All access via backend API
- Correlation via `trace_id`

#### Boundary 3: Scripts ↔ STATE/HISTORY

**Protocol**: File system (read-only for validation)  
**Access Pattern**: Read and validate, no writes  
**Validation Scripts**: From `.trae/manifest.json` validation_tools  
**Constraints**:
- Scripts validate but do not modify STATE/HISTORY
- Use schemas from `.trae/manifest.json`
- Report validation results

#### Boundary 4: All Components ↔ Manifest

**Protocol**: File system (read-only)  
**Access Pattern**: Read `.trae/manifest.json` as single source of truth  
**Usage**:
- Schema versions
- Validation tool paths
- Compatibility policy
- Security constraints
- CI environment detection

### Integration Points with STATE/HISTORY

#### Read Operations (v0.1)

All components can **read** STATE/HISTORY:

1. **Read `.trae/state.json`**:
   - Schema version: `1.0.0` (from manifest)
   - Schema file: `docs/STATE.schema.json`
   - Schema $id: `https://beamline.example.com/schemas/state/v1.0.0`
   - Artifact checksums format: `array_object_v1`

2. **Read `.trae/history.json`**:
   - Schema version: `1.0.0` (from manifest)
   - Schema file: `docs/HISTORY.schema.json`
   - Schema $id: `https://beamline.example.com/schemas/history/v1.0.0`
   - Format: Array of history entries with HMAC chain

3. **Read `.trae/manifest.json`**:
   - Single source of truth for all schema versions
   - Validation tool paths
   - Compatibility policy
   - Security constraints

#### Write Operations (v0.1)

**Forbidden** at v0.1:
- ❌ No direct writes to `.trae/state.json`
- ❌ No direct writes to `.trae/history.json`
- ❌ No schema modifications

**Allowed** (future, via Agent 1/8):
- Agent 1: Schema modifications
- Agent 8: STATE/HISTORY writes (with coordination)

## Constraints from Manifest

### Schema Versions

From `.trae/manifest.json`:

- **STATE schema**: version `1.0.0`, file: `docs/STATE.schema.json`, $id: `https://beamline.example.com/schemas/state/v1.0.0`
- **HISTORY schema**: version `1.0.0`, file: `docs/HISTORY.schema.json`, $id: `https://beamline.example.com/schemas/history/v1.0.0`
- **Artifact checksums format**: `array_object_v1` (version `1.0.0`)

### Compatibility Policy

From `.trae/manifest.json` compatibility_policy:

- **Versioning scheme**: Semver (MAJOR.MINOR.PATCH)
- **Major changes**: Breaking changes requiring migration
- **Minor changes**: Backward-compatible additions
- **Patch changes**: Bug fixes and clarifications

### CI Environment Detection

From `.trae/manifest.json` ci_env_detection:

- **Production detection**: Any of `CI=true`, `GITHUB_ACTIONS=true`, `GITLAB_CI=true`, `DRONE=true`, `CIRCLECI=true`, `TRAVIS=true`, `PRODUCTION=true`
- **Secret required**: `BEAMLINE_HMAC_SECRET` required in production
- **Runner requirements**: Bash (required), PowerShell (optional)

### Security Constraints

From `.trae/manifest.json` security:

- **HMAC masking policy**: `16+...` (first 16 hex characters + `...`)
- **Secret storage**: CI/CD secrets/variables only
- **Forbidden locations**: Repository code, public documentation, environment variables on servers, version control

### Validation Tools

From `.trae/manifest.json` validation_tools:

- `scripts/validate_state.sh`: State validation (required)
- `scripts/verify_hmac_chain.py`: HMAC chain verification (required)
- `scripts/check_schema_changes.sh`: Schema changes validation (required, ci_gate)
- `scripts/check_hmac_masking.sh`: HMAC masking verification (optional)
- `scripts/check_secret_leaks.sh`: Secret leak detection (optional)

## Consequences

### Positive

- **Clear boundaries**: Each module has distinct responsibilities
- **Read-only STATE/HISTORY**: Prevents accidental modifications
- **Manifest compliance**: All components align with single source of truth
- **Testability**: Clear boundaries enable isolated testing
- **Security**: HMAC masking and secret storage policies enforced

### Negative

- **Limited write access**: Components cannot directly update STATE/HISTORY (requires Agent 1/8)
- **Validation overhead**: All reads must validate against schemas
- **Dependency on manifest**: All components depend on `.trae/manifest.json`

### Neutral

- **Versioning**: Schema versions fixed at 1.0.0 for v0.1
- **Future extensibility**: Write operations can be added in future versions

## Alternatives Considered

### Alternative 1: Direct STATE/HISTORY Writes

**Description**: Allow all components to write to STATE/HISTORY

**Pros**:
- More flexible
- Faster updates

**Cons**:
- Risk of breaking HMAC chain
- Schema inconsistencies
- No audit trail coordination

**Why not chosen**: Need for centralized control and audit trail integrity

### Alternative 2: API for STATE/HISTORY

**Description**: Expose STATE/HISTORY via API endpoints

**Pros**:
- Centralized access control
- Easier to audit

**Cons**:
- Additional complexity
- Performance overhead
- Not needed for v0.1 (read-only)

**Why not chosen**: Overkill for v0.1 read-only requirements

## Implementation Notes

- Architecture aligns with `.trae/manifest.json` schema versions
- All module boundaries documented
- Integration points with STATE/HISTORY are read-only
- Validation scripts from manifest are used for local checks
- Security constraints (HMAC masking, secret storage) are enforced

## References

- `.trae/manifest.json`: Single source of truth for schema versions and validation rules
- `docs/STATE.schema.json`: State validation schema (version 1.0.0)
- `docs/HISTORY.schema.json`: History validation schema (version 1.0.0)
- `docs/CI_VALIDATION.md`: CI validation guide
- `docs/SCHEMA_VERSIONING.md`: Schema versioning policy
- `docs/archive/dev/AGENTS_CONTRACT.md`: Agent contract and responsibilities
- `.cursor/rules/agents/state-and-history-management.mdc`: STATE/HISTORY management rules

## Compliance

- ✅ Aligns with `.trae/manifest.json`
- ✅ Follows compatibility policy (semver)
- ✅ Respects security constraints (HMAC masking `16+...`, secret storage)
- ✅ Integrates with STATE/HISTORY (read-only at v0.1)
- ✅ Uses validation tools from manifest
- ✅ No schema modifications (STATE/HISTORY versions 1.0.0)


