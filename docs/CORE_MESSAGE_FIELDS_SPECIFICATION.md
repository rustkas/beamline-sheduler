# Core Message Fields Specification

**Version**: 1.0  
**Status**: Draft  
**Last Updated**: 2025-01-27  
**Scope**: All "core" messages in BeamLine Constructor (NATS / gRPC / REST DTO)

## Purpose

This document defines **strict, unambiguous contracts** for **mandatory fields** in all "core" messages across BeamLine Constructor services (Router, Gateway, Worker, Extensions). This specification ensures:

- All services understand these fields identically
- Validators and tests can strictly verify their presence and format
- Documentation is the single source of truth

## Scope

**Applies to**:
- All NATS messages (request/reply, pub/sub)
- All gRPC messages (request/response)
- All REST DTOs (request/response)

**Excludes**:
- Technical/service messages (health checks, metrics, internal events)
- Messages that explicitly opt out (documented exceptions)

## Core Fields Overview

| Field | Type | Format | Required In | CP Version |
|-------|------|--------|-------------|------------|
| `run_id` | string | UUID v4 or ULID | Multi-step workflows (CP2+) | CP2+ |
| `flow_id` | string | UUID v4 or ULID | Multi-step workflows (CP2+) | CP2+ |
| `step_id` | string | UUID v4 or ULID | Multi-step workflows (CP2+) | CP2+ |
| `idempotency_key` | string | Non-empty string | All business messages | CP1+ |
| `trace_id` | string | W3C Trace Context or UUID v4 | All messages (optional in CP1, required in CP2+) | CP1+ (optional), CP2+ (required) |
| `tenant_id` | string | Non-empty string | All business messages | CP1+ |
| `schema_version` | string | Semantic version (e.g., `"1"`) | All messages | CP1+ |

**Note**: `schema_version` is also known as `version` in NATS JSON payloads (see `API_CONTRACTS.md`).

## Field Specifications

### 1. `run_id`

#### Semantics

**What it is**: Identifier for a specific execution run of a pipeline/order/workflow.

**BeamLine Context**:
- Uniquely identifies a single execution instance of a multi-step workflow
- Remains constant across all steps within the same run
- Used for correlation across services (Router, Gateway, Worker, Extensions)

#### Obligation and Scope

**Required in**:
- All messages in multi-step workflows (CP2+)
- Messages that are part of a workflow execution
- Messages that need run-level correlation

**Optional in**:
- Single-step requests (CP1 baseline)
- Health checks and technical messages
- Messages that are not part of a workflow

**Non-empty requirement**: **MUST** be non-empty string if present (cannot be `""` or `null`)

#### Type and Format

**Type**: `string`

**Format**:
- **Preferred**: UUID v4 (e.g., `"550e8400-e29b-41d4-a716-446655440000"`)
- **Alternative**: ULID (e.g., `"01ARZ3NDEKTSV4RRFFQ69G5FAV"`)
- **Validation**: Must match UUID v4 regex or ULID format

**Examples**:
```json
"run_id": "550e8400-e29b-41d4-a716-446655440000"
"run_id": "01ARZ3NDEKTSV4RRFFQ69G5FAV"
```

#### Source and Generation Invariants

**Who generates**:
- **Gateway** (for new runs) or **Orchestrator** (CP3+)
- Generated once per run and propagated to all messages in that run

**Propagation**:
- **Router**: Passes through without modification
- **Worker**: Passes through without modification
- **Extensions**: Passes through without modification

**Invariants**:
- `run_id` is **unique** within the system (no two runs share the same `run_id`)
- `run_id` is **stable** across all steps in the same run (never changes)
- `run_id` is **immutable** (never modified after generation)

#### Usage in Logs and Metrics

**Logs**:
- **Always included** in JSON logs when present
- Field name: `run_id`
- Used for log correlation and filtering

**Metrics**:
- **Not used as label** (high cardinality risk)
- Used in trace context only

#### Security and Multi-Tenancy

**Multi-tenancy**:
- `run_id` is **tenant-scoped** (implicitly via `tenant_id`)
- Cannot be used to "jump" between tenants (must validate `tenant_id` first)
- Access control: Only accessible within the same tenant

**Security**:
- `run_id` is **not sensitive** (can be logged)
- No PII in `run_id` (random UUID/ULID)

---

### 2. `flow_id`

#### Semantics

**What it is**: Identifier for a flow definition/graph/process template.

**BeamLine Context**:
- Identifies the **definition** of a workflow (not the execution)
- Stable identifier for a workflow template
- Used to reference workflow definitions in policies and configurations

#### Obligation and Scope

**Required in**:
- All messages in multi-step workflows (CP2+)
- Messages that reference a workflow definition

**Optional in**:
- Single-step requests (CP1 baseline)
- Health checks and technical messages
- Messages that are not part of a workflow

**Non-empty requirement**: **MUST** be non-empty string if present (cannot be `""` or `null`)

#### Type and Format

**Type**: `string`

**Format**:
- **Preferred**: UUID v4 (e.g., `"550e8400-e29b-41d4-a716-446655440000"`)
- **Alternative**: ULID (e.g., `"01ARZ3NDEKTSV4RRFFQ69G5FAV"`)
- **Validation**: Must match UUID v4 regex or ULID format

**Examples**:
```json
"flow_id": "550e8400-e29b-41d4-a716-446655440000"
"flow_id": "01ARZ3NDEKTSV4RRFFQ69G5FAV"
```

#### Source and Generation Invariants

**Who generates**:
- **Orchestrator** or **Configuration Service** (when workflow is defined)
- Generated once per workflow definition and stored in configuration

**Propagation**:
- **Gateway**: Passes through from request or configuration
- **Router**: Passes through without modification
- **Worker**: Passes through without modification

**Invariants**:
- `flow_id` is **stable** (same workflow definition always has the same `flow_id`)
- `flow_id` is **deterministic** (can be looked up from workflow name/version)
- `flow_id` is **immutable** (never changes for a workflow definition)

#### Usage in Logs and Metrics

**Logs**:
- **Always included** in JSON logs when present
- Field name: `flow_id`
- Used for workflow-level log correlation

**Metrics**:
- **Can be used as label** (low cardinality - limited number of workflow definitions)
- Example: `router_requests_total{flow_id="..."}`

#### Security and Multi-Tenancy

**Multi-tenancy**:
- `flow_id` is **tenant-scoped** (workflow definitions are per-tenant)
- Cannot be used to access workflows from other tenants
- Access control: Validate `tenant_id` before accessing workflow definition

**Security**:
- `flow_id` is **not sensitive** (can be logged)
- No PII in `flow_id` (random UUID/ULID)

---

### 3. `step_id`

#### Semantics

**What it is**: Identifier for a specific step within a flow/workflow.

**BeamLine Context**:
- Identifies a specific step in a multi-step workflow execution
- Unique within a `run_id` (each step in a run has a unique `step_id`)
- Used for step-level correlation and tracking

#### Obligation and Scope

**Required in**:
- All messages in multi-step workflows (CP2+)
- Messages that are part of a specific step execution

**Optional in**:
- Single-step requests (CP1 baseline)
- Health checks and technical messages
- Messages that are not part of a workflow

**Non-empty requirement**: **MUST** be non-empty string if present (cannot be `""` or `null`)

#### Type and Format

**Type**: `string`

**Format**:
- **Preferred**: UUID v4 (e.g., `"550e8400-e29b-41d4-a716-446655440000"`)
- **Alternative**: ULID (e.g., `"01ARZ3NDEKTSV4RRFFQ69G5FAV"`)
- **Validation**: Must match UUID v4 regex or ULID format

**Examples**:
```json
"step_id": "550e8400-e29b-41d4-a716-446655440000"
"step_id": "01ARZ3NDEKTSV4RRFFQ69G5FAV"
```

#### Source and Generation Invariants

**Who generates**:
- **Orchestrator** or **Gateway** (for first step) or **Worker** (for subsequent steps)
- Generated once per step execution

**Propagation**:
- **Router**: Passes through without modification
- **Worker**: Passes through without modification
- **Extensions**: Passes through without modification

**Invariants**:
- `step_id` is **unique** within a `run_id` (no two steps in the same run share the same `step_id`)
- `step_id` is **deterministic** within a `flow_id` (same step position always generates same `step_id` pattern)
- `step_id` is **immutable** (never modified after generation)

#### Usage in Logs and Metrics

**Logs**:
- **Always included** in JSON logs when present
- Field name: `step_id`
- Used for step-level log correlation

**Metrics**:
- **Not used as label** (high cardinality risk)
- Used in trace context only

#### Security and Multi-Tenancy

**Multi-tenancy**:
- `step_id` is **tenant-scoped** (implicitly via `tenant_id` and `run_id`)
- Cannot be used to "jump" between tenants
- Access control: Validate `tenant_id` and `run_id` before accessing step

**Security**:
- `step_id` is **not sensitive** (can be logged)
- No PII in `step_id` (random UUID/ULID)

---

### 4. `idempotency_key`

#### Semantics

**What it is**: Key for preventing duplicate processing of the same request.

**BeamLine Context**:
- Prevents duplicate execution when the same request is retried
- Used for idempotency checks in Router, Gateway, and Worker
- Must be identical for retries of the same logical request

#### Obligation and Scope

**Required in**:
- All business messages (requests that can be retried)
- Messages that need idempotency guarantees

**Optional in**:
- Health checks and technical messages
- Messages that are inherently idempotent (read-only operations)

**Non-empty requirement**: **MUST** be non-empty string if present (cannot be `""` or `null`)

#### Type and Format

**Type**: `string`

**Format**:
- **Preferred**: UUID v4 (e.g., `"550e8400-e29b-41d4-a716-446655440000"`)
- **Alternative**: Client-provided string (e.g., `"client-request-123"`)
- **Validation**: Must be non-empty string (min length: 1, max length: 256)

**Examples**:
```json
"idempotency_key": "550e8400-e29b-41d4-a716-446655440000"
"idempotency_key": "client-request-123"
"idempotency_key": "order-42-retry-1"
```

#### Source and Generation Invariants

**Who generates**:
- **Client** (Gateway or external client) - **preferred**
- **Gateway** (if client does not provide) - **fallback**
- Must be generated **before** the first request attempt

**Propagation**:
- **Gateway**: Passes through from client or generates if missing
- **Router**: Passes through without modification
- **Worker**: Passes through without modification

**Invariants**:
- `idempotency_key` is **identical** for retries of the same logical request
- `idempotency_key` is **unique** per logical request (different requests have different keys)
- `idempotency_key` is **immutable** (never changes for retries)

#### Usage in Logs and Metrics

**Logs**:
- **Always included** in JSON logs when present
- Field name: `idempotency_key`
- Used for idempotency check logging

**Metrics**:
- **Not used as label** (high cardinality risk)
- Used in trace context only

#### Security and Multi-Tenancy

**Multi-tenancy**:
- `idempotency_key` is **tenant-scoped** (implicitly via `tenant_id`)
- Idempotency checks are per-tenant (same key in different tenants = different requests)
- Access control: Validate `tenant_id` before idempotency check

**Security**:
- `idempotency_key` is **not sensitive** (can be logged)
- No PII in `idempotency_key` (random UUID or client-provided identifier)

---

### 5. `trace_id`

#### Semantics

**What it is**: Identifier for distributed tracing (OpenTelemetry/W3C Trace Context).

**BeamLine Context**:
- Used for distributed tracing across services
- Compatible with OpenTelemetry (OTEL) trace context
- Propagated across all service boundaries (NATS, gRPC, HTTP)

#### Obligation and Scope

**Required in**:
- All business messages (CP2+)
- Messages that need distributed tracing

**Optional in**:
- CP1 baseline (optional for backward compatibility)
- Health checks and technical messages (optional)

**Non-empty requirement**: **MUST** be non-empty string if present (cannot be `""` or `null`)

#### Type and Format

**Type**: `string`

**Format**:
- **Preferred**: W3C Trace Context format (16 hex chars, e.g., `"4bf92f3577b34da6a3ce929d0e0e4736"`)
- **Alternative**: UUID v4 (e.g., `"550e8400-e29b-41d4-a716-446655440000"`)
- **Validation**: Must match W3C Trace Context regex or UUID v4 format

**Examples**:
```json
"trace_id": "4bf92f3577b34da6a3ce929d0e0e4736"
"trace_id": "550e8400-e29b-41d4-a716-446655440000"
```

#### Source and Generation Invariants

**Who generates**:
- **Gateway** (for new requests) or **Client** (if provided in headers)
- Generated once per request and propagated to all downstream services

**Propagation**:
- **Gateway**: Generates if missing, passes through if present
- **Router**: Passes through without modification
- **Worker**: Passes through without modification
- **Extensions**: Passes through without modification

**Invariants**:
- `trace_id` is **unique** per request (no two requests share the same `trace_id`)
- `trace_id` is **stable** across all services in the same request (never changes)
- `trace_id` is **immutable** (never modified after generation)
- `trace_id` is **OTEL-compatible** (can be used with OpenTelemetry SDKs)

#### Usage in Logs and Metrics

**Logs**:
- **Always included** in JSON logs
- Field name: `trace_id`
- Used for log correlation across services

**Metrics**:
- **Not used as label** (high cardinality risk)
- Used in trace context only (OTEL spans)

#### Security and Multi-Tenancy

**Multi-tenancy**:
- `trace_id` is **tenant-scoped** (implicitly via `tenant_id`)
- Cannot be used to "jump" between tenants
- Access control: Validate `tenant_id` before trace access

**Security**:
- `trace_id` is **not sensitive** (can be logged)
- No PII in `trace_id` (random UUID/W3C trace ID)

---

### 6. `tenant_id`

#### Semantics

**What it is**: Identifier for tenant/organization/project in multi-tenant system.

**BeamLine Context**:
- Identifies the tenant that owns the request
- Used for ACL (Access Control List) and quota enforcement
- Required for all business operations

#### Obligation and Scope

**Required in**:
- **ALL** business messages (no exceptions)
- All requests that need tenant isolation

**Optional in**:
- Health checks (technical messages)
- Metrics and internal events (if tenant-agnostic)

**Non-empty requirement**: **MUST** be non-empty string (cannot be `""` or `null`)

#### Type and Format

**Type**: `string`

**Format**:
- **Preferred**: Non-empty string (e.g., `"acme"`, `"tenant-123"`, `"org-abc"`)
- **Validation**: Must be non-empty string (min length: 1, max length: 256)
- **Pattern**: Alphanumeric, hyphens, underscores allowed (regex: `^[a-zA-Z0-9_-]+$`)

**Examples**:
```json
"tenant_id": "acme"
"tenant_id": "tenant-123"
"tenant_id": "org_abc_456"
```

#### Source and Generation Invariants

**Who generates**:
- **Client** (external client or Gateway) - **always provided by client**
- **Gateway** (extracts from authentication token or header)
- Never generated by Router or Worker (always from upstream)

**Propagation**:
- **Gateway**: Extracts from auth token or `X-Tenant-ID` header
- **Router**: Passes through without modification (validates presence)
- **Worker**: Passes through without modification
- **Extensions**: Passes through without modification

**Invariants**:
- `tenant_id` is **stable** (same tenant always has the same `tenant_id`)
- `tenant_id` is **immutable** (never changes for a tenant)
- `tenant_id` is **validated** at Gateway and Router (reject if missing or invalid)

#### Usage in Logs and Metrics

**Logs**:
- **Always included** in JSON logs
- Field name: `tenant_id`
- Used for tenant-scoped log filtering

**Metrics**:
- **Can be used as label** (medium cardinality - limited number of tenants)
- Example: `router_requests_total{tenant_id="acme"}`
- **Cardinality limit**: Monitor metric cardinality (max ~1000 tenants per metric)

#### Security and Multi-Tenancy

**Multi-tenancy**:
- `tenant_id` is **the primary isolation boundary**
- All ACL checks use `tenant_id`
- All quota checks use `tenant_id`
- Cannot access resources from other tenants (strict isolation)

**Security**:
- `tenant_id` is **validated** at Gateway (reject if missing or invalid)
- `tenant_id` is **validated** at Router (reject if missing or invalid)
- `tenant_id` is **not sensitive** (can be logged, but monitor for PII)
- **Access control**: All operations must validate `tenant_id` before processing

**Critical Security Rules**:
1. **Never process a request without `tenant_id`** (reject with `invalid_request` error)
2. **Never allow `tenant_id` to be modified** (pass through only)
3. **Always validate `tenant_id` against ACL** (before accessing resources)
4. **Always scope quotas by `tenant_id`** (enforce per-tenant limits)

---

### 7. `schema_version` (also `version`)

#### Semantics

**What it is**: Version of the message contract/schema format.

**BeamLine Context**:
- Identifies the version of the message schema (protobuf/NATS JSON format)
- Used for protocol negotiation and compatibility checks
- Enables backward compatibility and migration

#### Obligation and Scope

**Required in**:
- **ALL** messages (business and technical)
- All NATS messages (as `version` field)
- All gRPC messages (as protobuf package version)
- All REST DTOs (as `version` field in request body)

**Optional in**:
- None (always required)

**Non-empty requirement**: **MUST** be non-empty string (cannot be `""` or `null`)

#### Type and Format

**Type**: `string`

**Format**:
- **NATS JSON**: String `"1"` (for CP1/CP2-LC)
- **gRPC**: Package version suffix (e.g., `beamline.flow.v1`)
- **REST DTO**: String `"1"` (for CP1/CP2-LC)
- **Validation**: Must match semantic version pattern or simple version string

**Examples**:
```json
"version": "1"
"schema_version": "1"
```

**NATS JSON Format**:
```json
{
  "version": "1",
  ...
}
```

**gRPC Format**:
```protobuf
package beamline.flow.v1;  // v1 is the schema version
```

#### Source and Generation Invariants

**Who generates**:
- **Client** (Gateway or external client) - **always provided by client**
- **Gateway** (adds `version: "1"` if missing in NATS messages)
- Never generated by Router or Worker (always from upstream)

**Propagation**:
- **Gateway**: Adds `version: "1"` to NATS messages if missing
- **Router**: Validates `version` (rejects if missing or unsupported)
- **Worker**: Passes through without modification

**Invariants**:
- `schema_version` is **stable** (same version always has the same value)
- `schema_version` is **validated** at Router (reject if missing or unsupported)
- `schema_version` is **immutable** (never changes for a message)

#### Usage in Logs and Metrics

**Logs**:
- **Always included** in JSON logs
- Field name: `version` or `schema_version`
- Used for version compatibility logging

**Metrics**:
- **Can be used as label** (low cardinality - limited number of versions)
- Example: `router_requests_total{version="1"}`

#### Security and Multi-Tenancy

**Multi-tenancy**:
- `schema_version` is **tenant-agnostic** (same version for all tenants)
- Not used for access control

**Security**:
- `schema_version` is **not sensitive** (can be logged)
- `schema_version` is **validated** at Router (reject if unsupported)

**Critical Validation Rules**:
1. **Router MUST reject messages with missing `version`** (return `invalid_request` error)
2. **Router MUST reject messages with unsupported `version`** (return `invalid_request` error with `supported_versions: ["1"]`)
3. **Gateway MUST add `version: "1"` if missing** (for backward compatibility)

---

## Field Relationships

### Required Field Combinations

**CP1 Baseline** (minimum required):
- `tenant_id` (required)
- `schema_version` / `version` (required)
- `trace_id` (optional)
- `idempotency_key` (optional, but recommended)

**CP2+ Multi-Step Workflows** (all CP1 + additional):
- `tenant_id` (required)
- `schema_version` / `version` (required)
- `trace_id` (required)
- `idempotency_key` (required)
- `run_id` (required for multi-step workflows)
- `flow_id` (required for multi-step workflows)
- `step_id` (required for multi-step workflows)

### Field Dependencies

**If `run_id` is present**:
- `flow_id` **MUST** be present
- `step_id` **MUST** be present
- `tenant_id` **MUST** be present

**If `flow_id` is present**:
- `run_id` **MUST** be present (for execution context)
- `tenant_id` **MUST** be present

**If `step_id` is present**:
- `run_id` **MUST** be present
- `flow_id` **MUST** be present
- `tenant_id` **MUST** be present

## Validation Requirements

### Validator Requirements

**All validators MUST**:

1. **Check presence**:
   - Reject messages missing `tenant_id` → `invalid_request` error
   - Reject messages missing `schema_version` / `version` → `invalid_request` error
   - Reject messages missing `trace_id` (CP2+) → `invalid_request` error
   - Reject messages missing `idempotency_key` (CP2+) → `invalid_request` error

2. **Check format**:
   - Validate `tenant_id` format (non-empty, alphanumeric/hyphens/underscores)
   - Validate `schema_version` / `version` format (must be `"1"` for CP1/CP2-LC)
   - Validate `trace_id` format (W3C Trace Context or UUID v4)
   - Validate `run_id` format (UUID v4 or ULID) if present
   - Validate `flow_id` format (UUID v4 or ULID) if present
   - Validate `step_id` format (UUID v4 or ULID) if present
   - Validate `idempotency_key` format (non-empty string, max 256 chars)

3. **Check relationships**:
   - If `run_id` present → `flow_id` and `step_id` must be present
   - If `flow_id` present → `run_id` must be present
   - If `step_id` present → `run_id` and `flow_id` must be present

4. **Check non-empty**:
   - All fields (if present) must be non-empty strings (not `""` or `null`)

### Test Requirements

**All tests MUST**:

1. **Test missing fields**:
   - Test: Message without `tenant_id` → must reject with `invalid_request`
   - Test: Message without `schema_version` / `version` → must reject with `invalid_request`
   - Test: Message without `trace_id` (CP2+) → must reject with `invalid_request`

2. **Test invalid formats**:
   - Test: `tenant_id` is empty string → must reject with `invalid_request`
   - Test: `schema_version` / `version` is not `"1"` → must reject with `invalid_request`
   - Test: `trace_id` is invalid format → must reject with `invalid_request`

3. **Test field relationships**:
   - Test: `run_id` present but `flow_id` missing → must reject with `invalid_request`
   - Test: `flow_id` present but `run_id` missing → must reject with `invalid_request`

## Protocol-Specific Mappings

### NATS JSON

**Field Names**:
- `schema_version` → `version` (top-level field)
- All other fields use same names

**Example**:
```json
{
  "version": "1",
  "tenant_id": "acme",
  "request_id": "550e8400-e29b-41d4-a716-446655440000",
  "trace_id": "4bf92f3577b34da6a3ce929d0e0e4736",
  "idempotency_key": "client-request-123",
  "run_id": "550e8400-e29b-41d4-a716-446655440000",
  "flow_id": "550e8400-e29b-41d4-a716-446655440000",
  "step_id": "550e8400-e29b-41d4-a716-446655440000",
  ...
}
```

### gRPC (Protobuf)

**Field Names**:
- `schema_version` → Package version (e.g., `beamline.flow.v1`)
- All other fields use same names in proto messages

**Example**:
```protobuf
package beamline.flow.v1;  // schema_version = "v1"

message Message {
  string tenant_id = 2;
  string trace_id = 3;
  string run_id = 8;
  string flow_id = 9;
  string step_id = 10;
  string idempotency_key = 11;
  ...
}
```

### REST DTO

**Field Names**:
- `schema_version` → `version` (request body field)
- All other fields use same names

**Example**:
```json
{
  "version": "1",
  "tenant_id": "acme",
  "request_id": "550e8400-e29b-41d4-a716-446655440000",
  "trace_id": "4bf92f3577b34da6a3ce929d0e0e4736",
  ...
}
```

## References

- **API Contracts**: `docs/API_CONTRACTS.md` - Detailed message contracts
- **Proto-NATS Mapping**: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` - Proto to NATS mapping
- **API Registry**: `docs/ARCHITECTURE/api-registry.md` - REST API contracts
- **State Management**: `docs/STATE_MANAGEMENT_SPECIFICATION.md` - State management architecture

## Change History

**v1.0 (2025-01-27)**:
- Initial specification for core message fields
- Defined strict contracts for all 7 core fields
- Added validation and test requirements

