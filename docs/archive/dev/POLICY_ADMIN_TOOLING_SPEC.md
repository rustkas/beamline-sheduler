# Policy Admin/Ops Tooling Specification

## Purpose

This document defines the specification for admin/ops tooling for Router Policy management. It covers API/CLI design, integration with existing admin interfaces, and safe policy update workflows.

## Status

📅 **CP2+ Enhancement** (Design Specification)

**Current State**: Design and specification only. Implementation deferred to CP2+.

## Overview

### Problem

Without admin tooling:
- No way to view current policies (what policies are loaded, their configuration)
- No way to validate policies against schema before deployment
- No way to test policy decisions (dry-run: what would Router choose for a given request)
- No safe way to update policies (no versioning, no audit trail, risk of breaking changes)

### Solution

Admin/Ops tooling provides:
1. **Policy Inspection**: View current policies, their configuration, and status
2. **Policy Validation**: Validate policies against `policy.schema.json` before deployment
3. **Policy Dry-Run**: Test policy decisions without affecting production traffic
4. **Safe Policy Updates**: Versioned policy updates with audit trail and rollback capability

### Integration with Existing Admin Interfaces

**Existing Admin Interfaces**:
- **gRPC Admin API**: `router_admin_grpc.erl` (existing)
- **HTTP Admin API**: (if exists, to be verified)
- **CLI Tools**: (if exists, to be verified)

**Integration Strategy**:
- Extend existing gRPC admin API with policy management methods
- Add HTTP admin endpoints (if HTTP admin exists) or create new HTTP admin service
- Create CLI tool that uses gRPC/HTTP admin APIs

## Architecture

### Components

**1. Admin gRPC Service** (extend existing):
- **Module**: `router_admin_grpc.erl` (extend)
- **Service**: `beamline.router.v1.admin.RouterAdminService`
- **Methods**: Policy management RPCs

**2. Admin HTTP Service** (new or extend existing):
- **Module**: `router_admin_http.erl` (new or extend)
- **Endpoints**: REST API for policy management
- **Format**: JSON request/response

**3. CLI Tool** (new):
- **Module**: `router_policy_cli` (new Erlang escript or separate tool)
- **Commands**: `list`, `get`, `validate`, `dry-run`, `update`, `rollback`
- **Backend**: Uses gRPC or HTTP admin API

### Data Flow

```
CLI Tool → gRPC/HTTP Admin API → Router Admin Service → Policy Store/Applier
                                                      → Schema Validator
                                                      → Dry-Run Engine
                                                      → Version Manager
```

## API Specification

### gRPC Admin API (Extend Existing)

**Service**: `beamline.router.v1.admin.RouterAdminService`

**New Methods**:

#### 1. List Policies

**Method**: `ListPolicies`

**Request**:
```protobuf
message ListPoliciesRequest {
  string tenant_id = 1;  // Optional: filter by tenant
  string policy_id = 2;  // Optional: filter by policy ID
  int32 limit = 3;       // Optional: limit results (default: 100)
  int32 offset = 4;      // Optional: pagination offset
}
```

**Response**:
```protobuf
message ListPoliciesResponse {
  repeated PolicyInfo policies = 1;
  int32 total = 2;
  int32 limit = 3;
  int32 offset = 4;
}

message PolicyInfo {
  string tenant_id = 1;
  string policy_id = 2;
  string version = 3;
  string status = 4;  // active, inactive, deprecated
  int64 created_at = 5;
  int64 updated_at = 6;
  int32 provider_count = 7;
  bool has_fallbacks = 8;
  bool has_extensions = 9;
}
```

#### 2. Get Policy

**Method**: `GetPolicy`

**Request**:
```protobuf
message GetPolicyRequest {
  string tenant_id = 1;
  string policy_id = 2;
  string version = 3;  // Optional: specific version (default: latest)
}
```

**Response**:
```protobuf
message GetPolicyResponse {
  PolicyInfo info = 1;
  string policy_json = 2;  // Full policy JSON
  PolicyMetadata metadata = 3;
}

message PolicyMetadata {
  string schema_version = 1;
  int64 loaded_at = 2;
  int64 last_used_at = 3;
  int64 request_count = 4;
  map<string, string> checksums = 5;  // Artifact checksums
}
```

#### 3. Validate Policy

**Method**: `ValidatePolicy`

**Request**:
```protobuf
message ValidatePolicyRequest {
  string policy_json = 1;  // Policy JSON to validate
  bool strict = 2;         // Optional: strict validation (default: false)
}
```

**Response**:
```protobuf
message ValidatePolicyResponse {
  bool valid = 1;
  repeated ValidationError errors = 2;
  repeated ValidationWarning warnings = 3;
}

message ValidationError {
  string field = 1;
  string message = 2;
  string path = 3;  // JSON path to error
}

message ValidationWarning {
  string field = 1;
  string message = 2;
  string path = 3;
}
```

#### 4. Dry-Run Policy Decision

**Method**: `DryRunPolicyDecision`

**Request**:
```protobuf
message DryRunPolicyDecisionRequest {
  string tenant_id = 1;
  string policy_id = 2;  // Optional: use specific policy (default: tenant default)
  RouteRequest request = 3;  // Simulated route request
  map<string, string> context = 4;  // Optional: additional context
}

message RouteRequest {
  map<string, string> message = 1;  // Request message fields
  map<string, string> context = 2;  // Request context
}
```

**Response**:
```protobuf
message DryRunPolicyDecisionResponse {
  string provider_id = 1;
  string reason = 2;  // sticky, weighted, fallback, retry
  int32 priority = 3;
  repeated string steps = 4;  // Decision explanation steps
  map<string, string> context = 5;  // Decision context
  repeated ExtensionInfo extensions = 6;  // Extensions that would be executed
  PolicyDecisionMetadata metadata = 7;
}

message ExtensionInfo {
  string id = 1;
  string type = 2;  // pre, validator, post
  string subject = 3;
  int32 timeout_ms = 4;
}

message PolicyDecisionMetadata {
  string policy_version = 1;
  int64 decision_time_ms = 2;
  bool sticky_hit = 3;
  string selected_provider = 4;
  repeated string skipped_providers = 5;  // Providers skipped (unhealthy, circuit open)
}
```

#### 5. Update Policy

**Method**: `UpdatePolicy`

**Request**:
```protobuf
message UpdatePolicyRequest {
  string tenant_id = 1;
  string policy_id = 2;
  string policy_json = 3;
  string version = 4;  // Optional: new version (default: auto-increment)
  bool validate = 5;   // Optional: validate before update (default: true)
  bool dry_run = 6;    // Optional: dry-run update (default: false)
  string comment = 7;  // Optional: update comment for audit
}
```

**Response**:
```protobuf
message UpdatePolicyResponse {
  bool success = 1;
  string new_version = 2;
  string previous_version = 3;
  int64 updated_at = 4;
  repeated ValidationError validation_errors = 5;  // If validation failed
  DryRunPolicyDecisionResponse dry_run_result = 6;  // If dry_run=true
}
```

#### 6. Rollback Policy

**Method**: `RollbackPolicy`

**Request**:
```protobuf
message RollbackPolicyRequest {
  string tenant_id = 1;
  string policy_id = 2;
  string target_version = 3;  // Optional: rollback to specific version (default: previous)
  string comment = 4;  // Optional: rollback comment for audit
}
```

**Response**:
```protobuf
message RollbackPolicyResponse {
  bool success = 1;
  string current_version = 2;
  string previous_version = 3;
  int64 rolled_back_at = 4;
}
```

#### 7. Get Policy History

**Method**: `GetPolicyHistory`

**Request**:
```protobuf
message GetPolicyHistoryRequest {
  string tenant_id = 1;
  string policy_id = 2;
  int32 limit = 3;  // Optional: limit results (default: 50)
}
```

**Response**:
```protobuf
message GetPolicyHistoryResponse {
  repeated PolicyVersion versions = 1;
}

message PolicyVersion {
  string version = 1;
  string policy_json = 2;
  int64 created_at = 3;
  string created_by = 4;  // User/system that created version
  string comment = 5;
  bool is_active = 6;
}
```

### HTTP Admin API (New or Extend Existing)

**Base Path**: `/api/v1/admin/policies`

**Endpoints**:

#### 1. List Policies

**GET** `/api/v1/admin/policies`

**Query Parameters**:
- `tenant_id` (optional): Filter by tenant
- `policy_id` (optional): Filter by policy ID
- `limit` (optional, default: 100): Limit results
- `offset` (optional, default: 0): Pagination offset

**Response**: `200 OK`
```json
{
  "policies": [
    {
      "tenant_id": "tenant_123",
      "policy_id": "default",
      "version": "1.0",
      "status": "active",
      "created_at": "2025-01-27T12:00:00Z",
      "updated_at": "2025-01-27T12:00:00Z",
      "provider_count": 2,
      "has_fallbacks": true,
      "has_extensions": true
    }
  ],
  "total": 10,
  "limit": 100,
  "offset": 0
}
```

#### 2. Get Policy

**GET** `/api/v1/admin/policies/{tenant_id}/{policy_id}`

**Query Parameters**:
- `version` (optional): Specific version (default: latest)

**Response**: `200 OK`
```json
{
  "info": {
    "tenant_id": "tenant_123",
    "policy_id": "default",
    "version": "1.0",
    "status": "active"
  },
  "policy_json": {
    "version": "1.0",
    "providers": [...],
    "fallbacks": [...]
  },
  "metadata": {
    "schema_version": "1.0",
    "loaded_at": "2025-01-27T12:00:00Z",
    "last_used_at": "2025-01-27T12:05:00Z",
    "request_count": 1234
  }
}
```

#### 3. Validate Policy

**POST** `/api/v1/admin/policies/validate`

**Request Body**:
```json
{
  "policy_json": {
    "version": "1.0",
    "providers": [...]
  },
  "strict": false
}
```

**Response**: `200 OK`
```json
{
  "valid": true,
  "errors": [],
  "warnings": [
    {
      "field": "metadata",
      "message": "Field 'metadata' is deprecated",
      "path": "/metadata"
    }
  ]
}
```

#### 4. Dry-Run Policy Decision

**POST** `/api/v1/admin/policies/dry-run`

**Request Body**:
```json
{
  "tenant_id": "tenant_123",
  "policy_id": "default",
  "request": {
    "message": {
      "tenant_id": "tenant_123",
      "user_id": "user_456"
    },
    "context": {
      "session_id": "session_789"
    }
  }
}
```

**Response**: `200 OK`
```json
{
  "provider_id": "provider_a",
  "reason": "sticky",
  "priority": 100,
  "steps": [
    "1. Checked sticky session: found existing provider for key session_id = session_789"
  ],
  "context": {
    "tenant_id": "tenant_123",
    "trace_id": "trace_123"
  },
  "extensions": [
    {
      "id": "normalize_text",
      "type": "pre",
      "subject": "extensions.pre.normalize_text",
      "timeout_ms": 5000
    }
  ],
  "metadata": {
    "policy_version": "1.0",
    "decision_time_ms": 2,
    "sticky_hit": true,
    "selected_provider": "provider_a"
  }
}
```

#### 5. Update Policy

**PUT** `/api/v1/admin/policies/{tenant_id}/{policy_id}`

**Request Body**:
```json
{
  "policy_json": {
    "version": "1.1",
    "providers": [...]
  },
  "version": "1.1",
  "validate": true,
  "dry_run": false,
  "comment": "Add new provider for load balancing"
}
```

**Response**: `200 OK`
```json
{
  "success": true,
  "new_version": "1.1",
  "previous_version": "1.0",
  "updated_at": "2025-01-27T12:10:00Z"
}
```

#### 6. Rollback Policy

**POST** `/api/v1/admin/policies/{tenant_id}/{policy_id}/rollback`

**Request Body**:
```json
{
  "target_version": "1.0",
  "comment": "Rollback due to performance issues"
}
```

**Response**: `200 OK`
```json
{
  "success": true,
  "current_version": "1.0",
  "previous_version": "1.1",
  "rolled_back_at": "2025-01-27T12:15:00Z"
}
```

#### 7. Get Policy History

**GET** `/api/v1/admin/policies/{tenant_id}/{policy_id}/history`

**Query Parameters**:
- `limit` (optional, default: 50): Limit results

**Response**: `200 OK`
```json
{
  "versions": [
    {
      "version": "1.1",
      "policy_json": {...},
      "created_at": "2025-01-27T12:10:00Z",
      "created_by": "admin_user",
      "comment": "Add new provider",
      "is_active": false
    },
    {
      "version": "1.0",
      "policy_json": {...},
      "created_at": "2025-01-27T12:00:00Z",
      "created_by": "system",
      "comment": "Initial policy",
      "is_active": true
    }
  ]
}
```

## CLI Tool Specification

### CLI Commands

**Tool Name**: `router-policy` (or `beamline-policy`)

**Usage**:
```bash
router-policy [OPTIONS] COMMAND [ARGS...]
```

**Global Options**:
- `--grpc-endpoint URL`: gRPC admin endpoint (default: `localhost:9000`)
- `--http-endpoint URL`: HTTP admin endpoint (default: `http://localhost:8080`)
- `--tenant-id ID`: Default tenant ID
- `--format FORMAT`: Output format (`json`, `table`, `yaml`, default: `table`)
- `--verbose`: Verbose output

### Commands

#### 1. List Policies

**Command**: `list` (or `ls`)

**Usage**:
```bash
router-policy list [OPTIONS]
```

**Options**:
- `--tenant-id ID`: Filter by tenant
- `--policy-id ID`: Filter by policy ID
- `--limit N`: Limit results (default: 100)
- `--offset N`: Pagination offset

**Example**:
```bash
router-policy list --tenant-id tenant_123
```

**Output** (table format):
```
+-------------+-----------+---------+--------+---------------------+---------------------+
| Tenant ID   | Policy ID | Version | Status | Created At          | Updated At          |
+-------------+-----------+---------+--------+---------------------+---------------------+
| tenant_123  | default   | 1.0     | active | 2025-01-27 12:00:00 | 2025-01-27 12:00:00 |
| tenant_123  | high_vol  | 1.2     | active | 2025-01-27 11:00:00 | 2025-01-27 12:05:00 |
+-------------+-----------+---------+--------+---------------------+---------------------+
Total: 2
```

#### 2. Get Policy

**Command**: `get`

**Usage**:
```bash
router-policy get TENANT_ID POLICY_ID [OPTIONS]
```

**Options**:
- `--version VERSION`: Specific version (default: latest)
- `--json`: Output as JSON (instead of formatted)

**Example**:
```bash
router-policy get tenant_123 default
router-policy get tenant_123 default --version 1.0 --json
```

**Output** (formatted):
```
Policy: default
Tenant: tenant_123
Version: 1.0
Status: active

Providers:
  - provider_a (weight: 70%)
  - provider_b (weight: 30%)

Fallbacks:
  - Rule 1: timeout → provider_b
  - Rule 2: 5xx → provider_b

Extensions:
  - Pre: normalize_text
  - Validators: (none)
  - Post: (none)

Metadata:
  Loaded at: 2025-01-27 12:00:00
  Last used: 2025-01-27 12:05:00
  Request count: 1234
```

#### 3. Validate Policy

**Command**: `validate`

**Usage**:
```bash
router-policy validate POLICY_FILE [OPTIONS]
```

**Options**:
- `--strict`: Strict validation (treat warnings as errors)
- `--schema SCHEMA_FILE`: Custom schema file (default: `policy.schema.json`)

**Example**:
```bash
router-policy validate policy.json
router-policy validate policy.json --strict
```

**Output**:
```
Validating policy.json...
✓ Policy is valid

Warnings:
  - Field 'metadata' is deprecated (path: /metadata)

Validation time: 15ms
```

#### 4. Dry-Run Policy Decision

**Command**: `dry-run`

**Usage**:
```bash
router-policy dry-run TENANT_ID [POLICY_ID] [OPTIONS]
```

**Options**:
- `--request-file FILE`: Request JSON file
- `--context KEY=VALUE`: Additional context (can be repeated)
- `--json`: Output as JSON

**Example**:
```bash
router-policy dry-run tenant_123 default \
  --request-file request.json \
  --context session_id=session_789

# Or inline:
router-policy dry-run tenant_123 default \
  --context user_id=user_456 \
  --context session_id=session_789
```

**Request File Format** (`request.json`):
```json
{
  "message": {
    "tenant_id": "tenant_123",
    "user_id": "user_456"
  },
  "context": {
    "session_id": "session_789"
  }
}
```

**Output** (formatted):
```
Dry-run policy decision:
  Tenant: tenant_123
  Policy: default
  Version: 1.0

Decision:
  Provider: provider_a
  Reason: sticky
  Priority: 100

Steps:
  1. Checked sticky session: found existing provider for key session_id = session_789

Extensions:
  - Pre: normalize_text (subject: extensions.pre.normalize_text, timeout: 5000ms)

Metadata:
  Decision time: 2ms
  Sticky hit: true
  Selected provider: provider_a
```

#### 5. Update Policy

**Command**: `update`

**Usage**:
```bash
router-policy update TENANT_ID POLICY_ID POLICY_FILE [OPTIONS]
```

**Options**:
- `--version VERSION`: New version (default: auto-increment)
- `--no-validate`: Skip validation
- `--dry-run`: Dry-run update (don't actually update)
- `--comment TEXT`: Update comment for audit
- `--yes`: Skip confirmation prompt

**Example**:
```bash
router-policy update tenant_123 default policy.json \
  --version 1.1 \
  --comment "Add new provider for load balancing" \
  --yes
```

**Output**:
```
Validating policy.json...
✓ Policy is valid

Dry-running update...
✓ Dry-run successful (provider: provider_a, reason: weighted)

Updating policy...
✓ Policy updated successfully

  Previous version: 1.0
  New version: 1.1
  Updated at: 2025-01-27 12:10:00
```

#### 6. Rollback Policy

**Command**: `rollback`

**Usage**:
```bash
router-policy rollback TENANT_ID POLICY_ID [OPTIONS]
```

**Options**:
- `--version VERSION`: Target version (default: previous)
- `--comment TEXT`: Rollback comment for audit
- `--yes`: Skip confirmation prompt

**Example**:
```bash
router-policy rollback tenant_123 default \
  --version 1.0 \
  --comment "Rollback due to performance issues" \
  --yes
```

**Output**:
```
Rolling back policy...
  Current version: 1.1
  Target version: 1.0

✓ Policy rolled back successfully

  Current version: 1.0
  Previous version: 1.1
  Rolled back at: 2025-01-27 12:15:00
```

#### 7. Get Policy History

**Command**: `history`

**Usage**:
```bash
router-policy history TENANT_ID POLICY_ID [OPTIONS]
```

**Options**:
- `--limit N`: Limit results (default: 50)

**Example**:
```bash
router-policy history tenant_123 default
```

**Output** (table format):
```
+---------+---------------------+------------+--------------------------+----------+
| Version | Created At          | Created By | Comment                  | Active   |
+---------+---------------------+------------+--------------------------+----------+
| 1.1     | 2025-01-27 12:10:00 | admin_user | Add new provider          | No       |
| 1.0     | 2025-01-27 12:00:00 | system     | Initial policy            | Yes      |
+---------+---------------------+------------+--------------------------+----------+
```

## Policy Versioning

### Version Format

**Semantic Versioning**: `MAJOR.MINOR.PATCH`

**Examples**:
- `1.0.0` - Initial version
- `1.1.0` - Minor update (backward compatible)
- `2.0.0` - Major update (breaking changes)

**Auto-Increment Rules**:
- **PATCH**: Increment if only bug fixes or clarifications
- **MINOR**: Increment if backward-compatible additions
- **MAJOR**: Increment if breaking changes (remove fields, change types)

### Version Storage

**Storage**: Policy versions stored in policy store with version history

**Key Structure**: `{TenantId, PolicyId, Version}`

**Metadata**:
- `version`: Policy version string
- `created_at`: Timestamp when version created
- `created_by`: User/system that created version
- `comment`: Update comment
- `is_active`: Whether this version is currently active
- `policy_json`: Full policy JSON for this version

### Version Management

**Active Version**: Only one version per policy is active at a time

**Version History**: All versions are retained for audit and rollback

**Rollback**: Can rollback to any previous version (becomes new active version)

## Policy Validation

### Schema Validation

**Schema File**: `apps/otp/router/docs/schemas/policy.schema.json`

**Validation Library**: `jsonschema` (Python) or `jsx` (Erlang)

**Validation Levels**:
- **Basic**: Validate JSON structure and required fields
- **Strict**: Also validate deprecated fields, unused fields, best practices

**Validation Errors**:
- **Required Field Missing**: Policy missing required field
- **Invalid Type**: Field has wrong type (string vs integer)
- **Invalid Value**: Field value outside allowed range
- **Schema Mismatch**: Policy doesn't match schema structure

**Validation Warnings**:
- **Deprecated Field**: Field is deprecated (e.g., `metadata`, `defaults`)
- **Unused Field**: Field is not used in routing logic
- **Best Practice**: Policy doesn't follow best practices (e.g., empty fallbacks)

### Business Logic Validation

**Additional Validations** (beyond schema):
- **Provider Existence**: All provider IDs in policy exist in provider registry
- **Fallback Provider Existence**: All fallback provider IDs exist
- **Extension Existence**: All extension IDs exist in extension registry
- **Weight Normalization**: Provider weights sum to 100 (or normalized correctly)
- **Fallback Rule Conflicts**: No conflicting fallback rules (same condition, different providers)

## Dry-Run Engine

### Purpose

**Dry-run** allows testing policy decisions without affecting production traffic:
- Test policy changes before deployment
- Debug routing decisions
- Understand policy behavior for specific requests

### Implementation

**Module**: `router_policy_dry_run.erl` (new)

**Function**: `dry_run_decision/3`
```erlang
-spec dry_run_decision(TenantId, PolicyId, Request) ->
    {ok, DecisionResult} | {error, Reason}.
```

**Behavior**:
1. Load policy (from policy store or provided JSON)
2. Create route request from dry-run request
3. Execute policy application (same as production, but no side effects)
4. Return decision result (provider, reason, steps, extensions)

**Side Effects**:
- **No Side Effects**: Dry-run doesn't modify sticky store, doesn't send NATS messages, doesn't call providers
- **Read-Only**: Only reads from policy store, sticky store (for sticky lookup)
- **Isolated**: Dry-run doesn't affect production metrics or logs

### Dry-Run vs Production

**Differences**:
- **Sticky Store**: Dry-run reads from sticky store but doesn't update it
- **NATS**: Dry-run doesn't publish messages to NATS
- **Provider Calls**: Dry-run doesn't call providers (HTTP/gRPC)
- **Metrics**: Dry-run doesn't emit production metrics
- **Logs**: Dry-run logs to separate dry-run log (not production logs)

**Similarities**:
- **Policy Logic**: Same policy application logic
- **Decision Logic**: Same provider selection logic (sticky, weighted, fallbacks)
- **Extension Logic**: Same extension execution logic (but mocked, no actual NATS calls)

## Safe Policy Updates

### Update Workflow

**1. Validation**:
- Validate policy JSON against schema
- Validate business logic (provider existence, etc.)
- Check for breaking changes (if versioning policy requires)

**2. Dry-Run** (optional but recommended):
- Dry-run policy with sample requests
- Verify expected behavior
- Check for regressions

**3. Version Creation**:
- Create new version (auto-increment or specified)
- Store policy JSON and metadata
- Mark as inactive (not yet active)

**4. Activation**:
- Activate new version (deactivate previous version)
- Update policy store
- Invalidate policy cache (if caching implemented)

**5. Audit Logging**:
- Log policy update to audit trail
- Record who, when, what changed
- Store update comment

### Rollback Workflow

**1. Identify Target Version**:
- List policy history
- Select version to rollback to

**2. Validation**:
- Verify target version exists
- Check for conflicts (if any)

**3. Rollback**:
- Deactivate current version
- Activate target version
- Update policy store

**4. Audit Logging**:
- Log rollback to audit trail
- Record rollback reason

### Audit Trail

**Storage**: Policy update history stored in policy store or separate audit store

**Fields**:
- `timestamp`: When update occurred
- `tenant_id`: Tenant ID
- `policy_id`: Policy ID
- `action`: `create`, `update`, `rollback`, `delete`
- `version_from`: Previous version
- `version_to`: New version
- `user`: User/system that performed action
- `comment`: Update comment
- `policy_json`: Policy JSON (for update/rollback)

**Retention**: Policy history retained for audit compliance (configurable, default: 90 days)

## Security Considerations

### Authentication and Authorization

**gRPC Admin API**:
- Use existing admin authentication (if exists)
- Require admin role for policy management
- Validate tenant access (users can only manage their tenant's policies)

**HTTP Admin API**:
- Use JWT authentication (if HTTP admin exists)
- Require admin role for policy management
- Validate tenant access

**CLI Tool**:
- Use gRPC/HTTP admin API (inherits authentication)
- Support API key or token authentication
- Store credentials securely (not in plain text)

### Input Validation

**Policy JSON Validation**:
- Validate against schema before processing
- Sanitize input (prevent injection attacks)
- Limit policy size (prevent DoS)

**Request Validation**:
- Validate tenant_id, policy_id format
- Validate version format (semantic versioning)
- Limit request size

### Audit and Compliance

**Audit Logging**:
- Log all policy updates (who, when, what)
- Log all policy accesses (who viewed which policy)
- Log all dry-run requests (for debugging)

**Compliance**:
- Retain policy history for audit (configurable retention)
- Support compliance reporting (export audit logs)
- Support policy change approval workflows (future enhancement)

## Integration Points

### Existing Admin Interfaces

**1. gRPC Admin Service** (`router_admin_grpc.erl`):
- **Extend**: Add policy management RPCs to existing service
- **Service**: `beamline.router.v1.admin.RouterAdminService`
- **Port**: Same as existing admin gRPC (default: 9000)

**2. HTTP Admin Service** (if exists):
- **Extend**: Add policy management endpoints to existing service
- **Base Path**: `/api/v1/admin/policies`
- **Port**: Same as existing admin HTTP (if exists)

**3. CLI Tools** (if exist):
- **Extend**: Add policy commands to existing CLI
- **Or Create**: New `router-policy` CLI tool

### Policy Store Integration

**Module**: `router_policy_store.erl`

**New Functions** (to be added):
- `get_policy_version/3`: Get specific policy version
- `list_policy_versions/2`: List all versions for policy
- `create_policy_version/4`: Create new policy version
- `activate_policy_version/3`: Activate policy version
- `get_policy_history/2`: Get policy update history

### Policy Applier Integration

**Module**: `router_policy_applier.erl`

**Dry-Run Function** (to be added):
- `dry_run_policy/4`: Dry-run policy decision (no side effects)

**Integration**: Dry-run uses same logic as production, but with mocked dependencies

## Implementation Plan

### Phase 1: Core API (CP2+)

**Tasks**:
1. Extend gRPC admin service with policy management RPCs
2. Implement policy validation (schema + business logic)
3. Implement policy versioning (create, activate, rollback)
4. Implement policy history storage

**Deliverables**:
- gRPC admin API methods (ListPolicies, GetPolicy, ValidatePolicy, UpdatePolicy, RollbackPolicy, GetPolicyHistory)
- Policy validation module
- Policy versioning module

### Phase 2: Dry-Run Engine (CP2+)

**Tasks**:
1. Implement dry-run engine (`router_policy_dry_run.erl`)
2. Mock dependencies (NATS, provider calls, sticky store updates)
3. Integrate with policy applier
4. Add dry-run RPC to admin API

**Deliverables**:
- Dry-run engine module
- Dry-run gRPC method (DryRunPolicyDecision)
- Dry-run HTTP endpoint

### Phase 3: CLI Tool (CP2+)

**Tasks**:
1. Create CLI tool (`router-policy` escript or separate tool)
2. Implement all commands (list, get, validate, dry-run, update, rollback, history)
3. Add output formatting (table, JSON, YAML)
4. Add authentication support

**Deliverables**:
- CLI tool executable
- CLI documentation
- CLI integration tests

### Phase 4: HTTP Admin API (CP2+)

**Tasks**:
1. Create or extend HTTP admin service
2. Implement REST endpoints for policy management
3. Add authentication and authorization
4. Add OpenAPI/Swagger documentation

**Deliverables**:
- HTTP admin API endpoints
- OpenAPI/Swagger spec
- HTTP API integration tests

## Examples

### Example 1: Validate Policy Before Update

```bash
# Validate policy
router-policy validate new_policy.json

# If valid, update policy
router-policy update tenant_123 default new_policy.json \
  --version 1.1 \
  --comment "Add new provider" \
  --yes
```

### Example 2: Dry-Run Policy Change

```bash
# Dry-run policy update
router-policy update tenant_123 default new_policy.json \
  --dry-run \
  --comment "Test new provider selection"

# Review dry-run results, then apply
router-policy update tenant_123 default new_policy.json \
  --version 1.1 \
  --comment "Apply tested changes" \
  --yes
```

### Example 3: Rollback Policy

```bash
# View policy history
router-policy history tenant_123 default

# Rollback to previous version
router-policy rollback tenant_123 default \
  --version 1.0 \
  --comment "Rollback due to errors" \
  --yes
```

### Example 4: Debug Policy Decision

```bash
# Dry-run decision for specific request
router-policy dry-run tenant_123 default \
  --request-file request.json \
  --context session_id=session_789

# Output shows:
# - Selected provider
# - Decision reason (sticky, weighted, fallback)
# - Decision steps
# - Extensions that would be executed
```

## Risks and Non-Goals

### Risks

#### 1. Impact on CP1 Invariants

**Risk**: Admin tooling implementation must not break CP1 policy engine invariants.

**CP1 Invariants to Preserve**:
- ✅ **Policy Application Logic**: Policy applier must continue to work unchanged
- ✅ **Policy Store**: Policy store operations must remain compatible
- ✅ **Explanation Format**: Decision explanations must remain compatible with CP1 format
- ✅ **Backward Compatibility**: Legacy policy formats must continue to work

**Mitigation**:
- Admin tooling is **read-only** for policy application (doesn't modify routing logic)
- Policy updates go through same validation and storage as CP1
- Dry-run uses same logic as production (no side effects)
- Versioning is additive (doesn't change existing policy structure)

#### 2. Infrastructure Dependencies

**Risk**: Admin tooling requires additional infrastructure components.

**Affected Components**:
- **gRPC Admin Service**: Requires existing admin gRPC service or new service
- **HTTP Admin Service**: Requires HTTP admin service (new or extend existing)
- **Policy Store**: Requires policy store for versioning and history
- **Schema Validation**: Requires JSON Schema validation library
- **Authentication**: Requires authentication/authorization infrastructure

**Mitigation**:
- Extend existing admin gRPC service (no new infrastructure)
- HTTP admin service is optional (CLI can use gRPC)
- Policy store already exists (just add versioning)
- Schema validation uses existing `policy.schema.json`
- Authentication uses existing admin authentication

#### 3. Security Risks

**Risk**: Admin tooling exposes policy management operations.

**Affected Areas**:
- **Policy Updates**: Unauthorized policy updates could break routing
- **Policy Rollback**: Unauthorized rollbacks could revert critical fixes
- **Policy Access**: Unauthorized access to policy configurations

**Mitigation**:
- Require admin role for all policy management operations
- Validate tenant access (users can only manage their tenant's policies)
- Audit all policy operations (who, when, what changed)
- Policy updates require validation (can't deploy invalid policies)

#### 4. Performance Impact

**Risk**: Admin tooling operations may impact Router performance.

**Impact Areas**:
- **Policy Updates**: Policy store updates during active routing
- **Dry-Run**: Dry-run operations consume resources
- **Policy History**: Storing policy history adds storage overhead

**Mitigation**:
- Policy updates are infrequent (not per-request)
- Dry-run is isolated (doesn't affect production)
- Policy history is stored separately (doesn't impact policy store performance)
- Versioning is lightweight (just metadata, not full policy copies)

#### 5. Dry-Run Accuracy

**Risk**: Dry-run results may not match production behavior.

**Affected Scenarios**:
- **Sticky Sessions**: Dry-run may not have same sticky state as production
- **Circuit Breaker**: Dry-run may not have same circuit breaker state as production
- **Rate Limits**: Dry-run may not have same rate limit counters as production

**Mitigation**:
- Dry-run is **best effort** - provides approximate results
- Dry-run clearly documents limitations
- Dry-run uses same policy logic (differences are only in state)
- Production testing is still required (dry-run is for validation, not replacement)

### Non-Goals

#### 1. Real-Time Policy Updates (CP2)

**Non-Goal**: Hot-reload policy updates without Router restart.

**Rationale**:
- Policy updates require validation and versioning (not instant)
- Hot-reload adds complexity (state synchronization, rollback)
- Policy updates are infrequent (restart is acceptable for CP2)

**Future**: Hot-reload may be added in CP3+ if needed.

#### 2. Policy Update Approval Workflow

**Non-Goal**: Multi-stage approval process for policy updates.

**Rationale**:
- Approval workflow is operational process, not core functionality
- CP2 focuses on core admin tooling (update, rollback, validation)
- Approval can be handled externally (CI/CD, manual process)

**Future**: Approval workflow may be added in CP3+ or as separate tooling.

#### 3. Policy Diff/Comparison

**Non-Goal**: Built-in policy diff and comparison tool.

**Rationale**:
- Policy diff is operational tooling, not core functionality
- External tools (git diff, JSON diff) can be used
- CP2 focuses on core admin operations

**Future**: Policy diff may be added in CP3+ or as separate tooling.

#### 4. Policy Templates

**Non-Goal**: Policy templates and wizards for policy creation.

**Rationale**:
- Templates are operational tooling, not core functionality
- Policy JSON is human-readable (templates not required)
- CP2 focuses on core admin operations

**Future**: Templates may be added in CP3+ or as separate tooling.

#### 5. Policy Analytics

**Non-Goal**: Built-in analytics for policy usage and performance.

**Rationale**:
- Analytics are operational tooling, not core functionality
- Metrics are exported to Prometheus (external analytics can be used)
- CP2 focuses on core admin operations

**Future**: Analytics may be added in CP3+ or as separate tooling.

## References

- `apps/otp/router/src/router_admin_grpc.erl` - Existing gRPC admin service
- `apps/otp/router/src/router_policy_store.erl` - Policy store module
- `apps/otp/router/src/router_policy_applier.erl` - Policy applier module
- `apps/otp/router/docs/schemas/policy.schema.json` - Policy schema
- `docs/ROUTING_POLICY.md` - Routing policy specification
- `docs/archive/dev/CP2_ROUTER_PLAN.md` - CP2 Router plan

## Change History

**v1.0 (2025-01-27)**:
- Initial specification
- gRPC and HTTP API design
- CLI tool specification
- Policy versioning and validation
- Dry-run engine specification
- Safe update workflow

