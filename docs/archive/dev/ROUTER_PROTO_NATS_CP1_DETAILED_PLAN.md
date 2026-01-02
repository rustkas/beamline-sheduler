# Router Proto/NATS Consistency - CP1 Detailed Implementation Plan

**Date**: 2025-01-27  
**Based on**: `docs/archive/dev/ROUTER_PROTO_NATS_ACTION_PLAN.md`  
**Status**: Ready for Implementation  
**Version**: 1.0

**Related Documents**:
- **Consistency Report**: `docs/archive/dev/ROUTER_PROTO_NATS_CONSISTENCY.md` - Full analysis
- **Action Plan**: `docs/archive/dev/ROUTER_PROTO_NATS_ACTION_PLAN.md` - Prioritized actions
- **CP2+ Detailed Plan**: `docs/archive/dev/ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md` - CP2-LC instructions

## Purpose

This document provides detailed, step-by-step instructions for implementing all CP1 actions from the Proto/NATS consistency action plan. Each action includes:
- Exact file locations and line numbers (where applicable)
- Specific text to add/modify
- Examples of changes
- Verification steps
- Acceptance criteria

**Scope**: Documentation changes only (no code changes, no Proto file modifications)

**Important**: All CP1 actions are **documentation-only**. No code changes, no Proto file modifications, no breaking changes. All actions can be completed immediately without waiting for CP2-LC.

---

## Action CP1.1: Document Required Fields Enforcement at Runtime

### Overview

**Priority**: HIGH  
**Effort**: Low (documentation only)  
**Goal**: Clarify that Proto fields are optional (protobuf v3 semantics), but Router enforces required fields at runtime

### Files to Update

1. `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`
2. `docs/API_CONTRACTS.md`
3. `docs/archive/dev/CP1_BOUNDARIES_AND_CONTRACTS.md`

### Step-by-Step Instructions

#### Step 1: Update `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`

**Location**: After the "Router Service" section, before "Provider Service"

**Add new section**:

```markdown
## Field Optionality vs Runtime Requirements

### Proto Contract (Wire Protocol)

**Protobuf v3 Semantics**: All fields in Proto messages are **optional** by default. This allows backward compatibility when adding new fields.

**Actual Proto Fields** (from generated code `flow_pb.erl`):
- `Message`: All fields (1-7) are `optional`
- `RouteRequest`: All fields (1-3) are `optional`
- `RouteDecision`: All fields (1-6) are `optional`

### Runtime Validation (Router Layer)

**Router enforces required fields at runtime**, even though Proto contract allows optional fields. This provides:
- **Backward compatibility**: Proto contract allows optional fields
- **Runtime safety**: Router validates required fields and returns errors for missing fields

**Required Fields** (enforced at runtime):
- `Message`: `tenant_id`, `message_type`, `payload` (required)
- `RouteRequest`: `message` (required)
- `RouteDecision`: `provider_id`, `reason` (required)

**Validation Behavior**:
- Missing required fields → Router returns `ErrorResponse` with `code: "invalid_request"`
- Optional fields can be omitted → Router uses defaults or ignores
- Proto contract allows optional → No breaking changes when adding new optional fields

**Example - Minimal Valid Request**:
```json
{
  "message": {
    "message_id": "msg_123",
    "tenant_id": "tenant_abc",      // Required at runtime
    "message_type": "chat",          // Required at runtime
    "payload": "Hello"               // Required at runtime
  }
}
```

**Example - Full Request with Optional Fields**:
```json
{
  "message": {
    "message_id": "msg_123",
    "tenant_id": "tenant_abc",
    "trace_id": "trace_xyz",          // Optional
    "message_type": "chat",
    "payload": "Hello",
    "metadata": {"source": "gateway"}, // Optional
    "timestamp_ms": 1704067200000      // Optional
  },
  "policy_id": "default",             // Optional
  "context": {"user_id": "user_123"}   // Optional
}
```

**Error Response for Missing Required Field**:
```json
{
  "ok": false,
  "error": {
    "code": "invalid_request",
    "message": "Missing required field: tenant_id",
    "details": {"field": "message.tenant_id"}
  },
  "context": {
    "request_id": "req_123"
  }
}
```
```

#### Step 2: Update `docs/API_CONTRACTS.md`

**Location**: After "Overview" section, add new subsection

**Add section**:

```markdown
## Field Requirements: Proto vs Runtime

### Proto Contract Level

All fields in Proto messages are **optional** (protobuf v3 semantics). This allows:
- Backward compatibility when adding new fields
- Gradual migration of clients
- Flexible message construction

### Runtime Validation Level

Router enforces **required fields at runtime** via validation logic. Missing required fields result in `invalid_request` error.

**Required Fields**:
- `DecideRequest.message`: `tenant_id`, `message_type`, `payload` (required)
- `DecideRequest`: `message` (required)
- `DecideResponse.decision`: `provider_id`, `reason` (required)

**Validation Rules**:
1. Router validates all incoming requests against runtime requirements
2. Missing required fields → `ErrorResponse` with `code: "invalid_request"`
3. Optional fields can be omitted → Router handles gracefully
4. Proto contract remains optional → No breaking changes for Proto consumers

**Note**: This two-level approach (Proto optional, runtime required) is intentional and allows backward compatibility while ensuring data quality at runtime.
```

#### Step 3: Update `docs/archive/dev/CP1_BOUNDARIES_AND_CONTRACTS.md`

**Location**: In the "Protobuf Messages" section, after message definitions

**Add subsection**:

```markdown
### Field Optionality and Runtime Validation

**Proto Contract**: All fields are optional (protobuf v3 semantics)

**Runtime Validation**: Router enforces required fields:
- `Message`: `tenant_id`, `message_type`, `payload` (required at runtime)
- `RouteRequest`: `message` (required at runtime)
- `RouteDecision`: `provider_id`, `reason` (required at runtime)

**Rationale**: Proto optionality allows backward compatibility, while runtime validation ensures data quality. Missing required fields result in `invalid_request` error.
```

### Verification Steps

1. **Check all three files updated**:
   ```bash
   grep -l "Field Optionality\|Runtime Validation\|Proto vs Runtime" docs/ARCHITECTURE/PROTO_NATS_MAPPING.md docs/API_CONTRACTS.md docs/archive/dev/CP1_BOUNDARIES_AND_CONTRACTS.md
   ```

2. **Verify examples are present**:
   ```bash
   grep -A 5 "Minimal Valid Request\|Full Request with Optional" docs/ARCHITECTURE/PROTO_NATS_MAPPING.md
   ```

3. **Check error handling documented**:
   ```bash
   grep -A 3 "invalid_request\|Missing required field" docs/ARCHITECTURE/PROTO_NATS_MAPPING.md docs/API_CONTRACTS.md
   ```

### Acceptance Criteria

- ✅ All three files contain explanation of Proto optional vs runtime required
- ✅ Examples show both minimal and full payloads
- ✅ Error handling for missing required fields is documented
- ✅ Rationale for two-level approach is explained

---

## Action CP1.2: Clarify JSON Format vs Proto Contract

### Overview

**Priority**: HIGH  
**Effort**: Medium (documentation restructure)  
**Goal**: Clearly separate Proto contract from NATS JSON format with explicit labeling

### Files to Update

1. `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` (major restructure)
2. `docs/API_CONTRACTS.md` (add clarification section)

### Step-by-Step Instructions

#### Step 1: Restructure `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`

**Location**: Replace the "Router Service" section with restructured version

**Current structure** (to replace):
- Lines 11-102: Router Service section with mixed Proto/JSON

**New structure**:

```markdown
## Router Service (beamline.flow.v1)

### Proto Service: `Router.Decide`

**Package**: `beamline.flow.v1`  
**Service**: `Router`  
**RPC**: `Decide(RouteRequest) returns (RouteDecision)`

### Proto Contract (Wire Protocol / ABI)

**Source of Truth**: Generated code (`apps/otp/router/src/flow_pb.erl`, `apps/otp/router/include/flow_pb.hrl`)

**Proto Message Definitions**:

```protobuf
message Message {
  string message_id = 1;        // optional
  string tenant_id = 2;         // optional
  string trace_id = 3;          // optional
  string message_type = 4;      // optional
  bytes payload = 5;             // optional
  map<string, string> metadata = 6;  // repeated
  int64 timestamp_ms = 7;       // optional
}

message RouteRequest {
  Message message = 1;           // optional
  string policy_id = 2;          // optional
  map<string, string> context = 3; // repeated
}

message RouteDecision {
  string provider_id = 1;        // optional
  string reason = 2;             // optional
  int32 priority = 3;            // optional
  int64 expected_latency_ms = 4; // optional
  double expected_cost = 5;      // optional
  map<string, string> metadata = 6; // repeated
}
```

**Note**: All fields are optional in Proto (protobuf v3 semantics). Router enforces required fields at runtime (see Field Optionality section).

### NATS Subject: `beamline.router.v1.decide`

**Type**: Request-Reply  
**Request**: `RouteRequest` (protobuf) → JSON payload  
**Response**: `RouteDecision` (protobuf) → JSON payload

### NATS JSON Format (Logical Payload)

**Source of Truth**: This document (`PROTO_NATS_MAPPING.md`)

**Important**: NATS JSON format includes **Proto fields + NATS-specific fields** added at the adapter layer.

**Field Mapping**:

| NATS JSON Field | Proto Field | Source | Notes |
|----------------|-------------|--------|-------|
| `version` | ❌ Not in Proto | NATS adapter layer | NATS message version, used for protocol negotiation |
| `request_id` | ❌ Not in Proto | NATS adapter layer | NATS correlation ID for request-reply pattern |
| `task` | ❌ Not in Proto | NATS adapter layer | Task wrapper for NATS routing (may map to `message.payload`) |
| `constraints` | ❌ Not in Proto | NATS adapter layer | Routing constraints (may map to `context` or metadata) |
| `push_assignment` | ❌ Not in Proto | NATS adapter layer | Flag to trigger `ExecAssignment` publication |
| `message` | ✅ `RouteRequest.message` | Proto | Direct mapping from Proto |
| `policy_id` | ✅ `RouteRequest.policy_id` | Proto | Direct mapping from Proto |
| `context` | ✅ `RouteRequest.context` | Proto | Direct mapping from Proto |

**Request Format (NATS JSON)**:
```json
{
  "version": "1",              // ⚠️ NATS layer field (not in Proto)
  "request_id": "uuid",        // ⚠️ NATS layer field (not in Proto)
  "task": {...},               // ⚠️ NATS layer field (not in Proto)
  "constraints": {...},        // ⚠️ NATS layer field (not in Proto)
  "push_assignment": false,    // ⚠️ NATS layer field (not in Proto)
  "message": {...},            // ✅ Proto field (RouteRequest.message)
  "policy_id": "default",     // ✅ Proto field (RouteRequest.policy_id)
  "context": {...}             // ✅ Proto field (RouteRequest.context)
}
```

**Legend**:
- ⚠️ **NATS layer field**: Added at NATS adapter layer, not in Proto contract
- ✅ **Proto field**: Direct mapping from Proto message definition

**Response Format (NATS JSON)**:
```json
{
  "ok": true,
  "decision": {
    "provider_id": "string",    // ✅ Proto field (RouteDecision.provider_id)
    "reason": "string",         // ✅ Proto field (RouteDecision.reason)
    "priority": 0,              // ✅ Proto field (RouteDecision.priority)
    "expected_latency_ms": 0,   // ✅ Proto field (RouteDecision.expected_latency_ms)
    "expected_cost": 0.0,       // ✅ Proto field (RouteDecision.expected_cost)
    "metadata": {"key": "value"} // ✅ Proto field (RouteDecision.metadata)
  },
  "context": {
    "request_id": "string",     // ⚠️ NATS layer field (not in Proto)
    "trace_id": "string"        // ⚠️ NATS layer field (not in Proto)
  }
}
```

**Note**: NATS JSON format includes additional fields (`version`, `request_id`, `task`, `constraints`, `push_assignment`, `context.request_id`, `context.trace_id`) that are handled at the NATS adapter layer. These fields are **not** part of the Proto contract and should **not** be added to Proto files. See Section 1.5 in `ROUTER_PROTO_NATS_CONSISTENCY.md` for detailed explanation of two-level contract architecture.
```

#### Step 2: Update `docs/API_CONTRACTS.md`

**Location**: After "Overview" section, add new section

**Add section**:

```markdown
## Proto Contract vs NATS JSON Format

### Two-Level Architecture

Router uses a **two-level contract architecture**:
1. **Proto Contract** (wire protocol / ABI): Defines Proto message fields only
2. **NATS JSON Format** (logical payload): Includes Proto fields + NATS-specific fields

### NATS-Specific Fields

The following fields are **added at NATS adapter layer** and are **not** in Proto contract:
- `version` - NATS message version
- `request_id` - NATS correlation ID
- `task` - NATS-specific task wrapper
- `constraints` - NATS-specific routing constraints
- `push_assignment` - Flag to trigger ExecAssignment publication

**Source of Truth**:
- **Proto Contract**: Generated code (`flow_pb.erl`, `flow_pb.hrl`)
- **NATS JSON Format**: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`

**See**: `docs/archive/dev/ROUTER_PROTO_NATS_CONSISTENCY.md` Section 1.5 for complete explanation.
```

### Verification Steps

1. **Check restructured sections exist**:
   ```bash
   grep -A 2 "Proto Contract\|NATS JSON Format" docs/ARCHITECTURE/PROTO_NATS_MAPPING.md
   ```

2. **Verify field mapping table present**:
   ```bash
   grep -A 10 "NATS JSON Field\|Proto Field\|Source" docs/ARCHITECTURE/PROTO_NATS_MAPPING.md
   ```

3. **Check NATS layer fields are labeled**:
   ```bash
   grep "⚠️ NATS layer field\|✅ Proto field" docs/ARCHITECTURE/PROTO_NATS_MAPPING.md
   ```

### Acceptance Criteria

- ✅ Clear separation between Proto contract and NATS JSON format sections
- ✅ Field mapping table shows which fields are Proto vs NATS layer
- ✅ All NATS-specific fields are explicitly labeled
- ✅ Examples show both formats with clear markers
- ✅ Cross-reference to detailed explanation in consistency report

---

## Action CP1.3: Document Policy DSL to Proto Conversion

### Overview

**Priority**: MEDIUM  
**Effort**: Medium (documentation + examples)  
**Goal**: Document conversion logic between user-friendly DSL JSON and Proto representation

### Files to Update

1. `docs/ROUTING_POLICY.md`
2. `apps/otp/router/docs/schemas/policy.schema.json` (add comments)

### Step-by-Step Instructions

#### Step 1: Update `docs/ROUTING_POLICY.md`

**Location**: After "Storage" section, before "Integration" section

**Add new section**:

```markdown
## Policy DSL to Proto Conversion

### Overview

Router uses a **two-format policy system**:
- **DSL JSON Format**: User-friendly JSON that developers write (stored in database)
- **Proto Representation**: Proto `AdminPolicy` message used for gRPC communication

Router automatically converts DSL JSON to Proto `AdminPolicy` when needed.

### DSL JSON Format (User Input)

**What users write** (stored in database):
```json
{
  "version": "1.0",
  "providers": [
    {"name": "provider_a", "weight": 70},
    {"name": "provider_b", "weight": 30}
  ],
  "fallbacks": [
    {
      "when": {"status": ["timeout", "5xx"]},
      "retry": 2,
      "to": "provider_b"
    }
  ],
  "sticky": {
    "enabled": true,
    "session_key": "user_id",
    "ttl": "10m"
  }
}
```

### Proto Representation (gRPC / Storage)

**What's used in Proto/gRPC** (converted from DSL):
```protobuf
AdminPolicy {
  policy_id = "string"
  providers = [
    AdminProvider {id = "provider_a", weight = 0.7},
    AdminProvider {id = "provider_b", weight = 0.3}
  ]
  sticky = true  // bool only
  rules = [
    AdminRule {
      match = "status:timeout|5xx",  // converted from when
      prefer = []                     // empty
      fallback = "provider_b"         // from to
    }
  ]
}
```

**Note**: Proto `AdminPolicy` does not include:
- `version` - stored separately in database
- `tenant_id` - stored separately in database
- `sticky.session_key` - stored in Router internal state (ETS/Mnesia)
- `sticky.ttl` - stored in Router internal state (ETS/Mnesia)
- `fallbacks[].retry` - stored in Router internal state or metadata

### Conversion Logic

#### Providers Conversion

**DSL Format**:
```json
{
  "providers": [
    {"name": "provider_a", "weight": 70},
    {"name": "provider_b", "weight": 30}
  ]
}
```

**Proto Format**:
```protobuf
providers = [
  AdminProvider {id = "provider_a", weight = 0.7},
  AdminProvider {id = "provider_b", weight = 0.3}
]
```

**Conversion Rules**:
- `providers[].name` → `AdminProvider.id`
- `providers[].weight` (0-100) → `AdminProvider.weight` (0.0-1.0, divide by 100)
- Array order preserved

#### Fallbacks Conversion

**DSL Format**:
```json
{
  "fallbacks": [
    {
      "when": {"status": ["timeout", "5xx"]},
      "retry": 2,
      "to": "provider_b"
    }
  ]
}
```

**Proto Format**:
```protobuf
rules = [
  AdminRule {
    match = "status:timeout|5xx",  // converted from when
    prefer = [],                    // empty (no preferred providers)
    fallback = "provider_b"         // from to
  }
]
```

**Conversion Rules**:
- `fallbacks[].when` → `AdminRule.match` (converted to match expression)
- `fallbacks[].to` → `AdminRule.fallback`
- `fallbacks[].retry` → stored in Router internal state (not in Proto)
- `fallbacks[]` array → `AdminRule[]` array

**Match Expression Conversion**:
- `{"status": ["timeout", "5xx"]}` → `"status:timeout|5xx"`
- `{"message_type": "chat"}` → `"message_type:chat"`
- Complex conditions converted to match expression format

#### Sticky Conversion

**DSL Format**:
```json
{
  "sticky": {
    "enabled": true,
    "session_key": "user_id",
    "ttl": "10m"
  }
}
```

**Proto Format**:
```protobuf
sticky = true  // bool only
```

**Conversion Rules**:
- `sticky.enabled` → `AdminPolicy.sticky` (bool)
- `sticky.session_key` → stored in Router internal state (ETS/Mnesia `sticky_sessions` table)
- `sticky.ttl` → stored in Router internal state (ETS/Mnesia `sticky_sessions` table)

**Internal Storage**:
- Router stores `session_key` and `ttl` in ETS/Mnesia `sticky_sessions` table
- Not part of Proto `AdminPolicy` message
- Used for sticky session lookup and TTL expiration

### Conversion Implementation

**Router Code** (pseudo-code):
```erlang
convert_dsl_to_proto(DSLJson) ->
    Providers = [convert_provider(P) || P <- maps:get(<<"providers">>, DSLJson, [])],
    Rules = [convert_fallback(F) || F <- maps:get(<<"fallbacks">>, DSLJson, [])],
    Sticky = maps:get(<<"enabled">>, maps:get(<<"sticky">>, DSLJson, #{}), false),
    #admin_policy{
        providers = Providers,
        sticky = Sticky,
        rules = Rules
    }.

convert_provider(#{<<"name">> := Name, <<"weight">> := Weight}) ->
    #admin_provider{
        id = Name,
        weight = Weight / 100.0  % Convert 0-100 to 0.0-1.0
    }.

convert_fallback(#{<<"when">> := When, <<"to">> := To}) ->
    Match = convert_when_to_match(When),
    #admin_rule{
        match = Match,
        prefer = [],  % No preferred providers
        fallback = To
    }.
```

### Examples

#### Example 1: Simple Weighted Routing

**DSL**:
```json
{
  "providers": [
    {"name": "openai", "weight": 70},
    {"name": "anthropic", "weight": 30}
  ]
}
```

**Proto**:
```protobuf
AdminPolicy {
  providers = [
    AdminProvider {id = "openai", weight = 0.7},
    AdminProvider {id = "anthropic", weight = 0.3}
  ]
  sticky = false
  rules = []
}
```

#### Example 2: Complex Policy with Fallbacks and Sticky

**DSL**:
```json
{
  "providers": [
    {"name": "openai", "weight": 60},
    {"name": "anthropic", "weight": 40}
  ],
  "fallbacks": [
    {"when": {"status": ["timeout"]}, "retry": 2, "to": "anthropic"},
    {"when": {"status": ["5xx"]}, "retry": 1, "to": "openai"}
  ],
  "sticky": {
    "enabled": true,
    "session_key": "user_id",
    "ttl": "5m"
  }
}
```

**Proto**:
```protobuf
AdminPolicy {
  providers = [
    AdminProvider {id = "openai", weight = 0.6},
    AdminProvider {id = "anthropic", weight = 0.4}
  ]
  sticky = true
  rules = [
    AdminRule {
      match = "status:timeout",
      prefer = [],
      fallback = "anthropic"
    },
    AdminRule {
      match = "status:5xx",
      prefer = [],
      fallback = "openai"
    }
  ]
}
```

**Router Internal State** (not in Proto):
- `sticky_sessions` table: `{session_key: "user_id", ttl: "5m"}`
- Fallback retry counts: stored in Router internal state

### Field Mapping Summary

| DSL Field | Proto Field | Conversion | Internal Storage |
|-----------|-------------|------------|------------------|
| `version` | ❌ Not in Proto | N/A | Database (separate column) |
| `tenant_id` | ❌ Not in Proto | N/A | Database (separate column) |
| `providers[].name` | `AdminProvider.id` | Direct mapping | N/A |
| `providers[].weight` | `AdminProvider.weight` | Divide by 100 (0-100 → 0.0-1.0) | N/A |
| `fallbacks[].when` | `AdminRule.match` | Convert to match expression | N/A |
| `fallbacks[].to` | `AdminRule.fallback` | Direct mapping | N/A |
| `fallbacks[].retry` | ❌ Not in Proto | N/A | Router internal state |
| `sticky.enabled` | `AdminPolicy.sticky` | Direct mapping (bool) | N/A |
| `sticky.session_key` | ❌ Not in Proto | N/A | ETS/Mnesia `sticky_sessions` |
| `sticky.ttl` | ❌ Not in Proto | N/A | ETS/Mnesia `sticky_sessions` |

### References

- **DSL Schema**: `apps/otp/router/docs/schemas/policy.schema.json`
- **Proto Definition**: `apps/otp/router/src/flow_pb.erl` (generated)
- **Router Implementation**: `apps/otp/router/src/router_policy_store.erl`
```

#### Step 2: Update `apps/otp/router/docs/schemas/policy.schema.json`

**Location**: Add comments to JSON schema (JSON doesn't support comments, so add a separate documentation section)

**Create new file**: `apps/otp/router/docs/schemas/policy.schema.comments.md`

**Content**:

```markdown
# Policy Schema Comments

## Field Conversion Notes

This document explains how JSON schema fields map to Proto `AdminPolicy` message.

### `weights` (Schema) → `providers` (Proto)

**Schema**: `weights` is a map `{"provider_id": 0.0-100.0}`

**Proto**: `providers` is an array `AdminProvider[]` with `id` and `weight` fields

**Conversion**: Router converts map to array:
- Map key → `AdminProvider.id`
- Map value → `AdminProvider.weight` (divide by 100 to convert 0-100 to 0.0-1.0)

**Example**:
```json
// Schema format (user input)
{"weights": {"openai": 70, "anthropic": 30}}

// Proto format (converted)
{
  "providers": [
    {"id": "openai", "weight": 0.7},
    {"id": "anthropic", "weight": 0.3}
  ]
}
```

### `sticky` (Schema) → `sticky` (Proto)

**Schema**: `sticky` is an object with `enabled`, `session_key`, `ttl`

**Proto**: `sticky` is a boolean only

**Conversion**: Router extracts `enabled` → `sticky` (bool), stores `session_key` and `ttl` in internal state

**Example**:
```json
// Schema format (user input)
{"sticky": {"enabled": true, "session_key": "user_id", "ttl": "10m"}}

// Proto format (converted)
{"sticky": true}

// Router internal state (not in Proto)
// ETS/Mnesia: sticky_sessions table with {session_key: "user_id", ttl: "10m"}
```

### `fallback` (Schema) → `rules` (Proto)

**Schema**: `fallback` is an object with `provider` field

**Proto**: `rules` is an array `AdminRule[]` with `match`, `prefer`, `fallback` fields

**Conversion**: Router converts fallback to `AdminRule` with `fallback` field set

**Example**:
```json
// Schema format (user input)
{"fallback": {"provider": "anthropic"}}

// Proto format (converted)
{
  "rules": [
    {"match": "", "prefer": [], "fallback": "anthropic"}
  ]
}
```

**Note**: See `docs/ROUTING_POLICY.md` "Policy DSL to Proto Conversion" section for complete conversion logic.
```

### Verification Steps

1. **Check new section exists**:
   ```bash
   grep -A 2 "Policy DSL to Proto Conversion" docs/ROUTING_POLICY.md
   ```

2. **Verify conversion examples present**:
   ```bash
   grep -A 10 "DSL Format\|Proto Format" docs/ROUTING_POLICY.md
   ```

3. **Check field mapping table**:
   ```bash
   grep -A 5 "Field Mapping Summary" docs/ROUTING_POLICY.md
   ```

### Acceptance Criteria

- ✅ DSL format clearly documented with examples
- ✅ Proto representation clearly documented with examples
- ✅ Conversion logic explained step-by-step
- ✅ Field mapping table shows all conversions
- ✅ Internal-only fields (session_key, ttl, retry) documented
- ✅ Conversion implementation pseudo-code provided

---

## Action CP1.4: Document Missing Proto Files Status

### Overview

**Priority**: MEDIUM  
**Effort**: Low (documentation only)  
**Goal**: Document that Proto source files are missing and generated code is source of truth

### Files to Update

1. `proto/README.md`
2. `apps/otp/router/docs/GENERATION.md`

### Step-by-Step Instructions

#### Step 1: Update `proto/README.md`

**Location**: After "Structure" section, before "Services" section

**Add new section**:

```markdown
## Proto Source Files Status

**Current Status**: Proto source files (`proto/beamline/flow/v1/flow.proto`, `proto/beamline/provider/v1/provider.proto`) are currently **missing** (directories exist but are empty).

**Source of Truth**: Router uses generated code from `apps/otp/router/src/flow_pb.erl` and `apps/otp/router/include/flow_pb.hrl` as the authoritative source for message definitions.

**To Restore Proto Files**:
1. Extract Proto definitions from generated code (`flow_pb.erl`)
2. Recreate `.proto` files with proper protobuf syntax
3. Validate with `buf lint` and `buf build`
4. Regenerate code to verify: `rebar3 gpb compile`

**Note**: This restoration is deferred to CP2-LC. For CP1, generated code is the source of truth.
```

#### Step 2: Update `apps/otp/router/docs/GENERATION.md`

**Location**: After "Proto File Structure" section

**Add new subsection**:

```markdown
### Proto Source Files Status

**Current Status**: Proto source files are missing. Generated code is the source of truth.

**If Proto files are missing**:
- Generated code in `src/flow_pb.erl` and `include/flow_pb.hrl` is the authoritative source for message definitions
- Proto message structure can be verified by examining generated code
- To restore Proto files, extract definitions from generated code or use original `.proto` files if available

**Verification**:
```erlang
% Check message definitions
flow_pb:get_msg_names().
% Returns: ['Message', 'RouteRequest', 'RouteDecision', ...]

% Check field definitions
flow_pb:find_msg_def('Message').
% Returns: Field list with field numbers and types
```
```

### Verification Steps

1. **Check status documented**:
   ```bash
   grep -A 3 "Proto Source Files Status\|missing\|Source of Truth" proto/README.md apps/otp/router/docs/GENERATION.md
   ```

2. **Verify restoration instructions present**:
   ```bash
   grep -A 2 "To Restore\|restore" proto/README.md
   ```

### Acceptance Criteria

- ✅ Status of missing Proto files documented in both files
- ✅ Source of truth (generated code) clearly identified
- ✅ Instructions for restoration provided
- ✅ CP2-LC deferral mentioned

---

## Action CP1.5: Document Runtime Validation Rules

### Overview

**Priority**: LOW  
**Effort**: Low (documentation only)  
**Goal**: Document runtime validation rules and error codes

### Files to Update

1. `docs/API_CONTRACTS.md`
2. `apps/otp/router/docs/OPERATIONAL_GUIDE.md`

### Step-by-Step Instructions

#### Step 1: Update `docs/API_CONTRACTS.md`

**Location**: After "Validation (CP1/B)" section

**Add new subsection**:

```markdown
### Runtime Validation Rules

Router validates all incoming requests against Proto contract and runtime rules.

**Required Fields Validation**:
- Missing required fields → `ErrorResponse` with `code: "invalid_request"`
- Field type mismatches → `ErrorResponse` with `code: "invalid_request"`
- Field value out of range → `ErrorResponse` with `code: "invalid_request"`

**Error Codes for Validation Failures**:
- `invalid_request`: Missing required fields, invalid types, out of range values
- `unauthorized`: Authentication/authorization failed (if RBAC enabled)
- `internal`: Any other validation error

**Example Error Response**:
```json
{
  "ok": false,
  "error": {
    "code": "invalid_request",
    "message": "Missing required field: tenant_id",
    "details": {
      "field": "message.tenant_id",
      "type": "required_field_missing"
    }
  },
  "context": {
    "request_id": "req_123",
    "trace_id": "trace_xyz"
  }
}
```

**Validation Order**:
1. JSON parsing (malformed JSON → `invalid_request`)
2. Required fields check (missing required → `invalid_request`)
3. Field type validation (type mismatch → `invalid_request`)
4. Field value validation (out of range → `invalid_request`)
5. Business logic validation (policy not found → `policy_not_found`)
```

#### Step 2: Update `apps/otp/router/docs/OPERATIONAL_GUIDE.md`

**Location**: Add new section "Request Validation"

**Add section**:

```markdown
## Request Validation

Router validates all incoming requests against Proto contract and runtime rules.

**Validation Levels**:
1. **Proto Contract**: Validates against Proto message definitions (all fields optional)
2. **Runtime Rules**: Validates required fields and business logic constraints

**Required Fields** (enforced at runtime):
- `DecideRequest.message`: `tenant_id`, `message_type`, `payload`
- `DecideRequest`: `message`
- `DecideResponse.decision`: `provider_id`, `reason`

**Error Handling**:
- Validation failures return `ErrorResponse` with appropriate error code
- Error codes: `invalid_request`, `unauthorized`, `policy_not_found`, `internal`
- Error details include field name and validation failure type

**See**: `docs/API_CONTRACTS.md` "Runtime Validation Rules" section for detailed error codes and examples.
```

### Verification Steps

1. **Check validation rules documented**:
   ```bash
   grep -A 3 "Runtime Validation\|Required Fields Validation" docs/API_CONTRACTS.md
   ```

2. **Verify error codes listed**:
   ```bash
   grep -A 2 "invalid_request\|unauthorized\|policy_not_found" docs/API_CONTRACTS.md
   ```

### Acceptance Criteria

- ✅ Validation rules documented with error codes
- ✅ Error response examples provided
- ✅ Validation order explained
- ✅ Cross-references to related documentation

---

## Summary: CP1 Actions Checklist

### Action CP1.1: Document Required Fields Enforcement
- [ ] Update `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`
- [ ] Update `docs/API_CONTRACTS.md`
- [ ] Update `docs/archive/dev/CP1_BOUNDARIES_AND_CONTRACTS.md`
- [ ] Verify examples present
- [ ] Verify error handling documented

### Action CP1.2: Clarify JSON Format vs Proto Contract
- [ ] Restructure `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` (Router Service section)
- [ ] Add field mapping table
- [ ] Label NATS layer fields with ⚠️ marker
- [ ] Label Proto fields with ✅ marker
- [ ] Update `docs/API_CONTRACTS.md` with clarification section
- [ ] Verify clear separation between sections

### Action CP1.3: Document Policy DSL to Proto Conversion
- [ ] Add "Policy DSL to Proto Conversion" section to `docs/ROUTING_POLICY.md`
- [ ] Document DSL format with examples
- [ ] Document Proto representation with examples
- [ ] Document conversion logic step-by-step
- [ ] Add field mapping table
- [ ] Create `apps/otp/router/docs/schemas/policy.schema.comments.md`
- [ ] Verify all conversions documented

### Action CP1.4: Document Missing Proto Files Status
- [ ] Update `proto/README.md` with status section
- [ ] Update `apps/otp/router/docs/GENERATION.md` with status subsection
- [ ] Document source of truth (generated code)
- [ ] Provide restoration instructions
- [ ] Mention CP2-LC deferral

### Action CP1.5: Document Runtime Validation Rules
- [ ] Add "Runtime Validation Rules" section to `docs/API_CONTRACTS.md`
- [ ] Document error codes and examples
- [ ] Add "Request Validation" section to `apps/otp/router/docs/OPERATIONAL_GUIDE.md`
- [ ] Verify error handling documented

---

## Execution Order

All CP1 actions can be executed **in parallel** (different files, no dependencies):

1. **CP1.1** + **CP1.2** + **CP1.3** + **CP1.4** + **CP1.5** (all parallel)

**Estimated Time**: 2-3 days for all CP1 actions (documentation only)

---

## Verification After Completion

Run verification script:
```bash
# Check all files updated
grep -l "Field Optionality\|Runtime Validation\|Proto Contract\|NATS JSON Format\|Policy DSL to Proto\|Proto Source Files Status\|Runtime Validation Rules" \
  docs/ARCHITECTURE/PROTO_NATS_MAPPING.md \
  docs/API_CONTRACTS.md \
  docs/archive/dev/CP1_BOUNDARIES_AND_CONTRACTS.md \
  docs/ROUTING_POLICY.md \
  proto/README.md \
  apps/otp/router/docs/GENERATION.md \
  apps/otp/router/docs/OPERATIONAL_GUIDE.md
```

---

## References

### Related Documents
- **Consistency Report**: `docs/archive/dev/ROUTER_PROTO_NATS_CONSISTENCY.md` - Full analysis and findings
- **Action Plan**: `docs/archive/dev/ROUTER_PROTO_NATS_ACTION_PLAN.md` - Prioritized actions overview
- **CP2+ Detailed Plan**: `docs/archive/dev/ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md` - CP2-LC implementation instructions
- **Summary**: `docs/archive/dev/ROUTER_PROTO_NATS_CONSISTENCY_SUMMARY.md` - Brief version for CP reports

### Source Documents
- **CP1 Boundaries**: `docs/archive/dev/CP1_BOUNDARIES_AND_CONTRACTS.md`
- **Proto-NATS Mapping**: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`
- **API Contracts**: `docs/API_CONTRACTS.md`
- **Routing Policy**: `docs/ROUTING_POLICY.md`

