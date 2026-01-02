# Router Proto/NATS Contracts Consistency Report

**Date**: 2025-01-27  
**Step**: 1.1 - Proto/NATS Contracts and Documentation Verification  
**Status**: Analysis Complete  
**Version**: 1.0

## Executive Summary

Proto/NATS contracts consistency verification completed for Router. **NATS subject mapping is fully consistent** across all documentation sources. **Two-level contract architecture** (Proto wire protocol vs NATS JSON payload) is documented and clarified (Section 1.5). **CP2+ fields are documented but not yet in Proto code** (expected for CP1, deferred to CP2+). **Documentation improvements needed** to clarify required field enforcement at runtime and Policy DSL conversion logic.

**Key Findings**:
- ✅ **NATS Subjects**: Consistent mapping between `PROTO_NATS_MAPPING.md` and `NATS_SUBJECTS.md`
- ✅ **Two-Level Architecture**: Proto wire protocol (gRPC/ABI) vs NATS JSON payload (with adapter-layer fields) clarified
- ⚠️ **Documentation**: Required field enforcement at runtime needs clarification (Proto optional, runtime required)
- ⚠️ **Documentation**: Policy DSL to Proto conversion logic needs documentation
- ❌ **CP2+ Deferred**: Proto source files missing (restore in CP2+)
- ❌ **CP2+ Deferred**: CP2+ fields not in Proto code (add in CP2+, backward compatible)

**CP1 Readiness**: ✅ **READY** - No blocking issues. All inconsistencies are either resolved by design, documentation improvements, or deferred to CP2+.

**Report Structure**:
- **Section 1**: Source of Truth and Two-Level Architecture
- **Section 1.5**: Proto Contract vs NATS JSON Format (Detailed)
- **Section 2**: Proto ↔ NATS Subjects Verification
- **Section 3**: Proto Fields ↔ Routing Policy Verification
- **Section 4**: DSL Policies ↔ DTO/Proto Verification
- **Section 5**: Summary and Action Items
- **Section 6**: References

**Related Documents**:
- **Action Plan**: `docs/archive/dev/ROUTER_PROTO_NATS_ACTION_PLAN.md` - Prioritized action items
- **CP1 Detailed Plan**: `docs/archive/dev/ROUTER_PROTO_NATS_CP1_DETAILED_PLAN.md` - Step-by-step CP1 instructions
- **CP2+ Detailed Plan**: `docs/archive/dev/ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md` - Step-by-step CP2+ instructions
- **Summary**: `docs/archive/dev/ROUTER_PROTO_NATS_CONSISTENCY_SUMMARY.md` - Brief version for CP reports

---

## 1. Source of Truth

### Proto Sources
- **Generated Code**: `apps/otp/router/src/flow_pb.erl` (Erlang protobuf definitions)
- **Generated Headers**: `apps/otp/router/include/flow_pb.hrl` (Erlang record definitions)
- **Documentation**: `proto/README.md`, `docs/archive/dev/CP1_BOUNDARIES_AND_CONTRACTS.md`
- **Note**: Actual `.proto` files are missing from `proto/beamline/flow/v1/flow.proto` and `proto/beamline/provider/v1/provider.proto` directories (directories exist but are empty)

### NATS Sources
- **NATS Subjects**: `docs/NATS_SUBJECTS.md`
- **Proto-NATS Mapping**: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`
- **API Contracts**: `docs/API_CONTRACTS.md`

### DSL Policy Sources
- **Routing Policy**: `docs/ROUTING_POLICY.md`
- **Policy Schema**: `apps/otp/router/docs/schemas/policy.schema.json`
- **Test Examples**: `apps/otp/router/test/router_policy_store_SUITE.erl`, `apps/otp/router/test/router_core_SUITE.erl`

### Proto Contract vs NATS JSON Format: Two-Level Architecture

**CRITICAL**: Router uses a **two-level contract architecture** that separates the wire protocol (Proto/ABI) from the logical payload format (NATS JSON). Understanding this distinction is essential for interpreting the consistency findings below.

**Level 1: Official Proto Contract (Wire / ABI)**
- **Source of Truth**: Generated Proto code (`apps/otp/router/src/flow_pb.erl`, `apps/otp/router/include/flow_pb.hrl`)
- **Purpose**: Wire protocol and ABI for gRPC communication and binary serialization
- **Scope**: Only fields defined in Proto message definitions

**Level 2: Logical NATS JSON Payload**
- **Source of Truth**: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`
- **Purpose**: NATS message exchange format
- **Scope**: Proto fields + NATS-specific fields (`version`, `request_id`, `task`, `constraints`, `push_assignment`)

**Key Principle**: NATS-specific fields are added at the adapter layer, not in Proto contract. This is **by design**, not an inconsistency.

**Detailed Explanation**: See Section 1.5 below for complete two-level architecture documentation.

---

## 1.5 Proto Contract vs NATS JSON Format: Source of Truth (Detailed)

### Two-Level Contract Architecture

Router uses a **two-level contract architecture** that separates the wire protocol (Proto/ABI) from the logical payload format (NATS JSON).

#### Level 1: Official Proto Contract (Wire / ABI)

**Source of Truth**: Generated Proto code (`apps/otp/router/src/flow_pb.erl`, `apps/otp/router/include/flow_pb.hrl`)

**Definition**: The **official Proto contract** defines the wire protocol and ABI (Application Binary Interface) for gRPC communication and binary serialization.

**Characteristics**:
- **Format**: Protobuf binary (wire format) or Protobuf JSON (canonical JSON representation)
- **Scope**: Defines only fields that are part of the Proto message definitions
- **Purpose**: ABI contract for gRPC services and binary serialization
- **Validation**: Enforced by protobuf compiler and runtime

**Example** (`RouteRequest` from Proto):
```protobuf
message RouteRequest {
  Message message = 1;           // Proto field
  string policy_id = 2;          // Proto field
  map<string, string> context = 3; // Proto field
}
```

**Source of Truth**: 
- **Primary**: Generated code (`flow_pb.erl`, `flow_pb.hrl`) - authoritative
- **Secondary**: Proto source files (`proto/beamline/flow/v1/flow.proto`) - when available
- **Documentation**: `docs/archive/dev/CP1_BOUNDARIES_AND_CONTRACTS.md` (Proto message definitions)

#### Level 2: Logical NATS JSON Payload

**Source of Truth**: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`

**Definition**: The **logical NATS JSON payload** is the JSON format used for NATS message exchange. It includes Proto fields plus additional NATS-specific fields added at the adapter layer.

**Characteristics**:
- **Format**: JSON (human-readable, NATS-friendly)
- **Scope**: Includes Proto fields + NATS-specific fields (`version`, `request_id`, `task`, `constraints`, `push_assignment`)
- **Purpose**: NATS message exchange format with additional routing/metadata fields
- **Validation**: Enforced by Router NATS adapter layer

**Example** (NATS JSON payload for `RouteRequest`):
```json
{
  "version": "1",              // NATS layer field (not in Proto)
  "request_id": "uuid",        // NATS layer field (not in Proto)
  "task": {...},               // NATS layer field (not in Proto)
  "constraints": {...},        // NATS layer field (not in Proto)
  "push_assignment": false,    // NATS layer field (not in Proto)
  "message": {...},            // Proto field (from RouteRequest.message)
  "policy_id": "default",     // Proto field (from RouteRequest.policy_id)
  "context": {...}             // Proto field (from RouteRequest.context)
}
```

**Source of Truth**:
- **Primary**: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` - authoritative for NATS JSON format
- **Secondary**: `docs/API_CONTRACTS.md` (API contract examples)
- **Implementation**: Router NATS adapter (`router_nats_subscriber.erl`) handles conversion

### Relationship Between Levels

**Mapping Logic**:
1. **Proto → NATS JSON**: Router NATS adapter extracts Proto fields and adds NATS-specific fields
2. **NATS JSON → Proto**: Router NATS adapter extracts Proto fields and ignores NATS-specific fields
3. **Conversion**: NATS-specific fields (`version`, `request_id`, `task`, `constraints`, `push_assignment`) are handled at the adapter layer, not in Proto

**Field Mapping Table**:

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

### Source of Truth Summary

| Level | Source of Truth | Purpose | Validation |
|-------|----------------|---------|------------|
| **Proto Contract** | Generated code (`flow_pb.erl`, `flow_pb.hrl`) | Wire protocol / ABI | Protobuf compiler + runtime |
| **NATS JSON Format** | `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` | NATS message exchange | Router NATS adapter layer |

**Key Principle**: 
- **Proto contract** is the source of truth for **wire protocol** and **ABI** (gRPC, binary serialization)
- **NATS JSON format** is the source of truth for **NATS message exchange** (includes Proto fields + NATS-specific fields)
- **NATS adapter layer** handles conversion between Proto and NATS JSON formats

**Implications**:
- Proto contract changes require Proto file updates and code regeneration
- NATS JSON format changes require documentation updates and adapter layer changes
- NATS-specific fields (`version`, `request_id`, `task`, `constraints`, `push_assignment`) are **not** part of Proto contract and should **not** be added to Proto files

---

## 2. Proto ↔ NATS Subjects Verification

### 2.1 Router Service (beamline.flow.v1)

| Message/RPC | Proto Package.Version | NATS Subject | Source | Status | Comment |
|-------------|----------------------|--------------|--------|--------|---------|
| `Router.Decide` | `beamline.flow.v1` | `beamline.router.v1.decide` | PROTO_NATS_MAPPING.md, NATS_SUBJECTS.md | ✅ OK | Service correctly mapped |
| `RouteRequest` | `beamline.flow.v1` | `beamline.router.v1.decide` (request) | PROTO_NATS_MAPPING.md | ✅ OK | Request message mapped |
| `RouteDecision` | `beamline.flow.v1` | `beamline.router.v1.decide` (reply) | PROTO_NATS_MAPPING.md | ✅ OK | Response message mapped |
| `RouteDecision` | `beamline.flow.v1` | `beamline.router.v1.decide.reply` | NATS_SUBJECTS.md | ✅ OK | Alternative reply subject pattern |

**Findings**:
- ✅ Service mapping is consistent between `PROTO_NATS_MAPPING.md` and `NATS_SUBJECTS.md`
- ✅ Request-reply pattern correctly documented
- ✅ **Note**: `PROTO_NATS_MAPPING.md` shows extended JSON format with `version`, `request_id`, `task`, `constraints`, `push_assignment` fields that are **not present in Proto `RouteRequest`** - this is **by design** (see Section 1.5 for explanation of two-level contract architecture)

### 2.2 Provider Service (beamline.provider.v1)

| Message/RPC | Proto Package.Version | NATS Subject | Source | Status | Comment |
|-------------|----------------------|--------------|--------|--------|---------|
| `Provider.Invoke` | `beamline.provider.v1` | `beamline.provider.v1.invoke` | PROTO_NATS_MAPPING.md, NATS_SUBJECTS.md | ✅ OK | Service correctly mapped |
| `ProviderRequest` | `beamline.provider.v1` | `beamline.provider.v1.invoke` (request) | PROTO_NATS_MAPPING.md | ✅ OK | Request message mapped |
| `ProviderResponse` | `beamline.provider.v1` | `beamline.provider.v1.invoke` (reply) | PROTO_NATS_MAPPING.md | ✅ OK | Response message mapped |
| `Provider.InvokeStream` | `beamline.provider.v1` | `beamline.provider.v1.invoke.stream` | PROTO_NATS_MAPPING.md | ✅ OK | Streaming service mapped |
| `StreamChunk` | `beamline.provider.v1` | `beamline.provider.v1.invoke.stream` (stream) | PROTO_NATS_MAPPING.md | ✅ OK | Stream message mapped |

**Findings**:
- ✅ Provider service mapping is consistent
- ⚠️ **Note**: Proto files for provider are missing (cannot verify actual message definitions)

### 2.3 Ingress Service (beamline.ingress.v1)

| Message/RPC | Proto Package.Version | NATS Subject | Source | Status | Comment |
|-------------|----------------------|--------------|--------|--------|---------|
| `Ingress.Send` | `beamline.ingress.v1` | `beamline.ingress.v1.message` | PROTO_NATS_MAPPING.md, NATS_SUBJECTS.md | ✅ OK | Service correctly mapped |
| `IngressMessage` | `beamline.ingress.v1` | `beamline.ingress.v1.message` (request) | PROTO_NATS_MAPPING.md | ✅ OK | Request message mapped |
| `IngressAck` | `beamline.ingress.v1` | `beamline.ingress.v1.message` (reply) | PROTO_NATS_MAPPING.md | ✅ OK | Response message mapped |

**Findings**:
- ✅ Ingress service mapping is consistent

### 2.4 CAF/Execution Subjects

| Subject | Purpose | Source | Status | Comment |
|---------|---------|--------|--------|---------|
| `caf.exec.assign.v1` | Router → CAF execution assignment | NATS_SUBJECTS.md | ✅ OK | Documented, not in Proto mapping |
| `caf.exec.assign.v1.ack` | CAF → Router assignment acknowledgment | NATS_SUBJECTS.md | ✅ OK | Documented, not in Proto mapping |
| `caf.exec.result.v1` | CAF → Router execution results | NATS_SUBJECTS.md | ✅ OK | Documented, not in Proto mapping |
| `caf.worker.heartbeat.v1` | Worker heartbeat | NATS_SUBJECTS.md | ✅ OK | Documented, not in Proto mapping |
| `caf.exec.dlq.v1` | Dead-letter queue | NATS_SUBJECTS.md | ✅ OK | Documented, not in Proto mapping |

**Findings**:
- ✅ CAF subjects are documented in `NATS_SUBJECTS.md`
- ⚠️ **Note**: CAF subjects are **not** in `PROTO_NATS_MAPPING.md` (they use JSON contracts from `API_CONTRACTS.md`, not Proto)

### 2.5 Other Subjects

| Subject | Purpose | Source | Status | Comment |
|---------|---------|--------|--------|---------|
| `beamline.usage.v1.metered` | Usage events | NATS_SUBJECTS.md | ✅ OK | Documented, not in Proto mapping |
| `beamline.alert.v1.critical` | Critical alerts | NATS_SUBJECTS.md | ✅ OK | Documented, not in Proto mapping |
| `beamline.alert.v1.warning` | Warnings | NATS_SUBJECTS.md | ✅ OK | Documented, not in Proto mapping |
| `beamline.alert.v1.info` | Informational alerts | NATS_SUBJECTS.md | ✅ OK | Documented, not in Proto mapping |

**Findings**:
- ✅ All subjects from `NATS_SUBJECTS.md` are accounted for
- ⚠️ **Note**: Usage and Alert subjects use JSON contracts, not Proto (expected)

---

## 3. Proto Fields ↔ Routing Policy / Invariants Verification

### 3.1 Message Fields (from Generated Code)

**Actual Proto Fields** (from `flow_pb.erl`):
```erlang
'Message' = [
  {message_id, 1, string, optional},
  {tenant_id, 2, string, optional},
  {trace_id, 3, string, optional},
  {message_type, 4, string, optional},
  {payload, 5, bytes, optional},
  {metadata, 6, map<string, string>, repeated},
  {timestamp_ms, 7, int64, optional}
]
```

**Documented CP2+ Fields** (from `proto/README.md`, `PROTO_NATS_MAPPING.md`):
- `run_id` (field 8) - **NOT PRESENT in generated code**
- `flow_id` (field 9) - **NOT PRESENT in generated code**
- `step_id` (field 10) - **NOT PRESENT in generated code**
- `idempotency_key` (field 11) - **NOT PRESENT in generated code**
- `span_id` (field 12) - **NOT PRESENT in generated code**

| Field | Where Described (Doc) | Where in Proto | Type/Optionality | Status | Comment |
|-------|----------------------|----------------|------------------|--------|---------|
| `message_id` | API_CONTRACTS.md, PROTO_NATS_MAPPING.md | ✅ Field 1, optional | string, optional | ✅ OK | Present |
| `tenant_id` | API_CONTRACTS.md, PROTO_NATS_MAPPING.md, ROUTING_POLICY.md | ✅ Field 2, optional | string, optional | ⚠️ **INCONSISTENCY** | **Doc says required, Proto is optional** |
| `trace_id` | API_CONTRACTS.md, PROTO_NATS_MAPPING.md | ✅ Field 3, optional | string, optional | ✅ OK | Present |
| `message_type` | API_CONTRACTS.md, PROTO_NATS_MAPPING.md | ✅ Field 4, optional | string, optional | ⚠️ **INCONSISTENCY** | **Doc says required, Proto is optional** |
| `payload` | API_CONTRACTS.md, PROTO_NATS_MAPPING.md | ✅ Field 5, optional | bytes, optional | ⚠️ **INCONSISTENCY** | **Doc says required, Proto is optional** |
| `metadata` | API_CONTRACTS.md, PROTO_NATS_MAPPING.md | ✅ Field 6, repeated | map<string, string>, repeated | ✅ OK | Present |
| `timestamp_ms` | API_CONTRACTS.md, PROTO_NATS_MAPPING.md | ✅ Field 7, optional | int64, optional | ✅ OK | Present |
| `run_id` | PROTO_NATS_MAPPING.md (CP2+), proto/README.md | ❌ **NOT IN PROTO** | string, optional (CP2+) | ❌ **MISSING** | **Documented but not in generated code** |
| `flow_id` | PROTO_NATS_MAPPING.md (CP2+), proto/README.md | ❌ **NOT IN PROTO** | string, optional (CP2+) | ❌ **MISSING** | **Documented but not in generated code** |
| `step_id` | PROTO_NATS_MAPPING.md (CP2+), proto/README.md | ❌ **NOT IN PROTO** | string, optional (CP2+) | ❌ **MISSING** | **Documented but not in generated code** |
| `idempotency_key` | PROTO_NATS_MAPPING.md (CP2+), proto/README.md | ❌ **NOT IN PROTO** | string, optional (CP2+) | ❌ **MISSING** | **Documented but not in generated code** |
| `span_id` | PROTO_NATS_MAPPING.md (CP2+), proto/README.md | ❌ **NOT IN PROTO** | string, optional (CP2+) | ❌ **MISSING** | **Documented but not in generated code** |

**Critical Findings**:
1. ❌ **CP2+ fields are documented but NOT in generated Proto code** - Proto files are missing, so CP2+ fields cannot be verified
2. ⚠️ **Required vs Optional mismatch**: Documentation says `tenant_id`, `message_type`, `payload` are required, but Proto defines them as optional
3. ⚠️ **Runtime validation**: Router may enforce required fields at runtime, but Proto contract allows optional

### 3.2 RouteRequest Fields

**Actual Proto Fields** (from `flow_pb.erl`):
```erlang
'RouteRequest' = [
  {message, 1, Message, optional},
  {policy_id, 2, string, optional},
  {context, 3, map<string, string>, repeated}
]
```

**Documented Fields** (from `PROTO_NATS_MAPPING.md`):
- Extended JSON format includes: `version`, `request_id`, `task`, `constraints`, `push_assignment`, `message`, `policy_id`, `idempotency_key` (CP2+)

| Field | Where Described (Doc) | Where in Proto | Type/Optionality | Status | Comment |
|-------|----------------------|----------------|------------------|--------|---------|
| `message` | PROTO_NATS_MAPPING.md, API_CONTRACTS.md | ✅ Field 1, optional | Message, optional | ⚠️ **INCONSISTENCY** | **Doc says required, Proto is optional** |
| `policy_id` | PROTO_NATS_MAPPING.md, API_CONTRACTS.md | ✅ Field 2, optional | string, optional | ✅ OK | Present |
| `context` | PROTO_NATS_MAPPING.md, API_CONTRACTS.md | ✅ Field 3, repeated | map<string, string>, repeated | ✅ OK | Present |
| `version` | PROTO_NATS_MAPPING.md, API_CONTRACTS.md | ❌ **NOT IN PROTO** | string, required | ✅ **BY DESIGN** | **NATS layer field (see Section 1.5)** |
| `request_id` | PROTO_NATS_MAPPING.md, API_CONTRACTS.md | ❌ **NOT IN PROTO** | string, required | ✅ **BY DESIGN** | **NATS layer field (see Section 1.5)** |
| `task` | PROTO_NATS_MAPPING.md, API_CONTRACTS.md | ❌ **NOT IN PROTO** | object, optional | ✅ **BY DESIGN** | **NATS layer field (see Section 1.5)** |
| `constraints` | PROTO_NATS_MAPPING.md, API_CONTRACTS.md | ❌ **NOT IN PROTO** | object, optional | ✅ **BY DESIGN** | **NATS layer field (see Section 1.5)** |
| `push_assignment` | PROTO_NATS_MAPPING.md, API_CONTRACTS.md | ❌ **NOT IN PROTO** | boolean, optional | ✅ **BY DESIGN** | **NATS layer field (see Section 1.5)** |
| `idempotency_key` | PROTO_NATS_MAPPING.md (CP2+) | ❌ **NOT IN PROTO** | string, optional (CP2+) | ❌ **MISSING** | **Documented but not in generated code** |

**Critical Findings**:
1. ✅ **Extended JSON format fields are NOT in Proto `RouteRequest`** - This is **by design** (see Section 1.5). `PROTO_NATS_MAPPING.md` shows NATS JSON format with `version`, `request_id`, `task`, `constraints`, `push_assignment` that are added at NATS adapter layer, not in Proto contract. **Action**: Update documentation to explicitly label NATS-specific fields (see Action CP1.2 in `ROUTER_PROTO_NATS_ACTION_PLAN.md`).
2. ❌ **CP2+ `idempotency_key` field is documented but NOT in Proto** - CP2+ field needs to be added to Proto in CP2-LC (see Action CP2.3 in `ROUTER_PROTO_NATS_ACTION_PLAN.md` and `ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md`).
3. ✅ **JSON vs Proto relationship clarified**: NATS JSON payload format includes Proto fields + NATS-specific fields (see Section 1.5 for two-level contract architecture).

### 3.3 RouteDecision Fields

**Actual Proto Fields** (from `flow_pb.erl`):
```erlang
'RouteDecision' = [
  {provider_id, 1, string, optional},
  {reason, 2, string, optional},
  {priority, 3, int32, optional},
  {expected_latency_ms, 4, int64, optional},
  {expected_cost, 5, double, optional},
  {metadata, 6, map<string, string>, repeated}
]
```

**Documented Fields** (from `API_CONTRACTS.md`, `PROTO_NATS_MAPPING.md`):
- All fields match Proto definition

| Field | Where Described (Doc) | Where in Proto | Type/Optionality | Status | Comment |
|-------|----------------------|----------------|------------------|--------|---------|
| `provider_id` | API_CONTRACTS.md, PROTO_NATS_MAPPING.md | ✅ Field 1, optional | string, optional | ⚠️ **INCONSISTENCY** | **Doc says required, Proto is optional** |
| `reason` | API_CONTRACTS.md, PROTO_NATS_MAPPING.md | ✅ Field 2, optional | string, optional | ⚠️ **INCONSISTENCY** | **Doc says required, Proto is optional** |
| `priority` | API_CONTRACTS.md, PROTO_NATS_MAPPING.md | ✅ Field 3, optional | int32, optional | ✅ OK | Present |
| `expected_latency_ms` | API_CONTRACTS.md, PROTO_NATS_MAPPING.md | ✅ Field 4, optional | int64, optional | ✅ OK | Present |
| `expected_cost` | API_CONTRACTS.md, PROTO_NATS_MAPPING.md | ✅ Field 5, optional | double, optional | ✅ OK | Present |
| `metadata` | API_CONTRACTS.md, PROTO_NATS_MAPPING.md | ✅ Field 6, repeated | map<string, string>, repeated | ✅ OK | Present |

**Findings**:
- ✅ All fields are present in Proto
- ⚠️ **Required vs Optional mismatch**: Documentation says `provider_id` and `reason` are required, but Proto defines them as optional

### 3.4 Routing Policy Fields

**Key Fields for Routing Decisions** (from `ROUTING_POLICY.md`):
- `tenant_id` - Required for tenant-scoped policy access
- `policy_id` - Optional, defaults to "default"
- `context` - Used for sticky sessions (`session_key`)

| Policy Field | Where Described | Where in Proto | Status | Comment |
|--------------|-----------------|----------------|--------|---------|
| `tenant_id` | ROUTING_POLICY.md (RBAC) | ✅ `Message.tenant_id` (field 2) | ✅ OK | Present in Proto |
| `policy_id` | ROUTING_POLICY.md | ✅ `RouteRequest.policy_id` (field 2) | ✅ OK | Present in Proto |
| `context` | ROUTING_POLICY.md (sticky sessions) | ✅ `RouteRequest.context` (field 3) | ✅ OK | Present in Proto |
| `session_key` | ROUTING_POLICY.md (sticky) | ⚠️ In `context` map | ✅ OK | Stored in `context` map, not separate field |

**Findings**:
- ✅ All routing policy fields are present in Proto (via `Message.tenant_id`, `RouteRequest.policy_id`, `RouteRequest.context`)

---

## 4. DSL Policies ↔ DTO/Proto Verification

### 4.1 Policy DSL Elements

**From `ROUTING_POLICY.md`**:
```json
{
  "version": "1.0",
  "providers": [{"name": "provider_a", "weight": 70}, ...],
  "fallbacks": [{"when": {...}, "retry": 2, "to": "provider_b"}, ...],
  "sticky": {"enabled": true, "session_key": "user_id", "ttl": "10m"}
}
```

**From `policy.schema.json`**:
```json
{
  "policy_id": "string",
  "tenant_id": "string",
  "weights": {"provider_id": 0.0-100.0},
  "sticky": {"enabled": boolean, "session_key": "string"},
  "fallback": {"provider": "string"},
  "metadata": {}
}
```

**From Proto `AdminPolicy`** (from `flow_pb.erl`):
```erlang
'AdminPolicy' = [
  {policy_id, 1, string, optional},
  {providers, 2, AdminProvider[], repeated},
  {sticky, 3, bool, optional},
  {rules, 4, AdminRule[], repeated}
]

'AdminProvider' = [
  {id, 1, string, optional},
  {weight, 2, double, optional}
]

'AdminRule' = [
  {match, 1, string, optional},
  {prefer, 2, string[], repeated},
  {fallback, 3, string, optional}
]
```

| DSL Element | Description (from Doc) | Representation (DTO/Proto/Code) | Class | Comment |
|-------------|------------------------|----------------------------------|-------|---------|
| `version` | Policy version | ❌ **NOT IN PROTO** | Internal only | Stored in database, not in Proto |
| `providers` | Provider list with weights | ✅ `AdminPolicy.providers` (AdminProvider[]) | Has DTO/Proto | Present in Proto |
| `weights` | Provider weights map | ⚠️ Via `AdminProvider.weight` | Has DTO/Proto | Represented as `AdminProvider[]` array, not map |
| `fallbacks` | Fallback rules | ⚠️ Via `AdminRule.fallback` | Has DTO/Proto | Represented as `AdminRule[]` array, not `fallbacks` array |
| `sticky` | Sticky session config | ✅ `AdminPolicy.sticky` (bool) | ⚠️ **PARTIAL** | Proto has `sticky: bool`, but missing `session_key` and `ttl` |
| `session_key` | Sticky session key | ❌ **NOT IN PROTO** | Internal only | Used in Router code, not in Proto |
| `ttl` | Sticky session TTL | ❌ **NOT IN PROTO** | Internal only | Used in Router code, not in Proto |
| `rules` | Routing rules | ✅ `AdminPolicy.rules` (AdminRule[]) | Has DTO/Proto | Present in Proto |
| `match` | Rule match condition | ✅ `AdminRule.match` (string) | Has DTO/Proto | Present in Proto |
| `prefer` | Preferred providers | ✅ `AdminRule.prefer` (string[]) | Has DTO/Proto | Present in Proto |
| `fallback` | Fallback provider | ✅ `AdminRule.fallback` (string) | Has DTO/Proto | Present in Proto |
| `policy_id` | Policy identifier | ✅ `AdminPolicy.policy_id` | Has DTO/Proto | Present in Proto |
| `tenant_id` | Tenant identifier | ❌ **NOT IN AdminPolicy** | Internal only | Stored separately, not in Proto message |

**Critical Findings**:
1. ⚠️ **Structure mismatch** (expected, by design): 
   - Doc shows `weights` as map `{"provider_id": weight}`, Proto uses `AdminProvider[]` array
   - Doc shows `fallbacks` as array `[{"when": {...}, "retry": 2, "to": "provider_b"}]`, Proto uses `AdminRule[]` with `fallback` field
   - **Resolution**: Document conversion logic between DSL JSON and Proto (see Action CP1.3 in `ROUTER_PROTO_NATS_ACTION_PLAN.md`)
2. ⚠️ **Internal-only fields** (expected, by design):
   - `sticky.session_key` - not in Proto (only `sticky: bool`), stored in Router internal state (ETS/Mnesia)
   - `sticky.ttl` - not in Proto, stored in Router internal state (ETS/Mnesia)
   - `fallbacks[].when` - converted to `AdminRule.match` in Proto
   - `fallbacks[].retry` - stored in Router internal state, not in Proto
   - `version`, `tenant_id` - stored separately in database, not in Proto message
   - **Resolution**: Document that these fields are stored in Router internal state or database, not in Proto (see Action CP1.3)
3. ✅ **Conversion logic needed**: Router converts user-friendly DSL format to Proto `AdminPolicy` for storage and gRPC communication. This conversion should be documented (see Action CP1.3 in `ROUTER_PROTO_NATS_ACTION_PLAN.md`).

---

## 5. Summary and Action Items

### 5.1 Critical Issues

#### Issue 1: Missing Proto Files
- **Status**: ❌ **CRITICAL**
- **Description**: Proto files are missing from `proto/beamline/flow/v1/flow.proto` and `proto/beamline/provider/v1/provider.proto` (directories exist but are empty)
- **Impact**: Cannot verify actual Proto definitions, must rely on generated code
- **Action**: Restore or regenerate Proto files from generated code, or verify that generated code matches expected definitions

#### Issue 2: CP2+ Fields Not in Proto
- **Status**: ❌ **CRITICAL**
- **Description**: Documentation mentions CP2+ fields (`run_id`, `flow_id`, `step_id`, `idempotency_key`, `span_id` in `Message`, `idempotency_key` in `RouteRequest`), but they are NOT in generated Proto code
- **Impact**: CP2+ features cannot be used until Proto files are updated
- **Action**: 
  1. Add CP2+ fields to Proto files (fields 8-12 in `Message`, field 4 in `RouteRequest`)
  2. Regenerate code: `rebar3 gpb compile`
  3. Update documentation to reflect actual Proto state

#### Issue 3: JSON Format vs Proto Mismatch
- **Status**: ⚠️ **RESOLVED** (by design)
- **Description**: `PROTO_NATS_MAPPING.md` shows extended JSON format with `version`, `request_id`, `task`, `constraints`, `push_assignment` fields that are NOT in Proto `RouteRequest`
- **Root Cause**: This is **by design** - NATS JSON format includes NATS-specific fields added at the adapter layer, not part of Proto contract
- **Resolution**: See Section 1.5 "Proto Contract vs NATS JSON Format: Source of Truth" for detailed explanation
- **Impact**: None (expected behavior) - NATS JSON format is a logical payload format that includes Proto fields + NATS-specific fields
- **Action**: 
  1. ✅ **Documented**: Section 1.5 clarifies the two-level contract architecture
  2. ✅ **Clarified**: NATS-specific fields are added at NATS adapter layer, not in Proto
  3. ⏳ **Pending**: Update `PROTO_NATS_MAPPING.md` to explicitly label NATS-specific fields (see Action CP1.2 in `ROUTER_PROTO_NATS_ACTION_PLAN.md`)

#### Issue 4: Required vs Optional Mismatch
- **Status**: ⚠️ **WARNING**
- **Description**: Documentation says fields are required (`tenant_id`, `message_type`, `payload` in `Message`; `provider_id`, `reason` in `RouteDecision`), but Proto defines them as optional
- **Impact**: Runtime validation may enforce required fields, but Proto contract allows optional
- **Action**: 
  1. Document that required fields are enforced at runtime (not at Proto level)
  2. Or: Update Proto to mark fields as required (breaking change)
  3. Update documentation to clarify: "Required at runtime, optional in Proto"

#### Issue 5: Policy DSL Structure Mismatch
- **Status**: ⚠️ **WARNING**
- **Description**: 
  - Doc shows `weights` as map, Proto uses `AdminProvider[]` array
  - Doc shows `fallbacks` as array with `when`/`retry`/`to`, Proto uses `AdminRule[]` with `match`/`prefer`/`fallback`
  - Doc shows `sticky` with `session_key`/`ttl`, Proto only has `sticky: bool`
- **Impact**: Policy DSL structure does not match Proto representation
- **Action**: 
  1. Update documentation to show actual Proto structure
  2. Or: Update Proto to match documentation (breaking change)
  3. Document conversion logic between DSL JSON and Proto

### 5.2 Recommendations

1. **Restore Proto Files**: Create or restore `.proto` files in `proto/beamline/flow/v1/flow.proto` and `proto/beamline/provider/v1/provider.proto`
2. **Add CP2+ Fields**: Add CP2+ optional fields to Proto files (fields 8-12 in `Message`, field 4 in `RouteRequest`)
3. **Clarify JSON vs Proto**: Update `PROTO_NATS_MAPPING.md` to clearly distinguish:
   - Proto contract (what's in `.proto` files)
   - NATS JSON format (what's sent over NATS, may include additional fields)
4. **Document Required Fields**: Update documentation to clarify:
   - Proto level: fields are optional (protobuf allows optional)
   - Runtime level: Router enforces required fields via validation
5. **Policy DSL Documentation**: Update `ROUTING_POLICY.md` to show:
   - DSL JSON format (what users write)
   - Proto representation (what's stored/sent)
   - Conversion logic (how DSL maps to Proto)

### 5.3 Consistency Status

**Overall Status**: ⚠️ **INCONSISTENCIES FOUND** (Non-blocking for CP1)

- ✅ **NATS Subjects**: Consistent between `PROTO_NATS_MAPPING.md` and `NATS_SUBJECTS.md`
- ✅ **JSON Format vs Proto**: Two-level architecture clarified (Section 1.5) - NATS-specific fields are by design, not inconsistency
- ⚠️ **Proto Fields**: CP2+ fields documented but not in generated code (expected for CP1, deferred to CP2+)
- ⚠️ **Required vs Optional**: Documentation says required, Proto says optional (needs documentation clarification)
- ⚠️ **Policy DSL**: Structure mismatch between documentation and Proto (needs documentation clarification)

**CP1 Readiness**: ✅ **READY** - All inconsistencies are either:
- **Resolved by design** (JSON vs Proto two-level architecture - Section 1.5)
- **Documentation improvements** (no code changes required)
- **CP2+ deferred** (expected for CP1 baseline)

**Before declaring 100% readiness for CP2-LC, the following changes must be made**:
1. Restore/add Proto files (CP2.1 - see `ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md`)
2. Add CP2+ fields to Proto (CP2.2, CP2.3 - see `ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md`)
3. Document required field enforcement at runtime (CP1.1 - see `ROUTER_PROTO_NATS_CP1_DETAILED_PLAN.md`)
4. Align Policy DSL documentation with Proto structure (CP1.3 - see `ROUTER_PROTO_NATS_CP1_DETAILED_PLAN.md`)

---

## 6. References

### Related Documents
- **Action Plan**: `docs/archive/dev/ROUTER_PROTO_NATS_ACTION_PLAN.md` - Prioritized action items (CP1 and CP2-LC)
- **CP1 Detailed Plan**: `docs/archive/dev/ROUTER_PROTO_NATS_CP1_DETAILED_PLAN.md` - Step-by-step CP1 implementation instructions
- **CP2+ Detailed Plan**: `docs/archive/dev/ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md` - Step-by-step CP2-LC implementation instructions
- **Summary**: `docs/archive/dev/ROUTER_PROTO_NATS_CONSISTENCY_SUMMARY.md` - Brief version for CP reports

### Source Documents
- **Proto Definitions**: `apps/otp/router/src/flow_pb.erl` (generated), `apps/otp/router/include/flow_pb.hrl` (generated)
- **NATS Subjects**: `docs/NATS_SUBJECTS.md`
- **Proto-NATS Mapping**: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`
- **API Contracts**: `docs/API_CONTRACTS.md`
- **Routing Policy**: `docs/ROUTING_POLICY.md`
- **Policy Schema**: `apps/otp/router/docs/schemas/policy.schema.json`
- **CP1 Boundaries**: `docs/archive/dev/CP1_BOUNDARIES_AND_CONTRACTS.md`
- **CP2+ Fields**: `docs/archive/dev/ORCHESTRATOR_ROUTER_ABI_BRIDGE_RU.md`, `proto/README.md`

