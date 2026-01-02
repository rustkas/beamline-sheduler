---
version: 1.0
authors:
  - WORKER wrk-2: Architecture/Tech Lead
last_update: 2025-01-27T12:00:00Z
status: draft
rule_version: v10
message_protocol: v1
---

# Orchestrator/Router ABI Bridge Design

## Purpose

This document describes a minimal ABI bridge from the current Router ABI (`beamline.flow.v1`) to the future Orchestrator/Worker ABI (`beamline.orchestrator.v1`) from BeamLine Vision, without breaking CP1 contracts.

This ABI bridge specification explains how the contract between the Orchestrator and the Router evolves across BeamLine checkpoints.  
For the overall product vision, component map and the CP0–CP8 roadmap, see [docs/BEAMLINE_VISION_AND_ARCHITECTURE.md](cci:7://file:///home/rustkas/aigroup/docs/BEAMLINE_VISION_AND_ARCHITECTURE.md:0:0-0:0).

Эта спецификация ABI bridge описывает, как контракт между Orchestrator и Router эволюционирует по мере прохождения чекпоинтов BeamLine.  
За общим видением продукта, картой компонентов и дорожной картой CP0–CP8 обращайтесь к [docs/BEAMLINE_VISION_AND_ARCHITECTURE.md](cci:7://file:///home/rustkas/aigroup/docs/BEAMLINE_VISION_AND_ARCHITECTURE.md:0:0-0:0).

## Current State: Router ABI (beamline.flow.v1)

### Current Message Structure

**`Message`** (proto/beamline/flow/v1/flow.proto):
```protobuf
message Message {
  string message_id = 1;        // ✅ Present
  string tenant_id = 2;         // ✅ Present
  string trace_id = 3;          // ✅ Present (optional)
  string message_type = 4;      // ✅ Present
  bytes payload = 5;            // ✅ Present
  map<string, string> metadata = 6;  // ✅ Present (optional)
  int64 timestamp_ms = 7;       // ✅ Present (optional)
}
```

**`RouteRequest`**:
```protobuf
message RouteRequest {
  Message message = 1;           // ✅ Present
  string policy_id = 2;         // ✅ Present (optional)
  map<string, string> context = 3;  // ✅ Present (optional)
}
```

### What's Already Compatible

- ✅ **`trace_id`** — Already present in `Message` (field 3, optional)
- ✅ **`tenant_id`** — Already present in `Message` (field 2, required)
- ✅ **`message_id`** — Already present (can map to `step_id` in future)
- ✅ **`metadata`** — Can carry `run_id`, `flow_id`, `step_id` as key-value pairs

## Target State: Orchestrator/Worker ABI (beamline.orchestrator.v1)

### Required Fields (from BeamLine Vision)

According to `docs/BEAMLINE_VISION_AND_ARCHITECTURE.md` section 4.2, every message should include:

- ✅ **`run_id`** — Unique identifier of the run
- ✅ **`flow_id`** — Identifier of the process/flow definition
- ✅ **`step_id`** — Identifier of the particular step
- ✅ **`idempotency_key`** — Key for safe retries
- ✅ **`trace_id`** — For distributed tracing (already present)
- ⚠️ **`span_id`** — Optional span identifier for tracing

### Future Services

**OrchestratorService** (Erlang/OTP side):
- `StartRun` — Start and control runs
- `QueryStatus` — Query run/step statuses
- `StreamEvents` — Stream run-level and step-level events

**WorkerService** (CAF/C++ and other executors):
- `ExecuteStep` — Execute a step via unary or bidirectional streaming RPC
- `Heartbeat` — Send heartbeat and health information
- `AdvertiseCapabilities` — Advertise what step types the worker can handle

## Bridge Strategy: Backward-Compatible Evolution

### Phase 1: Add Optional Fields to Current ABI (CP1 → CP2)

**Goal**: Add Orchestrator fields as **optional** fields in `beamline.flow.v1` without breaking CP1.

**Changes to `Message`**:
```protobuf
message Message {
  // ... existing fields (1-7) ...
  
  // NEW: Optional Orchestrator fields (CP2+)
  string run_id = 8;              // Optional: Run identifier
  string flow_id = 9;             // Optional: Flow definition identifier
  string step_id = 10;            // Optional: Step identifier (can map from message_id)
  string idempotency_key = 11;    // Optional: Idempotency key for safe retries
  string span_id = 12;            // Optional: Span identifier for tracing
}
```

**Changes to `RouteRequest`**:
```protobuf
message RouteRequest {
  Message message = 1;
  string policy_id = 2;
  map<string, string> context = 3;
  
  // NEW: Optional idempotency key at request level (CP2+)
  string idempotency_key = 4;     // Optional: Request-level idempotency key
}
```

**Rationale**:
- ✅ **Backward compatible**: All new fields are optional
- ✅ **CP1 safe**: Existing code ignores unknown fields (protobuf behavior)
- ✅ **Forward compatible**: Future Orchestrator can populate these fields
- ✅ **Gradual migration**: Can start using fields in CP2, full adoption in CP3

### Phase 2: Create New Orchestrator Package (CP3+)

**Goal**: Create `beamline.orchestrator.v1` package with full Orchestrator/Worker services.

**New Package**: `proto/beamline/orchestrator/v1/orchestrator.proto`

**Services**:
```protobuf
package beamline.orchestrator.v1;

// Orchestrator service (Erlang/OTP side)
service Orchestrator {
  rpc StartRun(StartRunRequest) returns (StartRunResponse);
  rpc QueryStatus(QueryStatusRequest) returns (QueryStatusResponse);
  rpc StreamEvents(StreamEventsRequest) returns (stream ExecutionEvent);
}

// Worker service (CAF/C++ and other executors)
service Worker {
  rpc ExecuteStep(ExecuteStepRequest) returns (ExecuteStepResponse);
  rpc ExecuteStepStream(ExecuteStepRequest) returns (stream StepChunk);
  rpc Heartbeat(HeartbeatRequest) returns (HeartbeatResponse);
  rpc AdvertiseCapabilities(AdvertiseCapabilitiesRequest) returns (AdvertiseCapabilitiesResponse);
}
```

**Messages** (all include required fields):
```protobuf
message StartRunRequest {
  string run_id = 1;              // Required
  string flow_id = 2;             // Required
  string tenant_id = 3;           // Required
  string idempotency_key = 4;     // Required
  string trace_id = 5;            // Optional
  map<string, string> metadata = 6;  // Optional
  // ... flow definition, initial context, etc.
}

message ExecuteStepRequest {
  string run_id = 1;              // Required
  string flow_id = 2;             // Required
  string step_id = 3;             // Required
  string idempotency_key = 4;     // Required
  string trace_id = 5;            // Optional
  string span_id = 6;             // Optional
  string blob_uri = 7;            // Optional: Reference to large artifacts
  map<string, string> metadata = 8;  // Optional
  // ... step-specific parameters
}
```

**Migration Path**:
- Router can **optionally** populate `run_id`, `flow_id`, `step_id` in `Message` (Phase 1)
- Gateway can **optionally** pass these fields through
- When Orchestrator is ready, it uses `beamline.orchestrator.v1` directly
- Router can **bridge** between `beamline.flow.v1` and `beamline.orchestrator.v1` if needed

## Implementation Plan

### Step 1: Add Optional Fields to beamline.flow.v1 (CP2)

**File**: `proto/beamline/flow/v1/flow.proto`

**Changes**:
1. Add optional fields to `Message`:
   - `run_id` (field 8)
   - `flow_id` (field 9)
   - `step_id` (field 10)
   - `idempotency_key` (field 11)
   - `span_id` (field 12)

2. Add optional field to `RouteRequest`:
   - `idempotency_key` (field 4)

**Validation**:
- ✅ Run `buf lint` — should pass
- ✅ Run `buf breaking --against '.git#branch=main'` — should pass (no breaking changes)
- ✅ Verify CP1 tests still pass (fields are optional, ignored by existing code)

**Documentation**:
- Update `proto/README.md` with new optional fields
- Update `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` with field descriptions
- Mark fields as "CP2+ optional" in comments

### Step 2: Update Router to Support Optional Fields (CP2)

**File**: `apps/otp/router/src/router_nats_subscriber.erl`

**Changes**:
1. Extract optional fields from `Message`:
   ```erlang
   RunId = maps:get(run_id, Message, undefined),
   FlowId = maps:get(flow_id, Message, undefined),
   StepId = maps:get(step_id, Message, undefined),
   IdempotencyKey = maps:get(idempotency_key, Message, undefined),
   ```

2. Use fields for:
   - **Idempotency**: Use `idempotency_key` if present, fallback to `message_id`
   - **Tracing**: Use `run_id`, `flow_id`, `step_id` in trace spans
   - **Logging**: Include fields in structured logs

**Backward Compatibility**:
- ✅ If fields are missing, Router uses existing behavior (message_id for idempotency)
- ✅ No breaking changes to existing Router logic

### Step 3: Update Gateway to Pass Optional Fields (CP2)

**File**: `apps/gateway/src/routes/routes.service.ts`

**Changes**:
1. Extract fields from request (if present):
   ```typescript
   const runId = request.metadata?.run_id;
   const flowId = request.metadata?.flow_id;
   const stepId = request.metadata?.step_id;
   const idempotencyKey = request.idempotency_key || request.metadata?.idempotency_key;
   ```

2. Populate `Message` fields:
   ```typescript
   const message: Message = {
     message_id: request.message.message_id,
     tenant_id: tenantId,
     trace_id: traceId,
     message_type: request.message.message_type,
     payload: request.message.payload,
     metadata: request.message.metadata,
     timestamp_ms: request.message.timestamp_ms,
     // NEW: Optional Orchestrator fields
     run_id: runId,
     flow_id: flowId,
     step_id: stepId,
     idempotency_key: idempotencyKey,
     span_id: spanId,
   };
   ```

**Backward Compatibility**:
- ✅ If fields are missing, Gateway omits them (protobuf optional fields)
- ✅ Existing clients continue to work without changes

### Step 4: Create Orchestrator Package (CP3+)

**File**: `proto/beamline/orchestrator/v1/orchestrator.proto` (NEW)

**Structure**:
- Define `OrchestratorService` with `StartRun`, `QueryStatus`, `StreamEvents`
- Define `WorkerService` with `ExecuteStep`, `Heartbeat`, `AdvertiseCapabilities`
- All messages include required fields: `run_id`, `flow_id`, `step_id`, `idempotency_key`, `trace_id`

**Migration**:
- Router can **optionally** bridge `beamline.flow.v1` → `beamline.orchestrator.v1`
- Gateway can **optionally** use `beamline.orchestrator.v1` directly
- Both packages coexist (no breaking changes)

## Field Mapping Strategy

### Current → Future Mapping

| Current (beamline.flow.v1) | Future (beamline.orchestrator.v1) | Mapping Strategy |
|----------------------------|-----------------------------------|------------------|
| `message_id` | `step_id` | Direct mapping (message_id = step_id for single-step runs) |
| `tenant_id` | `tenant_id` | Direct mapping (already present) |
| `trace_id` | `trace_id` | Direct mapping (already present) |
| `metadata["run_id"]` | `run_id` | Extract from metadata → optional field |
| `metadata["flow_id"]` | `flow_id` | Extract from metadata → optional field |
| `metadata["idempotency_key"]` | `idempotency_key` | Extract from metadata → optional field |
| N/A | `span_id` | New optional field |

### Idempotency Key Strategy

**Current (CP1)**:
- Router uses `message_id` for idempotency (if idempotency enabled)
- Gateway uses `tenant_id:request_id` for idempotency

**Future (CP2+)**:
- Router uses `idempotency_key` if present, fallback to `message_id`
- Gateway uses `idempotency_key` if present, fallback to `tenant_id:request_id`
- Both components support both strategies (backward compatible)

## Breaking Change Analysis

### Safe Changes (No Breaking)

✅ **Adding optional fields** to existing messages:
- `Message.run_id` (field 8, optional)
- `Message.flow_id` (field 9, optional)
- `Message.step_id` (field 10, optional)
- `Message.idempotency_key` (field 11, optional)
- `Message.span_id` (field 12, optional)
- `RouteRequest.idempotency_key` (field 4, optional)

**Reason**: Protobuf allows adding optional fields without breaking existing code.

### Breaking Changes (Require New Version)

❌ **Removing fields**:
- Cannot remove `message_id`, `tenant_id`, `message_type`, `payload` (required fields)

❌ **Changing field types**:
- Cannot change `payload` from `bytes` to `string`
- Cannot change `timestamp_ms` from `int64` to `string`

❌ **Making optional fields required**:
- Cannot make `run_id`, `flow_id`, `step_id` required in `beamline.flow.v1`
- Must create new package `beamline.orchestrator.v1` with required fields

### Version Strategy

**beamline.flow.v1** (Current):
- **v1**: Current version (CP1 baseline)
- **v1.1**: Add optional Orchestrator fields (CP2, backward compatible)
- **v1.2+**: Minor additions (backward compatible)

**beamline.orchestrator.v1** (Future):
- **v1**: New package with required Orchestrator fields (CP3+)
- **v1.1+**: Minor additions (backward compatible)

**Coexistence**:
- Both packages can coexist
- Router can support both (bridge if needed)
- Gateway can support both (version negotiation)

## Migration Examples

### Example 1: CP1 Client → CP2 Router (Backward Compatible)

**CP1 Client** (no changes needed):
```json
{
  "message": {
    "message_id": "msg_123",
    "tenant_id": "tenant_abc",
    "trace_id": "trace_xyz",
    "message_type": "chat",
    "payload": "Hello"
  }
}
```

**CP2 Router** (handles both):
- Receives CP1 request (no `run_id`, `flow_id`, `step_id`)
- Uses `message_id` for idempotency (existing behavior)
- Works correctly (backward compatible)

### Example 2: CP2 Client → CP2 Router (New Fields)

**CP2 Client** (populates new fields):
```json
{
  "message": {
    "message_id": "msg_123",
    "tenant_id": "tenant_abc",
    "trace_id": "trace_xyz",
    "message_type": "chat",
    "payload": "Hello",
    "run_id": "run_456",
    "flow_id": "flow_789",
    "step_id": "step_012",
    "idempotency_key": "idem_key_345"
  }
}
```

**CP2 Router**:
- Receives CP2 request (with `run_id`, `flow_id`, `step_id`, `idempotency_key`)
- Uses `idempotency_key` for idempotency (new behavior)
- Includes `run_id`, `flow_id`, `step_id` in trace spans (new behavior)
- Works correctly (forward compatible)

### Example 3: CP3 Orchestrator → Router Bridge (Future)

**CP3 Orchestrator** (uses `beamline.orchestrator.v1`):
```protobuf
ExecuteStepRequest {
  run_id: "run_456"           // Required
  flow_id: "flow_789"         // Required
  step_id: "step_012"         // Required
  idempotency_key: "idem_key_345"  // Required
  trace_id: "trace_xyz"       // Optional
}
```

**Router Bridge** (converts to `beamline.flow.v1`):
- Converts `ExecuteStepRequest` → `RouteRequest`
- Maps `run_id`, `flow_id`, `step_id`, `idempotency_key` to `Message` optional fields
- Router processes as CP2 request
- Works correctly (bridge compatible)

## Acceptance Criteria

### Phase 1 (CP2): Optional Fields

- ✅ Optional fields added to `beamline.flow.v1.Message` and `RouteRequest`
- ✅ `buf lint` passes
- ✅ `buf breaking` passes (no breaking changes)
- ✅ CP1 tests pass (backward compatible)
- ✅ Router handles both CP1 and CP2 requests
- ✅ Gateway can populate optional fields
- ✅ Documentation updated

### Phase 2 (CP3+): Orchestrator Package

- ✅ `beamline.orchestrator.v1` package created
- ✅ `OrchestratorService` and `WorkerService` defined
- ✅ All messages include required fields (`run_id`, `flow_id`, `step_id`, `idempotency_key`, `trace_id`)
- ✅ Router can bridge `beamline.flow.v1` ↔ `beamline.orchestrator.v1`
- ✅ Gateway can use both packages
- ✅ Migration guide created

## Risks and Mitigations

### Risk 1: Field Number Conflicts

**Risk**: Adding fields to `Message` might conflict with future extensions.

**Mitigation**:
- Use field numbers 8-12 (safe range, no conflicts expected)
- Document field number allocation strategy
- Use `buf` to detect conflicts

### Risk 2: Client Compatibility

**Risk**: Old clients might not understand new optional fields.

**Mitigation**:
- All new fields are optional (protobuf handles unknown fields gracefully)
- Existing clients ignore unknown fields
- Test with CP1 clients to verify compatibility

### Risk 3: Migration Complexity

**Risk**: Migrating from `beamline.flow.v1` to `beamline.orchestrator.v1` might be complex.

**Mitigation**:
- Provide clear migration guide
- Support both packages during transition
- Router can bridge between packages

## References

- `docs/BEAMLINE_VISION_AND_ARCHITECTURE.md` - BeamLine Vision and ABI requirements (section 4)
- `proto/beamline/flow/v1/flow.proto` - Current Router ABI
- `.trae/manifest.json` - Schema versioning policy
- `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` - NATS subject mapping
- `docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY_UPDATED.md` - CP2 readiness assessment

