# Router Proto/NATS Consistency - CP2+ Detailed Implementation Plan (Draft)

**Date**: 2025-01-27  
**Based on**: `docs/archive/dev/ROUTER_PROTO_NATS_ACTION_PLAN.md`  
**Status**: Draft - Ready for CP2-LC Implementation  
**Checkpoint**: CP2-LC  
**Version**: 1.0

**Related Documents**:
- **Consistency Report**: `docs/archive/dev/ROUTER_PROTO_NATS_CONSISTENCY.md` - Full analysis
- **Action Plan**: `docs/archive/dev/ROUTER_PROTO_NATS_ACTION_PLAN.md` - Prioritized actions
- **CP1 Detailed Plan**: `docs/archive/dev/ROUTER_PROTO_NATS_CP1_DETAILED_PLAN.md` - CP1 instructions

## Purpose

This document provides detailed, step-by-step instructions for implementing CP2+ actions from the Proto/NATS consistency action plan. Each action includes:
- Exact procedures for restoring Proto files
- Safe addition of CP2+ fields with ABI compatibility checks
- Version gates and breaking change detection
- Migration procedures and backward compatibility verification

**Scope**: Proto file changes, code regeneration, ABI validation

**CRITICAL**: All CP2-LC actions require:
- ✅ CP1 actions completed (documentation improvements)
- ✅ `current_cp >= CP2-LC` in DevState (version gate)
- ✅ Proto files restored (CP2.1 - must be done first)
- ✅ `buf` tool installed and configured
- ✅ Version gates passing (`buf breaking` must pass)

**Note**: CP2.1 (restore Proto files) can be done **before** CP2-LC transition, but CP2.2 and CP2.3 (add CP2+ fields) require CP2-LC checkpoint.

---

## Action CP2.1: Restore Proto Source Files

### Overview

**Priority**: CRITICAL (blocks other CP2+ actions)  
**Effort**: Medium (extract from generated code or recreate)  
**Goal**: Restore Proto source files from generated code or recreate from scratch

### Prerequisites

1. **buf tool installed**:
   ```bash
   # macOS
   brew install bufbuild/buf/buf
   
   # Linux
   curl -sSL "https://github.com/bufbuild/buf/releases/latest/download/buf-$(uname -s)-$(uname -m)" -o "/usr/local/bin/buf"
   chmod +x /usr/local/bin/buf
   
   # Verify
   buf --version  # Should be 1.30.0 or later
   ```

2. **buf.yaml configured** (already exists in `proto/buf.yaml`):
   ```yaml
   version: v2
   breaking:
     use:
       - WIRE
   lint:
     use:
       - STANDARD
   ```

3. **Generated code available**:
   - `apps/otp/router/src/flow_pb.erl`
   - `apps/otp/router/include/flow_pb.hrl`

### Step-by-Step Instructions

#### Step 1: Extract Proto Definitions from Generated Code

**Method**: Reverse-engineer Proto syntax from Erlang field definitions

**Source**: `apps/otp/router/src/flow_pb.erl` - `get_msg_defs()` function

**Procedure**:

1. **Read generated code**:
   ```bash
   cd apps/otp/router
   grep -A 50 "get_msg_defs()" src/flow_pb.erl
   ```

2. **Extract Message definition**:
   ```erlang
   % From flow_pb.erl get_msg_defs()
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

3. **Convert to Proto syntax**:
   ```protobuf
   message Message {
     string message_id = 1;
     string tenant_id = 2;
     string trace_id = 3;
     string message_type = 4;
     bytes payload = 5;
     map<string, string> metadata = 6;
     int64 timestamp_ms = 7;
   }
   ```

#### Step 2: Create Proto File Structure

**Create directory structure**:
```bash
mkdir -p proto/beamline/flow/v1
mkdir -p proto/beamline/provider/v1
```

**Create `proto/beamline/flow/v1/flow.proto`**:

```protobuf
syntax = "proto3";

package beamline.flow.v1;

// Router service for routing decisions
service Router {
  // Decide routing for a message
  rpc Decide(RouteRequest) returns (RouteDecision);
}

// Incoming message for routing
message Message {
  string message_id = 1;        // Message identifier
  string tenant_id = 2;         // Tenant identifier (required at runtime)
  string trace_id = 3;          // Trace identifier for distributed tracing
  string message_type = 4;      // Message type (required at runtime)
  bytes payload = 5;             // Message payload (required at runtime)
  map<string, string> metadata = 6;  // Additional metadata
  int64 timestamp_ms = 7;       // Timestamp in milliseconds
}

// Routing request
message RouteRequest {
  Message message = 1;           // Message to route (required at runtime)
  string policy_id = 2;          // Policy identifier (optional, defaults to "default")
  map<string, string> context = 3; // Routing context (e.g., sticky session key)
}

// Routing decision
message RouteDecision {
  string provider_id = 1;        // Selected provider identifier (required at runtime)
  string reason = 2;             // Decision reason (required at runtime)
  int32 priority = 3;            // Provider priority
  int64 expected_latency_ms = 4; // Expected latency in milliseconds
  double expected_cost = 5;      // Expected cost
  map<string, string> metadata = 6; // Additional metadata
}

// Admin API messages (for policy management)
message AdminPolicy {
  string policy_id = 1;
  repeated AdminProvider providers = 2;
  bool sticky = 3;
  repeated AdminRule rules = 4;
}

message AdminProvider {
  string id = 1;
  double weight = 2;
}

message AdminRule {
  string match = 1;
  repeated string prefer = 2;
  string fallback = 3;
}

message UpsertPolicyRequest {
  string tenant_id = 1;
  AdminPolicy policy = 2;
}

message UpsertPolicyResponse {
  bool success = 1;
  string message = 2;
}
```

**Create `proto/beamline/provider/v1/provider.proto`**:

```protobuf
syntax = "proto3";

package beamline.provider.v1;

// Provider service for invoking providers
service Provider {
  // Invoke a provider
  rpc Invoke(ProviderRequest) returns (ProviderResponse);
  
  // Invoke a provider with streaming response
  rpc InvokeStream(ProviderRequest) returns (stream StreamChunk);
}

// Provider request
message ProviderRequest {
  string provider_id = 1;
  bytes payload = 2;
  map<string, string> context = 3;
}

// Provider response
message ProviderResponse {
  bool success = 1;
  bytes payload = 2;
  map<string, string> metadata = 3;
}

// Streaming chunk
message StreamChunk {
  bytes data = 1;
  bool done = 2;
}
```

#### Step 3: Validate Proto Files

**Run buf lint**:
```bash
cd proto
buf lint
```

**Expected output**: No errors (or only expected warnings)

**Run buf build**:
```bash
buf build
```

**Expected output**: Success (no errors)

**Check for syntax errors**:
```bash
# Verify Proto syntax
buf lint --error-format=json | jq '.'
```

#### Step 4: Regenerate Code and Verify

**Regenerate Erlang code**:
```bash
cd apps/otp/router
rebar3 gpb compile
```

**Compare generated code with existing**:
```bash
# Check if generated code matches existing
diff apps/otp/router/src/flow_pb.erl apps/otp/router/src/flow_pb.erl.bak || echo "Files differ - need to verify"
```

**Verification checklist**:
- [ ] Generated `flow_pb.erl` has same field definitions
- [ ] Generated `flow_pb.hrl` has same record definitions
- [ ] Field numbers match (1-7 for Message, 1-3 for RouteRequest, etc.)
- [ ] Field types match (string, bytes, int64, map, etc.)
- [ ] Service definitions match (`Router.Decide`)

**If differences found**:
1. Compare field by field
2. Check field numbers (must match exactly)
3. Check field types (must match exactly)
4. Adjust Proto file if needed
5. Regenerate and re-verify

#### Step 5: Update Documentation

**Update `proto/README.md`**:
```markdown
## Proto Source Files Status

**Current Status**: ✅ **RESTORED** - Proto source files are now available:
- `proto/beamline/flow/v1/flow.proto` - Router service definitions
- `proto/beamline/provider/v1/provider.proto` - Provider service definitions

**Source of Truth**: Proto source files (`.proto`) are now the authoritative source. Generated code is created from Proto files via `rebar3 gpb compile`.
```

**Update `apps/otp/router/docs/GENERATION.md`**:
```markdown
### Proto Source Files

**Status**: ✅ **RESTORED** - Proto source files are available in `proto/beamline/*/v1/*.proto`.

**Generation**: Run `rebar3 gpb compile` to regenerate code from Proto files.
```

### Verification Steps

1. **Check Proto files exist**:
   ```bash
   test -f proto/beamline/flow/v1/flow.proto && echo "✅ flow.proto exists" || echo "❌ flow.proto missing"
   test -f proto/beamline/provider/v1/provider.proto && echo "✅ provider.proto exists" || echo "❌ provider.proto missing"
   ```

2. **Verify buf validation**:
   ```bash
   cd proto
   buf lint && echo "✅ buf lint passed" || echo "❌ buf lint failed"
   buf build && echo "✅ buf build passed" || echo "❌ buf build failed"
   ```

3. **Verify code generation**:
   ```bash
   cd apps/otp/router
   rebar3 gpb compile && echo "✅ Code generation successful" || echo "❌ Code generation failed"
   ```

4. **Verify generated code matches**:
   ```bash
   # Check field definitions match
   grep -A 10 "get_msg_defs()" apps/otp/router/src/flow_pb.erl | grep -c "message_id.*1.*string" && echo "✅ Message fields match"
   ```

### Acceptance Criteria

- ✅ Proto files exist in `proto/beamline/flow/v1/flow.proto` and `proto/beamline/provider/v1/provider.proto`
- ✅ Proto files pass `buf lint` and `buf build`
- ✅ Generated code matches existing code (no breaking changes)
- ✅ Field numbers, types, and names match exactly
- ✅ Documentation updated to reference Proto files as source of truth
- ✅ All tests pass with regenerated code

### Rollback Plan

If restoration fails or causes issues:
1. Keep generated code as source of truth
2. Revert Proto files (delete or move to backup)
3. Update documentation to reflect status
4. Document issues for future restoration attempt

---

## Action CP2.2: Add CP2+ Optional Fields to Message

### Overview

**Priority**: HIGH (for CP2+ features)  
**Effort**: Medium (Proto changes + regeneration + testing)  
**Goal**: Add CP2+ optional fields to `Message` without breaking ABI

### Prerequisites

1. ✅ **CP2.1 completed** (Proto files restored)
2. ✅ **buf tool installed and configured**
3. ✅ **Version gate**: `current_cp >= CP2-LC` in DevState
4. ✅ **Git branch**: Create feature branch `cp2-add-message-fields`

### Step-by-Step Instructions

#### Step 1: Create Feature Branch and Backup

**Create feature branch**:
```bash
git checkout -b cp2-add-message-fields
```

**Backup current Proto file**:
```bash
cp proto/beamline/flow/v1/flow.proto proto/beamline/flow/v1/flow.proto.cp1-backup
```

**Verify current state**:
```bash
cd proto
buf lint  # Should pass
buf build  # Should pass
```

#### Step 2: Add CP2+ Fields to Message

**Edit `proto/beamline/flow/v1/flow.proto`**:

**Before** (CP1):
```protobuf
message Message {
  string message_id = 1;
  string tenant_id = 2;
  string trace_id = 3;
  string message_type = 4;
  bytes payload = 5;
  map<string, string> metadata = 6;
  int64 timestamp_ms = 7;
}
```

**After** (CP2+):
```protobuf
message Message {
  // CP1 Fields (existing)
  string message_id = 1;
  string tenant_id = 2;
  string trace_id = 3;
  string message_type = 4;
  bytes payload = 5;
  map<string, string> metadata = 6;
  int64 timestamp_ms = 7;
  
  // CP2+ Optional Fields (backward compatible)
  string run_id = 8;              // Run identifier for multi-step workflows
  string flow_id = 9;             // Flow definition identifier
  string step_id = 10;            // Step identifier within a flow
  string idempotency_key = 11;    // Message-level idempotency key
  string span_id = 12;            // Span identifier for distributed tracing
}
```

**Key Points**:
- ✅ All new fields are **optional** (protobuf v3 default)
- ✅ Field numbers **8-12** (continue from 7)
- ✅ All fields are **string** type (consistent with existing)
- ✅ Field numbers **must not conflict** with existing fields

#### Step 3: Validate Proto Syntax

**Run buf lint**:
```bash
cd proto
buf lint
```

**Expected output**: ✅ PASS (no errors)

**If errors occur**:
- Check syntax (missing semicolons, wrong types, etc.)
- Verify field numbers are unique
- Check package and service definitions

**Run buf build**:
```bash
buf build
```

**Expected output**: ✅ PASS (builds successfully)

#### Step 4: Check Breaking Changes (CRITICAL)

**Run buf breaking against main branch**:
```bash
cd proto
buf breaking --against '.git#branch=main'
```

**Expected output**: ✅ PASS (no breaking changes detected)

**Why this should pass**:
- All new fields are **optional** (protobuf v3 default)
- Optional fields are **backward compatible** (old clients ignore them)
- Field numbers are **new** (8-12, not reusing 1-7)
- No existing fields **removed or changed**

**If buf breaking fails**:
```
❌ BREAKING CHANGE DETECTED
```

**Possible causes**:
1. Field number conflict (reusing existing field number)
2. Field type change (changing existing field type)
3. Field name change (renaming existing field)
4. Field removal (removing existing field)

**Fix**:
- Check error message from `buf breaking`
- Verify field numbers are unique (8-12)
- Verify no existing fields modified
- Re-run `buf breaking` after fixes

#### Step 5: Regenerate Code

**Regenerate Erlang code**:
```bash
cd apps/otp/router
rebar3 gpb compile
```

**Verify generated code**:
```bash
# Check new fields in generated code
grep -A 20 "get_msg_defs()" src/flow_pb.erl | grep -E "run_id|flow_id|step_id|idempotency_key|span_id"
```

**Expected output**:
```erlang
{run_id, 8, string, optional},
{flow_id, 9, string, optional},
{step_id, 10, string, optional},
{idempotency_key, 11, string, optional},
{span_id, 12, string, optional}
```

**Verify header file**:
```bash
# Check record definition
grep -A 15 "'Message'" include/flow_pb.hrl
```

**Expected output**:
```erlang
-record('Message', {
    message_id :: string() | undefined,
    tenant_id :: string() | undefined,
    trace_id :: string() | undefined,
    message_type :: string() | undefined,
    payload :: binary() | undefined,
    metadata = [] :: [{string(), string()}],
    timestamp_ms :: integer() | undefined,
    run_id :: string() | undefined,        % CP2+
    flow_id :: string() | undefined,       % CP2+
    step_id :: string() | undefined,       % CP2+
    idempotency_key :: string() | undefined, % CP2+
    span_id :: string() | undefined        % CP2+
}).
```

#### Step 6: Update Router Code (if needed)

**Check if Router handles optional fields gracefully**:
```bash
# Check Router code for Message handling
grep -r "Message" apps/otp/router/src/*.erl | grep -v "flow_pb"
```

**Router should**:
- ✅ Handle optional fields gracefully (ignore if not present)
- ✅ Not require CP2+ fields for CP1 compatibility
- ✅ Use CP2+ fields if present (for CP2+ features)

**Example Router code** (pseudo-code):
```erlang
handle_message(Message) ->
    % CP1 fields (required at runtime)
    TenantId = Message#message.tenant_id,
    MessageType = Message#message.message_type,
    Payload = Message#message.payload,
    
    % CP2+ fields (optional, use if present)
    RunId = case Message#message.run_id of
        undefined -> undefined;
        R -> R
    end,
    FlowId = case Message#message.flow_id of
        undefined -> undefined;
        F -> F
    end,
    % ... handle CP2+ fields if present
    route_message(Message).
```

**If Router needs updates**:
1. Add handling for CP2+ fields (optional)
2. Add tests for CP2+ fields
3. Ensure backward compatibility (CP1 clients work without CP2+ fields)

#### Step 7: Add Tests

**Create test for CP2+ fields**:
```erlang
% In test/router_core_SUITE.erl or new test file
cp2_message_with_fields_test() ->
    Message = #message{
        message_id = "msg_123",
        tenant_id = "tenant_abc",
        message_type = "chat",
        payload = <<"Hello">>,
        run_id = "run_xyz",           % CP2+ field
        flow_id = "flow_123",         % CP2+ field
        step_id = "step_1",           % CP2+ field
        idempotency_key = "idem_key", % CP2+ field
        span_id = "span_abc"          % CP2+ field
    },
    % Test that Router handles CP2+ fields
    {ok, Decision} = router_core:decide(Message, "default", #{}),
    ?assertEqual("openai", Decision#route_decision.provider_id).
```

**Test backward compatibility**:
```erlang
cp1_message_without_cp2_fields_test() ->
    % CP1 message without CP2+ fields (should still work)
    Message = #message{
        message_id = "msg_123",
        tenant_id = "tenant_abc",
        message_type = "chat",
        payload = <<"Hello">>
        % No CP2+ fields
    },
    % Test that Router handles CP1 message
    {ok, Decision} = router_core:decide(Message, "default", #{}),
    ?assertEqual("openai", Decision#route_decision.provider_id).
```

#### Step 8: Update Documentation

**Update `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`**:

**Add CP2+ fields to Message definition**:
```markdown
### Proto Contract (Wire Protocol / ABI)

**Message** (with CP2+ fields):
```protobuf
message Message {
  // CP1 Fields
  string message_id = 1;
  string tenant_id = 2;
  string trace_id = 3;
  string message_type = 4;
  bytes payload = 5;
  map<string, string> metadata = 6;
  int64 timestamp_ms = 7;
  
  // CP2+ Optional Fields (backward compatible)
  string run_id = 8;              // Run identifier for multi-step workflows
  string flow_id = 9;             // Flow definition identifier
  string step_id = 10;            // Step identifier within a flow
  string idempotency_key = 11;    // Message-level idempotency key
  string span_id = 12;            // Span identifier for distributed tracing
}
```
```

**Update `proto/README.md`**:
```markdown
### Router Service (`beamline.flow.v1`)

**Service**: `Router.Decide`

**Messages**:
- `Message`: Incoming message for routing
  - **CP1 Fields** (1-7): `message_id`, `tenant_id`, `trace_id`, `message_type`, `payload`, `metadata`, `timestamp_ms`
  - **CP2+ Optional Fields** (8-12): `run_id`, `flow_id`, `step_id`, `idempotency_key`, `span_id`
  - **Backward Compatible**: CP1 clients can omit CP2+ fields
```

#### Step 9: Version Gate Check

**Verify DevState allows CP2+ changes**:
```bash
# Check current CP
grep "current_cp" .trae/state.json

# Should be: "current_cp": "CP2-LC" or higher
```

**If CP1**:
- ❌ **BLOCK**: Cannot proceed with CP2+ changes
- Update DevState to CP2-LC first
- Or create feature branch for CP2+ preparation

#### Step 10: Commit and Verify

**Commit changes**:
```bash
git add proto/beamline/flow/v1/flow.proto
git add apps/otp/router/src/flow_pb.erl
git add apps/otp/router/include/flow_pb.hrl
git commit -m "feat(proto): Add CP2+ optional fields to Message (fields 8-12)

- Add run_id, flow_id, step_id, idempotency_key, span_id
- All fields are optional (backward compatible)
- Verified with buf breaking (no breaking changes)
- Regenerated Erlang code
- Updated documentation"
```

**Run CI checks**:
```bash
# Run ABI checks (should pass)
cd proto
buf lint
buf breaking --against '.git#branch=main'

# Run Router tests (should pass)
cd apps/otp/router
rebar3 ct
```

### Verification Steps

1. **Check Proto file updated**:
   ```bash
   grep -A 5 "CP2+ Optional Fields" proto/beamline/flow/v1/flow.proto
   ```

2. **Verify buf breaking passes**:
   ```bash
   cd proto
   buf breaking --against '.git#branch=main' && echo "✅ No breaking changes" || echo "❌ Breaking changes detected"
   ```

3. **Verify generated code**:
   ```bash
   grep -E "run_id|flow_id|step_id|idempotency_key|span_id" apps/otp/router/src/flow_pb.erl
   ```

4. **Verify tests pass**:
   ```bash
   cd apps/otp/router
   rebar3 ct
   ```

### Acceptance Criteria

- ✅ CP2+ fields added to Proto `Message` (fields 8-12)
- ✅ `buf breaking` passes (no breaking changes detected)
- ✅ Generated code includes CP2+ fields
- ✅ Router handles CP2+ fields gracefully (ignores if not present)
- ✅ Backward compatibility verified (CP1 messages work)
- ✅ Tests added for CP2+ fields
- ✅ Documentation updated
- ✅ Version gate passed (`current_cp >= CP2-LC`)

### Rollback Plan

If CP2+ fields cause issues:
1. Revert Proto file: `git checkout proto/beamline/flow/v1/flow.proto.cp1-backup`
2. Regenerate code: `rebar3 gpb compile`
3. Verify tests pass
4. Document issues for future attempt

---

## Action CP2.3: Add CP2+ Optional Field to RouteRequest

### Overview

**Priority**: HIGH (for CP2+ features)  
**Effort**: Medium (Proto changes + regeneration + testing)  
**Goal**: Add CP2+ optional field `idempotency_key` to `RouteRequest` without breaking ABI

### Prerequisites

1. ✅ **CP2.1 completed** (Proto files restored)
2. ✅ **CP2.2 completed** (Message CP2+ fields added)
3. ✅ **buf tool installed and configured**
4. ✅ **Version gate**: `current_cp >= CP2-LC` in DevState

### Step-by-Step Instructions

#### Step 1: Create Feature Branch

**Create feature branch**:
```bash
git checkout -b cp2-add-route-request-field
```

**Backup current Proto file**:
```bash
cp proto/beamline/flow/v1/flow.proto proto/beamline/flow/v1/flow.proto.cp2.2-backup
```

#### Step 2: Add CP2+ Field to RouteRequest

**Edit `proto/beamline/flow/v1/flow.proto`**:

**Before** (CP1):
```protobuf
message RouteRequest {
  Message message = 1;
  string policy_id = 2;
  map<string, string> context = 3;
}
```

**After** (CP2+):
```protobuf
message RouteRequest {
  // CP1 Fields (existing)
  Message message = 1;
  string policy_id = 2;
  map<string, string> context = 3;
  
  // CP2+ Optional Field (backward compatible)
  string idempotency_key = 4;  // Request-level idempotency key (overrides message-level if both present)
}
```

**Key Points**:
- ✅ Field is **optional** (protobuf v3 default)
- ✅ Field number **4** (continue from 3)
- ✅ Field type is **string** (consistent with Message.idempotency_key)
- ✅ Field number **must not conflict** with existing fields

#### Step 3: Validate and Check Breaking Changes

**Run buf lint**:
```bash
cd proto
buf lint
```

**Run buf breaking**:
```bash
buf breaking --against '.git#branch=main'
```

**Expected output**: ✅ PASS (no breaking changes)

**Why this should pass**:
- Field is **optional** (backward compatible)
- Field number is **new** (4, not reusing 1-3)
- No existing fields **removed or changed**

#### Step 4: Regenerate Code

**Regenerate Erlang code**:
```bash
cd apps/otp/router
rebar3 gpb compile
```

**Verify generated code**:
```bash
grep -A 10 "'RouteRequest'" include/flow_pb.hrl
```

**Expected output**:
```erlang
-record('RouteRequest', {
    message :: #message{} | undefined,
    policy_id :: string() | undefined,
    context = [] :: [{string(), string()}],
    idempotency_key :: string() | undefined  % CP2+
}).
```

#### Step 5: Update Router Code (if needed)

**Check Router handling**:
- Router should handle optional `idempotency_key` gracefully
- If both `message.idempotency_key` and `RouteRequest.idempotency_key` present, use `RouteRequest.idempotency_key` (overrides message-level)

#### Step 6: Add Tests

**Test CP2+ field**:
```erlang
cp2_route_request_with_idempotency_key_test() ->
    Message = #message{...},
    Request = #route_request{
        message = Message,
        policy_id = "default",
        idempotency_key = "request_idem_key"  % CP2+ field
    },
    {ok, Decision} = router_core:decide_request(Request),
    ?assertEqual("openai", Decision#route_decision.provider_id).
```

#### Step 7: Update Documentation

**Update `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`**:
- Add `idempotency_key` to RouteRequest definition
- Document override behavior (request-level overrides message-level)

#### Step 8: Commit and Verify

**Commit changes**:
```bash
git add proto/beamline/flow/v1/flow.proto
git add apps/otp/router/src/flow_pb.erl
git add apps/otp/router/include/flow_pb.hrl
git commit -m "feat(proto): Add CP2+ optional field idempotency_key to RouteRequest

- Add idempotency_key field (field 4)
- Field is optional (backward compatible)
- Verified with buf breaking (no breaking changes)
- Regenerated Erlang code
- Updated documentation"
```

### Verification Steps

1. **Check Proto file updated**:
   ```bash
   grep -A 2 "idempotency_key = 4" proto/beamline/flow/v1/flow.proto
   ```

2. **Verify buf breaking passes**:
   ```bash
   cd proto
   buf breaking --against '.git#branch=main'
   ```

3. **Verify generated code**:
   ```bash
   grep "idempotency_key" apps/otp/router/include/flow_pb.hrl
   ```

### Acceptance Criteria

- ✅ CP2+ field added to Proto `RouteRequest` (field 4)
- ✅ `buf breaking` passes (no breaking changes)
- ✅ Generated code includes CP2+ field
- ✅ Router handles CP2+ field gracefully
- ✅ Backward compatibility verified
- ✅ Tests added
- ✅ Documentation updated

---

## Version Gates and Breaking Change Detection

### Version Gate Requirements

**Before making CP2+ Proto changes**:

1. **Check DevState**:
   ```bash
   grep "current_cp" .trae/state.json
   # Must be: "current_cp": "CP2-LC" or higher
   ```

2. **If CP1**:
   - ❌ **BLOCK**: Cannot proceed
   - Update DevState to CP2-LC first
   - Or create feature branch for CP2+ preparation

### Breaking Change Detection

**buf breaking configuration** (`proto/buf.yaml`):
```yaml
version: v2
breaking:
  use:
    - WIRE  # Wire format compatibility (most strict)
```

**WIRE compatibility rules**:
- ✅ **Allowed**: Adding optional fields (backward compatible)
- ❌ **Blocked**: Removing fields
- ❌ **Blocked**: Changing field types
- ❌ **Blocked**: Changing field numbers
- ❌ **Blocked**: Changing field names
- ❌ **Blocked**: Changing service/message names

### CI/CD Integration

**GitHub Actions** (`.github/workflows/validate.yml`):
```yaml
abi:
  name: ABI (Protobuf) Checks
  steps:
    - name: buf lint
      run: buf lint
    - name: buf breaking
      run: buf breaking --against origin/main
```

**Pre-commit hook** (optional):
```bash
#!/bin/bash
# .git/hooks/pre-commit
cd proto
buf lint || exit 1
buf breaking --against '.git#branch=main' || exit 1
```

### Breaking Change Workflow

**If breaking changes needed** (future CP3+):
1. Create new package version: `beamline.flow.v2`
2. Add new message types in v2 package
3. Keep v1 package for backward compatibility
4. Migrate clients gradually
5. Deprecate v1 after migration complete

---

## Summary: CP2+ Actions Checklist

### Action CP2.1: Restore Proto Files
- [ ] Install buf tool
- [ ] Extract Proto definitions from generated code
- [ ] Create `proto/beamline/flow/v1/flow.proto`
- [ ] Create `proto/beamline/provider/v1/provider.proto`
- [ ] Run `buf lint` (should pass)
- [ ] Run `buf build` (should pass)
- [ ] Regenerate code with `rebar3 gpb compile`
- [ ] Verify generated code matches existing
- [ ] Update documentation

### Action CP2.2: Add CP2+ Fields to Message
- [ ] Create feature branch
- [ ] Backup Proto file
- [ ] Add CP2+ fields (8-12) to Message
- [ ] Run `buf lint` (should pass)
- [ ] Run `buf breaking --against main` (should pass)
- [ ] Regenerate code
- [ ] Update Router code (if needed)
- [ ] Add tests
- [ ] Update documentation
- [ ] Verify version gate (`current_cp >= CP2-LC`)
- [ ] Commit and verify CI

### Action CP2.3: Add CP2+ Field to RouteRequest
- [ ] Create feature branch
- [ ] Backup Proto file
- [ ] Add `idempotency_key` field (4) to RouteRequest
- [ ] Run `buf lint` (should pass)
- [ ] Run `buf breaking --against main` (should pass)
- [ ] Regenerate code
- [ ] Update Router code (if needed)
- [ ] Add tests
- [ ] Update documentation
- [ ] Commit and verify CI

---

## Execution Order

**Sequential dependencies**:
1. **CP2.1** (must be first - restores Proto files)
2. **CP2.2** (depends on CP2.1)
3. **CP2.3** (depends on CP2.1, can be parallel with CP2.2)

**Estimated Time**: 3-5 days for all CP2+ actions

---

## References

### Related Documents
- **Consistency Report**: `docs/archive/dev/ROUTER_PROTO_NATS_CONSISTENCY.md` - Full analysis and findings
- **Action Plan**: `docs/archive/dev/ROUTER_PROTO_NATS_ACTION_PLAN.md` - Prioritized actions overview
- **CP1 Detailed Plan**: `docs/archive/dev/ROUTER_PROTO_NATS_CP1_DETAILED_PLAN.md` - CP1 implementation instructions
- **Summary**: `docs/archive/dev/ROUTER_PROTO_NATS_CONSISTENCY_SUMMARY.md` - Brief version for CP reports

### Configuration Files
- **buf.yaml**: `proto/buf.yaml` - Buf configuration for linting and breaking changes
- **buf.gen.yaml**: `proto/buf.gen.yaml` - Code generation configuration

### External Documentation
- **Buf Documentation**: https://docs.buf.build
- **Protobuf Guide**: https://developers.google.com/protocol-buffers
- **Protobuf v3 Language Guide**: https://developers.google.com/protocol-buffers/docs/proto3

