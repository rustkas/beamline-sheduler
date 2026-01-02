# Router Proto/NATS Consistency - Action Plan

**Date**: 2025-01-27  
**Based on**: `docs/archive/dev/ROUTER_PROTO_NATS_CONSISTENCY.md`  
**Status**: Ready for Implementation  
**Version**: 1.0

## Purpose

This document provides a prioritized action plan to address inconsistencies found in Proto/NATS contracts and documentation. Actions are split into:
- **CP1**: Documentation and runtime clarifications (no Proto changes, no code modifications)
- **CP2-LC**: Proto file changes, code regeneration, ABI validation (requires CP2-LC checkpoint)

**For detailed step-by-step instructions**, see:
- **CP1 Actions**: `docs/archive/dev/ROUTER_PROTO_NATS_CP1_DETAILED_PLAN.md`
- **CP2+ Actions**: `docs/archive/dev/ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md`

---

## CP1 Actions (Documentation Only)

### Priority 1: Critical Documentation Clarifications

#### Action CP1.1: Document Required Fields Enforcement at Runtime

**Status**: ⚠️ **HIGH PRIORITY**  
**Effort**: Low (documentation only)  
**Files to Update**:
- `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`
- `docs/API_CONTRACTS.md`
- `docs/archive/dev/CP1_BOUNDARIES_AND_CONTRACTS.md`

**What to Do**:
1. Add explicit section explaining:
   - **Proto Level**: All fields are optional (protobuf v3 semantics)
   - **Runtime Level**: Router enforces required fields via validation
   - **Required Fields** (enforced at runtime):
     - `Message`: `tenant_id`, `message_type`, `payload` (required)
     - `RouteRequest`: `message` (required)
     - `RouteDecision`: `provider_id`, `reason` (required)
2. Add note: "Proto contract allows optional fields for backward compatibility, but Router validates required fields at runtime and returns `invalid_request` error if missing"
3. Update examples to show both valid (with all fields) and minimal (with required fields only) payloads

**Acceptance Criteria**:
- ✅ Documentation clearly distinguishes Proto optional vs runtime required
- ✅ Examples show both minimal and full payloads
- ✅ Error handling documented for missing required fields

---

#### Action CP1.2: Clarify JSON Format vs Proto Contract

**Status**: ⚠️ **HIGH PRIORITY**  
**Effort**: Medium (documentation restructure)  
**Files to Update**:
- `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`
- `docs/API_CONTRACTS.md`

**What to Do**:
1. Restructure `PROTO_NATS_MAPPING.md` to have two clear sections:
   - **Section 1: Proto Contract** (what's in `.proto` files)
     - Show actual Proto message definitions
     - List fields from generated code (`flow_pb.erl`)
   - **Section 2: NATS JSON Format** (what's sent over NATS)
     - Show extended JSON format with additional fields
     - Document which fields are added at NATS layer (not in Proto)
     - Fields added at NATS layer:
       - `version` (NATS message version, not in Proto)
       - `request_id` (NATS correlation ID, not in Proto)
       - `task` (NATS-specific task wrapper, not in Proto)
       - `constraints` (NATS-specific constraints, not in Proto)
       - `push_assignment` (NATS-specific flag, not in Proto)
2. Add mapping table:
   | NATS JSON Field | Proto Field | Source | Notes |
   |----------------|-------------|--------|-------|
   | `version` | ❌ Not in Proto | NATS layer | Message version for NATS |
   | `request_id` | ❌ Not in Proto | NATS layer | Correlation ID for NATS |
   | `task` | ❌ Not in Proto | NATS layer | Task wrapper for NATS |
   | `constraints` | ❌ Not in Proto | NATS layer | Routing constraints |
   | `push_assignment` | ❌ Not in Proto | NATS layer | Flag to trigger ExecAssignment |
   | `message` | ✅ `RouteRequest.message` | Proto | Direct mapping |
   | `policy_id` | ✅ `RouteRequest.policy_id` | Proto | Direct mapping |
   | `context` | ✅ `RouteRequest.context` | Proto | Direct mapping |
3. Add note: "NATS JSON format includes additional fields for NATS-specific functionality. These fields are handled at the NATS adapter layer and are not part of the Proto contract."

**Acceptance Criteria**:
- ✅ Clear separation between Proto contract and NATS JSON format
- ✅ Mapping table shows which fields are Proto vs NATS layer
- ✅ Examples show both formats side-by-side

---

#### Action CP1.3: Document Policy DSL to Proto Conversion

**Status**: ⚠️ **MEDIUM PRIORITY**  
**Effort**: Medium (documentation + examples)  
**Files to Update**:
- `docs/ROUTING_POLICY.md`
- `apps/otp/router/docs/schemas/policy.schema.json` (add comments)

**What to Do**:
1. Add section "Policy DSL to Proto Conversion" in `ROUTING_POLICY.md`:
   - **DSL JSON Format** (what users write):
     ```json
     {
       "version": "1.0",
       "providers": [{"name": "provider_a", "weight": 70}, ...],
       "fallbacks": [{"when": {...}, "retry": 2, "to": "provider_b"}, ...],
       "sticky": {"enabled": true, "session_key": "user_id", "ttl": "10m"}
     }
     ```
   - **Proto Representation** (what's stored/sent):
     ```protobuf
     AdminPolicy {
       policy_id = "string"
       providers = [AdminProvider {id = "provider_a", weight = 0.7}, ...]
       sticky = true  // bool only, session_key/ttl stored separately
       rules = [AdminRule {match = "...", prefer = [...], fallback = "provider_b"}, ...]
     }
     ```
   - **Conversion Logic**:
     - `providers[]` (DSL) → `AdminProvider[]` (Proto): map `name` → `id`, `weight` → `weight`
     - `fallbacks[]` (DSL) → `AdminRule[]` (Proto): map `when` → `match`, `to` → `fallback`, `retry` stored in metadata
     - `sticky` (DSL) → `sticky: bool` (Proto): `enabled` → `sticky`, `session_key`/`ttl` stored in Router internal state
2. Add note: "DSL format is user-friendly JSON. Router converts DSL to Proto `AdminPolicy` for storage and gRPC communication. Some DSL fields (e.g., `sticky.session_key`, `sticky.ttl`) are stored in Router internal state, not in Proto."
3. Update `policy.schema.json` to add comments explaining conversion to Proto

**Acceptance Criteria**:
- ✅ DSL format clearly documented
- ✅ Proto representation clearly documented
- ✅ Conversion logic explained with examples
- ✅ Internal-only fields (session_key, ttl) documented

---

#### Action CP1.4: Document Missing Proto Files Status

**Status**: ⚠️ **MEDIUM PRIORITY**  
**Effort**: Low (documentation only)  
**Files to Update**:
- `proto/README.md`
- `apps/otp/router/docs/GENERATION.md`

**What to Do**:
1. Add note in `proto/README.md`:
   - "Proto source files (`proto/beamline/flow/v1/flow.proto`, `proto/beamline/provider/v1/provider.proto`) are currently missing. Router uses generated code from `apps/otp/router/src/flow_pb.erl` and `apps/otp/router/include/flow_pb.hrl` as the source of truth."
   - "To restore Proto files: Extract definitions from generated code or regenerate from original `.proto` files if available."
2. Add note in `apps/otp/router/docs/GENERATION.md`:
   - "If Proto files are missing, generated code in `src/flow_pb.erl` and `include/flow_pb.hrl` is the authoritative source for message definitions."

**Acceptance Criteria**:
- ✅ Status of missing Proto files documented
- ✅ Source of truth (generated code) clearly identified
- ✅ Instructions for restoration provided

---

### Priority 2: Runtime Validation Documentation

#### Action CP1.5: Document Runtime Validation Rules

**Status**: ⚠️ **LOW PRIORITY**  
**Effort**: Low (documentation only)  
**Files to Update**:
- `docs/API_CONTRACTS.md`
- `apps/otp/router/docs/OPERATIONAL_GUIDE.md`

**What to Do**:
1. Add section "Runtime Validation" in `API_CONTRACTS.md`:
   - List all required fields with validation rules
   - Document error codes for validation failures:
     - `invalid_request`: Missing required fields
     - `invalid_request`: Invalid field types
     - `invalid_request`: Field value out of range
   - Show example error responses
2. Add note in `OPERATIONAL_GUIDE.md`:
   - "Router validates all incoming requests against Proto contract and runtime rules. Validation failures return `invalid_request` error with details."

**Acceptance Criteria**:
- ✅ Validation rules documented
- ✅ Error codes and responses documented
- ✅ Examples provided

---

## CP2+ Actions (Proto Changes)

### Priority 1: Restore Proto Files

#### Action CP2.1: Restore Proto Source Files

**Status**: ❌ **CRITICAL** (blocks other CP2+ actions)  
**Effort**: Medium (extract from generated code or recreate)  
**Files to Create**:
- `proto/beamline/flow/v1/flow.proto`
- `proto/beamline/provider/v1/provider.proto`

**What to Do**:
1. **Option A**: Extract Proto definitions from generated code (`flow_pb.erl`)
   - Reverse-engineer Proto syntax from Erlang field definitions
   - Recreate `.proto` files with proper syntax
2. **Option B**: If original Proto files exist elsewhere, copy them to `proto/beamline/*/v1/`
3. Validate Proto files:
   ```bash
   cd proto
   buf lint
   buf build
   ```
4. Regenerate code to verify:
   ```bash
   cd apps/otp/router
   rebar3 gpb compile
   ```
5. Compare generated code with existing to ensure no changes

**Acceptance Criteria**:
- ✅ Proto files exist in `proto/beamline/flow/v1/flow.proto` and `proto/beamline/provider/v1/provider.proto`
- ✅ Proto files pass `buf lint` and `buf build`
- ✅ Generated code matches existing code (no breaking changes)
- ✅ Documentation updated to reference Proto files as source of truth

**Dependencies**: None (can be done independently)

---

### Priority 2: Add CP2+ Fields to Proto

#### Action CP2.2: Add CP2+ Optional Fields to Message

**Status**: ❌ **HIGH PRIORITY** (for CP2+ features)  
**Effort**: Medium (Proto changes + regeneration + testing)  
**Files to Update**:
- `proto/beamline/flow/v1/flow.proto` (after CP2.1)
- `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` (update to reflect actual Proto)

**What to Do**:
1. Add CP2+ optional fields to `Message`:
   ```protobuf
   message Message {
     // ... existing fields (1-7) ...
     
     // CP2+ Optional Fields (backward compatible)
     string run_id = 8;              // Optional: Run identifier for multi-step workflows
     string flow_id = 9;             // Optional: Flow definition identifier
     string step_id = 10;            // Optional: Step identifier within a flow
     string idempotency_key = 11;    // Optional: Message-level idempotency key
     string span_id = 12;            // Optional: Span identifier for distributed tracing
   }
   ```
2. Validate backward compatibility:
   ```bash
   cd proto
   buf breaking --against '.git#branch=main'
   ```
   - Should pass (all new fields are optional)
3. Regenerate code:
   ```bash
   cd apps/otp/router
   rebar3 gpb compile
   ```
4. Update Router code to handle new fields (if needed):
   - Check if Router already handles optional fields gracefully
   - Add tests for CP2+ fields
5. Update documentation:
   - `PROTO_NATS_MAPPING.md`: Update to show actual Proto with CP2+ fields
   - `proto/README.md`: Document CP2+ fields

**Acceptance Criteria**:
- ✅ CP2+ fields added to Proto `Message` (fields 8-12)
- ✅ `buf breaking` passes (no breaking changes)
- ✅ Generated code includes CP2+ fields
- ✅ Router handles CP2+ fields gracefully (ignores if not present)
- ✅ Documentation updated

**Dependencies**: CP2.1 (Proto files must exist)

**CP2+ Gate**: Requires `current_cp >= CP2-LC` in DevState

---

#### Action CP2.3: Add CP2+ Optional Field to RouteRequest

**Status**: ❌ **HIGH PRIORITY** (for CP2+ features)  
**Effort**: Medium (Proto changes + regeneration + testing)  
**Files to Update**:
- `proto/beamline/flow/v1/flow.proto` (after CP2.1)
- `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` (update to reflect actual Proto)

**What to Do**:
1. Add CP2+ optional field to `RouteRequest`:
   ```protobuf
   message RouteRequest {
     Message message = 1;
     string policy_id = 2;
     map<string, string> context = 3;
     
     // CP2+ Optional Field (backward compatible)
     string idempotency_key = 4;     // Optional: Request-level idempotency key
   }
   ```
2. Validate backward compatibility:
   ```bash
   cd proto
   buf breaking --against '.git#branch=main'
   ```
   - Should pass (new field is optional)
3. Regenerate code:
   ```bash
   cd apps/otp/router
   rebar3 gpb compile
   ```
4. Update Router code to handle new field (if needed):
   - Check if Router already handles optional fields gracefully
   - Add tests for CP2+ field
5. Update documentation:
   - `PROTO_NATS_MAPPING.md`: Update to show actual Proto with CP2+ field
   - `proto/README.md`: Document CP2+ field

**Acceptance Criteria**:
- ✅ CP2+ field added to Proto `RouteRequest` (field 4)
- ✅ `buf breaking` passes (no breaking changes)
- ✅ Generated code includes CP2+ field
- ✅ Router handles CP2+ field gracefully (ignores if not present)
- ✅ Documentation updated

**Dependencies**: CP2.1 (Proto files must exist). Can be done in parallel with CP2.2 (both add optional fields)

**CP2+ Gate**: Requires `current_cp >= CP2-LC` in DevState

---

### Priority 3: Policy DSL Proto Alignment (Optional)

#### Action CP2.4: Align Policy DSL with Proto Structure

**Status**: ⚠️ **LOW PRIORITY** (optional, breaking change)  
**Effort**: High (Proto changes + breaking changes + migration)  
**Files to Update**:
- `proto/beamline/flow/v1/flow.proto` (after CP2.1)
- `docs/ROUTING_POLICY.md`
- Router code (conversion logic)

**What to Do**:
1. **Option A**: Update Proto to match DSL (breaking change):
   - Change `AdminPolicy.providers` from `AdminProvider[]` to `map<string, double> weights`
   - Change `AdminPolicy.sticky` from `bool` to `StickyConfig {enabled, session_key, ttl}`
   - Change `AdminRule` to include `when` and `retry` fields
   - **Impact**: Breaking change, requires migration
2. **Option B**: Keep current Proto, document conversion (recommended):
   - Keep Proto as-is (already done in CP1.3)
   - Document conversion logic clearly
   - Add validation to ensure DSL → Proto conversion is correct

**Recommendation**: **Option B** (keep current Proto, document conversion)
- Avoids breaking changes
- Proto structure is already optimized for gRPC
- DSL format is user-friendly, conversion is acceptable

**Acceptance Criteria** (if Option A chosen):
- ✅ Proto structure matches DSL format
- ✅ Breaking change migration plan documented
- ✅ Router code updated to use new Proto structure
- ✅ Tests updated

**Dependencies**: CP2.1 (Proto files must exist)

**CP2+ Gate**: Requires `current_cp >= CP2-LC` in DevState

---

## Summary: CP1 vs CP2+ Split

### CP1 Actions (Documentation Only)

| Action | Priority | Effort | Status |
|--------|----------|--------|--------|
| CP1.1: Document Required Fields Enforcement | HIGH | Low | ⏳ Pending | See `ROUTER_PROTO_NATS_CP1_DETAILED_PLAN.md` |
| CP1.2: Clarify JSON Format vs Proto | HIGH | Medium | ⏳ Pending | See `ROUTER_PROTO_NATS_CP1_DETAILED_PLAN.md` |
| CP1.3: Document Policy DSL Conversion | MEDIUM | Medium | ⏳ Pending | See `ROUTER_PROTO_NATS_CP1_DETAILED_PLAN.md` |
| CP1.4: Document Missing Proto Files | MEDIUM | Low | ⏳ Pending | See `ROUTER_PROTO_NATS_CP1_DETAILED_PLAN.md` |
| CP1.5: Document Runtime Validation | LOW | Low | ⏳ Pending | See `ROUTER_PROTO_NATS_CP1_DETAILED_PLAN.md` |

**Total CP1 Effort**: ~2-3 days (documentation only, no code changes, no Proto modifications)

**Note**: All CP1 actions can be executed in parallel (different files, no dependencies). See `ROUTER_PROTO_NATS_CP1_DETAILED_PLAN.md` for step-by-step instructions.

### CP2+ Actions (Proto Changes)

| Action | Priority | Effort | Dependencies | CP2+ Gate |
|--------|----------|--------|---------------|-----------|
| CP2.1: Restore Proto Files | CRITICAL | Medium | None | No | See `ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md` |
| CP2.2: Add CP2+ Fields to Message | HIGH | Medium | CP2.1 | Yes | See `ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md` |
| CP2.3: Add CP2+ Field to RouteRequest | HIGH | Medium | CP2.1 | Yes | See `ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md` |
| CP2.4: Align Policy DSL (Optional) | LOW | High | CP2.1 | Yes | See `ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md` |

**Total CP2-LC Effort**: ~1-2 weeks (Proto changes + testing + documentation + ABI validation)

**Note**: CP2.1 can be done before CP2-LC transition, but CP2.2 and CP2.3 require CP2-LC checkpoint. See `ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md` for step-by-step instructions.

---

## Execution Order

### Phase 1: CP1 Documentation (Can Start Immediately)

1. **CP1.1** → **CP1.2** → **CP1.3** → **CP1.4** → **CP1.5**
   - All can be done in parallel (different files)
   - No dependencies between actions
   - Can complete in 2-3 days

### Phase 2: CP2+ Proto Changes (After CP1, Before CP2 Transition)

1. **CP2.1**: Restore Proto files (blocks other CP2+ actions)
2. **CP2.2 + CP2.3**: Add CP2+ fields (can be done together)
3. **CP2.4**: Align Policy DSL (optional, low priority)

**Timing**: CP2+ actions should be completed **after** transitioning to CP2-LC in DevState (version gate requirement).

---

## Risk Assessment

### CP1 Actions
- **Risk**: Low (documentation only, no code changes)
- **Mitigation**: Review documentation changes before committing

### CP2+ Actions
- **Risk**: Medium (Proto changes may break existing code)
- **Mitigation**:
  - Always run `buf breaking` before committing
  - Regenerate code and verify no breaking changes
  - Run full test suite after changes
  - Gate CP2+ changes behind `current_cp >= CP2-LC`

---

## References

### Related Documents
- **Consistency Report**: `docs/archive/dev/ROUTER_PROTO_NATS_CONSISTENCY.md` - Full analysis and findings
- **CP1 Detailed Plan**: `docs/archive/dev/ROUTER_PROTO_NATS_CP1_DETAILED_PLAN.md` - Step-by-step CP1 instructions
- **CP2+ Detailed Plan**: `docs/archive/dev/ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md` - Step-by-step CP2-LC instructions
- **Summary**: `docs/archive/dev/ROUTER_PROTO_NATS_CONSISTENCY_SUMMARY.md` - Brief version for CP reports

### Source Documents
- **CP1 Baseline**: `docs/archive/dev/CP1_BASELINE.md`
- **CP2 Transition Plan**: `docs/archive/dev/CP2_TRANSITION_PLAN_ROUTER.md`
- **Proto Generation**: `apps/otp/router/docs/GENERATION.md`
- **NATS Mapping**: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`

