# Router Documentation Review Feedback

**Date**: 2025-01-27  
**Status**: ✅ Review Complete  
**Documents Reviewed**:
- `../../../apps/otp/router/docs/dev/ROUTER_CP1_SMOKE_CHECKLIST.md`
- `../../../apps/otp/router/docs/dev/ROUTER_PROTO_CONSISTENCY.md`
- `docs/archive/dev/GATEWAY_CP1_SMOKE_DETAILED_CHECKLIST.md`

## Quick Feedback Summary

### 1. ROUTER_CP1_SMOKE_CHECKLIST.md

#### ✅ Strengths
- Clear structure with actionable checkboxes
- Good command examples
- Proper test suite references

#### 🔧 Improvements Needed

**1.1. Tighten Formulations**

**Line 73**: "Router is alive, accepts messages via NATS, executes basic flow, not violate CP1 invariants"
- **Change to**: "Router accepts NATS messages, executes routing decisions, maintains CP1 invariants"

**Line 99**: "Reference Scenarios (links to tests...)"
- **Change to**: "Scenarios" (remove redundant "Reference")

**Line 150**: "Full idempotency is CP2+ feature. CP1 minimal guarantee..."
- **Change to**: "Idempotency: CP2+ feature. CP1: no duplicate assignment on re-delivery (if stated in CP1 requirements)."
- **Add link**: `docs/archive/dev/ROUTER_CP1_IDEMPOTENCY_SCOPE.md`

**1.2. Strengthen Verifiability**

**Line 44**: "Implementation: `apps/otp/router/Makefile`, `apps/otp/router/rebar.config`"
- **Add**: Specific Makefile targets: `make test`, `make dialyzer`
- **Add**: Test command verification: `rebar3 ct --suite router_core_SUITE`

**Line 95**: "Tests: `apps/otp/router/test/router_health_SUITE.erl`"
- **Add**: Specific test function: `test_health_endpoint/1`
- **Add**: Verification command: `rebar3 ct --suite router_health_SUITE`

**Line 121**: "Tests: `apps/otp/router/test/router_nats_subscriber_caf_SUITE.erl`, `apps/otp/router/test/router_core_SUITE.erl`"
- **Add**: Specific test functions: `test_happy_path_decide/1`, `test_basic_decision/1`
- **Add**: Verification: `rebar3 ct --suite router_nats_subscriber_caf_SUITE --suite router_core_SUITE`

**Line 143**: "Tests: `apps/otp/router/test/router_nats_contract_validation_SUITE.erl`, `apps/otp/router/test/router_error_SUITE.erl`"
- **Add**: Specific test functions: `test_invalid_request/1`, `test_missing_tenant_id/1`
- **Add**: Verification: `rebar3 ct --suite router_nats_contract_validation_SUITE --suite router_error_SUITE`

**1.3. Add Missing References**

**Line 167**: "Tests: `apps/otp/router/test/router_idempotency_SUITE.erl` (if exists)"
- **Add**: Check if exists: `grep -r "router_idempotency_SUITE" apps/otp/router/test/`
- **Add**: If exists, add specific test: `test_cp1_minimal_idempotency/1`
- **Add**: Link to scope doc: `docs/archive/dev/ROUTER_CP1_IDEMPOTENCY_SCOPE.md`

**Line 225**: "ADR References: `docs/ADR/ADR-XXX-jetstream.md`"
- **Fix**: Replace `ADR-XXX` with actual ADR number or remove if doesn't exist
- **Add**: Check: `ls docs/ADR/ADR-*-jetstream.md`

**Line 230**: "DTO compliance: `docs/ARCHITECTURE/api-registry.md`"
- **Add**: Specific section: `docs/ARCHITECTURE/api-registry.md#rest-api-c-gateway`

**Line 231**: "Proto contract: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`"
- **Add**: Specific section: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md#router-service-beamlineflowv1`

### 2. ROUTER_PROTO_CONSISTENCY.md

#### ✅ Strengths
- Clear invariant definitions
- Good verification commands
- Proper subject → handler mapping

#### 🔧 Improvements Needed

**2.1. Tighten Formulations**

**Line 5**: "This document defines **contract invariants** between protobuf schemas and Router code."
- **Change to**: "Contract invariants: Proto ↔ Router code. Router **must** conform to protobuf definitions and NATS mapping."

**Line 19**: "Proto source files are currently **missing**. Generated code... is the source of truth."
- **Change to**: "Proto source files: **missing** (CP2-LC restoration). Source of truth: generated code (`flow_pb.erl`, `flow_pb.hrl`)."
- **Add link**: `docs/archive/dev/ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md#proto-file-restoration`

**Line 45**: "Rules that must always be satisfied"
- **Change to**: "Invariants (always satisfied)"

**2.2. Strengthen Verifiability**

**Line 64**: "Implementation: `apps/otp/router/src/router_nats_subscriber.erl` (subject: `beamline.router.v1.decide`)"
- **Add**: Specific line: `apps/otp/router/src/router_nats_subscriber.erl:11` (subject definition)
- **Add**: Verification: `grep -n "beamline.router.v1.decide" apps/otp/router/src/router_nats_subscriber.erl`

**Line 76**: "Implementation: Generated code must be regenerated after proto changes"
- **Add**: Specific command: `rebar3 gpb_compile` or `buf generate`
- **Add**: Verification script: `scripts/check_proto_sync.sh`

**Line 95**: "Implementation: `apps/otp/router/src/router_nats_subscriber.erl` - Validation logic"
- **Add**: Specific function: `validate_decide_request/2` or `handle_decide_request/2`
- **Add**: Verification: `grep -n "validate\|required" apps/otp/router/src/router_nats_subscriber.erl`

**Line 103**: "Implementation: NATS adapter layer adds fields..."
- **Add**: Specific module: `router_nats_subscriber.erl` or `router_caf_adapter.erl`
- **Add**: Verification: `grep -n "version\|request_id\|task" apps/otp/router/src/router_nats_subscriber.erl`

**2.3. Add Missing References**

**Line 157**: "Router Module/Function: `router_nats_subscriber.erl:handle_decide_request/2`"
- **Add**: Specific line range: `apps/otp/router/src/router_nats_subscriber.erl:116-161`
- **Add**: Test reference: `apps/otp/router/test/router_nats_subscriber_caf_SUITE.erl:test_happy_path_decide/1`

**Line 160**: "Router Module/Function: `router_caf_adapter.erl:publish_assignment/2`"
- **Add**: Test reference: `apps/otp/router/test/router_caf_adapter_SUITE.erl`
- **Add**: Verification: `grep -n "publish_assignment" apps/otp/router/src/router_caf_adapter.erl`

**Line 162**: "Router Module/Function: `router_result_consumer.erl:publish_usage_event/2`"
- **Add**: Test reference: `apps/otp/router/test/router_result_consumer_SUITE.erl`
- **Add**: Verification: `grep -n "publish_usage_event\|beamline.usage.v1.metered" apps/otp/router/src/router_result_consumer.erl`

**Line 213**: "ADR on protobuf/compatibility (if exists)"
- **Fix**: Check if exists: `ls docs/ADR/ADR-*-proto*.md docs/ADR/ADR-*-compatibility*.md`
- **Add**: If exists, add link; if not, remove "(if exists)" and note: "No ADR yet"

### 3. GATEWAY_CP1_SMOKE_DETAILED_CHECKLIST.md

#### ✅ Strengths
- Very detailed, file-by-file breakdown
- Good gap analysis
- Clear action items

#### 🔧 Improvements Needed

**3.1. Tighten Formulations**

**Line 11**: "This document provides a **point-by-point checklist**..."
- **Change to**: "Point-by-point checklist: Gateway CP1-smoke implementation (Phase 2/3 breakdown)."

**Line 27**: "**Gap**: `api-registry.md` does not define `POST /api/v1/routes/decide` DTO structure."
- **Change to**: "Gap: `api-registry.md` missing `POST /api/v1/routes/decide` definition."

**Line 100**: "**Verification**:"
- **Change to**: "**Verification Checklist**:"

**3.2. Strengthen Verifiability**

**Line 18**: "Handler: `handle_decide()` (lines 1191-1274)"
- **Add**: Verification: `grep -n "handle_decide" apps/c-gateway/src/http_server.c | head -1`

**Line 19**: "Validation: `validate_decide_request()` (line 831+)"
- **Add**: Specific line: `apps/c-gateway/src/http_server.c:831`
- **Add**: Verification: `grep -n "validate_decide_request" apps/c-gateway/src/http_server.c`

**Line 20**: "NATS Build: `build_route_request_json()` (line 887+)"
- **Add**: Specific line: `apps/c-gateway/src/http_server.c:887`
- **Add**: Verification: `grep -n "build_route_request_json" apps/c-gateway/src/http_server.c`

**Line 25**: "`docs/ARCHITECTURE/api-registry.md` - **Missing** `POST /api/v1/routes/decide` definition"
- **Add**: Verification: `grep -n "POST /api/v1/routes/decide" docs/ARCHITECTURE/api-registry.md`
- **Add**: Expected: No matches (confirms gap)

**3.3. Add Missing References**

**Line 100**: "**Verification**:"
- **Add**: Test reference: `tests/integration/c-gateway-routes.test.ts:14` (POST /api/v1/routes/decide test)
- **Add**: Smoke script: `scripts/gateway_router_cp1_smoke.sh:125` (happy path test)

**Line 143**: "**Verification Command**:"
- **Add**: Expected output example: `"version": "1"` in JSON
- **Add**: Failure case: Missing `version` → error

**Line 268**: "**Verification Command**:"
- **Add**: Expected error JSON structure example
- **Add**: Test reference: `scripts/gateway_router_cp1_smoke.sh:232` (validation error test)

## Cross-Document Improvements

### Missing Test Suite References

**Add to all documents**:
- `router_observability_SUITE.erl` - Observability tests (CP1)
- `router_cp1_minimal_mode_SUITE.erl` - CP1 mode enforcement
- `router_e2e_smoke_SUITE.erl` - E2E smoke tests
- `router_gateway_contract_smoke_SUITE.erl` - Gateway ↔ Router contract tests

**Specific test functions to reference**:
- `router_observability_SUITE.erl:test_log_format_json/1` - Log format verification
- `router_observability_SUITE.erl:test_pii_filtering/1` - PII filtering
- `router_observability_SUITE.erl:test_health_endpoint/1` - Health endpoint
- `router_cp1_minimal_mode_SUITE.erl:test_cp1_blocks_ack_consumer/1` - CP1 enforcement
- `router_e2e_smoke_SUITE.erl:test_happy_path_rbac_to_audit/1` - E2E smoke
- `router_gateway_contract_smoke_SUITE.erl` - Gateway contract tests

### Missing Script References

**Add to all documents**:
- `scripts/validate_all_projects.sh` - Main validation script
- `scripts/check_proto.sh` - Proto validation
- `scripts/gateway_router_cp1_smoke.sh` - Gateway ↔ Router smoke
- `scripts/observability/validate_observability.sh` - Observability validation

### Missing Documentation References

**Add to all documents**:
- `docs/CP1_ACCEPTANCE_REPORT.md#router-appsotprouter--cp1-status` - Router CP1 status
- `docs/archive/dev/ROUTER_CP1_COMPLETE_IMPLEMENTATION_REPORT.md` - CP1 implementation report
- `docs/archive/dev/ROUTER_PROTO_NATS_CONSISTENCY.md` - Proto/NATS consistency report
- `docs/archive/dev/ROUTER_CP1_IDEMPOTENCY_SCOPE.md` - Idempotency scope

## Priority Actions

### High Priority (Do First)
1. **ROUTER_CP1_SMOKE_CHECKLIST.md**: Add specific test function names and verification commands
2. **ROUTER_PROTO_CONSISTENCY.md**: Add line numbers and grep commands for verification
3. **Both**: Fix ADR references (remove `ADR-XXX` or find actual ADR)

### Medium Priority
4. **ROUTER_CP1_SMOKE_CHECKLIST.md**: Tighten formulations (remove redundant words)
5. **ROUTER_PROTO_CONSISTENCY.md**: Add test suite references for each handler
6. **GATEWAY_CP1_SMOKE_DETAILED_CHECKLIST.md**: Add verification commands with expected outputs

### Low Priority
7. **All**: Add cross-references to other key documents
8. **All**: Add missing test suite references (observability, CP1 mode, E2E)

## Summary

**Total Issues Found**: 25+
- **Formulations to tighten**: 8
- **Verifiability to strengthen**: 12
- **Missing references**: 10+

**Estimated Fix Time**: 30-45 minutes per document

**Impact**: High - These improvements will make documents more actionable and verifiable for developers and auditors.

