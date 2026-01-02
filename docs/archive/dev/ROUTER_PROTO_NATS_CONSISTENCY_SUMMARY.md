# Router Proto/NATS Consistency - Summary for CP Report

**Date**: 2025-01-27  
**Source**: `docs/archive/dev/ROUTER_PROTO_NATS_CONSISTENCY.md`  
**Status**: Analysis Complete  
**Version**: 1.0

**Purpose**: Brief summary for inclusion in CP1 acceptance reports. For detailed analysis, see the full consistency report.

## Executive Summary

Proto/NATS contracts consistency verification completed for Router. **NATS subject mapping is consistent** across all documentation. **Two-level contract architecture** (Proto wire protocol vs NATS JSON payload) is documented and clarified. **CP2+ fields are documented but not yet in Proto** (expected for CP1). **Documentation improvements needed** to clarify required field enforcement at runtime and Policy DSL conversion logic.

## Key Findings

### ✅ Consistent Areas

- **NATS Subjects**: All subjects correctly mapped between `PROTO_NATS_MAPPING.md` and `NATS_SUBJECTS.md`
- **Service Mapping**: Router, Provider, and Ingress services correctly mapped to NATS subjects
- **JSON vs Proto Architecture**: Two-level contract architecture clarified (Proto wire protocol + NATS JSON payload with adapter-layer fields)

### ⚠️ Documentation Improvements Needed (CP1)

- **Required vs Optional Fields**: Documentation states fields are required (`tenant_id`, `message_type`, `payload` in `Message`; `provider_id`, `reason` in `RouteDecision`), but Proto defines them as optional. **Resolution**: Document that required fields are enforced at runtime, not at Proto level (protobuf v3 semantics allow optional fields for backward compatibility)
- **Policy DSL Structure**: Documentation shows user-friendly DSL format (map-based `weights`, array-based `fallbacks`), but Proto uses array-based structures (`AdminProvider[]`, `AdminRule[]`). **Resolution**: Document conversion logic between DSL JSON and Proto representation

### ❌ CP2+ Items (Deferred to CP2+)

- **Missing Proto Files**: Proto source files (`proto/beamline/flow/v1/flow.proto`, `proto/beamline/provider/v1/provider.proto`) are missing (directories exist but empty). Generated code (`flow_pb.erl`, `flow_pb.hrl`) is current source of truth. **Action**: Restore Proto files in CP2+
- **CP2+ Fields Not in Proto**: CP2+ fields (`run_id`, `flow_id`, `step_id`, `idempotency_key`, `span_id` in `Message`; `idempotency_key` in `RouteRequest`) are documented but not in generated Proto code. **Action**: Add CP2+ fields to Proto files in CP2+ (backward compatible, all optional)

## Action Items

### CP1 (Documentation Only)

- ✅ **Clarified**: Two-level contract architecture (Section 1.5 in consistency report)
- ⏳ **Pending**: Update `PROTO_NATS_MAPPING.md` to explicitly label NATS-specific fields (`version`, `request_id`, `task`, `constraints`, `push_assignment`)
- ⏳ **Pending**: Document required field enforcement at runtime (Proto optional, runtime required)
- ⏳ **Pending**: Document Policy DSL to Proto conversion logic

### CP2-LC (Proto Changes)

- ⏳ **Pending**: Restore Proto source files (CP2.1 - see `ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md`)
- ⏳ **Pending**: Add CP2+ optional fields to Proto (`Message` fields 8-12, `RouteRequest` field 4) (CP2.2, CP2.3 - see `ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md`)

## Impact Assessment

**CP1 Readiness**: ✅ **READY** - All inconsistencies are either:
- **Resolved by design** (JSON vs Proto two-level architecture)
- **Documentation improvements** (no code changes required)
- **CP2+ deferred** (expected for CP1 baseline)

**No blocking issues** for CP1 acceptance. All critical contract mappings are consistent and documented.

---

## Brief Version for CP Report (1-2 paragraphs + bullets)

### Paragraph Version

Router Proto/NATS contracts consistency verification completed. **NATS subject mapping is fully consistent** across all documentation sources. The two-level contract architecture (Proto wire protocol for gRPC/ABI vs NATS JSON payload with adapter-layer fields) has been documented and clarified. CP2+ fields (`run_id`, `flow_id`, `step_id`, `idempotency_key`, `span_id`) are documented but not yet in Proto code, which is expected for CP1 baseline and will be added in CP2+. Documentation improvements are needed to clarify required field enforcement at runtime (Proto fields are optional per protobuf v3 semantics, but Router enforces required fields at runtime) and Policy DSL conversion logic (user-friendly DSL format vs Proto array-based structures).

### Bullet Points Version

- ✅ **NATS Subjects**: Consistent mapping between `PROTO_NATS_MAPPING.md` and `NATS_SUBJECTS.md`
- ✅ **Two-Level Architecture**: Proto wire protocol (gRPC/ABI) vs NATS JSON payload (with adapter-layer fields) clarified
- ⚠️ **Documentation**: Required field enforcement at runtime needs clarification (Proto optional, runtime required)
- ⚠️ **Documentation**: Policy DSL to Proto conversion logic needs documentation
- ❌ **CP2-LC Deferred**: Proto source files missing (restore in CP2-LC, see `ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md`)
- ❌ **CP2-LC Deferred**: CP2+ fields not in Proto code (add in CP2-LC, backward compatible, see `ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md`)

**Status**: ✅ **CP1 READY** - No blocking issues. All inconsistencies are either resolved by design, documentation improvements, or deferred to CP2-LC.

**Next Steps**:
- **CP1**: Complete documentation improvements (see `ROUTER_PROTO_NATS_CP1_DETAILED_PLAN.md`)
- **CP2-LC**: Restore Proto files and add CP2+ fields (see `ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md`)

