# CP1 Idempotency Scope Clarification

**Date**: 2025-01-27  
**Status**: Clarified  
**Version**: 1.0

## Purpose

This document clarifies the CP1 idempotency scope vs CP2+ idempotency features, based on analysis of CP1 acceptance reports and ADR-012.

---

## CP1 Idempotency Scope

### CP1: No Idempotency Requirements

**Finding**: After reviewing CP1 acceptance reports and ADR-012, **CP1 does NOT require idempotency guarantees**.

**Evidence**:
- `docs/ADR/ADR-012-idempotency-layer.md` - Idempotency layer is a **CP2+ feature**
- `docs/archive/dev/CP1_BASELINE.md:62-102` - Idempotency is listed as **CP2+ feature** (enabled by default in CP2 baseline)
- `apps/otp/router/src/beamline_router.app.src:67` - `idempotency_enabled` flag is **CP2+ feature**
- `apps/otp/router/docs/OPERATIONAL_GUIDE.md:737-748` - Idempotency is **CP2-LC operational feature**

**Conclusion**: **CP1 does NOT include idempotency guarantees**. Idempotency is a CP2+ feature and should not be included in CP1 promises.

---

## CP2+ Idempotency Features

### Idempotency Layer (CP2+)

**Component**: `router_idempotency.erl`  
**Feature Flag**: `idempotency_enabled` (default: `true` in CP2 baseline)  
**Purpose**: Prevents duplicate processing of messages (results, ACKs, usage events)

**Usage**:
- Used in `router_result_consumer.erl` - Prevents duplicate result processing
- Used in `router_ack_consumer.erl` - Prevents duplicate ACK processing
- Used in usage event emission - Prevents duplicate billing

**Storage**: ETS table with TTL (default: 3600 seconds / 1 hour)

**Status**: ✅ **CP2+ feature, NOT part of CP1**

---

## CP1 vs CP2+ Separation

### CP1 Scope (No Idempotency)

**CP1 Router guarantees**:
- ✅ Routing decisions are deterministic (weighted round-robin)
- ✅ Error handling is consistent
- ✅ Policy enforcement is reliable
- ❌ **NO idempotency guarantees** (duplicate messages may be processed)

### CP2+ Scope (Idempotency Included)

**CP2+ Router guarantees**:
- ✅ All CP1 guarantees
- ✅ Idempotency layer prevents duplicate processing
- ✅ ETS-based deduplication with TTL
- ✅ Fail-open strategy (idempotency errors don't block processing)

---

## Recommendations

### For CP1 Acceptance

1. **Document CP1 Scope**:
   - ✅ CP1 does NOT include idempotency guarantees
   - ✅ Idempotency is a CP2+ feature
   - ✅ CP1 acceptance should NOT require idempotency tests

2. **Update CP1 Reports**:
   - ✅ Document that idempotency is deferred to CP2+
   - ✅ Ensure no CP1 promises include idempotency
   - ✅ Clarify CP1 vs CP2+ feature separation

3. **Test Coverage**:
   - ✅ CP1 tests should NOT include idempotency tests
   - ✅ CP2+ tests should include idempotency tests (`router_idempotency_SUITE.erl`)

---

## References

- **ADR-012**: `docs/ADR/ADR-012-idempotency-layer.md` - Idempotency Layer for Message Processing
- **CP1 Baseline**: `docs/archive/dev/CP1_BASELINE.md` - CP1 baseline features (idempotency is CP2+)
- **CP1 Acceptance Report**: `docs/CP1_ACCEPTANCE_REPORT.md` - CP1 acceptance criteria
- **Router Idempotency**: `apps/otp/router/src/router_idempotency.erl` - Idempotency implementation (CP2+)
- **Operational Guide**: `apps/otp/router/docs/OPERATIONAL_GUIDE.md` - CP2-LC operational features

