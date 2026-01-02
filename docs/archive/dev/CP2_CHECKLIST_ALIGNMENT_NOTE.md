# CP2 Checklist Alignment with Wave 1 Plans

**Version**: 1.0  
**Date**: 2025-01-27  
**Checkpoint**: CP2-LC  
**Status**: 📋 **ALIGNMENT NOTE** (CP2)

---

## Executive Summary

This document verifies alignment between `docs/CP2_CHECKLIST.md` and CP2 Wave 1 plans (Worker Reliability + Observability), ensuring consistency in roles, ETAs, and scope.

**Key Finding**: CP2_CHECKLIST tasks are **complementary** to Wave 1, not overlapping. Wave 1 focuses on Worker (CAF) reliability and Observability metrics, while CP2_CHECKLIST focuses on Router/Gateway features.

---

## Alignment Verification

### 1. JetStream durability & redelivery

**CP2_CHECKLIST**:
- **Worker**: wrk-1 (Router/OTP)
- **Scope**: Router JetStream implementation
- **ETA**: 3-5 days
- **Artifacts**: `router_jetstream.erl`, JetStream config, DLQ subjects

**Wave 1 Alignment**: ✅ **No Conflict**
- Wave 1 Worker Reliability (wrk-3) focuses on **CAF Worker** retry/timeout/queue
- JetStream is **Router** feature (wrk-1), separate from Worker reliability
- **Reference**: `docs/CP2_CHECKLIST.md#jetstream-durability--redelivery`

---

### 2. Idempotency layer

**CP2_CHECKLIST**:
- **Worker**: wrk-1 (Router/OTP)
- **Scope**: Router idempotency implementation
- **ETA**: 3-5 days
- **Artifacts**: `router_idem.erl`, ETS tables, config keys

**Wave 1 Alignment**: ✅ **No Conflict**
- Wave 1 Worker Reliability focuses on **CAF Worker** retry policies
- Idempotency is **Router** feature (wrk-1), separate from Worker retry logic
- **Reference**: `docs/CP2_CHECKLIST.md#idempotency-layer`

---

### 3. ACL (tenant/roles)

**CP2_CHECKLIST**:
- **Worker**: wrk-1 (Router/OTP)
- **Scope**: Router ACL implementation
- **ETA**: 3-4 days
- **Artifacts**: `router_acl.erl`, policy config, audit log integration

**Wave 1 Alignment**: ✅ **No Conflict**
- Wave 1 does not include ACL features
- ACL is **Router** feature (wrk-1), separate from Wave 1 scope
- **Reference**: `docs/CP2_CHECKLIST.md#acl`

---

### 4. Admin gRPC

**CP2_CHECKLIST**:
- **Worker**: wrk-2 (Gateway/C)
- **Scope**: Gateway admin gRPC implementation
- **ETA**: 3-5 days
- **Artifacts**: `admin_grpc.c`, RouterAdmin client, config

**Wave 1 Alignment**: ✅ **No Conflict**
- Wave 1 does not include admin gRPC features
- Admin gRPC is **Gateway** feature (wrk-2), separate from Wave 1 scope
- **Reference**: `docs/CP2_CHECKLIST.md#admin-grpc`

---

### 5. Headers propagation

**CP2_CHECKLIST**:
- **Worker**: wrk-2 (Gateway/C)
- **Scope**: Headers propagation Router → Gateway → Worker
- **ETA**: 2-4 days
- **Artifacts**: `router_headers.erl`, `headers.c`, docs

**Wave 1 Alignment**: ✅ **No Conflict**
- Wave 1 Worker Reliability focuses on **internal** Worker retry/timeout/queue
- Headers propagation is **cross-component** feature (wrk-2), separate from Wave 1
- **Reference**: `docs/CP2_CHECKLIST.md#headers-propagation`

---

### 6. Observability expansion

**CP2_CHECKLIST**:
- **Worker**: wrk-3 (Obs/Docs)
- **Scope**: Router observability (OTel spans, metrics, dashboards)
- **ETA**: 3-5 days
- **Artifacts**: `router_observability.erl`, updated logging/tracing config, dashboards

**Wave 1 Alignment**: ⚠️ **Role Clarification Needed**

**CP2_CHECKLIST Role**: wrk-3 (Obs/Docs) - Router observability  
**Wave 1 Role**: wrk-obs1 (Observability CP2) - Prometheus metrics for all components

**Scope Comparison**:
- **CP2_CHECKLIST**: Router-specific observability (OTel spans, Router metrics)
- **Wave 1 Observability**: Prometheus metrics for **all components** (Router, Gateway, Worker)

**Alignment**:
- ✅ **Complementary**: CP2_CHECKLIST focuses on Router OTel spans, Wave 1 focuses on Prometheus metrics
- ✅ **No Overlap**: Different observability aspects (OTel vs Prometheus)
- ⚠️ **Role Clarification**: CP2_CHECKLIST uses "wrk-3 (Obs/Docs)", Wave 1 uses "wrk-obs1"
  - **Resolution**: CP2_CHECKLIST wrk-3 = Router observability (OTel), Wave 1 wrk-obs1 = All components metrics (Prometheus)

**References**:
- `docs/CP2_CHECKLIST.md#observability-expansion`
- `docs/archive/dev/CP2_OBSERVABILITY_WAVE1.md` - Wave 1 Observability specification
- `docs/archive/dev/CP2_OBSERVABILITY_BACKLOG.md` - Complete Observability backlog

---

### 7. CI DevState gates

**CP2_CHECKLIST**:
- **Worker**: wrk-3 (Obs/Docs)
- **Scope**: DevState verify scripts, CI integration
- **ETA**: 1-2 days
- **Artifacts**: `devstate_verify.sh`, CI workflow

**Wave 1 Alignment**: ⚠️ **Role Clarification Needed**

**CP2_CHECKLIST Role**: wrk-3 (Obs/Docs) - CI DevState gates  
**Wave 1 Role**: wrk-1 (Schemas, Manifest & CI Gates) - CI validation gates

**Scope Comparison**:
- **CP2_CHECKLIST**: DevState verify scripts and CI integration
- **Wave 1 CI Integration**: Fast checks, baseline tests, optional checks (see `docs/archive/dev/CP2_WAVE1_CI_INTEGRATION_PLAN.md`)

**Alignment**:
- ✅ **Complementary**: CP2_CHECKLIST focuses on DevState gates, Wave 1 focuses on Wave 1-specific CI checks
- ✅ **No Overlap**: Different CI aspects (DevState vs Wave 1 validation)
- ⚠️ **Role Clarification**: CP2_CHECKLIST uses "wrk-3 (Obs/Docs)", but DevState gates are typically wrk-1 responsibility
  - **Resolution**: DevState gates should be wrk-1 (Schemas, Manifest & CI Gates), not wrk-3

**References**:
- `docs/CP2_CHECKLIST.md#ci-devstate-gates`
- `docs/archive/dev/CP2_WAVE1_CI_INTEGRATION_PLAN.md` - Wave 1 CI integration plan
- `.cursor/rules/agents/wrk-1-schemas-manifest-ci-gates.mdc` - wrk-1 rules (CI gates responsibility)

---

## Role Clarification

### Worker Role Mapping

| CP2_CHECKLIST Task | CP2_CHECKLIST Worker | Actual Responsibility | Wave 1 Equivalent |
|-------------------|---------------------|----------------------|-------------------|
| JetStream | wrk-1 (Router/OTP) | ✅ Correct (Router feature) | N/A (Router, not Worker) |
| Idempotency | wrk-1 (Router/OTP) | ✅ Correct (Router feature) | N/A (Router, not Worker) |
| ACL | wrk-1 (Router/OTP) | ✅ Correct (Router feature) | N/A (Router, not Worker) |
| Admin gRPC | wrk-2 (Gateway/C) | ✅ Correct (Gateway feature) | N/A (Gateway, not Worker) |
| Headers | wrk-2 (Gateway/C) | ✅ Correct (Gateway feature) | N/A (Gateway, not Worker) |
| Observability | wrk-3 (Obs/Docs) | ⚠️ Router OTel (wrk-3 Router) | wrk-obs1 (All components Prometheus) |
| CI DevState | wrk-3 (Obs/Docs) | ⚠️ Should be wrk-1 | wrk-1 (CI gates) |

**Key Insight**: 
- **CP2_CHECKLIST wrk-3** = Router observability (OTel spans) - **Router Core** responsibility
- **Wave 1 wrk-3** = Worker Reliability (CAF Worker) - **Worker** responsibility
- **Wave 1 wrk-obs1** = Observability metrics (all components) - **Observability** responsibility

**These are different workers with different scopes.**

---

## ETA Alignment

### CP2_CHECKLIST ETAs

| Task | ETA | Worker |
|------|-----|--------|
| JetStream | 3-5 days | wrk-1 |
| Idempotency | 3-5 days | wrk-1 |
| ACL | 3-4 days | wrk-1 |
| Admin gRPC | 3-5 days | wrk-2 |
| Headers | 2-4 days | wrk-2 |
| Observability | 3-5 days | wrk-3 |
| CI DevState | 1-2 days | wrk-3 |

### Wave 1 ETAs

| Component | ETA | Worker |
|-----------|-----|--------|
| Worker Reliability | ~9.5 days (~2 weeks) | wrk-3 (Worker Reliability) |
| Observability | ~7 days (~1.5 weeks) | wrk-obs1 |

**Alignment**: ✅ **No Conflict**
- CP2_CHECKLIST tasks: Router/Gateway features (wrk-1, wrk-2)
- Wave 1 tasks: Worker Reliability + Observability (wrk-3 Worker, wrk-obs1)
- **Different scopes, different workers, no overlap**

---

## Recommendations

### 1. Update CP2_CHECKLIST Role Assignments

**Observability expansion**:
- **Current**: wrk-3 (Obs/Docs)
- **Recommended**: Clarify as "wrk-3 Router Core (Router observability)" or "wrk-obs1 (if all components)"
- **Rationale**: Avoid confusion with Wave 1 wrk-3 (Worker Reliability)

**CI DevState gates**:
- **Current**: wrk-3 (Obs/Docs)
- **Recommended**: wrk-1 (Schemas, Manifest & CI Gates)
- **Rationale**: CI gates are wrk-1 responsibility per rules

---

### 2. Add References to Wave 1 Documents

**In CP2_CHECKLIST.md, add references**:

**Observability expansion section**:
```markdown
## Observability expansion

- Acceptance criteria: [existing criteria]
- Planned artifacts: [existing artifacts]

**Related Documents**:
- `docs/archive/dev/CP2_OBSERVABILITY_WAVE1.md` - Wave 1 Observability (Prometheus metrics for all components)
- `docs/archive/dev/CP2_OBSERVABILITY_BACKLOG.md` - Complete Observability backlog
- **Note**: CP2_CHECKLIST focuses on Router OTel spans, Wave 1 focuses on Prometheus metrics (complementary)
```

**CI DevState gates section**:
```markdown
## CI DevState gates

- Acceptance criteria: [existing criteria]
- Planned artifacts: [existing artifacts]

**Related Documents**:
- `docs/archive/dev/CP2_WAVE1_CI_INTEGRATION_PLAN.md` - Wave 1 CI integration plan
- `.cursor/rules/agents/wrk-1-schemas-manifest-ci-gates.mdc` - wrk-1 CI gates responsibility
- **Note**: DevState gates are wrk-1 responsibility, Wave 1 CI integration is complementary
```

---

### 3. Clarify Worker Role Naming

**Recommendation**: Use explicit role names to avoid confusion:

- **wrk-1**: Schemas, Manifest & CI Gates (Router ABI/Contracts)
- **wrk-2**: Architecture/Tech Lead (Gateway/C)
- **wrk-3 Router**: Router Core (Erlang/OTP Router implementation)
- **wrk-3 Worker**: Worker Reliability (CAF Worker reliability)
- **wrk-obs1**: Observability CP2 (All components metrics)

**Or use distinct IDs**:
- **wrk-1**: Schemas, Manifest & CI Gates
- **wrk-2**: Architecture/Tech Lead
- **wrk-13**: Router Core (Erlang/OTP)
- **wrk-3**: Worker Reliability (CAF Worker)
- **wrk-obs1**: Observability CP2

---

## Summary

### Alignment Status: ✅ **ALIGNED**

**Key Findings**:
1. ✅ **No Scope Overlap**: CP2_CHECKLIST tasks (Router/Gateway) are separate from Wave 1 tasks (Worker Reliability + Observability)
2. ✅ **Complementary**: CP2_CHECKLIST and Wave 1 address different aspects (Router features vs Worker reliability)
3. ⚠️ **Role Clarification**: Some role assignments in CP2_CHECKLIST need clarification (Observability, CI DevState)
4. ✅ **ETA Consistency**: ETAs are reasonable and don't conflict (different workers, different scopes)

**Action Items**:
1. Add references to Wave 1 documents in CP2_CHECKLIST.md
2. Clarify role assignments (Observability → wrk-3 Router or wrk-obs1, CI DevState → wrk-1)
3. Consider explicit role naming to avoid confusion

---

## References

### CP2_CHECKLIST
- `docs/CP2_CHECKLIST.md` - Main checklist document

### Wave 1 Documents
- `docs/archive/dev/CP2_WORKER_RELIABILITY_WAVE1.md` - Worker Reliability Wave 1
- `docs/archive/dev/CP2_OBSERVABILITY_WAVE1.md` - Observability Wave 1
- `docs/archive/dev/CP2_WAVE1_PRODUCT_SUMMARY.md` - Product summary

### Backlogs
- `docs/archive/dev/CP2_WORKER_RELIABILITY_BACKLOG.md` - Worker Reliability backlog
- `docs/archive/dev/CP2_OBSERVABILITY_BACKLOG.md` - Observability backlog

### CI Integration
- `docs/archive/dev/CP2_WAVE1_CI_INTEGRATION_PLAN.md` - CI integration plan

### Worker Rules
- `.cursor/rules/agents/wrk-1-schemas-manifest-ci-gates.mdc` - wrk-1 rules (CI gates)

---

## Change History

**v1.0 (2025-01-27)**:
- Initial CP2 Checklist Alignment Note
- Verification of all CP2_CHECKLIST sections with Wave 1
- Role clarification and recommendations
- ETA alignment verification

---

**Last Updated**: 2025-01-27  
**Checkpoint**: CP2-LC  
**Status**: Alignment Verified, Recommendations Provided

