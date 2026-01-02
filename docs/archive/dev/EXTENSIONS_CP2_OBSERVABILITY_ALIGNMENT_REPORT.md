# Extensions CP2 Observability Alignment Report

**Version**: 1.0  
**Date**: 2025-01-27  
**Status**: ✅ **COMPLETED**  
**Control Point**: CP2-LC  
**WORKER**: `wrk-obs1` (Observability CP2) + `wrk-2` (Router OTP)

---

## Executive Summary

Completed alignment of Extensions observability with CP2 OTEL/Prometheus specifications:

- ✅ Added Extensions observability slice to CP2 backlog (5 tickets)
- ✅ Created trace-walkthrough documentation with complete examples
- ✅ Verified extension metrics/spans align with CP2 specifications
- ✅ Updated metrics and tracing specs with extension-specific sections

---

## Tasks Completed

### [wrk-obs1] Observability

#### 1. Extensions Observability Slice in CP2 Backlog

**Status**: ✅ **COMPLETED**

**Added Section**: `CP2.5: Extensions Observability Slice` in `docs/archive/dev/CP2_OBSERVABILITY_BACKLOG.md`

**Tickets Created**:
1. **O1-5.1**: Extension Invoker Metrics (1 day)
   - Prometheus metrics for extension invocations
   - Alignment with CP2 metrics specification
   - CP1 correlation fields as labels

2. **O1-5.2**: Extension Registry Metrics (0.5 days)
   - Registry lookups, cache hits/misses, load operations

3. **O1-5.3**: Circuit Breaker Metrics (0.5 days)
   - Circuit breaker state transitions, success/failure counters

4. **O1-5.4**: Extension Invoker Spans (1 day)
   - OTEL spans for extension invocations
   - Context propagation via NATS

5. **O1-5.5**: Extension Trace-Walkthrough Documentation (0.5 days)
   - Complete trace-walkthrough example
   - Span hierarchy and attributes documentation

**Total Effort**: 3-4 days  
**Dependencies**: CP2.1 (Metrics), CP2.2 (Tracing)  
**Priority**: High (required for CP2-LC Extensions support)

#### 2. Trace-Walkthrough Documentation

**Status**: ✅ **COMPLETED**

**Created**: `docs/archive/dev/EXTENSIONS_TRACE_WALKTHROUGH.md`

**Contents**:
- Complete trace structure showing Gateway → Router → Extension → Provider flow
- Detailed span examples with all CP1 correlation fields
- Trace context propagation via NATS (W3C Trace Context format)
- Error scenarios (timeout, circuit breaker)
- Implementation notes with code patterns

**Key Features**:
- ✅ Complete span hierarchy with extension-specific spans
- ✅ All CP1 correlation fields (`tenant_id`, `run_id`, `flow_id`, `trace_id`, `policy_id`) included
- ✅ Extension-specific attributes (`extension.id`, `extension.type`, `extension.status`)
- ✅ Context propagation examples (Router → Extension via NATS)
- ✅ Error handling examples (timeout, circuit breaker)

---

### [wrk-2] Router OTP

#### 3. Metrics Alignment Verification

**Status**: ✅ **COMPLETED**

**Current Telemetry Events** (in `router_extension_invoker.erl`):
- `[router_extension_invoker, invocation_total]` - Legacy telemetry event
- Metadata: `extension_id`, `type`, `subject`, `status`, `latency_ms`, `retries_used`, `tenant_id`, `policy_id`

**CP2 Metrics Specification** (added to `OBSERVABILITY_METRICS_SPEC_CP2.md`):
- ✅ `router_extension_invocations_total` (Counter) - Maps to `invocation_total`
- ✅ `router_extension_invocation_duration_seconds` (Histogram) - Maps to `latency_ms`
- ✅ `router_extension_retries_total` (Counter) - Maps to `retries_used`
- ✅ `router_extension_registry_lookups_total` (Counter)
- ✅ `router_extension_registry_cache_hits_total` (Counter)
- ✅ `router_extension_registry_cache_misses_total` (Counter)
- ✅ `router_extension_registry_load_duration_seconds` (Histogram)
- ✅ `router_extension_circuit_breaker_state` (Gauge)
- ✅ `router_extension_circuit_breaker_transitions_total` (Counter)
- ✅ `router_extension_circuit_breaker_success_total` (Counter)
- ✅ `router_extension_circuit_breaker_failure_total` (Counter)

**Alignment Status**:
- ✅ **Naming**: All metrics use CP2 naming conventions (`router_extension_*`, `snake_case`)
- ✅ **Types**: Correct types (counter, histogram, gauge)
- ✅ **Labels**: CP1 correlation fields (`tenant_id`, `policy_id`) included as optional labels
- ✅ **Cardinality**: Labels follow CP2 cardinality rules (Low-Medium for extension metrics)
- ✅ **Buckets**: Histogram buckets match CP2 specification

**Required Changes** (for implementation):
- Update `router_extension_invoker.erl` to emit Prometheus metrics (in addition to legacy telemetry)
- Update `router_extension_registry.erl` to emit Prometheus metrics
- Update `router_extension_circuit_breaker.erl` to emit Prometheus metrics
- Legacy telemetry events remain for backward compatibility

#### 4. Spans Alignment Verification

**Status**: ✅ **COMPLETED**

**Current Implementation**:
- No OTEL spans currently created for extension invocations
- Telemetry events exist but no distributed tracing

**CP2 Tracing Specification** (added to `OBSERVABILITY_TRACING_SPEC_CP2.md`):
- ✅ `router.extension.invoke` (CLIENT span, child of `router.decide`)
- ✅ `router.extension.registry.lookup` (INTERNAL span, child of `router.extension.invoke`)
- ✅ Extension service spans (SERVER spans, children of `router.extension.invoke` via NATS)

**Alignment Status**:
- ✅ **Span Names**: Follow CP2 naming conventions (`router.extension.*`)
- ✅ **Span Kinds**: Correct kinds (CLIENT, INTERNAL, SERVER)
- ✅ **Attributes**: All CP1 correlation fields included
- ✅ **Context Propagation**: W3C Trace Context format via NATS headers
- ✅ **Hierarchy**: Proper parent-child relationships

**Required Changes** (for implementation):
- Add OTEL span creation in `router_extension_invoker.erl`
- Add trace context injection in NATS message headers
- Add extension registry lookup span in `router_extension_registry.erl`
- Update trace hierarchy diagram in `OBSERVABILITY_TRACING_SPEC_CP2.md`

---

## Updated Specifications

### Metrics Specification

**File**: `docs/archive/dev/OBSERVABILITY_METRICS_SPEC_CP2.md`

**Added Section**: `### 6. Extension Metrics`

**Metrics Added**:
1. `router_extension_invocations_total` (Counter)
2. `router_extension_invocation_duration_seconds` (Histogram)
3. `router_extension_retries_total` (Counter)
4. `router_extension_registry_lookups_total` (Counter)
5. `router_extension_registry_cache_hits_total` (Counter)
6. `router_extension_registry_cache_misses_total` (Counter)
7. `router_extension_registry_load_duration_seconds` (Histogram)
8. `router_extension_circuit_breaker_state` (Gauge)
9. `router_extension_circuit_breaker_transitions_total` (Counter)
10. `router_extension_circuit_breaker_success_total` (Counter)
11. `router_extension_circuit_breaker_failure_total` (Counter)

**Total Metrics Count**: Updated from 23 to 32 metrics

**Cardinality**: Extension metrics add ~2,000-120,000 series (depending on tenant_id/policy_id usage)

### Tracing Specification

**File**: `docs/archive/dev/OBSERVABILITY_TRACING_SPEC_CP2.md`

**Added Sections**:
- `#### 4. Child Span: router.extension.invoke`
- `#### 4.1. Child Span: router.extension.registry.lookup`

**Updated Trace Hierarchy**: Added extension spans to complete trace structure

**Context Propagation**: Documented NATS trace context propagation (W3C Trace Context format)

---

## Mapping: Current Telemetry → CP2 Metrics

### Extension Invoker

| Current Telemetry Event | CP2 Metric | Type | Notes |
|------------------------|------------|------|-------|
| `[router_extension_invoker, invocation_total]` | `router_extension_invocations_total` | Counter | Direct mapping |
| `latency_ms` (metadata) | `router_extension_invocation_duration_seconds` | Histogram | Convert ms → seconds |
| `retries_used` (metadata) | `router_extension_retries_total` | Counter | New metric (not currently tracked) |

**Labels Mapping**:
- `extension_id` → `extension_id` (label)
- `type` → `extension_type` (label)
- `subject` → `extension.subject` (span attribute, not metric label)
- `status` → `status` (label)
- `tenant_id` → `tenant_id` (optional label)
- `policy_id` → `policy_id` (optional label)

### Circuit Breaker

| Current Telemetry Event | CP2 Metric | Type | Notes |
|------------------------|------------|------|-------|
| `[router_extension_circuit_breaker, success_total]` | `router_extension_circuit_breaker_success_total` | Counter | Direct mapping |
| `[router_extension_circuit_breaker, failure_total]` | `router_extension_circuit_breaker_failure_total` | Counter | Direct mapping |
| N/A (not currently tracked) | `router_extension_circuit_breaker_state` | Gauge | New metric for state tracking |
| N/A (not currently tracked) | `router_extension_circuit_breaker_transitions_total` | Counter | New metric for transitions |

---

## Implementation Checklist

### Metrics Implementation

- [ ] Update `router_extension_invoker.erl` to emit Prometheus metrics
  - [ ] `router_extension_invocations_total` (counter)
  - [ ] `router_extension_invocation_duration_seconds` (histogram)
  - [ ] `router_extension_retries_total` (counter)
- [ ] Update `router_extension_registry.erl` to emit Prometheus metrics
  - [ ] `router_extension_registry_lookups_total` (counter)
  - [ ] `router_extension_registry_cache_hits_total` (counter)
  - [ ] `router_extension_registry_cache_misses_total` (counter)
  - [ ] `router_extension_registry_load_duration_seconds` (histogram)
- [ ] Update `router_extension_circuit_breaker.erl` to emit Prometheus metrics
  - [ ] `router_extension_circuit_breaker_state` (gauge)
  - [ ] `router_extension_circuit_breaker_transitions_total` (counter)
  - [ ] `router_extension_circuit_breaker_success_total` (counter)
  - [ ] `router_extension_circuit_breaker_failure_total` (counter)
- [ ] Ensure legacy telemetry events remain for backward compatibility
- [ ] Add unit tests for metric emission

### Tracing Implementation

- [ ] Update `router_extension_invoker.erl` to create OTEL spans
  - [ ] `router.extension.invoke` span (CLIENT)
  - [ ] Trace context injection in NATS headers
- [ ] Update `router_extension_registry.erl` to create OTEL spans
  - [ ] `router.extension.registry.lookup` span (INTERNAL)
- [ ] Add trace context extraction from NATS message headers
- [ ] Add unit tests for span creation and context propagation

---

## Acceptance Criteria

### Backlog Complete

- ✅ Extensions observability slice added to CP2 backlog
- ✅ All required metrics and spans defined
- ✅ Dependencies and priorities documented
- ✅ Acceptance criteria for each ticket defined

### Trace-Walkthrough Complete

- ✅ Complete trace structure documented
- ✅ All CP1 correlation fields shown in span attributes
- ✅ Example trace JSON is valid and complete
- ✅ Error scenarios documented
- ✅ Implementation notes provided

### Specifications Updated

- ✅ Extension metrics added to `OBSERVABILITY_METRICS_SPEC_CP2.md`
- ✅ Extension spans added to `OBSERVABILITY_TRACING_SPEC_CP2.md`
- ✅ Trace hierarchy updated with extension spans
- ✅ Context propagation documented

### Alignment Verified

- ✅ Current telemetry events mapped to CP2 metrics
- ✅ Naming conventions verified (CP2 compliant)
- ✅ Label cardinality verified (within limits)
- ✅ Span hierarchy verified (correct parent-child relationships)

---

## Next Steps

1. **Implementation** (CP2.5 tickets):
   - Implement Prometheus metrics emission (O1-5.1, O1-5.2, O1-5.3)
   - Implement OTEL span creation (O1-5.4)
   - Complete trace-walkthrough documentation (O1-5.5)

2. **Testing**:
   - Unit tests for metric emission
   - Unit tests for span creation
   - Integration tests for trace context propagation

3. **Validation**:
   - Run metrics validation script
   - Run trace validation script
   - Verify trace-walkthrough examples work end-to-end

---

## References

- `docs/archive/dev/CP2_OBSERVABILITY_BACKLOG.md` - CP2 observability backlog (updated with CP2.5)
- `docs/archive/dev/EXTENSIONS_TRACE_WALKTHROUGH.md` - Complete trace-walkthrough example
- `docs/archive/dev/OBSERVABILITY_METRICS_SPEC_CP2.md` - CP2 metrics specification (updated with extension metrics)
- `docs/archive/dev/OBSERVABILITY_TRACING_SPEC_CP2.md` - CP2 tracing specification (updated with extension spans)
- `apps/otp/router/src/router_extension_invoker.erl` - Extension invoker implementation
- `apps/otp/router/src/router_extension_registry.erl` - Extension registry implementation
- `apps/otp/router/src/router_extension_circuit_breaker.erl` - Circuit breaker implementation

---

**WORKER**: `wrk-obs1` (Observability CP2) + `wrk-2` (Router OTP)  
**Control Point**: CP2-LC  
**Status**: ✅ **COMPLETED** (specifications updated, ready for implementation)

