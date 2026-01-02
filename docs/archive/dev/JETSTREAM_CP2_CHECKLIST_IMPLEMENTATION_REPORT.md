# JetStream CP2 Checklist Implementation Report

**AS-IS NOTE (repo audit)**:

- This report references `apps/otp/router/test/router_jetstream_e2e_SUITE.erl` as containing CP2 checklist tests.
- In the current repository state, `router_jetstream_e2e_SUITE.erl` exists but is a **placeholder** (empty suite).
- JetStream-related scenarios currently present in the repository are implemented in suites such as:
  - `apps/otp/router/test/router_jetstream_e2e_integration_SUITE.erl`
  - `apps/otp/router/test/router_jetstream_unit_SUITE.erl`
  but these run with `nats_mode=mock` and mock `router_nats` (they validate behavior, not real NATS/JetStream transport).

Treat this report as **potentially OUTDATED** until the referenced E2E suites are restored or the repository is reconciled.

**Date**: 2025-01-27
**Status**: ✅ **COMPLETED**
**Workers**: wrk-2 (Router/OTP)

---

## Executive Summary

This report documents the implementation of CP2 Checklist tests for JetStream durability & redelivery, as specified in `docs/CP2_CHECKLIST.md`. All required tests have been added to `router_jetstream_e2e_SUITE.erl`, and DLQ payload format has been updated to include context fields (`trace_id`, `tenant_id`, `error_code`). Documentation has been synchronized to reflect the actual DLQ payload structure.

---

## Tasks Completed

### 1. Added CP2 Checklist Tests

**File**: `apps/otp/router/test/router_jetstream_e2e_SUITE.erl`

**Tests Added** (from `docs/CP2_CHECKLIST.md`):

1. ✅ **`test_durable_subscription_survives_restart/1`**
   - **GIVEN**: Message published before Router restart
   - **WHEN**: Router restarts, JetStream consumer is recreated
   - **THEN**: Message is still delivered once; ACK is ok; ack_total counters increase
   - **Reference**: `docs/CP2_CHECKLIST.md#test_durable_subscription_survives_restart`

2. ✅ **`test_redelivery_until_ack_or_maxdeliver/1`**
   - **GIVEN**: Handler artificially returns error N times
   - **WHEN**: MaxDeliver=3, backoff=[1s,5s]
   - **THEN**: Message delivered 3 times; after 3rd error goes to DLQ; `router_redelivery_total{reason="error"} == 2`; `router_dlq_total == 1`
   - **Reference**: `docs/CP2_CHECKLIST.md#test_redelivery_until_ack_or_maxdeliver`

3. ✅ **`test_dlq_payload_contains_context/1`**
   - **Verifies**: DLQ message contains `trace_id`, `tenant_id`, `original_subject`, `error_code`
   - **Reference**: `docs/CP2_CHECKLIST.md#test_dlq_payload_contains_context`

4. ✅ **`test_ack_latency_within_target/1`**
   - **GIVEN**: Normal load (N messages)
   - **WHEN**: All processed without errors
   - **THEN**: p95 ACK latency (via metrics or test measurements) <= target from CP2 profile
   - **Reference**: `docs/CP2_CHECKLIST.md#test_ack_latency_within_target`

### 2. Updated DLQ Payload Format

**File**: `apps/otp/router/src/router_jetstream.erl`

**Changes**:
- ✅ Updated `build_dlq_payload/2` to include `trace_id` and `tenant_id` extracted from original message headers/payload
- ✅ Added `error_code` field derived from `reason`:
  - `maxdeliver_exhausted` → `MAXDELIVER_EXHAUSTED`
  - `validation_failed` → `VALIDATION_FAILED`
  - `processing_error` → `PROCESSING_ERROR`
- ✅ Updated `build_dlq_headers/2` to include `trace_id` and `tenant_id` in DLQ headers

**DLQ Payload Structure** (from `router_jetstream.erl`):
```json
{
  "original_subject": "beamline.router.v1.decide",
  "msg_id": "msg-uuid",
  "reason": "maxdeliver_exhausted",
  "error_code": "MAXDELIVER_EXHAUSTED",
  "timestamp": 1706367600123,
  "trace_id": "trace_def456",
  "tenant_id": "tenant_123",
  "message": {
    "id": "msg-uuid",
    "subject": "beamline.router.v1.decide",
    "headers": {
      "trace_id": "trace_def456",
      "tenant_id": "tenant_123"
    },
    "payload": {...}
  }
}
```

**DLQ Headers**:
```json
{
  "x-dlq-reason": "maxdeliver_exhausted",
  "x-original-msg-id": "msg-uuid",
  "trace_id": "trace_def456",
  "tenant_id": "tenant_123"
}
```

### 3. Updated Documentation

#### `docs/NATS_SUBJECTS.md`

- ✅ Updated DLQ Message Format section with actual implementation from `router_jetstream.erl`
- ✅ Added note about `dlq_include_full_message` configuration option
- ✅ Documented DLQ headers structure

#### `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`

- ✅ Updated "DLQ Message Format" section with source of truth reference (`router_jetstream.erl`)
- ✅ Added `error_code` field to DLQ payload example
- ✅ Added `trace_id` and `tenant_id` as top-level fields in DLQ payload
- ✅ Updated "DLQ Payload Fields" table to include `error_code`, `trace_id`, `tenant_id`
- ✅ Added note about `dlq_include_full_message` configuration option

---

## Verification

- ✅ All 4 CP2 Checklist tests added to `router_jetstream_e2e_SUITE.erl`
- ✅ DLQ payload includes `trace_id`, `tenant_id`, and `error_code` fields
- ✅ DLQ headers include `trace_id` and `tenant_id`
- ✅ Documentation synchronized with actual implementation
- ✅ Source of truth references added to documentation

---

## References

- `docs/CP2_CHECKLIST.md` - CP2 Checklist requirements
- `apps/otp/router/test/router_jetstream_e2e_SUITE.erl` - E2E test suite
- `apps/otp/router/src/router_jetstream.erl` - JetStream implementation
- `docs/NATS_SUBJECTS.md` - NATS subjects documentation
- `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` - Proto to NATS mapping documentation

---

## Conclusion

All CP2 Checklist requirements for JetStream durability & redelivery have been implemented. The tests verify durable subscriptions, MaxDeliver enforcement, DLQ payload context, and ACK latency targets. The DLQ payload format now includes all required context fields for debugging and reprocessing.

