# Router Message Intake Stage 2: CP-Level Summary

**Date**: 2025-01-27  
**Phase**: Этап 2. Message intake и базовая валидация  
**Status**: ✅ **COMPLETED**  
**CP Level**: CP2-LC (extends CP1 baseline)

## Executive Summary

Этап 2 (Stage 2) формализует и доводит до production-уровня входной поток сообщений Router'а через три последовательных шага:

1. **Stage 2.1**: Миграция `decide` subject на JetStream с durable subscriptions
2. **Stage 2.2**: Унифицированный слой валидации для всех входящих сообщений
3. **Stage 2.3**: Детерминированная обработка ошибок с кодами, audit, метриками и DLQ

**Результат**: Router гарантирует, что любое сообщение, достигшее бизнес-логики, соответствует схеме, версии и корреляционным полям, или детерминированно отклоняется с полным audit trail.

## Stage 2.1: JetStream Migration for Decide Subject

### Achievements

- ✅ **Миграция на JetStream**: `router_decide_consumer.erl` заменяет `router_nats_subscriber.erl`
- ✅ **Durable Subscriptions**: Сообщения не теряются при перезапуске Router'а
- ✅ **Queue Groups**: Горизонтальное масштабирование через `router-decide-group`
- ✅ **MaxDeliver Exhaustion**: ETS-based tracking предотвращает бесконечные retries
- ✅ **Explicit ACK Policy**: Гарантия обработки сообщений

### Key Components

- **Module**: `router_decide_consumer.erl` (new)
- **Configuration**: `nats_js_durable_group_decide`, `nats_js_deliver_group_decide`, `nats_js_max_deliver`
- **Tests**: `router_decide_consumer_SUITE.erl` (10 test cases)
- **Documentation**: Updated `PROTO_NATS_MAPPING.md`, `NATS_SUBJECTS.md`

### CP2 Impact

- **CP1 Baseline**: Basic NATS pub/sub (request-reply)
- **CP2 Enhancement**: JetStream durable subscriptions with ACK/NAK semantics
- **Backward Compatibility**: CP1 Gateway can still publish to JetStream stream

## Stage 2.2: Unified Intake Validation Layer

### Achievements

- ✅ **Unified Validator**: `router_intake_validator.erl` - единый слой валидации для всех subjects
- ✅ **Protobuf Decode**: `decide` messages используют protobuf decode (fallback на JSON)
- ✅ **Version Validation**: Multi-source version extraction (subject + payload + headers)
- ✅ **Correlation Fields**: Полная валидация `tenant_id`, `run_id`, `flow_id`, `step_id`, `idempotency_key`, `trace_id`
- ✅ **Format Validation**: UUID v4, ULID, W3C Trace Context validation
- ✅ **Tenant Validation**: Интеграция с `router_tenant_validator`
- ✅ **Idempotency Check**: Интеграция с `router_idempotency` для duplicate detection

### Key Components

- **Module**: `router_intake_validator.erl` (new)
- **Integration**: `router_decide_consumer.erl`, `router_result_consumer.erl`, `router_ack_consumer.erl`
- **Validation Types**:
  - Schema validation (protobuf decode for decide, JSON for result/ack)
  - Version validation (multi-source extraction)
  - Correlation fields validation (format + dependencies)
  - Tenant validation (allowlist + ACL)
  - Idempotency check (duplicate detection)

### CP2 Impact

- **CP1 Baseline**: Basic JSON parsing, minimal validation
- **CP2 Enhancement**: Unified validation layer with protobuf support, format validation, tenant/idempotency checks
- **Guarantee**: Any message reaching business logic is guaranteed to be schema-compliant, version-compatible, and have all correlation fields

## Stage 2.3: Deterministic Error Handling

### Achievements

- ✅ **Error Codes**: `router_intake_error_codes.erl` - стандартизированные коды ошибок
- ✅ **Error Handler**: `router_intake_error_handler.erl` - централизованная обработка ошибок
- ✅ **DLQ Support**: Configurable DLQ subject patterns, payload hash (not full payload)
- ✅ **Audit Logging**: Structured JSON audit entries with PII filtering
- ✅ **Metrics**: Telemetry metrics for validation errors, DLQ events, failures
- ✅ **MaxDeliver Exhaustion**: Detection and handling to prevent infinite retries
- ✅ **Gateway Integration**: Error code mapping to HTTP 4xx/5xx for external systems

### Key Components

- **Modules**: 
  - `router_intake_error_codes.erl` (new) - 6 error codes
  - `router_intake_error_handler.erl` (new) - error handling orchestration
- **Error Codes**:
  - `SCHEMA_VALIDATION_FAILED` - schema/parsing errors
  - `VERSION_UNSUPPORTED` - unsupported protocol version
  - `CORRELATION_FIELDS_INVALID` - correlation field issues
  - `TENANT_FORBIDDEN` - tenant ACL violations
  - `IDEMPOTENCY_VIOLATION` - duplicate detection
  - `INTERNAL_VALIDATION_ERROR` - internal errors
- **Error Handling Flow**:
  1. Audit logging (structured JSON, PII filtered)
  2. Metrics emission (telemetry)
  3. DLQ publication (for schema errors, best-effort)
  4. NATS message fate (ACK/NAK based on error type)
  5. Error response (for request-reply pattern)

### CP2 Impact

- **CP1 Baseline**: Basic error responses, no DLQ, minimal audit
- **CP2 Enhancement**: Deterministic error handling with DLQ, comprehensive audit trail, standardized error codes, Gateway-compatible error mapping

## Technical Deliverables

### New Modules

1. **`router_decide_consumer.erl`**: JetStream-based decide message consumer
2. **`router_intake_validator.erl`**: Unified intake validation layer
3. **`router_intake_error_codes.erl`**: Standardized error code definitions
4. **`router_intake_error_handler.erl`**: Centralized error handling orchestration

### Modified Modules

1. **`router_result_consumer.erl`**: Integrated intake validator
2. **`router_ack_consumer.erl`**: Integrated intake validator
3. **`router_metrics.erl`**: Added intake validation metrics
4. **`beamline_router_sup.erl`**: Updated supervisor tree

### Configuration

- **JetStream**: `nats_js_durable_group_decide`, `nats_js_deliver_group_decide`, `nats_js_max_deliver`, `nats_js_ack_wait_seconds`, `nats_js_backoff_seconds`
- **DLQ**: `dlq_enabled`, `dlq_subject_pattern`
- **Idempotency**: `idempotency_ttl_seconds`
- **Tenant Validation**: `result_ack_allowed_tenants`

### Tests

- **`router_decide_consumer_SUITE.erl`**: 10 test cases
- **`router_intake_error_codes_SUITE.erl`**: 9 test cases
- **`router_intake_error_handler_SUITE.erl`**: 9 test cases
- **`router_intake_e2e_SUITE.erl`**: 16 test cases (13 basic + 3 hard failure scenarios)

### Documentation

- **`INTAKE_ERROR_HANDLING.md`**: Complete error handling specification
- **`PROTO_NATS_MAPPING.md`**: Updated with ExecResult and ExecAssignmentAck contracts
- **`NATS_SUBJECTS.md`**: Updated with JetStream and DLQ details
- **`ROUTER_INTAKE_E2E_TEST_CHECKLIST.md`**: Comprehensive e2e test checklist
- **`ROUTER_CONTRACTS_VERIFICATION_REPORT.md`**: Contract verification report

## Metrics Added

- `router_intake_validation_errors_total` - validation error counter
- `router_intake_messages_total` - total messages processed
- `router_intake_dlq_messages_total` - DLQ publication counter
- `router_intake_dlq_publish_failed_total` - DLQ publication failures
- `router_jetstream_maxdeliver_exhausted_total` - MaxDeliver exhaustion counter

## Key Invariants Established

1. **Schema Compliance**: All messages reaching business logic are schema-compliant (protobuf or JSON)
2. **Version Compatibility**: All messages belong to supported protocol version
3. **Correlation Fields**: All messages have complete correlation fields (tenant_id, run_id, flow_id, step_id, idempotency_key, trace_id)
4. **Deterministic Rejection**: Invalid messages are deterministically rejected with audit trail and DLQ
5. **No Data Loss**: Messages not ACKed remain in JetStream for redelivery
6. **Graceful Degradation**: Router continues operating during network/NATS failures

## CP1 vs CP2 Comparison

| Feature | CP1 Baseline | CP2 Enhancement (Stage 2) |
|---------|--------------|--------------------------|
| **NATS Integration** | Basic pub/sub | JetStream durable subscriptions |
| **Message Validation** | Basic JSON parsing | Unified validation layer (protobuf + JSON) |
| **Error Handling** | Basic error responses | Deterministic error handling with DLQ |
| **Audit Trail** | Minimal logging | Comprehensive structured audit logs |
| **Metrics** | Basic counters | Detailed intake validation metrics |
| **Idempotency** | Not required | ETS-based duplicate detection |
| **Tenant Validation** | Not required | Allowlist + ACL validation |
| **DLQ Support** | Not available | Configurable DLQ with payload hash |

## Production Readiness

### ✅ Completed

- All three stages implemented and tested
- Comprehensive test coverage (44 test cases across 4 suites)
- Complete documentation with examples
- Error handling with graceful degradation
- DLQ support for invalid messages
- Audit trail for all validation failures
- Metrics for monitoring and alerting

### 🔄 Future Enhancements (Post-CP2)

- Protobuf decode for result/ack messages (when contracts ready)
- Enhanced e2e tests for hard failure scenarios
- Gateway contract integration verification

## References

- **Implementation Reports**:
  - `ROUTER_MESSAGE_INTAKE_IMPLEMENTATION_REPORT.md` - Initial implementation
  - `ROUTER_MESSAGE_INTAKE_FINAL_REPORT.md` - Final implementation
  - `ROUTER_INTAKE_VALIDATOR_COMPLETE_REPORT.md` - Validator completion
  - `ROUTER_DECIDE_VALIDATION_IMPROVEMENTS_REPORT.md` - Decide validation improvements
  - `ROUTER_DLQ_IMPROVEMENTS_REPORT.md` - DLQ improvements
- **Test Reports**:
  - `ROUTER_INTAKE_TESTS_RUN_REPORT.md` - Full test run report (e2e, chaos, load tests)
- **Specifications**:
  - `ROUTER_MESSAGE_INTAKE_VALIDATION_SPEC.md` - Stage 2.2 specification
  - `ROUTER_INTAKE_ERROR_HANDLING_SPEC.md` - Stage 2.3 specification
- **Documentation**:
  - `INTAKE_ERROR_HANDLING.md` - Error handling guide
  - `ROUTER_INTAKE_E2E_TEST_CHECKLIST.md` - E2E test checklist
  - `ROUTER_CONTRACTS_VERIFICATION_REPORT.md` - Contract verification

