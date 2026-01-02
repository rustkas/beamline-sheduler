# Router Intake E2E Test Checklist

**Date**: 2025-01-27  
**Purpose**: Comprehensive checklist for Router intake validation and DLQ e2e tests  
**Test Suite**: `router_intake_e2e_SUITE.erl`

**AS-IS NOTE (repo audit)**:

- The suite `apps/otp/router/test/router_intake_e2e_SUITE.erl` was **not found** in the current repository state.
- Treat this document as a **checklist/specification** of intended E2E coverage, not as evidence that the suite exists or passes today.

## Test Coverage Overview

**Total Tests**: 16 e2e tests (13 basic + 3 hard failure scenarios)  
**Coverage**: 
- Decide, Result, ACK validation flows with DLQ, audit, metrics (13 tests)
- Hard failure scenarios: network flakiness, NATS unavailability, partial DLQ failure (3 tests)

## Decide Validation Tests

### ✅ 1. Successful Validation Flow

**Test**: `test_e2e_decide_validation_success`

**Checklist**:
- [ ] Valid decide request (all required fields present)
- [ ] Protobuf decode succeeds
- [ ] Version validation passes (`version: "1"`)
- [ ] Correlation fields validation passes (tenant_id, run_id, flow_id, step_id if present)
- [ ] Tenant validation passes (if enabled)
- [ ] Idempotency check passes (new request)
- [ ] Message ACKed after successful processing
- [ ] Response published to reply subject
- [ ] No DLQ publication
- [ ] No audit error logs
- [ ] Metrics: `router_intake_messages_total{status="ok"}` incremented

**Expected Behavior**:
- Message processed successfully
- Router publishes `DecideResponse` to reply subject
- NATS message ACKed

### ✅ 2. Schema Validation Error

**Test**: `test_e2e_decide_validation_schema_error`

**Checklist**:
- [ ] Invalid payload (not valid JSON or protobuf)
- [ ] Protobuf decode fails
- [ ] Error code: `SCHEMA_VALIDATION_FAILED`
- [ ] DLQ publication triggered
- [ ] DLQ subject: `beamline.router.v1.decide.dlq` (or configured pattern)
- [ ] DLQ payload contains:
  - [ ] `original_subject`
  - [ ] `original_payload_hash` (SHA256)
  - [ ] `validation_error.code` = `"SCHEMA_VALIDATION_FAILED"`
  - [ ] `validation_error.message`
  - [ ] `validation_error.severity` = `"error"`
  - [ ] `context` (filtered, no PII)
  - [ ] `received_at` timestamp
  - [ ] `router_node_id`
- [ ] Audit log entry created:
  - [ ] `event_type` = `"router.intake.validation_failed"`
  - [ ] `error_code` = `"SCHEMA_VALIDATION_FAILED"`
  - [ ] `error_message` present
  - [ ] `subject` = `"beamline.router.v1.decide"`
  - [ ] `received_at` timestamp
  - [ ] `router_node_id` present
- [ ] Metrics emitted:
  - [ ] `router_intake_validation_errors_total{error_code="SCHEMA_VALIDATION_FAILED"}`
  - [ ] `router_intake_dlq_messages_total{reason="validation_failed"}`
  - [ ] `router_intake_messages_total{status="failed"}`
- [ ] NATS message ACKed (don't retry)
- [ ] Error response published (if request-reply pattern)

**Expected Behavior**:
- Message rejected at schema validation stage
- DLQ publication succeeds
- Audit log entry created
- Metrics incremented
- Message ACKed (no retry)

### ✅ 3. Version Validation Error

**Test**: `test_e2e_decide_validation_version_error`

**Checklist**:
- [ ] Request with unsupported version (`version: "2"`)
- [ ] Version validation fails
- [ ] Error code: `VERSION_UNSUPPORTED`
- [ ] DLQ publication triggered
- [ ] Audit log entry created
- [ ] Metrics emitted
- [ ] NATS message ACKed

**Expected Behavior**:
- Message rejected at version validation stage
- DLQ publication succeeds
- Audit log entry created
- Metrics incremented

### ✅ 4. Correlation Fields Validation Error

**Test**: `test_e2e_decide_validation_correlation_error`

**Checklist**:
- [ ] Request with missing tenant_id
- [ ] Correlation fields validation fails
- [ ] Error code: `CORRELATION_FIELDS_INVALID`
- [ ] DLQ publication triggered
- [ ] Audit log entry created
- [ ] Metrics emitted
- [ ] NATS message ACKed

**Expected Behavior**:
- Message rejected at correlation fields validation stage
- DLQ publication succeeds
- Audit log entry created

### ✅ 5. Tenant Validation Error

**Test**: `test_e2e_decide_validation_tenant_error`

**Checklist**:
- [ ] Request with forbidden tenant_id
- [ ] Tenant validation fails (not in allowlist)
- [ ] Error code: `TENANT_FORBIDDEN`
- [ ] DLQ publication triggered
- [ ] Audit log entry created
- [ ] Metrics emitted
- [ ] NATS message ACKed

**Expected Behavior**:
- Message rejected at tenant validation stage
- DLQ publication succeeds
- Audit log entry created

### ✅ 6. DLQ Publication Verification

**Test**: `test_e2e_decide_validation_dlq_publication`

**Checklist**:
- [ ] DLQ subject pattern verification:
  - [ ] Default pattern: `{original_subject}.dlq` (e.g., `beamline.router.v1.decide.dlq`)
  - [ ] Custom pattern: `dlq_subject_pattern` configuration respected
- [ ] DLQ payload structure verification:
  - [ ] `original_subject` present and correct
  - [ ] `original_payload_hash` present (SHA256 hex)
  - [ ] `validation_error` object present with:
    - [ ] `code` (error code string)
    - [ ] `message` (human-readable message)
    - [ ] `severity` (`"error"` or `"warn"`)
  - [ ] `context` present (filtered, no PII)
  - [ ] `received_at` timestamp (milliseconds)
  - [ ] `router_node_id` present
- [ ] DLQ publication success:
  - [ ] `router_nats:publish_with_ack` called
  - [ ] DLQ message published successfully
  - [ ] DLQ metric incremented: `router_intake_dlq_messages_total`
- [ ] DLQ publication failure handling:
  - [ ] Error logged (non-blocking)
  - [ ] DLQ failure metric incremented: `router_intake_dlq_publish_failed_total`
  - [ ] Processing continues (best-effort)

**Expected Behavior**:
- DLQ subject follows configured pattern
- DLQ payload contains all required fields
- DLQ publication succeeds or fails gracefully

### ✅ 7. Audit Logging Verification

**Test**: `test_e2e_decide_validation_audit_logging`

**Checklist**:
- [ ] Audit entry structure verification:
  - [ ] `event_type` = `"router.intake.validation_failed"`
  - [ ] `error_code` present (machine-readable)
  - [ ] `error_message` present (human-readable)
  - [ ] `subject` present (NATS subject)
  - [ ] `tenant_id` present (if available)
  - [ ] `run_id`, `flow_id`, `step_id` present (if available)
  - [ ] `idempotency_key` present (if available)
  - [ ] `trace_id` present (if available)
  - [ ] `received_at` timestamp (milliseconds)
  - [ ] `router_node_id` present
  - [ ] `msg_id` present (if available)
- [ ] Audit logging severity:
  - [ ] ERROR severity for most errors
  - [ ] WARN severity for `IDEMPOTENCY_VIOLATION`
- [ ] PII filtering:
  - [ ] Full payload NOT logged
  - [ ] Secrets NOT logged (api_key, password, token, etc.)
  - [ ] Only metadata and correlation fields logged
- [ ] Audit log format:
  - [ ] Structured JSON format
  - [ ] Via `router_logger:error/warn`

**Expected Behavior**:
- Audit entry created with all required fields
- PII filtered from audit entries
- Severity matches error code

### ✅ 8. Metrics Emission Verification

**Test**: `test_e2e_decide_validation_metrics`

**Checklist**:
- [ ] Validation error metric:
  - [ ] `router_intake_validation_errors_total` emitted
  - [ ] Labels: `error_code`, `subject`, `tenant_id`
  - [ ] Count incremented by 1
- [ ] DLQ metric:
  - [ ] `router_intake_dlq_messages_total` emitted (if DLQ enabled)
  - [ ] Labels: `reason`, `error_code`, `subject`
  - [ ] Count incremented by 1
- [ ] Total messages metric:
  - [ ] `router_intake_messages_total` emitted
  - [ ] Labels: `subject`, `status` (`"ok"` or `"failed"`)
  - [ ] Count incremented by 1
- [ ] DLQ failure metric (if DLQ publication fails):
  - [ ] `router_intake_dlq_publish_failed_total` emitted
  - [ ] Labels: `reason`, `error_code`, `subject`, `failure_reason`
  - [ ] Count incremented by 1
- [ ] Metric emission via telemetry:
  - [ ] `telemetry:execute` called with correct event name
  - [ ] Measurements: `#{count => 1}`
  - [ ] Metadata: correct labels

**Expected Behavior**:
- All metrics emitted with correct labels
- Counts incremented correctly
- Metrics available for monitoring

### ✅ 9. Error Response Verification

**Test**: `test_e2e_decide_validation_error_response`

**Checklist**:
- [ ] Error response published (for request-reply pattern):
  - [ ] Reply subject: `{original_subject}.reply` (e.g., `beamline.router.v1.decide.reply`)
  - [ ] Response format:
    - [ ] `ok` = `false`
    - [ ] `error.code` = Gateway-compatible code (e.g., `"invalid_request"`)
    - [ ] `error.message` = human-readable message
    - [ ] `error.intake_error_code` = original intake error code (e.g., `"SCHEMA_VALIDATION_FAILED"`)
    - [ ] `context.request_id` present (if available)
    - [ ] `context.trace_id` present (if available)
- [ ] Error code mapping:
  - [ ] `SCHEMA_VALIDATION_FAILED` → `invalid_request` (HTTP 400)
  - [ ] `VERSION_UNSUPPORTED` → `invalid_request` (HTTP 400)
  - [ ] `CORRELATION_FIELDS_INVALID` → `invalid_request` (HTTP 400)
  - [ ] `TENANT_FORBIDDEN` → `unauthorized` (HTTP 401)
  - [ ] `IDEMPOTENCY_VIOLATION` → `invalid_request` (HTTP 400)
  - [ ] `INTERNAL_VALIDATION_ERROR` → `internal` (HTTP 500)
- [ ] Response published via `router_nats:publish`

**Expected Behavior**:
- Error response published to reply subject
- Gateway-compatible error code included
- Original intake error code included for debugging

## Result Validation Tests

### ✅ 10. Successful Result Validation

**Test**: `test_e2e_result_validation_success`

**Checklist**:
- [ ] Valid result message (all required fields present)
- [ ] JSON decode succeeds (CP1 baseline, protobuf when ready)
- [ ] Version validation passes
- [ ] Correlation fields validation passes
- [ ] Message ACKed after successful processing
- [ ] No DLQ publication
- [ ] No audit error logs
- [ ] Metrics: `router_intake_messages_total{status="ok"}` incremented

**Expected Behavior**:
- Result message processed successfully
- NATS message ACKed

### ✅ 11. Result Validation Error

**Test**: `test_e2e_result_validation_error`

**Checklist**:
- [ ] Invalid result message (invalid JSON/protobuf)
- [ ] Schema validation fails
- [ ] Error code: `SCHEMA_VALIDATION_FAILED`
- [ ] DLQ publication triggered
- [ ] DLQ subject: `caf.exec.result.v1.dlq` (or configured pattern)
- [ ] Audit log entry created
- [ ] Metrics emitted
- [ ] NATS message ACKed

**Expected Behavior**:
- Result message rejected at validation stage
- DLQ publication succeeds
- Audit log entry created

## ACK Validation Tests

### ✅ 12. Successful ACK Validation

**Test**: `test_e2e_ack_validation_success`

**Checklist**:
- [ ] Valid ack message (all required fields present)
- [ ] JSON decode succeeds (CP1 baseline, protobuf when ready)
- [ ] Version validation passes
- [ ] Correlation fields validation passes
- [ ] Message ACKed after successful processing
- [ ] No DLQ publication
- [ ] No audit error logs
- [ ] Metrics: `router_intake_messages_total{status="ok"}` incremented

**Expected Behavior**:
- ACK message processed successfully
- NATS message ACKed

### ✅ 13. ACK Validation Error

**Test**: `test_e2e_ack_validation_error`

**Checklist**:
- [ ] Invalid ack message (invalid JSON/protobuf)
- [ ] Schema validation fails
- [ ] Error code: `SCHEMA_VALIDATION_FAILED`
- [ ] DLQ publication triggered
- [ ] DLQ subject: `caf.exec.assign.v1.ack.dlq` (or configured pattern)
- [ ] Audit log entry created
- [ ] Metrics emitted
- [ ] NATS message ACKed

**Expected Behavior**:
- ACK message rejected at validation stage
- DLQ publication succeeds
- Audit log entry created

## Additional Test Scenarios (Future)

### MaxDeliver Exhaustion

**Checklist**:
- [ ] Message exceeds `max_deliver` attempts
- [ ] MaxDeliver exhaustion detected
- [ ] Message ACKed (prevent infinite retries)
- [ ] DLQ publication triggered
- [ ] Audit log entry created
- [ ] Metric: `router_jetstream_maxdeliver_exhausted_total` incremented

### Idempotency Duplicate Handling

**Checklist**:
- [ ] Duplicate request with same `idempotency_key`
- [ ] Idempotency check detects duplicate
- [ ] Message ACKed without processing
- [ ] Cached response returned (if available)
- [ ] No DLQ publication
- [ ] Audit log entry created (WARN severity)
- [ ] Metric: `router_intake_validation_errors_total{error_code="IDEMPOTENCY_VIOLATION"}` incremented

### DLQ Configuration Variations

**Checklist**:
- [ ] DLQ enabled (`dlq_enabled: true`)
- [ ] DLQ disabled (`dlq_enabled: false`) - no DLQ publication
- [ ] Default DLQ subject pattern (`undefined` = append `.dlq`)
- [ ] Custom DLQ subject pattern (e.g., `beamline.router.v1.intake.dlq`)
- [ ] DLQ publication failure handling (best-effort)

### Multiple Error Types

**Checklist**:
- [ ] Schema error → DLQ + audit + metrics
- [ ] Version error → DLQ + audit + metrics
- [ ] Correlation error → DLQ + audit + metrics
- [ ] Tenant error → DLQ + audit + metrics
- [ ] Idempotency violation → audit + metrics (no DLQ)
- [ ] Internal error → NAK (retry) or DLQ (if MaxDeliver exhausted)

## Hard Failure Scenarios (Critical Edge Cases)

### 🔥 14. Network Flakiness (Intermittent Connection Failures)

**Test**: `test_e2e_network_flakiness_intermittent_failures`

**Scenario**: NATS connection experiences intermittent failures during message processing (timeouts, connection drops, partial writes).

**Checklist**:
- [ ] **Setup**: Simulate network flakiness:
  - [ ] NATS connection drops during message processing
  - [ ] NATS publish operations timeout intermittently
  - [ ] Partial TCP writes (connection closes mid-write)
  - [ ] Network latency spikes (delayed responses)
- [ ] **Message Processing During Flakiness**:
  - [ ] Valid decide request arrives during network flakiness
  - [ ] Validation succeeds (local operation, no network dependency)
  - [ ] Response publication fails (NATS timeout/connection error)
  - [ ] Router handles failure gracefully:
    - [ ] Error logged with context (subject, request_id, error type)
    - [ ] Metric: `router_nats_publish_failures_total` incremented
    - [ ] Original NATS message NOT ACKed (will be redelivered)
    - [ ] No crash or exception propagation
- [ ] **DLQ Publication During Flakiness**:
  - [ ] Invalid message arrives during network flakiness
  - [ ] Validation fails (schema error)
  - [ ] DLQ publication attempted but fails (NATS unavailable)
  - [ ] Router handles DLQ failure:
    - [ ] Error logged: `"Failed to publish to DLQ"`
    - [ ] Metric: `router_intake_dlq_publish_failed_total` incremented
    - [ ] Original message ACKed (validation failed, don't retry)
    - [ ] Audit log entry created (despite DLQ failure)
    - [ ] Processing continues (DLQ is best-effort)
- [ ] **Connection Recovery**:
  - [ ] Router detects connection loss (`tcp_closed` or `tcp_error`)
  - [ ] Reconnection logic triggered:
    - [ ] Reconnection attempt logged
    - [ ] Exponential backoff applied (if configured)
    - [ ] Max reconnect attempts respected
  - [ ] After reconnection:
    - [ ] Pending messages can be processed
    - [ ] New messages processed successfully
    - [ ] DLQ publication succeeds (if retried)
- [ ] **Metrics During Flakiness**:
  - [ ] `router_nats_connection_failures_total` incremented
  - [ ] `router_nats_reconnect_attempts_total` incremented
  - [ ] `router_nats_publish_failures_total` incremented
  - [ ] `router_intake_dlq_publish_failed_total` incremented (if DLQ fails)
  - [ ] Connection status tracked (connected/disconnected)
- [ ] **No Data Loss**:
  - [ ] Messages not ACKed remain in JetStream (will be redelivered)
  - [ ] Audit logs created even if DLQ fails
  - [ ] Metrics emitted even if network operations fail
  - [ ] No message loss during flakiness

**Expected Behavior**:
- Router continues operating during network flakiness
- Validation and local operations succeed
- Network operations (publish, DLQ) fail gracefully
- Messages not ACKed are redelivered after recovery
- No crashes or data loss
- Comprehensive error logging and metrics

**Failure Modes to Test**:
- [ ] Connection timeout during publish
- [ ] Connection closed during publish
- [ ] Partial write (connection closes mid-write)
- [ ] NATS server temporarily unavailable (connection refused)
- [ ] Network partition (connection alive but no response)

### 🔥 15. NATS Unavailability (Complete Service Failure)

**Test**: `test_e2e_nats_unavailability_complete_failure`

**Scenario**: NATS service is completely unavailable (server down, network partition, DNS failure).

**Checklist**:
- [ ] **Setup**: Simulate NATS unavailability:
  - [ ] NATS server not running or unreachable
  - [ ] Connection attempts fail immediately
  - [ ] All NATS operations return errors
- [ ] **Initial Connection Failure**:
  - [ ] Router startup with NATS unavailable:
    - [ ] Connection attempt fails
    - [ ] Router logs warning: `"NATS connection failed, using mock mode"`
    - [ ] Router switches to mock mode (if configured)
    - [ ] Router continues running (doesn't crash)
    - [ ] Metric: `router_nats_connection_failures_total` incremented
- [ ] **Message Processing During NATS Unavailability**:
  - [ ] Valid decide request arrives (via JetStream, which may buffer)
  - [ ] Validation succeeds (local operation)
  - [ ] Response publication fails (NATS unavailable):
    - [ ] Error logged: `"Failed to publish response"`
    - [ ] Error type classified: `nats_unavailable` or `connection_failed`
    - [ ] Metric: `router_nats_publish_failures_total{error_kind="nats_unavailable"}` incremented
    - [ ] Original message NOT ACKed (will be redelivered when NATS recovers)
    - [ ] No exception propagation
- [ ] **DLQ Publication During NATS Unavailability**:
  - [ ] Invalid message arrives (via JetStream buffer)
  - [ ] Validation fails (schema error)
  - [ ] DLQ publication attempted but fails (NATS unavailable):
    - [ ] Error logged: `"Failed to publish to DLQ"`
    - [ ] Error reason: `nats_unavailable` or `connection_failed`
    - [ ] Metric: `router_intake_dlq_publish_failed_total{reason="nats_unavailable"}` incremented
    - [ ] Original message ACKed (validation failed, don't retry)
    - [ ] Audit log entry created (despite DLQ failure)
    - [ ] Processing continues (DLQ is best-effort)
- [ ] **Reconnection Attempts**:
  - [ ] Router attempts reconnection:
    - [ ] Reconnection attempt logged
    - [ ] Exponential backoff applied (if configured)
    - [ ] Max reconnect attempts respected (default: 10)
    - [ ] Metric: `router_nats_reconnect_attempts_total` incremented
  - [ ] After max attempts:
    - [ ] Router logs error: `"NATS reconnection exhausted, switching to mock mode"`
    - [ ] Router switches to mock mode (if configured)
    - [ ] Router continues running (doesn't crash)
- [ ] **NATS Recovery**:
  - [ ] NATS service becomes available
  - [ ] Router detects recovery (reconnection succeeds):
    - [ ] Connection restored logged
    - [ ] Reconnect attempts reset to 0
    - [ ] Router switches back to real mode (if was in mock)
  - [ ] After recovery:
    - [ ] Pending messages processed (redelivered by JetStream)
    - [ ] New messages processed successfully
    - [ ] DLQ publication succeeds (if retried)
    - [ ] Response publication succeeds
- [ ] **Metrics During Unavailability**:
  - [ ] `router_nats_connection_failures_total` incremented
  - [ ] `router_nats_reconnect_attempts_total` incremented
  - [ ] `router_nats_publish_failures_total{error_kind="nats_unavailable"}` incremented
  - [ ] `router_intake_dlq_publish_failed_total{reason="nats_unavailable"}` incremented
  - [ ] Connection status: `disconnected` or `mock`
- [ ] **No Data Loss**:
  - [ ] Messages not ACKed remain in JetStream (will be redelivered)
  - [ ] Audit logs created even if NATS unavailable
  - [ ] Metrics emitted even if NATS unavailable
  - [ ] No message loss during unavailability
  - [ ] Router state preserved (no corruption)

**Expected Behavior**:
- Router continues operating during NATS unavailability
- Validation and local operations succeed
- Network operations fail gracefully with proper error classification
- Messages not ACKed are redelivered after NATS recovery
- No crashes or data loss
- Comprehensive error logging and metrics
- Reconnection logic works correctly

**Failure Modes to Test**:
- [ ] NATS server completely down
- [ ] Network partition (Router can't reach NATS)
- [ ] DNS failure (can't resolve NATS hostname)
- [ ] Firewall blocking NATS port
- [ ] NATS server overloaded (connection refused)

### 🔥 16. Partial DLQ Failure (DLQ Service Degraded)

**Test**: `test_e2e_partial_dlq_failure_degraded_service`

**Scenario**: DLQ publication partially fails (some messages succeed, some fail, or DLQ subject doesn't exist, or DLQ service is degraded).

**Checklist**:
- [ ] **Setup**: Simulate partial DLQ failure:
  - [ ] DLQ subject doesn't exist (404-like error)
  - [ ] DLQ service degraded (timeouts, rate limiting)
  - [ ] DLQ service intermittently available
  - [ ] DLQ subject permissions issue (403-like error)
- [ ] **DLQ Publication Failure - Subject Not Found**:
  - [ ] Invalid message arrives (schema error)
  - [ ] Validation fails
  - [ ] DLQ publication attempted but fails (subject doesn't exist):
    - [ ] Error logged: `"Failed to publish to DLQ"`
    - [ ] Error reason: `subject_not_found` or `invalid_subject`
    - [ ] Metric: `router_intake_dlq_publish_failed_total{reason="subject_not_found"}` incremented
    - [ ] Original message ACKed (validation failed, don't retry)
    - [ ] Audit log entry created (despite DLQ failure)
    - [ ] Processing continues (DLQ is best-effort)
- [ ] **DLQ Publication Failure - Timeout**:
  - [ ] Invalid message arrives (version error)
  - [ ] Validation fails
  - [ ] DLQ publication attempted but times out:
    - [ ] Error logged: `"Failed to publish to DLQ"`
    - [ ] Error reason: `timeout` or `publish_timeout`
    - [ ] Metric: `router_intake_dlq_publish_failed_total{reason="timeout"}` incremented
    - [ ] Original message ACKed
    - [ ] Audit log entry created
    - [ ] Processing continues
- [ ] **DLQ Publication Failure - Rate Limiting**:
  - [ ] Multiple invalid messages arrive rapidly
  - [ ] Some DLQ publications succeed, some fail (rate limited):
    - [ ] Successful DLQ publications: metric `router_intake_dlq_messages_total` incremented
    - [ ] Failed DLQ publications: metric `router_intake_dlq_publish_failed_total{reason="rate_limited"}` incremented
    - [ ] All original messages ACKed (validation failed)
    - [ ] All audit log entries created
    - [ ] Processing continues for all messages
- [ ] **DLQ Publication Failure - Permission Denied**:
  - [ ] Invalid message arrives (correlation error)
  - [ ] Validation fails
  - [ ] DLQ publication attempted but fails (permission denied):
    - [ ] Error logged: `"Failed to publish to DLQ"`
    - [ ] Error reason: `permission_denied` or `unauthorized`
    - [ ] Metric: `router_intake_dlq_publish_failed_total{reason="permission_denied"}` incremented
    - [ ] Original message ACKed
    - [ ] Audit log entry created
    - [ ] Processing continues
- [ ] **Mixed Success/Failure Scenarios**:
  - [ ] Batch of invalid messages arrives:
    - [ ] Some DLQ publications succeed (messages 1, 3, 5)
    - [ ] Some DLQ publications fail (messages 2, 4, 6)
    - [ ] All messages ACKed (validation failed)
    - [ ] All audit log entries created
    - [ ] Metrics reflect both successes and failures:
      - [ ] `router_intake_dlq_messages_total` = 3 (successes)
      - [ ] `router_intake_dlq_publish_failed_total` = 3 (failures)
- [ ] **DLQ Service Recovery**:
  - [ ] DLQ service becomes available again
  - [ ] New invalid messages arrive
  - [ ] DLQ publication succeeds:
    - [ ] DLQ message published successfully
    - [ ] Metric: `router_intake_dlq_messages_total` incremented
    - [ ] No DLQ failure metric incremented
- [ ] **Metrics During Partial DLQ Failure**:
  - [ ] `router_intake_dlq_messages_total` incremented (for successes)
  - [ ] `router_intake_dlq_publish_failed_total` incremented (for failures)
  - [ ] Failure reasons tracked: `subject_not_found`, `timeout`, `rate_limited`, `permission_denied`
  - [ ] Labels include: `error_code`, `subject`, `failure_reason`
- [ ] **No Data Loss**:
  - [ ] All original messages ACKed (validation failed)
  - [ ] All audit log entries created (even if DLQ fails)
  - [ ] Metrics reflect all operations (successes and failures)
  - [ ] No message loss during DLQ failures
  - [ ] Router continues processing new messages

**Expected Behavior**:
- Router handles DLQ failures gracefully (best-effort)
- Original messages ACKed even if DLQ fails
- Audit logs created even if DLQ fails
- Metrics track both successes and failures
- No crashes or data loss
- Router continues processing new messages
- DLQ recovery works correctly

**Failure Modes to Test**:
- [ ] DLQ subject doesn't exist
- [ ] DLQ service timeout
- [ ] DLQ service rate limiting
- [ ] DLQ permission denied
- [ ] DLQ service degraded (intermittent failures)
- [ ] DLQ service overloaded (slow responses)

**Critical Invariants**:
- [ ] **DLQ failures never block message processing**
- [ ] **Audit logs always created (even if DLQ fails)**
- [ ] **Original messages always ACKed (validation failed)**
- [ ] **Metrics always emitted (successes and failures)**
- [ ] **No exception propagation from DLQ failures**

## Test Execution

### Prerequisites

- [ ] Router application started
- [ ] NATS mock configured (`nats_mode: mock`)
- [ ] DLQ enabled (`dlq_enabled: true`)
- [ ] Telemetry enabled (`telemetry_enabled: true`)
- [ ] All required mocks configured (router_nats, router_logger, telemetry)

### Running Tests

```bash
cd apps/otp/router
rebar3 ct --suite router_intake_e2e_SUITE
```

### Expected Results

- [ ] All 16 tests pass (13 basic + 3 hard failure scenarios)
- [ ] No test failures or errors
- [ ] All assertions verified
- [ ] All mocks called as expected
- [ ] Hard failure scenarios verify graceful degradation

## Verification Points

### DLQ Verification

- [ ] DLQ subject correct (default or configured pattern)
- [ ] DLQ payload structure complete
- [ ] Payload hash present (SHA256)
- [ ] No full payload in DLQ (security)
- [ ] Error details present
- [ ] Context filtered (no PII)

### Audit Verification

- [ ] Audit entry structure complete
- [ ] All required fields present
- [ ] PII filtered
- [ ] Severity correct
- [ ] Timestamp present
- [ ] Router node ID present

### Metrics Verification

- [ ] All metrics emitted
- [ ] Labels correct
- [ ] Counts incremented
- [ ] No missing metrics

### Error Response Verification

- [ ] Error response published (request-reply pattern)
- [ ] Gateway-compatible error code
- [ ] Original intake error code included
- [ ] Context fields present

## References

- **Test Suite**: `apps/otp/router/test/router_intake_e2e_SUITE.erl`
- **Error Codes**: `apps/otp/router/src/router_intake_error_codes.erl`
- **Error Handler**: `apps/otp/router/src/router_intake_error_handler.erl`
- **Validator**: `apps/otp/router/src/router_intake_validator.erl`
- **Documentation**: `apps/otp/router/docs/INTAKE_ERROR_HANDLING.md`

