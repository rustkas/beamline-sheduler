---
version: 1.0
authors:
  - WORKER wrk-9: Documentation & Developer Experience
last_update: 2025-01-27T00:00:00Z
status: approved
rule_version: v10
message_protocol: v1
---

# CP2 Readiness: Router + Gateway JetStream Integration

**AS-IS NOTE (repo audit)**:

- This document contains CP2 JetStream design/claims that are **not fully aligned** with the current repository state.
- In current `apps/otp/router/src/router_nats.erl`, the NATS/JetStream *transport layer* is implemented as a **stub** (connection/publish/publish_with_ack/subscribe/ack/nak).
- JetStream-related *processing logic* (ACK/NAK tracking, backoff, metrics, DLQ payload building) is implemented primarily in:
  - `apps/otp/router/src/router_jetstream.erl`
  - `apps/otp/router/src/router_decide_consumer.erl`
  - `apps/otp/router/src/router_result_consumer.erl`
- Several test suite names referenced below exist only as placeholders (empty suites) or are not present in `apps/otp/router/test/`.

Treat this document as **OUTDATED** until reconciled with the current code and test suite inventory.

**Checkpoint**: CP2-LC  
**Components**: `apps/otp/router` (Erlang/OTP) + Gateway (NestJS/C++)  
**Status**: ✅ **COMPLETE**  
**Focus**: JetStream integration, durable subscriptions, ACK/NAK, redelivery

## Purpose

This document provides detailed specifications for JetStream integration in CP2-LC. It complements the overview document (`docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY_OVERVIEW.md`) with JetStream-specific requirements, implementation details, and test coverage.

**Reference**: `docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY_OVERVIEW.md` for overall CP2 readiness context.

---

## JetStream Overview

### What JetStream Adds in CP2

**CP1 Baseline** (for comparison):
- Basic NATS pub/sub (request-reply pattern)
- No message persistence
- No guaranteed delivery
- No redelivery tracking

**CP2 JetStream**:
- **Durable Subscriptions**: Subscriptions survive Router restarts
- **ACK/NAK Semantics**: Explicit acknowledgment of message processing
- **Redelivery Tracking**: Automatic redelivery on NAK with MaxDeliver exhaustion detection
- **Consumer Groups**: Support for multiple Router instances (load balancing)
- **Message Persistence**: Messages persisted in JetStream stream

### Architecture Decision

**ADR-011**: JetStream E2E with Durable Subscriptions (`docs/ADR/ADR-011-jetstream-e2e.md`)

**Key Decision**: Router uses JetStream durable subscriptions instead of basic NATS pub/sub for guaranteed message delivery and redelivery tracking.

---

## Router JetStream Implementation

### Module Structure

**Primary modules (AS-IS)**:

- `apps/otp/router/src/router_jetstream.erl` (JetStream semantics, metrics, delivery tracking, DLQ payload building)
- `apps/otp/router/src/router_nats.erl` (NATS API surface; currently contains stub transport internals)
- `apps/otp/router/src/router_decide_consumer.erl` (consumer logic, ACK/NAK decisions)
- `apps/otp/router/src/router_result_consumer.erl` (consumer logic, ACK/NAK decisions)

**Key Functions**:
- `router_nats:subscribe_jetstream/5` - request durable consumer creation (currently stubbed internally)
- `router_nats:publish/2` - publish (currently stubbed internally)
- `router_nats:publish_with_ack/3` - publish with ack (currently stubbed internally)
- `router_nats:ack_message/1` / `router_nats:nak_message/1` - ack/nak (currently stubbed internally)
- `router_jetstream:ack/1` / `router_jetstream:nak/2,3` - ack/nak wrappers with metrics/backoff

### Durable Subscription Configuration

**Stream Name**: `beamline.router.v1` (configurable via `ROUTER_JETSTREAM_STREAM` env var)

**Consumer Configuration**:
- **Durable Name**: `router-consumer` (configurable via `ROUTER_JETSTREAM_CONSUMER` env var)
- **Ack Policy**: Explicit (manual ACK/NAK)
- **Max Deliver**: 5 (configurable via `ROUTER_JETSTREAM_MAX_DELIVER` env var)
- **Ack Wait**: 30 seconds (configurable via `ROUTER_JETSTREAM_ACK_WAIT` env var)

**Subject Pattern**: `beamline.router.v1.decide` (matches stream subject filter)

### ACK/NAK Semantics

**ACK Conditions** (Router sends ACK):
- Message processed successfully
- Routing decision returned
- No validation errors

**NAK Conditions** (Router sends NAK):
- Validation failures (tenant, policy, message format)
- Policy not found (after fallback attempts)
- Internal errors (non-recoverable)

**NAK Error Details**:
- Error code (e.g., `INVALID_TENANT`, `POLICY_NOT_FOUND`, `INTERNAL_ERROR`)
- Error message (human-readable description)
- Retry hint (whether retry is recommended)

### Redelivery Tracking

**Redelivery Logic**:
1. Router receives message with `redelivery_count` header
2. Router checks `redelivery_count` against `MaxDeliver` limit
3. If `redelivery_count >= MaxDeliver`: Router sends NAK with `MaxDeliverExhausted` error
4. If `redelivery_count < MaxDeliver`: Router processes message normally

**MaxDeliver Exhaustion**:
- Router detects MaxDeliver exhaustion
- Router sends NAK with `MaxDeliverExhausted` error
- Router logs error with message details
- Router emits metric: `router_redelivery_exhausted_total`

**Redelivery Metrics**:
- `router_redelivery_total` - Total redelivery count
- `router_redelivery_exhausted_total` - MaxDeliver exhaustion count
- `router_redelivery_latency_seconds` - Redelivery latency

### Consumer Groups (Load Balancing)

**Multiple Router Instances**:
- Multiple Router instances can subscribe to the same durable consumer
- JetStream distributes messages across Router instances (load balancing)
- Each Router instance processes messages independently
- ACK/NAK from one instance does not affect other instances

**Configuration**:
- Consumer name: `router-consumer` (shared across instances)
- Queue group: `router-group` (optional, for explicit load balancing)

---

## Gateway JetStream Integration

### Gateway → Router Flow

**HTTP Request → NATS JetStream**:
1. Gateway receives HTTP request (`POST /api/v1/routes/decide`)
2. Gateway validates request and extracts headers (`X-Tenant-ID`, `X-Trace-ID`, `X-Idempotency-Key`)
3. Gateway transforms HTTP JSON → NATS JSON payload
4. Gateway publishes to JetStream stream (`beamline.router.v1.decide`)
5. Gateway waits for Router reply (with timeout)

**Router Reply → HTTP Response**:
1. Router processes message and returns `RouteDecision` via NATS reply
2. Gateway receives Router reply
3. Gateway transforms NATS JSON → HTTP JSON
4. Gateway returns HTTP response to client

### ACK/NAK Handling in Gateway

**ACK Handling**:
- Gateway receives ACK from Router (implicit via successful reply)
- Gateway processes Router reply normally
- Gateway returns HTTP 200 OK with `RouteDecision`

**NAK Handling**:
- Gateway receives NAK from Router (via error reply)
- Gateway extracts error code and message from NAK
- Gateway maps NAK error to HTTP status code:
  - `INVALID_TENANT` → HTTP 403 Forbidden
  - `POLICY_NOT_FOUND` → HTTP 404 Not Found
  - `INTERNAL_ERROR` → HTTP 500 Internal Server Error
  - `MaxDeliverExhausted` → HTTP 503 Service Unavailable
- Gateway returns HTTP error response to client

**Redelivery Handling**:
- Gateway does not handle redelivery directly (JetStream handles it)
- Gateway may receive the same message multiple times (if Router NAKs)
- Gateway should handle idempotency (via `idempotency_key`)

---

## Test Coverage

### Router JetStream Tests

**Test Suites (AS-IS inventory)**:

- `apps/otp/router/test/router_jetstream_e2e_SUITE.erl` exists but is currently a **placeholder** (empty suite).
- `apps/otp/router/test/router_jetstream_e2e_integration_SUITE.erl` exists and contains JetStream-related scenarios, but it runs with `nats_mode=mock` and mocks `router_nats` via `meck`.

**Test Cases**:
1. **Durable Subscription Creation**: Verify durable subscription is created on Router startup
2. **ACK on Success**: Verify Router sends ACK on successful message processing
3. **NAK on Validation Failure**: Verify Router sends NAK on validation failures
4. **Redelivery Tracking**: Verify redelivery count is tracked correctly
5. **MaxDeliver Exhaustion**: Verify MaxDeliver exhaustion is detected and logged
6. **Consumer Groups**: Verify multiple Router instances can share consumer group
7. **Restart Recovery**: Verify durable subscription survives Router restart

**Test Suite**: `apps/otp/router/test/router_jetstream_redelivery_SUITE.erl`

AS-IS: suite name is referenced here but was **not found** in `apps/otp/router/test/` during repository audit.

**Test Cases**:
1. **Redelivery on NAK**: Verify message is redelivered after NAK
2. **Redelivery Count Tracking**: Verify redelivery count increments correctly
3. **MaxDeliver Limit**: Verify MaxDeliver limit is enforced
4. **Redelivery Metrics**: Verify redelivery metrics are emitted correctly

### Gateway JetStream Tests

**Test Suite**: Gateway JetStream E2E tests (if exists)

**Test Cases**:
1. **HTTP → JetStream → Router**: Verify Gateway publishes to JetStream and receives Router reply
2. **ACK Handling**: Verify Gateway handles ACK correctly
3. **NAK Handling**: Verify Gateway handles NAK correctly and maps to HTTP status codes
4. **Redelivery Handling**: Verify Gateway handles redelivery scenarios

### E2E JetStream Tests

**Test Suite**: `apps/otp/router/test/router_jetstream_e2e_SUITE.erl`

**Test Cases**:
1. **Full Flow**: HTTP request → Gateway → JetStream → Router → NATS reply → Gateway → HTTP response
2. **Restart Recovery**: Router restart → durable subscription recovery → message processing continues
3. **Load Balancing**: Multiple Router instances → messages distributed correctly
4. **Redelivery E2E**: NAK → redelivery → successful processing

**Reference**: `docs/archive/dev/JETSTREAM_E2E_IDEMPOTENCY_TESTS_REPORT.md` for detailed E2E test results.

---

## Configuration

### Environment Variables

**Router**:
- `ROUTER_JETSTREAM_STREAM` - JetStream stream name (default: `beamline.router.v1`)
- `ROUTER_JETSTREAM_CONSUMER` - Durable consumer name (default: `router-consumer`)
- `ROUTER_JETSTREAM_MAX_DELIVER` - MaxDeliver limit (default: `5`)
- `ROUTER_JETSTREAM_ACK_WAIT` - ACK wait timeout in seconds (default: `30`)

**Gateway**:
- `GATEWAY_JETSTREAM_STREAM` - JetStream stream name (default: `beamline.router.v1`)
- `GATEWAY_JETSTREAM_TIMEOUT` - Request timeout in seconds (default: `5`)

### JetStream Stream Configuration

**Stream Configuration** (created by infrastructure/deployment):
- **Name**: `beamline.router.v1`
- **Subjects**: `beamline.router.v1.*`
- **Storage**: File (persistent)
- **Replication**: 3 (for HA)
- **Retention**: Work queue (messages deleted after ACK)
- **Max Age**: 1 hour (messages expire after 1 hour if not ACKed)

---

## Monitoring and Observability

### Metrics

**Router JetStream Metrics**:
- `router_jetstream_messages_received_total` - Total messages received from JetStream
- `router_jetstream_ack_total` - Total ACK count
- `router_jetstream_nak_total` - Total NAK count
- `router_jetstream_redelivery_total` - Total redelivery count
- `router_jetstream_redelivery_exhausted_total` - MaxDeliver exhaustion count
- `router_jetstream_processing_latency_seconds` - Message processing latency

**Labels**: `tenant_id`, `policy_id`, `error_code` (for NAK metrics)

### Logs

**Router JetStream Logs**:
- **ACK**: `{"level":"INFO","component":"router","message":"Message ACKed","message_id":"...","tenant_id":"..."}`
- **NAK**: `{"level":"WARN","component":"router","message":"Message NAKed","message_id":"...","error_code":"...","error_message":"..."}`
- **Redelivery**: `{"level":"INFO","component":"router","message":"Message redelivered","message_id":"...","redelivery_count":2}`
- **MaxDeliver Exhaustion**: `{"level":"ERROR","component":"router","message":"MaxDeliver exhausted","message_id":"...","redelivery_count":5}`

### Alerts

**Prometheus Alert Rules** (from `docs/observability/router-alert-rules.yaml`):
- **High NAK Rate**: NAK rate exceeds threshold (indicates validation or processing issues)
- **Redelivery Exhaustion**: MaxDeliver exhaustion rate exceeds threshold (indicates persistent failures)
- **High Redelivery Rate**: Redelivery rate exceeds threshold (indicates transient failures)

---

## References

### Core Documentation

- `docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY_OVERVIEW.md` - CP2 readiness overview
- `docs/ADR/ADR-011-jetstream-e2e.md` - JetStream architecture decision
- `docs/archive/dev/JETSTREAM_E2E_IDEMPOTENCY_TESTS_REPORT.md` - JetStream E2E test results

### Implementation References

- `apps/otp/router/src/router_nats.erl` - Router JetStream implementation
- `apps/otp/router/test/router_jetstream_e2e_SUITE.erl` - JetStream E2E tests
- `apps/otp/router/test/router_jetstream_redelivery_SUITE.erl` - Redelivery tests

### Operational Guides

- `apps/otp/router/docs/OPERATIONAL_GUIDE.md` - Router operational guide (JetStream section)
- `docs/observability/router-alert-rules.yaml` - Prometheus alert rules

---

## Change History

**v1.0 (2025-01-27)**:
- Initial version with detailed JetStream specifications
- Router and Gateway implementation details
- Test coverage and configuration
- Monitoring and observability

