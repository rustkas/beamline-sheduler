---
version: 1.0
status: accepted
date: 2025-01-27
deciders:
  - CP1-ROUTER Implementation Team
related_adrs:
  - ADR-006: NATS for Inter-Service Communication
supersedes: []
superseded_by: []
---

# ADR-011: JetStream E2E with Durable Subscriptions and ACK/NAK

## Status

accepted

## Context

Router needs reliable message processing for:
- CAF execution results (`caf.exec.result.v1`)
- CAF assignment acknowledgments (`caf.exec.assign.v1.ack`)
- Guaranteed delivery and at-least-once semantics
- Protection against message loss during failures
- Controlled redelivery on processing errors
- Horizontal scaling with queue groups

Standard NATS pub-sub does not provide:
- Message persistence
- Delivery guarantees
- Redelivery on failures
- At-least-once delivery semantics

## Decision

Use **NATS JetStream** with durable subscriptions, explicit ACK policy, and delivery count tracking for E2E reliability.

**Key Components**:

1. **Durable Subscriptions**:
   - Durable consumer names (e.g., `router-results`, `router-acks`)
   - Explicit ACK policy (messages must be explicitly acknowledged)
   - Queue groups for horizontal scaling (deliver group)

2. **ACK/NAK Operations**:
   - `ACK`: Message processed successfully, remove from queue
   - `NAK`: Message processing failed, request redelivery (respects MaxDeliver)
   - `In-Progress`: Extend ACK wait time for long-running operations

3. **Delivery Count Tracking**:
   - Track delivery count per `msg_id` in ETS table
   - Detect MaxDeliver exhaustion (delivery count >= MaxDeliver)
   - Emit metrics for exhausted messages
   - Clean up tracking on successful ACK

4. **MaxDeliver Configuration**:
   - Default: 3 delivery attempts
   - Configurable via `nats_js_max_deliver`
   - Prevents infinite redelivery loops

5. **Headers Priority**:
   - Headers have priority over payload for `trace_id`, `tenant_id`, `version`
   - Supports distributed tracing context propagation
   - Enables tenant validation from headers

**Implementation**:
- `router_nats.erl`: JetStream subscription and ACK/NAK operations
- `router_result_consumer.erl`: Durable subscription for `caf.exec.result.v1`
- `router_ack_consumer.erl`: Durable subscription for `caf.exec.assign.v1.ack`
- ETS table `router_delivery_count` for delivery count tracking

## Consequences

### Positive

- **Reliability**: Guaranteed message delivery with persistence
- **Fault Tolerance**: Automatic redelivery on failures
- **Scalability**: Queue groups enable horizontal scaling
- **Observability**: Delivery count tracking and metrics
- **Controlled Redelivery**: MaxDeliver prevents infinite loops
- **At-Least-Once Semantics**: Messages are delivered at least once

### Negative

- **Complexity**: Requires understanding of JetStream concepts
- **Resource Usage**: ETS table for delivery count tracking
- **Configuration**: Requires proper MaxDeliver and ACK wait configuration
- **Message Ordering**: JetStream may reorder messages in queue groups

### Neutral

- **Performance**: Slight overhead for ACK/NAK operations
- **Monitoring**: Need to monitor delivery count and exhausted messages

## Alternatives Considered

### Alternative 1: Standard NATS Pub-Sub

**Description**: Use standard NATS pub-sub without JetStream

**Pros**:
- Simpler implementation
- Lower latency
- No persistence overhead

**Cons**:
- No delivery guarantees
- Messages lost on consumer failure
- No redelivery mechanism
- No at-least-once semantics

**Why not chosen**: Reliability requirements demand guaranteed delivery

### Alternative 2: Database-backed Message Queue

**Description**: Use database (PostgreSQL, Redis) for message queue

**Pros**:
- Strong consistency
- Transactional guarantees
- Rich query capabilities

**Cons**:
- Additional infrastructure dependency
- Higher latency
- More complex integration
- Not optimized for message streaming

**Why not chosen**: NATS JetStream provides better performance and integration

### Alternative 3: Apache Kafka

**Description**: Use Kafka for message streaming

**Pros**:
- Strong ordering guarantees
- High throughput
- Mature ecosystem

**Cons**:
- Heavyweight infrastructure
- Complex setup and maintenance
- Overkill for current requirements

**Why not chosen**: NATS JetStream is lighter and sufficient

## Implementation Notes

**Durable Subscription Configuration**:
```erlang
subscribe_jetstream(Subject, DurableGroup, explicit, DeliverGroup, push)
```

**ACK/NAK Operations**:
```erlang
router_nats:ack_message(MsgId)      % Success
router_nats:nak_message(MsgId)       % Failure, redeliver
router_nats:in_progress_message(MsgId)  % Extend ACK wait
```

**Delivery Count Tracking**:
- ETS table: `router_delivery_count`
- Key: `msg_id`
- Value: delivery count
- Cleanup: on successful ACK

**MaxDeliver Exhaustion Detection**:
- Check delivery count >= MaxDeliver before NAK
- Emit metric: `router_jetstream_maxdeliver_exhausted_total`
- Remove from tracking (message will not be redelivered)

## CAF Adapter Under Load

**Context**: Router publishes `ExecAssignment` messages to CAF via NATS using `router_caf_adapter`. When CAF is slow or overloaded, Router must handle timeouts and errors gracefully.

**Behavior**:

1. **Timeout Handling**:
   - If NATS `publish_with_ack` times out (CAF not responding), Router retries with exponential backoff
   - Retry configuration: `caf_max_retries` (default: 3), `caf_retry_base_ms` (default: 100ms)
   - Metrics: `router_assignment_retry_total` (emitted on each retry attempt)

2. **Error Handling**:
   - Connection failures (`connection_failed`): Retried up to `caf_max_retries`
   - Other errors: Classified via `classify_nats_error/1` and retried if retriable
   - Metrics: `router_assignment_publish_failures_total` (emitted on final failure)

3. **Retry Exhaustion**:
   - When all retries are exhausted, Router emits `router_retry_exhausted_total` metric
   - Assignment is not published (returns `error` to caller)
   - No automatic redelivery (assignment is lost, but decision can be retried at Gateway level)

4. **Interaction with JetStream Redelivery**:
   - CAF adapter failures do NOT trigger JetStream redelivery (assignment is not in JetStream queue)
   - If CAF processes assignment but is slow to respond, results may arrive late via `caf.exec.result.v1`
   - Late results are handled by `router_result_consumer` with normal JetStream redelivery semantics

**Metrics**:
- `router_assignment_retry_total`: Count of retry attempts (labels: `assignment_id`, `request_id`)
- `router_assignment_published_total`: Count of successful publications (labels: `assignment_id`, `request_id`)
- `router_retry_exhausted_total`: Count of exhausted retries (labels: `assignment_id`, `request_id`, `error_kind`, `retries`)
- `router_assignment_publish_failures_total`: Count of publish failures (labels: `assignment_id`, `request_id`, `error_kind`, `subject`)

**Thresholds**:
- High retry rate: `rate(router_assignment_retry_total[5m]) > 10/min` indicates CAF overload
- Retry exhaustion: `rate(router_retry_exhausted_total[5m]) > 0` indicates CAF unavailable or severely overloaded
- Failure rate: `rate(router_assignment_publish_failures_total[5m]) > 5/min` indicates NATS/CAF connectivity issues

**See Also**:
- `apps/otp/router/src/router_caf_adapter.erl`: Implementation
- `apps/otp/router/test/router_caf_adapter_load_thresholds_SUITE.erl`: Test scenarios
- `docs/observability/router-alert-rules.yaml`: Alert rules for CAF overload

## Runbook: CAF Adapter Alerts

### Alert: RouterCAFHighRetryRate

**When this alert fires**: CAF adapter retry rate > ~10/min for 5 minutes (CAF overload).

**Steps**:
1. **Check retry metrics**:
   ```bash
   rate(router_assignment_retry_total[5m])
   ```

2. **Identify CAF overload**:
   - Check CAF health and response times
   - Review CAF logs for processing delays
   - Check if CAF is handling assignments slowly

3. **Immediate actions**:
   - Scale CAF horizontally if overloaded
   - Check CAF resource usage (CPU, memory)
   - Review CAF processing logic for bottlenecks

4. **Long-term fixes**:
   - Optimize CAF processing performance
   - Review assignment payload size
   - Consider rate limiting at Router level

**Related Documentation**:
- `docs/ADR/ADR-011-jetstream-e2e.md#caf-adapter-under-load` - CAF adapter behavior
- `apps/otp/router/test/router_caf_adapter_load_thresholds_SUITE.erl` - Load threshold tests

### Alert: RouterCAFRetryExhausted

**When this alert fires**: CAF adapter exhausted all retries (rate > 0 for 1 minute).

**Steps**:
1. **Check retry exhaustion metrics**:
   ```bash
   rate(router_retry_exhausted_total[5m])
   ```

2. **Identify CAF unavailability**:
   - Check if CAF service is down
   - Verify NATS connectivity to CAF
   - Review CAF logs for errors

3. **Immediate actions**:
   - Restart CAF service if down
   - Check NATS connectivity
   - Verify CAF subscription is active

4. **Recovery**:
   - Once CAF is available, assignments will resume
   - Lost assignments cannot be recovered (not in JetStream queue)
   - Gateway can retry decisions if needed

**Related Documentation**:
- `docs/ADR/ADR-011-jetstream-e2e.md#caf-adapter-under-load` - Retry exhaustion behavior
- `apps/otp/router/test/router_caf_adapter_load_thresholds_SUITE.erl#test-caf-retry-exhaustion-emits-metrics` - Retry exhaustion test

### Alert: RouterCAFHighPublishFailureRate

**When this alert fires**: CAF adapter publish failure rate > ~5/min for 5 minutes (NATS/CAF connectivity issues).

**Steps**:
1. **Check publish failure metrics**:
   ```bash
   rate(router_assignment_publish_failures_total[5m]) by (error_kind, subject)
   ```

2. **Identify connectivity issues**:
   - Check NATS connection health
   - Verify CAF subscription is active
   - Review error_kind label for specific error type

3. **Fix connectivity**:
   - Restart NATS connection if needed
   - Verify NATS subject is correct
   - Check CAF subscription configuration

**Related Documentation**:
- `docs/ADR/ADR-011-jetstream-e2e.md#caf-adapter-under-load` - Publish failure handling
- `apps/otp/router/test/router_caf_adapter_load_thresholds_SUITE.erl#test-caf-error-response-reflected-in-metrics` - Error handling test

## Updates

**2025-01-27**: Enhanced redelivery observability:
- `router_jetstream_redelivery_total` now supports labels (`assignment_id`, `request_id`, `reason`, `source`)
- Enables detailed filtering and alerting by redelivery source and reason
- Structured logging added for correlation: `"Message redelivery requested"` (INFO level)
- Full fault injection coverage validated
- See: `../../apps/otp/router/docs/archive/dev_reports/OBSERVABILITY_REDELIVERY_METRICS_TASK_COMPLETE.md`

## References

- `docs/NATS_SUBJECTS.md`: NATS subjects and JetStream configuration
- `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`: Headers and payload mapping
- `apps/otp/router/src/router_nats.erl`: JetStream implementation
- `apps/otp/router/src/router_result_consumer.erl`: Result consumer
- `apps/otp/router/src/router_ack_consumer.erl`: ACK consumer
- `apps/otp/router/src/router_caf_adapter.erl`: CAF adapter implementation
- `../../apps/otp/router/docs/archive/dev_reports/OBSERVABILITY_REDELIVERY_METRICS_TASK_COMPLETE.md`: Redelivery metrics implementation
- ADR-006: NATS for Inter-Service Communication

## Compliance

- ✅ Aligns with `.trae/manifest.json`
- ✅ Follows compatibility policy
- ✅ Respects security constraints
- ✅ Integrates with STATE/HISTORY

