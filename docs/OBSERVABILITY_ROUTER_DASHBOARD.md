# Router Observability Dashboard

This document describes the Router JetStream observability dashboard and explains how to interpret its metrics and panels. It provides a shared language for engineers and SREs to detect delivery issues, distinguish infrastructure problems from business rejections, and respond to incidents effectively. Use it as a practical guide for configuring alerts and for step-by-step investigations when something goes wrong.

**Status**: Production-ready specification  
**Last Updated**: 2025-01-27

---

## 1. Goals of the Dashboard

The Router dashboard is intended to answer three main questions:

**Primary Objectives**:

- **Early detection of delivery problems** (redelivery bursts, DLQ growth, MaxDeliver exhaustion).

- **Track NATS infrastructure failures** (connect/publish/ack/reconnect errors).

- **Understand business rejections** (tenant/usage-rejections) and their causes.

- **Assist incident investigation** through detailed labels (`request_id`, `msg_id`, `reason`, `tenant_id`, etc.).

### 1.1 Is Message Delivery Healthy?

**Goal**: Ensure messages are delivered in time, do not loop on retries, and do not accumulate in DLQ.

**Key Indicators**:
- Messages are delivered successfully on first attempt
- No messages exhausting MaxDeliver limit
- Minimal DLQ volume (only truly problematic messages)
- We can see when deliveries start failing for specific assignments or tenants

**Success Criteria**:
- Redelivery rate < 5% of total messages
- MaxDeliver exhaustion = 0 (critical)
- DLQ rate < 0.1% of total messages

### 1.2 Are There Any NATS / JetStream Infrastructure Problems?

**Goal**: Detect issues with NATS connections, reconnects, publish failures, and ACK failures before they impact message delivery.

**Key Indicators**:
- Connection stability (no frequent connect/reconnect failures)
- Publish operation success rate
- ACK operation success rate
- We observe when MaxDeliver limits are being exhausted

**Success Criteria**:
- Connection failures < 1 per hour
- Publish failure rate < 0.1%
- ACK failure rate < 0.1%
- Reconnection failures < 1 per day

### 1.3 Are There Problematic Tenants or Payloads?

**Goal**: Identify tenants or payloads that systematically cause problems (rejections, high retry rates, DLQ entries).

**Key Indicators**:
- We see which tenants produce rejected or malformed messages
- We can distinguish expected / quota-based rejections from real incidents
- Tenant rejection rate by tenant_id
- DLQ rate by tenant_id

**Success Criteria**:
- No single tenant causing > 10% of total rejections
- No single tenant causing > 5% of total redeliveries
- Tenant rejection rate < 1% of total messages

---

## 1.4 Dashboard Panel Groups Overview

The Router JetStream dashboard is organized into the following panel groups:

### Overview
- **Purpose**: High-level health indicators
- **Key Questions**: Is the system healthy overall? Are there any critical issues?
- **Panels**: Total DLQ rate, total redelivery rate, total NATS failures, MaxDeliver exhaustion rate

### Redelivery
- **Purpose**: Monitor message redelivery patterns
- **Key Questions**: Which assignments/tenants are causing redeliveries? What are the reasons?
- **Panels**: Redelivery rate by assignment, by tenant, by delivery_count, by reason, redelivery vs successful processing

### DLQ and MaxDeliver
- **Purpose**: Track messages that cannot be processed
- **Key Questions**: Are messages exhausting MaxDeliver? What's entering DLQ and why?
- **Panels**: DLQ inflow total/by reason/by tenant/by assignment, MaxDeliver exhausted rate/by assignment, top assignments by DLQ/MaxDeliver

### NATS Infrastructure Errors
- **Purpose**: Monitor NATS/JetStream infrastructure health
- **Key Questions**: Are there connection issues? Are publish/ACK operations failing?
- **Panels**: NATS connect/reconnect failures, publish failures by reason, ACK failures, correlation with redeliveries

### Tenant / Usage Rejections
- **Purpose**: Track business-level rejections
- **Key Questions**: Which tenants are being rejected? What are the rejection reasons?
- **Panels**: Tenant rejections total/by tenant/by reason, correlation with DLQ/MaxDeliver

### Correlation / Overview
- **Purpose**: Cross-metric analysis and correlation
- **Key Questions**: How do different metrics relate? What's the degradation chain?
- **Panels**: Stacked graph (redelivery + DLQ + MaxDeliver), heatmap (redelivery rate by assignment × tenant)

### Performance & Capacity
- **Purpose**: Monitor processing performance and capacity limits
- **Key Questions**: Is Router overloaded? Are there processing bottlenecks?
- **Panels**: Processing latency (P50/P95/P99), queue depth, in-flight messages, backpressure status

---

## 2. Key Metrics

Below are the main metrics used on the dashboard.  

All examples assume Prometheus-style counters.

### 2.1 `router_jetstream_redelivery_total`

**Scenario ID**: `JS-001` (High Redelivery Rate), `JS-005` (High Redelivery from Specific Source)  
**Alert**: `RouterJetStreamHighRedeliveryRate`, `RouterJetStreamHighRedeliveryFromSource`  
**Alert File**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (lines 38-86)  
**Coverage Analysis**: `../apps/otp/router/docs/dev/OBS_COVERAGE_ANALYSIS.md#js-001-high-redelivery-rate`

**What We Measure**

Counts the number of **message redeliveries** performed by JetStream for Router consumers. Incremented when JetStream delivers the same message again (e.g. after a negative ACK, processing error, or timeout).

**Why This Metric Matters**

- **Early warning signal**: Redeliveries indicate processing instability before messages fail completely
- **Degradation indicator**: Growing redeliveries → potential DLQ growth → potential MaxDeliver exhaustion
- **Problem localization**: Labels (`assignment_id`, `tenant_id`, `reason`) help identify root cause quickly

**What to Do When You See Redeliveries**

1. **Check assignment breakdown**: Which handler/assignment is failing?
   ```promql
   sum by (assignment_id) (rate(router_jetstream_redelivery_total[5m]))
   ```

2. **Check tenant breakdown**: Is it one problematic tenant or system-wide?
   ```promql
   sum by (tenant_id) (rate(router_jetstream_redelivery_total[5m]))
   ```

3. **Check reason breakdown**: What's causing the failures?
   ```promql
   sum by (reason) (rate(router_jetstream_redelivery_total[5m]))
   ```

4. **Correlate with other metrics**: 
   - If NATS failures are high → infrastructure issue
   - If tenant rejections are high → business rule issue
   - If both are low → application logic issue

**When to Worry**

- **Warning**: Redelivery rate > 5% of total messages for > 5 minutes
- **Critical**: Redelivery rate > 10% of total messages for > 5 minutes
- **Action required**: Any sustained growth in redeliveries for a specific assignment or tenant

**Important Labels**

- `assignment_id`  

  Identifier of the route/assignment the message is processed for. Helps localize problems to specific pipelines.

- `tenant_id`  

  Tenant for which the message is processed. Helps detect noisy or misconfigured tenants.

- `source`  

  Source of the message (upstream service, stream, or subject).

- `msg_id`  

  Unique Router/JetStream message identifier. Used for deep debugging, usually in logs.

- `request_id`  

  Correlation ID across services. Useful when debugging specific incidents.

- `delivery_count`  

  Current delivery attempt number (1, 2, 3, …).

- `max_deliver`  

  Configured maximum number of deliveries for this stream/consumer.

- `reason`  

  Categorical reason code for redelivery. **Real values from code**:
  - `tenant_validation_failed` - Tenant validation failed
  - `backoff` - Backoff retry
  - `backpressure` - Backpressure detected
  - `ack_error` - ACK operation failed
  - `nak_error` - NAK operation failed
  - `processing_timeout` - Processing timeout
  - `publish_error` - Publish operation failed
  - `nats_disconnect` - NATS connection lost
  - `maxdeliver_exhausted` - MaxDeliver limit reached

**Dashboard Panels**

- **Redelivery rate (by assignment)**

  **Scenario IDs**: S2 (Processing Delays → Redelivery Growth), JS-001 (High Redelivery Rate), JS-004 (Redelivery Queue Growth)  
  **Coverage Matrix**: `../apps/otp/router/docs/archive/dev_reports/JETSTREAM_OBS_COVERAGE_MATRIX.md#s2-processing-delays--redelivery-growth`  
  **Alert**: `RouterJetStreamHighRedeliveryRate` (Scenario ID: JS-001)

  - Query:  

    ```promql
    sum by (assignment_id) (rate(router_jetstream_redelivery_total[5m]))
    ```

  - Use cases:

    - Find assignments with the highest retry rate.

    - Detect sudden spikes in redeliveries.

    - Shows which types of tasks/workers most frequently fail.

- **Redelivery rate (by tenant)**

  - Query:  

    ```promql
    sum by (tenant_id) (rate(router_jetstream_redelivery_total[5m]))
    ```

  - Use cases:

    - Detect tenants that systematically trigger retries.

    - Helps identify "toxic" clients (bad payloads, excessive load).

- **Redelivery vs Successful processing (stacked bar/ratio)**

  - Upper layer: redeliveries, lower layer: successful processing.

  - Useful for tracking degradation in processing quality.

  - Query:

    ```promql
    # Redeliveries
    sum(rate(router_jetstream_redelivery_total[5m]))
    # Successful processing (if available)
    sum(rate(router_jetstream_processed_success_total[5m]))
    ```

- **Redeliveries by delivery_count**

  - Query:  

    ```promql
    sum by (delivery_count) (rate(router_jetstream_redelivery_total[5m]))
    ```

  - Use cases:

    - Identify "long tails" of messages stuck on high delivery counts.

    - Shows how close messages are to `max_deliver` limit.

- **Redeliveries by reason**

  - Query:  

    ```promql
    sum by (reason) (rate(router_jetstream_redelivery_total[5m]))
    ```

  - Use cases:

    - Understand root causes of redeliveries (tenant validation, backpressure, ACK errors).

---

### 2.2 `router_jetstream_maxdeliver_exhausted_total`

**Scenario ID**: `JS-002` (MaxDeliver Exhaustion)  
**Alert**: `RouterJetStreamMaxDeliverExhausted`  
**Alert File**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (lines 92-114)  
**Coverage Analysis**: `../apps/otp/router/docs/dev/OBS_COVERAGE_ANALYSIS.md#js-002-maxdeliver-exhaustion`

**What We Measure**

Counts messages for which JetStream **exhausted the MaxDeliver limit** (i.e. message could not be successfully processed after N deliveries). Typically, such messages are moved to DLQ or discarded, depending on configuration.

**Why This Metric Matters**

- **Irrecoverable failure indicator**: The system tried N times and still couldn't process the message
- **Code quality signal**: Often indicates unfixed bugs or persistently invalid payloads
- **Critical alert trigger**: Expected to be ≈0 in normal operation; any value > 0 requires immediate investigation

**What to Do When You See MaxDeliver Exhausted**

1. **Immediate action**: This is a critical issue - investigate immediately
   ```promql
   sum(rate(router_jetstream_maxdeliver_exhausted_total[5m]))
   ```

2. **Identify problematic assignment**: Which handler is failing?
   ```promql
   sum by (assignment_id) (rate(router_jetstream_maxdeliver_exhausted_total[5m]))
   ```

3. **Check tenant distribution**: Is it one tenant or multiple?
   ```promql
   sum by (tenant_id) (rate(router_jetstream_maxdeliver_exhausted_total[1h]))
   ```

4. **Review logs**: Use `msg_id` or `request_id` from metrics to find specific log entries

5. **Investigation checklist**:
   - Is the message payload malformed?
   - Is downstream service consistently failing?
   - Is MaxDeliver configuration too low?
   - Is there a code bug in the handler?

**When to Worry**

- **Critical**: Any value > 0 for > 1 minute (immediate investigation required)
- **Action**: Follow MaxDeliver runbook (Section 5.4.3)

**Important Labels**

- `assignment_id`

- `tenant_id`

- `source`

- `reason`  

  High-level reason why the message exhausted MaxDeliver. **Real value from code**:
  - `maxdeliver_exhausted` - MaxDeliver limit reached (always this value)

- `msg_id`

- `request_id`

- `delivery_count`  

  Delivery count at the moment of exhaustion (usually equals `max_deliver`).

- `max_deliver`

**Dashboard Panels**

- **MaxDeliver exhausted rate (overall)**

  **Scenario IDs**: S3 (MaxDeliver Exhaustion), JS-002 (MaxDeliver Exhaustion)  
  **Coverage Matrix**: `../apps/otp/router/docs/archive/dev_reports/JETSTREAM_OBS_COVERAGE_MATRIX.md#s3-maxdeliver-exhaustion-partial-messages`  
  **Alert**: `RouterJetStreamMaxDeliverExhausted` (Scenario ID: JS-002)

  - Query:  

    ```promql
    sum(rate(router_jetstream_maxdeliver_exhausted_total[5m]))
    ```

  - Use cases:

    - Expected to be ≈0 in normal operation; any spike requires investigation.

- **MaxDeliver exhausted by assignment**

  - Query:  

    ```promql
    sum by (assignment_id) (rate(router_jetstream_maxdeliver_exhausted_total[5m]))
    ```

  - Use cases:

    - Detect assignments where messages systematically cannot be processed.

- **Top N assignments by MaxDeliver exhausted**

  - Table/bar chart sorted by descending order.

  - Query:

    ```promql
    topk(10, sum by (assignment_id) (rate(router_jetstream_maxdeliver_exhausted_total[24h])))
    ```

- **Top tenants by MaxDeliver exhausted**

  - Query:  

    ```promql
    sum by (tenant_id) (rate(router_jetstream_maxdeliver_exhausted_total[1h]))
    ```

  - Use cases:

    - Identify tenants whose events are consistently failing.

    - Find clients with "poisonous" messages.

- **MaxDeliver exhausted by reason**

  - Query:  

    ```promql
    sum by (reason) (rate(router_jetstream_maxdeliver_exhausted_total[5m]))
    ```

  - Use cases:

    - Separate expected failures (e.g. tenant misconfiguration) from bugs (e.g. `internal_error`).

---

### 2.3 `router_dlq_total`

**Scenario ID**: `JS-003` (DLQ Growth)  
**Alert**: `RouterDLQHighRate` (documented in dashboard, missing from alert rules - see `../apps/otp/router/docs/dev/OBS_COVERAGE_ANALYSIS.md#missing-dlq-alert-in-alert-rules`)  
**Alert File**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (missing - **GAP**)  
**Coverage Analysis**: `../apps/otp/router/docs/dev/OBS_COVERAGE_ANALYSIS.md#js-003-dlq-growth`

**What We Measure**

Counts messages that are **sent to the Router Dead-Letter Queue (DLQ)**. Represents messages that Router decided not to process any further.

**Why This Metric Matters**

- **Unprocessed message volume**: Shows how many messages require manual or offline processing
- **Data loss risk**: Growing DLQ indicates potential SLA violations or data loss
- **System health indicator**: DLQ growth often precedes or follows MaxDeliver exhaustion

**What to Do When You See DLQ Growth**

1. **Check DLQ rate**: How fast are messages entering DLQ?
   ```promql
   sum(rate(router_dlq_total[5m]))
   ```

2. **Check if MaxDeliver exhaustion is the cause**:
   ```promql
   sum(rate(router_jetstream_maxdeliver_exhausted_total[5m]))
   ```

3. **Check breakdown by labels** (labels now available):
   ```promql
   sum by (reason) (rate(router_dlq_total[5m]))
   sum by (assignment_id) (rate(router_dlq_total[5m]))
   sum by (tenant_id) (rate(router_dlq_total[5m]))
   ```

4. **Investigation steps**:
   - If `maxdeliver_exhausted` → Follow MaxDeliver runbook
   - If `payload_invalid` → Review payload validation rules
   - If `unsupported_version` → Update protocol version support
   - If `business_rule_violation` → Review business rules

5. **DLQ recovery decision**:
   - Can messages be safely reprocessed?
   - Is a hotfix needed before reprocessing?
   - Use `msg_id` or `request_id` to identify specific problematic messages

**When to Worry**

- **Warning**: > 0.1 messages/sec for > 5 minutes
- **Critical**: > 1 messages/sec for > 1 minute
- **Action**: Follow DLQ runbook (Section 5.4.3)

**Important Labels**

**Current Status**: No labels (metric is simple counter). Future enhancement should add labels for better observability.

**Recommended Future Labels**:
- `assignment_id`

- `tenant_id`

- `source`

- `msg_id`

- `reason`  

  Reason for DLQ (e.g. `schema_validation_failed`, `max_retries_hit`, `unsupported_operation`, `maxdeliver_exhausted`).

- `request_id`

**Dashboard Panels**

- **DLQ inflow (rate of messages entering DLQ)**

  - Query:  

    ```promql
    sum(rate(router_dlq_total[5m]))
    ```

  - Use cases:

    - Global view of DLQ health.

    - Monitor DLQ growth rate.

- **DLQ inflow by reason** (when labels added)

  - Query:  

    ```promql
    sum by (reason) (rate(router_dlq_total[5m]))
    ```

  - Use cases:

    - Understand what types of errors dominate the DLQ.

    - Helps quickly identify what broke (validation, business rules, dependencies).

    - Common reasons: `max_deliver_exhausted`, `payload_invalid`, `unsupported_version`, `business_rule_violation`.

- **DLQ inflow by tenant** (when labels added)

  - Query:  

    ```promql
    sum by (tenant_id) (rate(router_dlq_total[5m]))
    ```

  - Use cases:

    - Identify problematic tenants.

    - Find tenants accumulating DLQ messages.

- **DLQ by assignment** (labels now available)

  **Scenario IDs**: S3 (MaxDeliver Exhaustion), JS-003 (DLQ Growth)  
  **Coverage Matrix**: `../apps/otp/router/docs/archive/dev_reports/JETSTREAM_OBS_COVERAGE_MATRIX.md#js-003-dlq-growth`  
  **Alert**: `RouterDLQHighRate` (warning), `RouterDLQHighRateCritical` (critical) (Scenario ID: JS-003)

  - Query:  

    ```promql
    sum by (assignment_id) (rate(router_dlq_total[5m]))
    ```

  - Use cases:

    - Link DLQ behavior to specific flows/pipelines.

    - Identify which handler/assignment is sending to DLQ.

- **DLQ by reason** (labels now available)

  **Scenario IDs**: S3 (MaxDeliver Exhaustion), JS-003 (DLQ Growth)  
  **Coverage Matrix**: `../apps/otp/router/docs/archive/dev_reports/JETSTREAM_OBS_COVERAGE_MATRIX.md#js-003-dlq-growth`  
  **Alert**: `RouterDLQHighRate` (warning), `RouterDLQHighRateCritical` (critical) (Scenario ID: JS-003)

  - Query:  

    ```promql
    sum by (reason) (rate(router_dlq_total[5m]))
    ```

  - Use cases:

    - Understand why messages are going to DLQ.

    - Identify common failure patterns.

- **DLQ by tenant** (labels now available)

  - Query:  

    ```promql
    sum by (tenant_id) (rate(router_dlq_total[5m]))
    ```

  - Use cases:

    - Identify problematic tenants.

    - Find tenants accumulating DLQ messages.

---

### 2.4 NATS Failures: `router_nats_*_failures_total`

**Scenario IDs**: 
- `NATS-001` (NATS Connection Failures) - Alert: `RouterNatsConnectionFailures`, `RouterNatsFrequentReconnects`
- `NATS-002` (NATS Connection Down) - Alert: `RouterNatsConnectionDown`
- `NATS-003` (NATS Reconnection Exhausted) - Alert: `RouterNatsReconnectionExhausted`
- `NATS-004` (High Publish Failure Rate) - Alert: `RouterNatsHighPublishFailureRate`, `RouterNatsHighPublishWithAckFailureRate`
- `NATS-005` (High ACK Failure Rate) - Alert: `RouterNatsHighAckFailureRate`
- `NATS-006` (High NAK Failure Rate) - Alert: `RouterNatsHighNakFailureRate`
- `NATS-007` (High Subscribe Failure Rate) - Alert: `RouterNatsHighSubscribeFailureRate`
- `NATS-008` (Pending Operations Queue Full) - Alert: `RouterNatsPendingOperationsQueueFull`

**Alert File**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (lines 175-460)  
**Coverage Analysis**: `../apps/otp/router/docs/dev/OBS_COVERAGE_ANALYSIS.md#nats-infrastructure-scenarios`

**What We Measure**

These metrics represent **infrastructure-level errors** when Router interacts with NATS / JetStream. They track failures at the connection, publish, ACK, and reconnect levels.

**Why These Metrics Matter**

- **Infrastructure vs application**: Helps distinguish "our code is broken" from "NATS is down"
- **Early warning**: NATS failures often precede redelivery spikes and DLQ growth
- **Root cause identification**: Connection failures → publish failures → ACK failures → redeliveries

**What to Do When You See NATS Failures**

1. **Check connection health**: Are we connecting to NATS?
   ```promql
   sum(rate(router_nats_connect_failures_total[5m]))
   sum(rate(router_nats_reconnect_failures_total[5m]))
   ```

2. **Check operation failures**: Are publish/ACK operations failing?
   ```promql
   sum(rate(router_nats_publish_failures_total[5m]))
   sum(rate(router_nats_ack_failures_total[5m]))
   ```

3. **Correlate with redeliveries**: Do NATS failures correlate with redelivery spikes?
   ```promql
   # Overlay these queries on the same graph
   sum(rate(router_nats_publish_failures_total[5m]))
   sum(rate(router_jetstream_redelivery_total[5m]))
   ```

4. **Investigation steps**:
   - Check NATS cluster health (external monitoring)
   - Review network connectivity
   - Verify NATS configuration (auth, TLS, endpoints)
   - Check if failures are transient or persistent

**When to Worry**

- **Warning**: Any connection/reconnect failures for > 5 minutes
- **Critical**: > 1 connection failure per hour
- **Action**: If persistent, escalate as infrastructure incident

#### 2.4.1 `router_nats_connect_failures_total`

**Purpose**

- Counts failed attempts to **establish a connection** to NATS.

- Indicates NATS unavailability or network issues.

**Important Labels**

**Current Status**: ✅ **Labels implemented** - `reason`, `cluster`, `source`  
**Implementation**: `apps/otp/router/src/router_nats.erl` (lines 78-82)  
**Coverage Analysis**: `../apps/otp/router/docs/dev/OBS_COVERAGE_ANALYSIS.md#nats-001-nats-connection-failures`

**Recommended Future Labels**:
- `source` or `cluster`  

  Which NATS cluster or endpoint Router tried to connect to.

- `reason`  

  Reason for failure (e.g. `dns_error`, `auth_failed`, `timeout`).

**Panels**

- **NATS connect/reconnect failures**

  - Query:  

    ```promql
    # Connect failures
    sum(rate(router_nats_connect_failures_total[5m]))
    # Reconnect failures
    sum(rate(router_nats_reconnect_failures_total[5m]))
    ```

  - Use cases:

    - Early detection of NATS downtime or misconfiguration.

    - Any growth signals cluster/network problems.

    - Two lines: connect and reconnect for correlation.

  - **Panel ID**: `router-nats-connection-failures` (Section 4.4, Panel 1)

---

#### 2.4.2 `router_nats_reconnect_failures_total`

**Scenario ID**: `NATS-001` (NATS Connection Failures)  
**Alert**: `RouterNatsConnectionFailures`, `RouterNatsFrequentReconnects`  
**Alert File**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (lines 182-252)  
**Coverage Analysis**: `../apps/otp/router/docs/dev/OBS_COVERAGE_ANALYSIS.md#nats-001-nats-connection-failures`

**Purpose**

- Counts failed **reconnect attempts** to NATS after a disconnect.

- Persistent growth suggests unstable connectivity or misconfiguration.

**Important Labels**

**Current Status**: ✅ **Labels implemented** - `reason`, `cluster`, `attempt`  
**Implementation**: `apps/otp/router/src/router_nats.erl` (lines 367-373)  
**Coverage Analysis**: `../apps/otp/router/docs/dev/OBS_COVERAGE_ANALYSIS.md#nats-001-nats-connection-failures`

**Recommended Future Labels**:
- `source` / `cluster`

- `reason`

**Panels**

- **Reconnect failures over time**

  - Query:  

    ```promql
    sum(rate(router_nats_reconnect_failures_total[5m]))
    ```

  - Use cases:

    - Detect flapping connections and unstable clusters.

---

#### 2.4.3 `router_nats_publish_failures_total`

**Scenario ID**: `NATS-004` (High Publish Failure Rate)  
**Alert**: `RouterNatsHighPublishFailureRate`, `RouterNatsHighPublishWithAckFailureRate`  
**Alert File**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (lines 311-359)  
**Coverage Analysis**: `../apps/otp/router/docs/dev/OBS_COVERAGE_ANALYSIS.md#nats-004-high-publish-failure-rate`

**Purpose**

- Counts failures when Router **publishes messages** to NATS / JetStream.

- Possible reasons:

  - cluster not available,

  - stream not found,

  - resource limits,

  - authorization issues.

**Important Labels**

**Current Status**: ✅ **Labels implemented** - `reason`, `subject`, `stream`, `source`  
**Implementation**: `apps/otp/router/src/router_nats.erl` (lines 403-410, 419-426)  
**Coverage Analysis**: `../apps/otp/router/docs/dev/OBS_COVERAGE_ANALYSIS.md#nats-004-high-publish-failure-rate`

**Recommended Future Labels**:
- `source`

- `subject` or `stream` (if available)

- `reason`

**Panels**

- **NATS publish failures by reason** (labels now available)

  - Query:  

    ```promql
    sum by (reason) (rate(router_nats_publish_failures_total[5m]))
    ```

  - Use cases:

    - Helps distinguish `timeout` from `authorization` or `no_route`.

    - Breakdown by `reason` and `source` for root cause analysis.

- **Publish failures by subject/stream** (labels now available)

  - Query:  

    ```promql
    sum by (subject) (rate(router_nats_publish_failures_total[5m]))
    sum by (stream) (rate(router_nats_publish_failures_total[5m]))
    ```

  - Use cases:

    - Identify specific streams/subjects affected by infrastructure issues.

- **Correlation with redeliveries / DLQ**

  - Use overlay:

    ```promql
    rate(router_nats_publish_failures_total[5m])
    ```

    ```promql
    rate(router_jetstream_redelivery_total[5m])
    ```

    ```promql
    rate(router_dlq_total[5m])
    ```

  - Use cases:

    - Correlate infrastructure failures with message delivery problems.

---

#### 2.4.4 `router_nats_ack_failures_total`

**Purpose**

- Counts failures when Router tries to **ACK/NACK messages** back to NATS/JetStream.

- Indicates issues such as:

  - connection drops during ACK,

  - protocol errors,

  - consumer misconfiguration.

**Important Labels**

**Current Status**: No labels (metric is simple counter).

**Recommended Future Labels**:
- `source`

- `subject` or `stream`

- `reason`

**Panels**

- **NATS ACK failures**

  - Query:  

    ```promql
    sum(rate(router_nats_ack_failures_total[5m]))
    ```

  - Use cases:

    - Detect ACK instability that leads to unintended redeliveries.

    - Important because ACK errors can lead to cascading redeliveries.

    - Correlate with redelivery spikes.

---

### 2.5 Tenant Rejection / Usage: `router_results_tenant_rejected_total`

**Scenario ID**: `TENANT-001` (High Tenant Rejection Rate)  
**Alert**: `RouterTenantRejectedHigh`  
**Alert File**: `apps/otp/router/docs/PROMETHEUS_ALERTS.md` (lines 169-177)  
**Coverage Analysis**: `../apps/otp/router/docs/dev/OBS_COVERAGE_ANALYSIS.md#tenant-001-high-tenant-rejection-rate`

**What We Measure**

Counts cases where Router **rejects results or messages due to tenant-related constraints**, for example: quota exceeded, invalid tenant configuration, blacklisting / feature disabled.

**Why This Metric Matters**

- **Business vs infrastructure**: Distinguishes "we intentionally rejected" from "system failure"
- **Quota management**: Helps analyze quota fairness and plan tariff upgrades
- **Security signal**: Frequent rejections may indicate abuse or attacks
- **Configuration validation**: Helps identify misconfigured tenants

**What to Do When You See Tenant Rejections**

1. **Check overall rejection rate**: Is this normal or a spike?
   ```promql
   sum(rate(router_results_tenant_rejected_total[5m]))
   ```

2. **Identify problematic tenants**: Which tenants are being rejected?
   ```promql
   sum by (tenant_id) (rate(router_results_tenant_rejected_total[5m]))
   ```

3. **Check rejection reasons**: Why are they being rejected?
   ```promql
   sum by (reason) (rate(router_results_tenant_rejected_total[5m]))
   ```

4. **Decision tree**:
   - If `tenant_id not in allowlist` → Check tenant configuration
   - If `tenant_id is required` → Check payload format
   - If `no policy found for tenant_id` → Check policy registry
   - If rejection rate is high for one tenant → Investigate for abuse/attack

5. **Distinguish expected vs unexpected**:
   - **Expected**: Tenant over quota (business as usual)
   - **Unexpected**: All traffic for a tenant suddenly blocked (incident)

**When to Worry**

- **Warning**: > 1% of total messages for > 5 minutes
- **Critical**: > 5% of total messages for > 1 minute
- **Action**: If unexpected, investigate tenant configuration and policy registry

**Important Labels**

- `tenant_id`

- `reason`  

  Rejection reason. **Real values from code**:
  - `tenant_id is required` - Tenant ID missing
  - `tenant_id cannot be empty` - Tenant ID is empty
  - `tenant_id not in allowlist` - Tenant not in allowlist
  - `no policy found for tenant_id` - Policy not found (may be allowed depending on config)
  - `tenant_id must be a binary` - Invalid tenant ID format

- `source`

- `assignment_id` (if applicable)

- `request_id`

- `error_code`

**Dashboard Panels**

- **Tenant rejection rate (total)**

  - Query:  

    ```promql
    sum(rate(router_results_tenant_rejected_total[5m]))
    ```

  - Use cases:

    - Overall level of business rejections.

- **Tenant rejections by tenant (top N)**

  - Query:  

    ```promql
    topk(10, sum by (tenant_id) (rate(router_results_tenant_rejected_total[5m])))
    ```

  - Use cases:

    - Find tenants frequently rejected by Router.

    - Separate panel with sorting for quick identification.

- **Tenant rejections by reason**

  - Query:  

    ```promql
    sum by (reason) (rate(router_results_tenant_rejected_total[5m]))
    ```

  - Use cases:

    - Separate business-as-usual rejections from bugs.

    - Understanding: quotas, tariffs, blocks, etc.

    - Common reasons: `quota_exceeded`, `plan_not_supported`, `tenant_disabled`, `usage_limit_reached`.

- **Correlation with DLQ and MaxDeliver**

  - Investigate if tenant rejections correlate with increased DLQ or exhausted MaxDeliver.

  - Query:

    ```promql
    sum(rate(router_results_tenant_rejected_total[5m])) 
    and 
    sum(rate(router_dlq_total[5m]))
    ```

---

## 2.6 Metrics Verification and Label Status

This section documents the **actual implementation status** of metrics and labels as verified in the codebase.

### 2.6.1 Metric Existence Verification

**✅ Verified Metrics** (all exist in code):

- `router_jetstream_redelivery_total` - ✅ Implemented in `router_jetstream.erl`
- `router_jetstream_maxdeliver_exhausted_total` - ✅ Implemented in `router_result_consumer.erl`, `router_ack_consumer.erl`, `router_decide_consumer.erl`
- `router_dlq_total` - ✅ Implemented in `router_jetstream.erl`
- `router_nats_connect_failures_total` - ✅ Implemented in `router_nats.erl`
- `router_nats_publish_failures_total` - ✅ Implemented in `router_nats.erl`
- `router_nats_ack_failures_total` - ✅ Implemented in `router_nats.erl`
- `router_nats_reconnect_failures_total` - ✅ Implemented in `router_nats.erl`
- `router_results_tenant_rejected_total` - ✅ Implemented in `router_result_consumer.erl`

**Metric Types** (all verified as counters):

- All metrics listed above are **counters** (increment-only), matching documentation assumptions.

### 2.6.2 Label Availability Status

**Metrics with Full Label Support**:

- `router_jetstream_redelivery_total`:
  - ✅ Required: `assignment_id`, `request_id`, `reason`, `source`
  - ✅ Optional: `msg_id`, `tenant_id`

- `router_jetstream_maxdeliver_exhausted_total`:
  - ✅ Required: `assignment_id`, `request_id`, `reason`
  - ✅ Optional: `msg_id`, `delivery_count`, `max_deliver`, `tenant_id`

- `router_results_tenant_rejected_total`:
  - ✅ Required: `assignment_id`, `request_id`, `reason`, `tenant_id`
  - ✅ Optional: `msg_id`

**Metrics Without Labels** (current implementation):

- `router_dlq_total` - ⚠️ **No labels** (simple counter)
- `router_nats_connect_failures_total` - ⚠️ **No labels** (simple counter)
- `router_nats_publish_failures_total` - ⚠️ **No labels** (simple counter)
- `router_nats_ack_failures_total` - ⚠️ **No labels** (simple counter)
- `router_nats_reconnect_failures_total` - ⚠️ **No labels** (simple counter)

**Note**: Dashboard panels that reference labels for metrics without labels are marked as "when labels added" or "future enhancement".

### 2.6.3 Reason Values Synchronization

**Real `reason` values from code** (synchronized with `router_jetstream.erl`):

**For `router_jetstream_redelivery_total`**:
- `tenant_validation_failed`
- `backoff`
- `backpressure`
- `ack_error`
- `nak_error`
- `processing_timeout`
- `publish_error`
- `nats_disconnect`
- `maxdeliver_exhausted`
- (fallback: any atom converted to binary)

**For `router_jetstream_maxdeliver_exhausted_total`**:
- `maxdeliver_exhausted` (always this value)

**For `router_results_tenant_rejected_total`**:
- `tenant_id is required`
- `tenant_id cannot be empty`
- `tenant_id not in allowlist`
- `no policy found for tenant_id`
- `tenant_id must be a binary`

**Source Values** (for `router_jetstream_redelivery_total`):
- `tenant_validation`
- `backoff`
- `backpressure`
- `ack_failure`
- `nak_failure`
- `processing_timeout`
- `publish_failure`
- `nats_disconnect`
- `maxdeliver_exhausted`

### 2.6.4 Documentation Alignment

**Status**: ✅ **Aligned**

- All metric names in documentation match code implementation.
- All documented labels match actual label availability.
- All `reason` values in documentation are real values from code.
- Metrics without labels are clearly marked in documentation.

**Reference**: See `../apps/otp/router/docs/dev/METRICS_CONTRACT_SPECIFICATION.md` for authoritative metric contracts.

---

## 3. Labels and Investigation Patterns

### 3.1 Core Labels

**Important Labels: What to Look at First**

- `assignment_id`  

  Primary axis for routing behavior. Use this to answer "which flow is broken?"

  - Answers: *which part of Router/which type of tasks is failing?*

- `tenant_id`  

  Primary axis for multi-tenancy. Use this to answer "which tenant is affected?"

  - Allows:

    - seeing specific client problems,

    - separating incidents "for one client" vs "for everyone".

- `reason`  

  Category of error or decision; crucial for grouping DLQ, rejections, and exhausted MaxDeliver.

  - Immediately gives a hypothesis: validation, network, business rule, quota, etc.

- `delivery_count` / `max_deliver`  

  Show how deep into retry logic messages go.

  - Show the dynamics of redeliveries and how close we are to DLQ/MaxDeliver.

- `request_id`, `msg_id`  

  Critical for pinpoint debugging: allow transitioning to logs/traces.

  - `request_id`: Cross-service correlation for a particular request.

  - `msg_id`: Low-level debugging, usually via logs.

- `source`  

  Where messages come from (upstream service, topic, or stream).

  - Helps localize problems to specific streams/topics/subsystems.

### 3.2 Recommended Use of Labels on the Dashboard

- Use `assignment_id` and `tenant_id` as **primary grouping dimensions** on graphs.

- Use `reason` for **drill-down panels** and for understanding failure types.

- Include `source` in legends when correlating with upstream systems.

- Reserve `request_id` and `msg_id` mainly for ad-hoc debugging and logs, not high-cardinality graphs.

---

## 4. Dashboard Panels: How to Use Them

This section explains **how to use each panel group** to understand Router JetStream health and make decisions. Each panel group answers specific questions and guides you through investigation workflows.

### 4.1 Overview Panel Group

**Purpose**: Get a quick health check - is the system healthy overall?

**When to Use**: 
- Start here when investigating any incident
- Daily health checks
- On-call handoff reviews

**Key Panels**:
- **Total DLQ rate**: Are messages accumulating in DLQ?
- **Total redelivery rate**: Are messages being redelivered frequently?
- **Total NATS failures**: Are there infrastructure problems?
- **MaxDeliver exhaustion rate**: Are messages failing completely?

**How to Read**:
- **Green**: All metrics within normal ranges
- **Yellow**: One or more metrics showing warning levels
- **Red**: Critical metrics (MaxDeliver exhausted, high DLQ rate)

**Next Steps Based on What You See**:
- If DLQ is high → Go to "DLQ and MaxDeliver" panel group
- If redeliveries are high → Go to "Redelivery" panel group
- If NATS failures are high → Go to "NATS Infrastructure Errors" panel group

### 4.2 Redelivery Panel Group

**Purpose**: Understand what's being redelivered and why - early warning of processing instability.

**When to Use**:
- Redelivery rate is high in Overview
- Investigating processing failures
- Identifying problematic assignments or tenants

**Key Panels and How to Use Them**:

1. **"Redelivery rate by assignment"**
   - **Question**: Which handlers/assignments are failing most?
   - **How to use**: Click on a high-value assignment to drill down
   - **Action**: If one assignment dominates, investigate that handler's logic
   - **Query**: `sum by (assignment_id) (rate(router_jetstream_redelivery_total[5m]))`

2. **"Redelivery rate by tenant"**
   - **Question**: Is it one problematic tenant or system-wide?
   - **How to use**: Identify "toxic" tenants with high rejection rates
   - **Action**: If one tenant dominates, check their payloads and configuration
   - **Query**: `sum by (tenant_id) (rate(router_jetstream_redelivery_total[5m]))`

3. **"Redelivery by reason"**
   - **Question**: What's causing the redeliveries?
   - **How to use**: Understand root cause (validation, timeout, NATS error, etc.)
   - **Action**: 
     - If `tenant_validation_failed` → Check tenant configuration
     - If `processing_timeout` → Check downstream service health
     - If `nats_disconnect` → Check NATS infrastructure
   - **Query**: `sum by (reason) (rate(router_jetstream_redelivery_total[5m]))`

4. **"Redelivery vs Successful processing"** (stacked)
   - **Question**: What's the ratio of failures to successes?
   - **How to use**: See degradation in processing quality over time
   - **Action**: If redelivery layer is growing, investigate before it becomes DLQ

5. **"Avg/Max delivery_count per assignment"** (if available)
   - **Question**: How close are messages to MaxDeliver limit?
   - **How to use**: Identify assignments where messages are near exhaustion
   - **Action**: If delivery_count is close to max_deliver, investigate urgently

### 4.3 DLQ and MaxDeliver

- **Graph "DLQ inflow total"**

  - `sum(rate(router_dlq_total[5m]))`

- **Graph "DLQ inflow by reason"**

  - Helps quickly classify the problem.

- **Graph "MaxDeliver exhausted rate"**

  - Ideally ≈0. Any spike is an alert.

- **Table "Top assignments by DLQ / MaxDeliver exhausted (last 24h)"**

### 4.4 NATS Infrastructure Errors

**Scenario IDs**: NATS-001 (NATS Connection Failures), NATS-002 (NATS Reconnection Failures), NATS-003 (NATS Publish Failures), NATS-004 (NATS ACK Failures), NATS-005 (NATS Subscribe Failures), NATS-006 (NATS NAK Failures)  
**Coverage Matrix**: `../apps/otp/router/docs/archive/dev_reports/JETSTREAM_OBS_COVERAGE_MATRIX.md` (NATS-001 to NATS-008)

- **Graph "NATS connect / reconnect failures"**

  **Scenario IDs**: NATS-001 (NATS Connection Failures), NATS-002 (NATS Reconnection Failures)  
  **Alert**: `RouterNatsConnectionFailures`, `RouterNatsFrequentReconnects` (Scenario IDs: NATS-001, NATS-002)

  - Two lines: connect and reconnect.

- **Graph "NATS publish failures by reason"**

  **Scenario IDs**: NATS-003 (NATS Publish Failures)  
  **Alert**: `RouterNatsHighPublishFailureRate` (Scenario ID: NATS-003)

  - Helps distinguish `timeout` from `authorization` or `no_route`.

- **Graph "NATS ACK failures"**

  **Scenario IDs**: S1 (Intermittent ACK/NAK Errors), NATS-004 (NATS ACK Failures)  
  **Alert**: `RouterNatsHighAckFailureRate` (Scenario IDs: S1, NATS-004)

  - Correlate with redelivery spikes.

### 4.5 Tenant / Usage Rejections Panel Group

**Purpose**: Understand business-level rejections - intentional vs unintentional.

**When to Use**:
- Tenant rejection rate is high
- Distinguishing business rejections from infrastructure failures
- Investigating quota or configuration issues

**Key Panels and How to Use Them**:

1. **"Tenant rejections total"**
   - **Question**: What's the overall level of business rejections?
   - **How to use**: Baseline for understanding if rejections are normal
   - **Action**: If spike, investigate tenant breakdown
   - **Query**: `sum(rate(router_results_tenant_rejected_total[5m]))`

2. **"Tenant rejections by tenant_id (top N)"**
   - **Question**: Which tenants are being rejected most?
   - **How to use**: Identify problematic tenants
   - **Action**: 
     - If one tenant dominates → Check their configuration
     - If multiple tenants → Check policy registry
   - **Query**: `topk(10, sum by (tenant_id) (rate(router_results_tenant_rejected_total[5m])))`

3. **"Tenant rejections by reason"**
   - **Question**: Why are tenants being rejected?
   - **How to use**: Understand rejection categories
   - **Action**:
     - If `tenant_id not in allowlist` → Check allowlist configuration
     - If `tenant_id is required` → Check payload format
     - If `no policy found for tenant_id` → Check policy registry
   - **Query**: `sum by (reason) (rate(router_results_tenant_rejected_total[5m]))`

4. **"Correlation with DLQ / MaxDeliver"**
   - **Question**: Do tenant rejections lead to DLQ?
   - **How to use**: Understand if business rejections cause message failures
   - **Action**: If correlated, review rejection logic and MaxDeliver configuration

### 4.6 Correlation / Overview Panel Group

**Purpose**: See the big picture - how metrics relate and degradation chains.

**When to Use**:
- Understanding incident timeline
- Identifying degradation patterns
- Root cause analysis

**Key Panels and How to Use Them**:

1. **"Stacked graph: Redelivery + DLQ + MaxDeliver exhausted"**
   - **Question**: What's the degradation chain?
   - **How to use**: Timeline shows progression:
     - First: Redeliveries grow (early warning)
     - Then: DLQ grows (messages accumulating)
     - Finally: MaxDeliver exhausted (complete failure)
   - **Action**: If you see this pattern, investigate early (at redelivery stage)
   - **Query**: Stack all three metrics on same graph

2. **"Heatmap: Redelivery rate (assignment × tenant)"**
   - **Question**: Which assignment + tenant combinations are problematic?
   - **How to use**: Visual identification of hot spots
   - **Action**: Focus investigation on hot spots (dark cells)
   - **Query**: `sum by (assignment_id, tenant_id) (rate(router_jetstream_redelivery_total[5m]))`

3. **"Correlation: NATS failures vs redeliveries"**
   - **Question**: Do infrastructure problems cause application problems?
   - **How to use**: Overlay NATS failures and redeliveries
   - **Action**: If correlated, fix infrastructure first

### 4.7 Performance & Capacity Panel Group

**Scenario IDs**: `PERF-001` (Processing Latency Degradation), `PERF-002` (Backpressure Active), `PERF-003` (High Queue Depth), `PERF-004` (High In-Flight Messages)  
**Alerts**: `RouterIntakeLatencyHigh`, `RouterIntakeBackpressureActive`, `RouterIntakeQueueDepthHigh`, `RouterIntakeInflightHigh`  
**Alert File**: `apps/otp/router/docs/PROMETHEUS_ALERTS.md` (lines 245-276)  
**Coverage Analysis**: `../apps/otp/router/docs/dev/OBS_COVERAGE_ANALYSIS.md#performance-and-degradation-scenarios`

**Purpose**: Monitor processing performance, capacity limits, and overload conditions.

**When to Use**:
- Processing latency is high
- Queue depth is growing
- Backpressure is active
- Capacity planning and scaling decisions

**Key Panels and How to Use Them**:

1. **"Processing Latency (P50/P95/P99)"**
   - **Question**: What's the processing latency distribution?
   - **How to use**: Track latency percentiles over time
   - **Action**: 
     - If P95 > 2000ms → Investigate processing bottlenecks
     - If P99 spikes → Check for slow downstream services
   - **Query**: 
     ```promql
     router_intake_processing_latency_p50{subject="beamline.router.v1.decide"}
     router_intake_processing_latency_p95{subject="beamline.router.v1.decide"}
     router_intake_processing_latency_p99{subject="beamline.router.v1.decide"}
     ```
   - **Panel ID**: `router-processing-latency-percentiles`

2. **"Queue Depth (Pending Messages)"**
   - **Question**: How many messages are waiting to be processed?
   - **How to use**: Monitor queue depth trends
   - **Action**:
     - If > 100 → Warning (investigate processing rate)
     - If > 1000 → Critical (backpressure may activate)
   - **Query**: 
     ```promql
     router_jetstream_pending_messages{subject="beamline.router.v1.decide"}
     ```
   - **Panel ID**: `router-queue-depth`

3. **"In-Flight Messages"**
   - **Question**: How many messages are currently being processed?
   - **How to use**: Track concurrent processing load
   - **Action**:
     - If > 200 → Warning (processing may be slow)
     - If > 500 → Critical (overload condition)
   - **Query**: 
     ```promql
     router_intake_inflight_messages{subject="beamline.router.v1.decide"}
     ```
   - **Panel ID**: `router-inflight-messages`

4. **"Backpressure Status"**
   - **Question**: Is Router overloaded and rejecting new messages?
   - **How to use**: Gauge showing backpressure state (0=inactive, 1=active)
   - **Action**:
     - If active → Router is overloaded, new requests rejected (HTTP 503)
     - Check queue depth, latency, and in-flight messages
   - **Query**: 
     ```promql
     router_intake_backpressure_active{subject="beamline.router.v1.decide"}
     ```
   - **Panel ID**: `router-backpressure-status`

5. **"Backpressure Triggers"**
   - **Question**: What triggered backpressure activation?
   - **How to use**: Breakdown by trigger type (queue, latency, inflight)
   - **Action**: Identify which capacity limit was exceeded
   - **Query**: 
     ```promql
     sum by (trigger) (rate(router_intake_backpressure_triggered_total[5m]))
     ```
   - **Panel ID**: `router-backpressure-triggers`

6. **"Processing Rate vs Queue Depth"**
   - **Question**: Is processing keeping up with incoming messages?
   - **How to use**: Overlay processing rate and queue depth
   - **Action**: 
     - If queue growing while processing rate stable → Processing bottleneck
     - If both increasing → Incoming rate exceeds capacity
   - **Query**: 
     ```promql
     rate(router_jetstream_delivered_messages_total[5m])
     router_jetstream_pending_messages
     ```
   - **Panel ID**: `router-processing-rate-vs-queue`

---

## 5. Investigation Scenarios

### 5.1 "DLQ is Growing for a Single Assignment or Tenant"

**Symptoms**: `router_dlq_total` increasing, concentrated on specific `assignment_id` or `tenant_id`.

**Investigation Steps**:

1. Open **DLQ by assignment** and **DLQ by tenant**:

   - Identify which `assignment_id` / `tenant_id` is responsible for the spike.

   - Query (when labels added):

     ```promql
     sum by (assignment_id) (rate(router_dlq_total[5m]))
     sum by (tenant_id) (rate(router_dlq_total[5m]))
     ```

2. Drill down by `reason`:

   - Check if this is a known error (e.g. validation issues) or a new failure category.

   - Query (when labels added):

     ```promql
     sum by (reason) (rate(router_dlq_total{assignment_id="<problematic_assignment>"}[5m]))
     ```

3. Check **redeliveries**:

   - `router_jetstream_redelivery_total` filtered by the same `assignment_id` / `tenant_id`.

   - If redeliveries are high before DLQ, messages are repeatedly failing.

   - Query:

     ```promql
     sum(rate(router_jetstream_redelivery_total{assignment_id="<problematic_assignment>"}[5m])) by (reason, delivery_count)
     ```

4. Check **MaxDeliver exhaustion**:

   - Verify if MaxDeliver exhaustion is the cause of DLQ entries.

   - Query:

     ```promql
     sum(rate(router_jetstream_maxdeliver_exhausted_total{assignment_id="<problematic_assignment>"}[5m])) by (reason)
     ```

5. If tenant-related:

   - Inspect `router_results_tenant_rejected_total` for the same `tenant_id`.

   - Query:

     ```promql
     sum(rate(router_results_tenant_rejected_total{tenant_id="<problematic_tenant>"}[5m])) by (reason)
     ```

   - Verify configuration, quotas, and feature flags.

---

### 5.2 "Spike in Redeliveries Without DLQ Growth"

**Symptoms**: `router_jetstream_redelivery_total` increasing sharply, but `router_dlq_total` and `router_jetstream_maxdeliver_exhausted_total` remain low/zero.

**Investigation Steps**:

1. Look at **redeliveries over time**:

   - `router_jetstream_redelivery_total` (global and by assignment).

   - Query:

     ```promql
     sum(rate(router_jetstream_redelivery_total[5m])) by (source, reason)
     ```

2. Confirm DLQ stays flat:

   - `router_dlq_total` remains low/stable.

   - Query:

     ```promql
     sum(rate(router_dlq_total[5m]))
     ```

3. Check **delivery_count distribution**:

   - Verify messages are at low delivery_count (1st, 2nd retry).

   - Query:

     ```promql
     sum(rate(router_jetstream_redelivery_total[5m])) by (delivery_count)
     ```

4. Check **NATS failures**:

   - `router_nats_connect_failures_total`

   - `router_nats_reconnect_failures_total`

   - `router_nats_publish_failures_total`

   - `router_nats_ack_failures_total`

   - Query:

     ```promql
     sum(rate(router_nats_connect_failures_total[5m]))
     sum(rate(router_nats_reconnect_failures_total[5m]))
     sum(rate(router_nats_publish_failures_total[5m]))
     sum(rate(router_nats_ack_failures_total[5m]))
     ```

5. Check **tenant rejections**:

   - Verify if tenant rejections are causing redeliveries.

   - Query:

     ```promql
     sum(rate(router_results_tenant_rejected_total[5m])) by (tenant_id, reason)
     ```

**Interpretation**:

- Likely a temporary infrastructure issue causing retries but ultimately successful deliveries.

- If redeliveries return to baseline and DLQ stays low, the incident is transient.

- If redeliveries continue, messages may eventually exhaust MaxDeliver → DLQ.

---

### 5.3 "NATS Infrastructure Incident"

**Symptoms**: Infrastructure failures leading to message delivery problems.

**Typical Sequence**:

1. **Early signal:**

   - Spike in `router_nats_connect_failures_total` and/or `router_nats_reconnect_failures_total`.

2. **Propagation to delivery:**

   - Increase in `router_nats_publish_failures_total` and `router_nats_ack_failures_total`.

   - Growth in `router_jetstream_redelivery_total`.

3. **If incident persists:**

   - Growth in `router_jetstream_maxdeliver_exhausted_total`.

   - Possible increase in `router_dlq_total`.

**Investigation Steps**:

1. Check connection failures:

   ```promql
   sum(rate(router_nats_connect_failures_total[5m]))
   ```

2. Check reconnection failures:

   ```promql
   sum(rate(router_nats_reconnect_failures_total[5m]))
   ```

3. Check publish/ACK failures:

   ```promql
   sum(rate(router_nats_publish_failures_total[5m]))
   sum(rate(router_nats_ack_failures_total[5m]))
   ```

4. Check redelivery rate:

   ```promql
   sum(rate(router_jetstream_redelivery_total[5m]))
   ```

5. Check MaxDeliver exhaustion:

   ```promql
   sum(rate(router_jetstream_maxdeliver_exhausted_total[5m]))
   ```

6. Check DLQ rate:

   ```promql
   sum(rate(router_dlq_total[5m]))
   ```

**Recommended Actions on Dashboard**:

- Correlate NATS failures and redeliveries on a single time graph.

- Check which clusters, subjects, or assignments are most affected.

- Use this view to decide whether to escalate as an infrastructure incident.

---

## 5.4 Practical Debugging Guides

### 5.4.1 How to Debug a Redelivery Spike

**Step-by-step debugging process**:

1. **Check Redelivery by Assignment**:
   ```promql
   sum by (assignment_id) (rate(router_jetstream_redelivery_total[5m]))
   ```
   - Identify which assignment is causing the spike.

2. **Narrow Down to Specific Assignment/Tenant**:
   ```promql
   sum by (tenant_id, reason) (rate(router_jetstream_redelivery_total{assignment_id="<problematic_assignment>"}[5m]))
   ```
   - Find specific tenant and reason combination.

3. **Check DLQ / MaxDeliver**:
   ```promql
   sum(rate(router_dlq_total[5m]))
   sum(rate(router_jetstream_maxdeliver_exhausted_total[5m]))
   ```
   - Verify if redeliveries are progressing to DLQ.

4. **Check NATS Errors**:
   ```promql
   sum(rate(router_nats_connect_failures_total[5m]))
   sum(rate(router_nats_publish_failures_total[5m]))
   sum(rate(router_nats_ack_failures_total[5m]))
   ```
   - Correlate with infrastructure issues.

5. **Transition to Logs**:
   - Use `request_id` or `msg_id` from metrics to find corresponding log entries.
   - Search logs for specific `assignment_id` and `tenant_id` combination.

**Common Causes**:
- Tenant validation failures → Check tenant configuration
- Processing timeouts → Check downstream service health
- NATS connection issues → Check NATS cluster status
- Backpressure → Check consumer lag and processing capacity

### 5.4.2 How to Distinguish Infrastructure vs Business Issues

**Infrastructure Problems** (NATS/JetStream):

**Signals**:
- `router_nats_*_failures_total` metrics are growing
- Redeliveries increase, but `router_results_tenant_rejected_total` stays flat
- Connection/reconnect failures spike
- Publish/ACK failures correlate with redelivery spikes

**Queries**:
```promql
# Infrastructure health
sum(rate(router_nats_connect_failures_total[5m])) + 
sum(rate(router_nats_reconnect_failures_total[5m])) +
sum(rate(router_nats_publish_failures_total[5m])) +
sum(rate(router_nats_ack_failures_total[5m]))
```

**Business Problems** (Tenant/Usage):

**Signals**:
- `router_results_tenant_rejected_total` is growing
- `reason` values like `tenant_id not in allowlist`, `tenant_id is required`
- Tenant-specific rejections without NATS errors
- Quota/usage limit rejections

**Queries**:
```promql
# Business rejections
sum by (reason, tenant_id) (rate(router_results_tenant_rejected_total[5m]))
```

**Decision Tree**:
1. If NATS failures > 0 → Infrastructure issue
2. If tenant rejections > 0 AND NATS failures = 0 → Business issue
3. If both > 0 → Mixed issue (investigate correlation)

### 5.4.3 Mini-Runbook for MaxDeliver / DLQ

**Scenario 1: MaxDeliver Exhausted > 0**

**Immediate Actions**:
1. Check which assignment is exhausting:
   ```promql
   sum by (assignment_id) (rate(router_jetstream_maxdeliver_exhausted_total[5m]))
   ```

2. Check reason (should be `maxdeliver_exhausted`):
   ```promql
   sum by (reason) (rate(router_jetstream_maxdeliver_exhausted_total[5m]))
   ```

3. Check tenant distribution:
   ```promql
   sum by (tenant_id) (rate(router_jetstream_maxdeliver_exhausted_total[1h]))
   ```

4. **Investigation**:
   - Review logs for specific `msg_id` or `request_id`
   - Check if messages are malformed or downstream is consistently failing
   - Verify MaxDeliver configuration (may be too low)

5. **Resolution**:
   - If code bug: Fix and redeploy
   - If configuration: Adjust MaxDeliver limit
   - If downstream issue: Fix downstream service
   - If malformed payloads: Fix payload validation

**Scenario 2: DLQ Inflow Spike**

**Immediate Actions**:
1. Check DLQ rate:
   ```promql
   sum(rate(router_dlq_total[5m]))
   ```

2. Check if MaxDeliver exhaustion is the cause:
   ```promql
   sum(rate(router_jetstream_maxdeliver_exhausted_total[5m]))
   ```

3. **Investigation** (when labels added):
   ```promql
   sum by (reason) (rate(router_dlq_total[5m]))
   sum by (assignment_id) (rate(router_dlq_total[5m]))
   sum by (tenant_id) (rate(router_dlq_total[5m]))
   ```

4. **Resolution**:
   - **If `maxdeliver_exhausted`**: Follow MaxDeliver exhaustion runbook
   - **If `payload_invalid`**: Review payload validation rules
   - **If `unsupported_version`**: Update protocol version support
   - **If `business_rule_violation`**: Review business rules

5. **DLQ Recovery**:
   - Evaluate if DLQ messages can be safely reprocessed
   - Check if hotfix is needed before reprocessing
   - Use `msg_id` or `request_id` to identify specific problematic messages

---

## 6. Alerting (Formalized Rules)

Formalized alert definitions with conditions, thresholds, duration, and severity. For detailed alert definitions and runbooks, see `apps/otp/router/docs/PROMETHEUS_ALERTS.md`.

### 6.1 Critical Alerts

#### 6.1.1 DLQ High Rate (DLQ Inflow Spike)

**Scenario ID**: `JS-003` (DLQ Growth)  
**Alert Name**: `RouterDLQHighRate`  
**Alert File**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (**⚠️ MISSING** - see `../apps/otp/router/docs/dev/OBS_COVERAGE_ANALYSIS.md#missing-dlq-alert-in-alert-rules`)  
**Coverage Analysis**: `../apps/otp/router/docs/dev/OBS_COVERAGE_ANALYSIS.md#js-003-dlq-growth`

**Condition**:
```promql
sum(rate(router_dlq_total[5m])) > threshold
```

**Thresholds**:
- **Warning**: > 0.1 messages/sec for > 5 minutes
- **Critical**: > 1 messages/sec for > 1 minute

**Severity**: `critical` (for critical threshold), `warning` (for warning threshold)

**Duration**: `5m` (warning), `1m` (critical)

**Description**: Rapid growth in DLQ indicates messages are failing to process and accumulating.

**Dashboard Panels**:
- **DLQ inflow total** - Section 4.3, Panel 1
- **DLQ inflow by reason** - Section 4.3, Panel 2 (when labels added)
- **DLQ inflow by tenant** - Section 4.3, Panel 3 (when labels added)
- **DLQ by assignment** - Section 4.3, Panel 4 (when labels added)

**Recommended Actions**:
1. Check DLQ by reason (when labels added)
2. Check MaxDeliver exhaustion rate
3. Review logs for specific `msg_id` or `request_id`
4. Follow DLQ runbook (Section 5.4.3)

#### 6.1.2 MaxDeliver Exhausted Non-Zero

**Scenario ID**: `JS-002` (MaxDeliver Exhaustion)  
**Alert Name**: `RouterJetStreamMaxDeliverExhausted`  
**Alert File**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (lines 92-114)  
**Coverage Analysis**: `../apps/otp/router/docs/dev/OBS_COVERAGE_ANALYSIS.md#js-002-maxdeliver-exhaustion`

**Condition**:
```promql
sum(rate(router_jetstream_maxdeliver_exhausted_total[5m])) > 0
```

**Threshold**: > 0 for > 1 minute

**Severity**: `critical`

**Duration**: `1m`

**Description**: Any message exhausting MaxDeliver indicates irrecoverable processing failures.

**Dashboard Panels**:
- **MaxDeliver exhausted rate (overall)** - Section 2.2, Panel 1
- **MaxDeliver exhausted by assignment** - Section 2.2, Panel 2
- **Top assignments by MaxDeliver exhausted** - Section 2.2, Panel 3
- **Top tenants by MaxDeliver exhausted** - Section 2.2, Panel 4

**Recommended Actions**:
1. Check which assignment is exhausting (Section 5.4.3)
2. Review logs for specific `msg_id` or `request_id`
3. Check if MaxDeliver configuration is appropriate
4. Follow MaxDeliver runbook (Section 5.4.3)

#### 6.1.3 High Redelivery Rate for Assignment

**Scenario ID**: `JS-001` (High Redelivery Rate)  
**Alert Name**: `RouterJetStreamHighRedeliveryRate`  
**Alert File**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (lines 38-62)  
**Coverage Analysis**: `../apps/otp/router/docs/dev/OBS_COVERAGE_ANALYSIS.md#js-001-high-redelivery-rate`

**Condition**:
```promql
sum(rate(router_jetstream_redelivery_total[5m])) by (assignment_id) > threshold
```

**Thresholds**:
- **Warning**: > 5% of total messages for > 5 minutes
- **Critical**: > 10% of total messages for > 5 minutes

**Severity**: `critical` (for critical threshold), `warning` (for warning threshold)

**Duration**: `5m` (both thresholds)

**Description**: High redelivery rate for specific assignment indicates processing instability.

**Dashboard Panels**:
- **Redelivery rate by assignment** - Section 4.2, Panel 1
- **Redelivery rate by tenant** - Section 4.2, Panel 2
- **Redelivery by reason** - Section 4.2, Panel 3
- **Redelivery vs Successful processing** - Section 4.2, Panel 4

**Recommended Actions**:
1. Follow redelivery debugging guide (Section 5.4.1)
2. Check NATS infrastructure errors
3. Check tenant rejections
4. Review assignment-specific logs

### 6.2 Warning Alerts

#### 6.2.1 NATS Connection / Reconnect Failures Spike

**Scenario ID**: `NATS-001` (NATS Connection Failures)  
**Alert Name**: `RouterNatsConnectionFailures`, `RouterNatsFrequentReconnects`  
**Alert File**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (lines 182-252)  
**Coverage Analysis**: `../apps/otp/router/docs/dev/OBS_COVERAGE_ANALYSIS.md#nats-001-nats-connection-failures`

**Condition**:
```promql
sum(rate(router_nats_connect_failures_total[5m])) + 
sum(rate(router_nats_reconnect_failures_total[5m])) > threshold
```

**Thresholds**:
- **Warning**: > 0 for > 5 minutes
- **Critical**: > 1 per hour

**Severity**: `critical` (for critical threshold), `warning` (for warning threshold)

**Duration**: `5m` (warning), `1h` (critical)

**Description**: NATS connection failures indicate infrastructure problems.

**Dashboard Panels**:
- **NATS connect/reconnect failures** - Section 4.4, Panel 1
- **NATS connection status** - Section 4.4 (implied)

**Recommended Actions**:
1. Check NATS cluster health
2. Review network connectivity
3. Check NATS configuration (auth, TLS, endpoints)
4. Correlate with redelivery spikes

#### 6.2.2 Tenant Rejections Spike for Specific Tenant

**Scenario ID**: `TENANT-001` (High Tenant Rejection Rate)  
**Alert Name**: `RouterTenantRejectedHigh`  
**Alert File**: `apps/otp/router/docs/PROMETHEUS_ALERTS.md` (lines 169-177)  
**Coverage Analysis**: `../apps/otp/router/docs/dev/OBS_COVERAGE_ANALYSIS.md#tenant-001-high-tenant-rejection-rate`

**Condition**:
```promql
sum(rate(router_results_tenant_rejected_total[5m])) by (tenant_id) > threshold
```

**Thresholds**:
- **Warning**: > 1% of total messages for > 5 minutes
- **Critical**: > 5% of total messages for > 1 minute

**Severity**: `critical` (for critical threshold), `warning` (for warning threshold)

**Duration**: `5m` (warning), `1m` (critical)

**Description**: High tenant rejection rate indicates business rule violations or configuration issues.

**Dashboard Panels**:
- **Tenant rejections total** - Section 4.5, Panel 1
- **Tenant rejections by tenant_id (top N)** - Section 4.5, Panel 2
- **Tenant rejections by reason** - Section 4.5, Panel 3
- **Correlation with DLQ / MaxDeliver** - Section 4.5, Panel 4

**Recommended Actions**:
1. Check rejection reason (Section 5.4.2)
2. Verify tenant configuration
3. Review quota/usage limits
4. Check for suspicious activity

### 6.3 Alert Threshold Selection Guidelines

**For Production**:
- Use stricter thresholds (lower values, shorter duration)
- Monitor baseline for 1-2 weeks before setting final thresholds
- Adjust based on actual incident patterns

**For Staging/Test**:
- Use relaxed thresholds (higher values, longer duration)
- Focus on detecting systematic issues, not transient failures

**Threshold Tuning**:
1. Start with conservative thresholds (higher values)
2. Monitor for false positives
3. Gradually tighten thresholds based on baseline
4. Document threshold rationale in alert annotations

These alerts should point SRE/owners to the corresponding dashboard sections for deeper analysis.

---

## 7. Glossary

**Redelivery**: The process of JetStream delivering the same message again after a negative ACK (NAK), processing error, or timeout. Redeliveries occur when a message cannot be successfully processed on the first attempt.

**DLQ (Dead Letter Queue)**: A queue where messages that cannot be processed are sent after exhausting retry attempts or meeting other failure criteria. DLQ messages require manual or offline processing.

**MaxDeliver**: The maximum number of times JetStream will attempt to deliver a message before considering it failed. When MaxDeliver is exhausted, the message is typically sent to DLQ or discarded.

**Tenant Rejection**: A business-level rejection where Router refuses to process a message due to tenant-related constraints (quota exceeded, tenant disabled, invalid configuration, etc.). Distinguished from infrastructure failures.

**Assignment ID**: Identifier of a specific route/assignment/pipeline in Router. Used to localize problems to specific message processing flows.

**Request ID**: Correlation identifier across services for a particular request. Used for distributed tracing and log correlation.

**Message ID (msg_id)**: Unique identifier for a message in JetStream/NATS. Used for low-level debugging and message tracking.

**Source**: Origin or cause of a redelivery or error (e.g., `tenant_validation`, `ack_failure`, `publish_failure`). Helps identify which subsystem triggered the issue.

**Reason**: Categorical code explaining why an event occurred (e.g., `tenant_validation_failed`, `maxdeliver_exhausted`, `processing_timeout`). Used for grouping and analysis.

---

## 8. References

- **Observability Overview**: `docs/OBSERVABILITY.md` - General observability requirements
- **Prometheus Alerts**: `apps/otp/router/docs/PROMETHEUS_ALERTS.md` - Detailed alert definitions
- **Alert Rules**: `apps/otp/router/docs/observability/router-alert-rules.yaml` - Alert rules configuration
- **Coverage Analysis**: `../apps/otp/router/docs/dev/OBS_COVERAGE_ANALYSIS.md` - Complete scenario-to-alert-to-dashboard mapping
- **NATS Connection Resilience**: `apps/otp/router/docs/NATS_CONNECTION_RESILIENCE.md` - NATS metrics and resilience
- **NATS Subjects**: `apps/otp/router/docs/NATS_SUBJECTS.md` - NATS subject definitions
- **Reliability Guide**: `apps/otp/router/docs/RELIABILITY_FAULT_TOLERANCE.md` - Fault tolerance mechanisms
- **Operational Guide**: `apps/otp/router/docs/OPERATIONAL_GUIDE.md` - Operational procedures

---

## 9. Future Enhancements

### 8.1 Metric Label Enhancements

**Priority 1** (Critical for observability):
- Add labels to `router_dlq_total`: `assignment_id`, `reason`, `source`, `tenant_id`, `msg_id`
- Add labels to NATS failure metrics: `reason`, `subject`, `tenant_id`

**Priority 2** (Nice to have):
- Add `delivery_count` and `max_deliver` labels to all JetStream metrics
- Add `trace_id` label for distributed tracing correlation

### 8.2 Additional Metrics

**Recommended**:
- `router_jetstream_pending_messages` (gauge) - Current pending messages in JetStream consumer
- `router_jetstream_ack_pending_messages` (gauge) - Messages awaiting ACK
- `router_jetstream_delivered_messages_total` (counter) - Total delivered messages
- `router_jetstream_redelivered_messages_total` (counter) - Total redelivered messages (JetStream-level, not Router-level)

### 8.3 Dashboard Enhancements

**Recommended**:
- Add correlation panels (redelivery vs infrastructure errors, DLQ vs MaxDeliver exhaustion)
- Add prediction panels (trend analysis, anomaly detection)
- Add drill-down views (detailed assignment/tenant analysis)
- Add log correlation (link metrics to logs via request_id, msg_id)
