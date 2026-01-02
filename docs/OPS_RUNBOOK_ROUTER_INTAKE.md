# Router Intake Operations Runbook

**Version**: CP2-LC  
**Date**: 2025-11-26  
**Status**: Production Ready  
**Target Audience**: SRE/Ops teams

---

## Overview

This runbook provides operational procedures for troubleshooting and resolving Router intake failures. It covers symptoms, diagnosis, and remediation for common intake issues including DLQ growth, validation errors, and NATS failures.

**Key Components**:
- Router Intake Validation (`router_intake_validator.erl`)
- Error Handling (`router_intake_error_handler.erl`)
- DLQ Support (configurable DLQ subject)
- JetStream Integration (durable subscriptions, ACK/NAK)
- Backpressure Detection (`router_intake_backpressure.erl`)

---

## Table of Contents

1. [Health Checks](#health-checks)
2. [Common Symptoms](#common-symptoms)
3. [Diagnosis Procedures](#diagnosis-procedures)
4. [Remediation Steps](#remediation-steps)
5. [Emergency Procedures](#emergency-procedures)
6. [Monitoring & Alerts](#monitoring--alerts)
7. [Troubleshooting](#troubleshooting)

---

## Health Checks

### 1. Router Intake Health

#### Check Router Process Status

**Via Erlang Shell**:
```erlang
%% Check Router is running
whereis(router_decide_consumer).

%% Check Router supervisor
whereis(beamline_router_sup).

%% Check process tree
supervisor:which_children(beamline_router_sup).
```

**Expected Output**:
```erlang
%% Consumer running
<0.123.0>

%% Supervisor running
<0.100.0>

%% Process tree
[{router_decide_consumer, <0.123.0>, worker, [router_decide_consumer]}, ...]
```

#### Check Intake Metrics

**Via Prometheus/Grafana**:
```promql
# Total messages processed
rate(router_intake_messages_total[5m])

# Validation errors
rate(router_intake_validation_errors_total[5m])

# DLQ messages
rate(router_intake_dlq_messages_total[5m])

# Backpressure status
router_intake_backpressure_active{subject="beamline.router.v1.decide"}
```

**Expected Values**:
- `router_intake_messages_total{status="ok"}` > 0 (messages being processed)
- `router_intake_validation_errors_total` ≈ 0 (no validation errors)
- `router_intake_dlq_messages_total` ≈ 0 (no DLQ messages)
- `router_intake_backpressure_active` = 0 (no backpressure)

#### Check JetStream Consumer Status

**Via NATS CLI**:
```bash
# Check consumer info
nats consumer info JS beamline.router.v1.decide router-decide-consumer

# Check pending messages
nats consumer info JS beamline.router.v1.decide router-decide-consumer --json | jq '.num_pending'

# Check redelivered messages
nats consumer info JS beamline.router.v1.decide router-decide-consumer --json | jq '.num_redelivered'
```

**Expected Values**:
- `num_pending` < 100 (low queue depth)
- `num_redelivered` ≈ 0 (no excessive redeliveries)
- `delivered.consumer_seq` increasing (messages being processed)

---

## Common Symptoms

### Symptom 1: DLQ Growth

**Indicators**:
- `router_intake_dlq_messages_total` increasing rapidly
- DLQ subject (`beamline.router.v1.decide.dlq`) accumulating messages
- Alerts: `RouterIntakeDLQHigh` (if configured)

**Possible Causes**:
1. **Schema Validation Failures**: Invalid protobuf/JSON format
2. **Version Mismatch**: Unsupported schema version
3. **Correlation Field Errors**: Missing/invalid `run_id`, `flow_id`, `step_id`
4. **Tenant Validation Failures**: Tenant not in allowlist
5. **Idempotency Violations**: Duplicate messages (if idempotency enabled)

**Diagnosis**: See [Diagnosis: DLQ Growth](#diagnosis-dlq-growth)

### Symptom 2: Validation Errors Spike

**Indicators**:
- `router_intake_validation_errors_total` increasing
- Error codes: `SCHEMA_VALIDATION_FAILED`, `VERSION_UNSUPPORTED`, `CORRELATION_FIELDS_INVALID`
- Audit logs showing validation failures

**Possible Causes**:
1. **Client Sending Invalid Messages**: Corrupt protobuf/JSON
2. **Schema Version Mismatch**: Client using unsupported version
3. **Missing Required Fields**: Client not sending mandatory fields
4. **Format Errors**: Invalid UUID, W3C Trace Context format

**Diagnosis**: See [Diagnosis: Validation Errors](#diagnosis-validation-errors)

### Symptom 3: NATS Connection Failures

**Indicators**:
- `router_nats_connection_errors_total` increasing
- `router_nats_reconnect_total` increasing
- Router logs showing NATS connection errors
- Messages not being received

**Possible Causes**:
1. **NATS Server Down**: NATS server unavailable
2. **Network Issues**: Network partition or connectivity problems
3. **TLS Configuration**: TLS certificate/key issues
4. **Authentication Failures**: NATS authentication credentials invalid

**Diagnosis**: See [Diagnosis: NATS Failures](#diagnosis-nats-failures)

### Symptom 4: Backpressure Active

**Indicators**:
- `router_intake_backpressure_active` = 1
- `router_jetstream_pending_messages` > 1000
- `router_intake_processing_latency_p95` > 5000ms
- New requests rejected with HTTP 503

**Possible Causes**:
1. **High Message Volume**: Incoming rate exceeds processing capacity
2. **Slow Processing**: Processing latency too high
3. **Resource Exhaustion**: CPU/memory limits reached

**Diagnosis**: See [Diagnosis: Backpressure](#diagnosis-backpressure)

---

## Diagnosis Procedures

### Diagnosis: DLQ Growth

#### Step 1: Check DLQ Metrics

**Via Prometheus/Grafana**:
```promql
# DLQ message rate
rate(router_intake_dlq_messages_total[5m]) by (error_code, subject)

# DLQ publish failures
rate(router_intake_dlq_publish_failed_total[5m]) by (reason, error_code)

# Total DLQ messages
sum(router_intake_dlq_messages_total)
```

**What to Look For**:
- Which error codes are most common?
- Is DLQ publication failing?
- Which subjects are affected?

#### Step 2: Check Error Distribution

**Via Prometheus/Grafana**:
```promql
# Validation errors by code
rate(router_intake_validation_errors_total[5m]) by (error_code, subject)

# Error breakdown
topk(10, rate(router_intake_validation_errors_total[5m]))
```

**What to Look For**:
- `SCHEMA_VALIDATION_FAILED` - Invalid message format
- `VERSION_UNSUPPORTED` - Schema version mismatch
- `CORRELATION_FIELDS_INVALID` - Missing/invalid correlation fields
- `TENANT_FORBIDDEN` - Tenant not allowed
- `IDEMPOTENCY_VIOLATION` - Duplicate message

#### Step 3: Inspect DLQ Messages

**Via NATS CLI**:
```bash
# List DLQ messages (sample)
nats stream view JS beamline.router.v1.decide.dlq --limit 10

# Get message details
nats stream view JS beamline.router.v1.decide.dlq --seq 123 --json | jq '.data'
```

**What to Look For**:
- Message payload structure
- Error metadata (error_code, error_message)
- Correlation fields (tenant_id, trace_id, run_id)

#### Step 4: Check Audit Logs

**Via Logs**:
```bash
# Search for validation failures
grep "intake.validation.failed" /var/log/router/router.log | tail -20

# Search for specific error codes
grep "SCHEMA_VALIDATION_FAILED" /var/log/router/router.log | tail -20
```

**What to Look For**:
- Error codes and messages
- Tenant IDs and trace IDs
- Timestamps and frequency

### Diagnosis: Validation Errors

#### Step 1: Check Error Rate

**Via Prometheus/Grafana**:
```promql
# Validation error rate
rate(router_intake_validation_errors_total[5m]) by (error_code)

# Error rate over time
rate(router_intake_validation_errors_total[1h])
```

**What to Look For**:
- Sudden spike vs gradual increase
- Which error codes are most common
- Correlation with traffic patterns

#### Step 2: Check Message Format

**Via Logs**:
```bash
# Check for protobuf decode errors
grep "protobuf_decode_failed" /var/log/router/router.log | tail -10

# Check for JSON parse errors
grep "json_parse_failed" /var/log/router/router.log | tail -10
```

**What to Look For**:
- Protobuf decode failures (corrupt messages)
- JSON parse errors (malformed JSON)
- Payload size issues

#### Step 3: Check Schema Version

**Via Logs**:
```bash
# Check for version errors
grep "VERSION_UNSUPPORTED" /var/log/router/router.log | tail -10

# Check version distribution
grep "schema_version" /var/log/router/router.log | awk '{print $NF}' | sort | uniq -c
```

**What to Look For**:
- Unsupported schema versions
- Version distribution (which versions are used)

#### Step 4: Check Correlation Fields

**Via Logs**:
```bash
# Check for correlation field errors
grep "CORRELATION_FIELDS_INVALID" /var/log/router/router.log | tail -10

# Check for missing fields
grep "missing.*run_id\|flow_id\|step_id" /var/log/router/router.log | tail -10
```

**What to Look For**:
- Missing required correlation fields
- Invalid field formats (UUID, W3C Trace Context)

### Diagnosis: NATS Failures

**Test Coverage**: This scenario is verified by fault injection tests:
- Test: `test_nats_connection_loss_recovery` (`router_jetstream_fault_injection_SUITE`)
- Smoke test: Yes (see `apps/otp/router/test/FAULT_INJECTION_SMOKE_TESTS.md`)
- Guarantees:
  - Router reconnects automatically after NATS connection loss
  - Consumers continue functioning after reconnection
  - New messages processed without anomalies
  - ETS state preserved during reconnection

**If router doesn't recover as expected**, check:
- Test scenario: `test_ets_state_preservation_during_nats_restart` for ETS state verification
- Test scenario: `test_jetstream_consumer_reconnection` for consumer reconnection behavior

#### Step 1: Check NATS Connection Status

**Via Erlang Shell**:
```erlang
%% Check NATS connection
router_nats:get_connection_status().

%% Check connection PID
whereis(router_nats_connection).
```

**Expected Output**:
```erlang
%% Connection status
{ok, connected}

%% Connection PID
<0.456.0>
```

#### Step 2: Check NATS Metrics

**Via Prometheus/Grafana**:
```promql
# Connection errors
rate(router_nats_connection_errors_total[5m]) by (reason)

# Reconnection events
rate(router_nats_reconnect_total[5m])

# Connection status
router_nats_connection_status
```

**What to Look For**:
- Connection error reasons
- Reconnection frequency
- Connection status (0 = disconnected, 1 = connected)

#### Step 3: Check NATS Server Health

**Via NATS CLI**:
```bash
# Check NATS server info
nats server info

# Check JetStream status
nats stream info JS

# Check consumer status
nats consumer info JS beamline.router.v1.decide router-decide-consumer
```

**What to Look For**:
- NATS server running
- JetStream enabled
- Consumer active and healthy

#### Step 4: Check Network Connectivity

**Via Command Line**:
```bash
# Test NATS connectivity
nc -zv nats-server.example.com 4222

# Test TLS connection
openssl s_client -connect nats-server.example.com:4222 -CAfile /path/to/ca.pem
```

**What to Look For**:
- Network connectivity
- TLS handshake success
- Certificate validation

### Diagnosis: Backpressure

#### Step 1: Check Backpressure Status

**Via Prometheus/Grafana**:
```promql
# Backpressure active
router_intake_backpressure_active{subject="beamline.router.v1.decide"}

# Backpressure triggers
rate(router_intake_backpressure_triggered_total[5m]) by (trigger)

# Backpressure rejections
rate(router_intake_backpressure_rejections_total[5m])
```

**What to Look For**:
- Backpressure active (1) or inactive (0)
- Which triggers activated (queue, latency, inflight)
- Rejection rate

#### Step 2: Check Queue Depth

**Via Prometheus/Grafana**:
```promql
# Pending messages
router_jetstream_pending_messages{subject="beamline.router.v1.decide"}

# ACK pending messages
router_jetstream_ack_pending_messages{subject="beamline.router.v1.decide"}
```

**What to Look For**:
- Pending messages > 1000 (overload threshold)
- ACK pending messages (messages awaiting ACK)

#### Step 3: Check Processing Latency

**Via Prometheus/Grafana**:
```promql
# Processing latency percentiles
router_intake_processing_latency_p95{subject="beamline.router.v1.decide"}
router_intake_processing_latency_p99{subject="beamline.router.v1.decide"}

# Latency histogram
histogram_quantile(0.95, rate(router_intake_processing_latency_seconds_bucket[5m]))
```

**What to Look For**:
- P95 latency > 5000ms (overload threshold)
- Latency trend (increasing or stable)

#### Step 4: Check In-Flight Messages

**Via Prometheus/Grafana**:
```promql
# In-flight messages
router_intake_inflight_messages{subject="beamline.router.v1.decide"}

# Maximum in-flight (peak)
router_intake_inflight_messages_max{subject="beamline.router.v1.decide"}
```

**What to Look For**:
- In-flight messages > 500 (overload threshold)
- Peak in-flight messages

---

## Remediation Steps

### Remediation: DLQ Growth

#### Step 1: Identify Root Cause

**Actions**:
1. Check error code distribution (see [Diagnosis: DLQ Growth](#diagnosis-dlq-growth))
2. Inspect sample DLQ messages
3. Check audit logs for patterns

**Decision Tree**:
- **Schema Validation Failures** → Check client message format
- **Version Mismatch** → Update client or Router schema support
- **Correlation Field Errors** → Fix client to send required fields
- **Tenant Validation Failures** → Update tenant allowlist
- **Idempotency Violations** → Check idempotency TTL settings

#### Step 2: Fix Root Cause

**Schema Validation Failures**:
```erlang
%% Check protobuf schema version
application:get_env(beamline_router, schema_version).

%% Verify message format
%% (Check client code, ensure protobuf encoding is correct)
```

**Version Mismatch**:
```erlang
%% Check supported versions
application:get_env(beamline_router, supported_schema_versions).

%% Update client to use supported version
%% OR add version support to Router (requires code change)
```

**Tenant Validation Failures**:
```erlang
%% Check tenant allowlist
router_policy_store:get_tenant_allowlist().

%% Add tenant to allowlist (if authorized)
router_policy_store:add_tenant_to_allowlist(<<"tenant_id">>).
```

**Idempotency Violations**:
```erlang
%% Check idempotency TTL
application:get_env(beamline_router, idempotency_ttl_seconds).

%% Increase TTL if processing latency is high
application:set_env(beamline_router, idempotency_ttl_seconds, 7200).  %% 2 hours
```

#### Step 3: Monitor Recovery

**Actions**:
1. Monitor DLQ message rate (should decrease)
2. Monitor validation error rate (should decrease)
3. Check audit logs for continued errors

**Metrics to Watch**:
```promql
# DLQ message rate (should decrease)
rate(router_intake_dlq_messages_total[5m])

# Validation error rate (should decrease)
rate(router_intake_validation_errors_total[5m])
```

### Remediation: Validation Errors

#### Step 1: Identify Error Pattern

**Actions**:
1. Check error code distribution
2. Check error frequency (spike vs gradual)
3. Correlate with client changes

**Common Patterns**:
- **Sudden Spike**: Recent client deployment or configuration change
- **Gradual Increase**: Client drift or schema evolution
- **Specific Tenant**: Tenant-specific issue

#### Step 2: Fix Client Issues

**Schema Validation Failures**:
- Verify client protobuf encoding
- Check payload size limits
- Verify required fields are present

**Version Mismatch**:
- Update client to use supported schema version
- Or coordinate Router schema version update

**Correlation Field Errors**:
- Ensure client sends all required fields (`run_id`, `flow_id`, `step_id`)
- Verify field formats (UUID v4, W3C Trace Context)

#### Step 3: Temporary Workaround (If Needed)

**Disable Validation** (NOT RECOMMENDED for production):
```erlang
%% Disable validation (emergency only)
application:set_env(beamline_router, intake_validation_enabled, false).
```

**Warning**: This disables all validation and should only be used in emergencies.

### Remediation: NATS Failures

**Expected Behavior** (verified by tests):
- Router should reconnect automatically (verified by `test_nats_connection_loss_recovery`)
- Router should not require restart (verified by `test_prolonged_fault_period_recovery_no_router_restart`)
- ETS state should be preserved (verified by `test_ets_state_preservation_during_nats_restart`)

**If router doesn't recover automatically**, this indicates a bug (tests should catch this).

#### Step 1: Check NATS Server

**Actions**:
1. Verify NATS server is running
2. Check NATS server logs
3. Verify NATS server health

**Via Command Line**:
```bash
# Check NATS server status
systemctl status nats-server

# Check NATS server logs
journalctl -u nats-server -n 100

# Check NATS server health
curl http://nats-server.example.com:8222/healthz
```

#### Step 2: Restart NATS Connection

**Via Erlang Shell**:
```erlang
%% Restart NATS connection
router_nats:reconnect().

%% Check connection status
router_nats:get_connection_status().
```

**Expected Output**:
```erlang
%% Reconnection successful
{ok, reconnected}

%% Connection status
{ok, connected}
```

#### Step 3: Fix TLS/Auth Issues

**TLS Certificate Issues**:
```erlang
%% Check TLS configuration
application:get_env(beamline_router, nats_tls_enabled).
application:get_env(beamline_router, nats_tls_cert_file).
application:get_env(beamline_router, nats_tls_key_file).

%% Verify certificate files exist and are readable
file:read_file("/path/to/cert.pem").
```

**Authentication Issues**:
```erlang
%% Check NATS credentials
application:get_env(beamline_router, nats_username).
application:get_env(beamline_router, nats_password).

%% Verify credentials (check NATS server logs)
```

#### Step 4: Network Troubleshooting

**Actions**:
1. Check network connectivity
2. Check firewall rules
3. Verify NATS server accessibility

**Via Command Line**:
```bash
# Test connectivity
ping nats-server.example.com

# Test port accessibility
telnet nats-server.example.com 4222

# Check firewall rules
iptables -L -n | grep 4222
```

### Remediation: Backpressure

#### Step 1: Identify Overload Cause

**Actions**:
1. Check which triggers activated (queue, latency, inflight)
2. Check message processing rate
3. Check resource utilization (CPU, memory)

**Decision Tree**:
- **High Queue Depth** → Processing too slow or volume too high
- **High Latency** → Processing bottlenecks or resource exhaustion
- **High In-Flight** → Too many concurrent messages

#### Step 2: Scale Router Instances

**Actions**:
1. Add more Router instances (horizontal scaling)
2. Verify load balancing (JetStream queue groups)
3. Monitor queue depth reduction

**Via Kubernetes/Orchestration**:
```bash
# Scale Router deployment
kubectl scale deployment router --replicas=3

# Check pod status
kubectl get pods -l app=router

# Monitor queue depth
watch -n 5 'nats consumer info JS beamline.router.v1.decide router-decide-consumer --json | jq .num_pending'
```

#### Step 3: Reduce Incoming Traffic

**Actions**:
1. Enable Gateway rate limiting (if not already enabled)
2. Reduce rate limits temporarily
3. Coordinate with clients to reduce traffic

**Via Gateway Configuration**:
```bash
# Reduce rate limits (temporary)
export GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT=25  # Reduce from 50 to 25

# Restart Gateway
systemctl restart c-gateway
```

#### Step 4: Optimize Processing

**Actions**:
1. Check processing bottlenecks
2. Optimize slow operations
3. Increase resource limits (CPU, memory)

**Via Erlang Shell**:
```erlang
%% Check process memory
erlang:memory(processes_used).

%% Check process count
erlang:system_info(process_count).

%% Profile slow operations
fprof:apply(router_decide_consumer, handle_decide_message, [Subject, Payload, Headers, MsgId]).
```

---

## Emergency Procedures

### Emergency: Router Not Processing Messages

**Symptoms**:
- No messages being processed
- Queue depth growing
- Router process alive but not consuming

**Immediate Actions**:
1. **Check Router Process**:
   ```erlang
   %% Verify Router is running
   whereis(router_decide_consumer).
   
   %% Check for crashes
   supervisor:which_children(beamline_router_sup).
   ```

2. **Restart Router Consumer**:
   ```erlang
   %% Restart consumer
   supervisor:restart_child(beamline_router_sup, router_decide_consumer).
   
   %% Verify restart
   whereis(router_decide_consumer).
   ```

3. **Check NATS Connection**:
   ```erlang
   %% Check connection
   router_nats:get_connection_status().
   
   %% Reconnect if needed
   router_nats:reconnect().
   ```

### Emergency: DLQ Flooding

**Symptoms**:
- DLQ messages accumulating rapidly
- Validation errors spiking
- System overwhelmed

**Immediate Actions**:
1. **Temporarily Disable DLQ** (if safe):
   ```erlang
   %% Disable DLQ (emergency only)
   application:set_env(beamline_router, dlq_enabled, false).
   ```

2. **Reduce Incoming Traffic**:
   ```bash
   # Reduce Gateway rate limits
   export GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT=10
   systemctl restart c-gateway
   ```

3. **Investigate Root Cause**:
   - Check error code distribution
   - Inspect sample DLQ messages
   - Check audit logs

### Emergency: NATS Unavailable

**Symptoms**:
- NATS connection errors
- Messages not being received
- Router unable to publish

**Immediate Actions**:
1. **Check NATS Server**:
   ```bash
   # Check NATS server status
   systemctl status nats-server
   
   # Restart NATS server (if needed)
   systemctl restart nats-server
   ```

2. **Router Will Auto-Reconnect**:
   - Router automatically reconnects when NATS available
   - Check reconnection metrics: `router_nats_reconnect_total`

3. **Monitor Recovery**:
   ```promql
   # Connection status
   router_nats_connection_status
   
   # Reconnection events
   rate(router_nats_reconnect_total[5m])
   ```

---

## Monitoring & Alerts

### Abuse Detection Metrics

**Router Abuse Metrics**:
- `router_abuse_empty_payload_total`: Counter for empty payload abuse events
- `router_abuse_heavy_payload_total`: Counter for heavy payload abuse events
- `router_abuse_targeted_tenant_total`: Counter for targeted tenant attacks
- `router_abuse_payload_size_distribution`: Histogram for payload size distribution per tenant

**Interpreting Abuse Metrics**:
- **Normal**: All abuse metrics ≈ 0, no alerts firing
- **Warning**: Occasional abuse events (< threshold), alerts firing but not critical
- **Critical**: High rate of abuse events (> threshold), multiple alerts firing, requires immediate action

**Query Examples**:
```promql
# Empty payload abuse rate
rate(router_abuse_empty_payload_total[5m]) by (tenant_id)

# Heavy payload abuse rate
rate(router_abuse_heavy_payload_total[5m]) by (tenant_id)

# Targeted tenant abuse rate
rate(router_abuse_targeted_tenant_total[5m]) by (tenant_id)

# Payload size distribution (p95)
histogram_quantile(0.95, rate(router_abuse_payload_size_distribution_bucket[5m])) by (tenant_id)
```

### Abuse Detection Alerts

**Router Abuse Alerts**:

1. **RouterAbuseEmptyPayloadDetected** / **RouterAbuseEmptyPayloadHigh**:
   - **Trigger**: Empty payload abuse detected (> 0 events/min) or high rate (> 20 events/min)
   - **Severity**: Warning
   - **Action**: Check logs for tenant_id, review payload validation thresholds
   - **Remediation**: Block offending tenant if confirmed abuse, adjust min_payload_size if false positive

2. **RouterAbuseHeavyPayloadDetected** / **RouterAbuseHeavyPayloadHigh**:
   - **Trigger**: Heavy payload abuse detected (> 0 events/min) or high rate (> 10 events/min)
   - **Severity**: Warning / Critical
   - **Action**: Check payload size distribution, review tenant behavior
   - **Remediation**: Block tenant if confirmed abuse, adjust large_payload_threshold if false positive

3. **RouterAbuseTargetedTenantDetected** / **RouterAbuseTargetedTenantHigh**:
   - **Trigger**: Targeted tenant attack detected (> 0 events/min) or high rate (> 5 events/min)
   - **Severity**: Warning / Critical
   - **Action**: **CRITICAL** - Review tenant logs immediately, check for DDoS
   - **Remediation**: Temporarily block tenant, contact tenant owner, review rate limits

**Steps When Abuse Alert Fires**:
1. **Check Metrics**: Review abuse metrics in Prometheus/Grafana
2. **Check Logs**: Review structured logs for abuse events (tenant_id, request_id, trace_id)
3. **Identify Pattern**: Determine abuse type (empty payload, heavy payload, targeted tenant)
4. **Verify Legitimacy**: Check if false positive (legitimate use case vs actual abuse)
5. **Take Action**: Block tenant, adjust thresholds, or contact tenant owner
6. **Monitor**: Continue monitoring metrics and logs after remediation

## Monitoring & Alerts (Legacy)

### Key Metrics to Monitor

**Intake Processing**:
- `router_intake_messages_total{status="ok"}` - Successful messages
- `router_intake_messages_total{status="failed"}` - Failed messages
- `router_intake_validation_errors_total` - Validation errors

**DLQ**:
- `router_intake_dlq_messages_total` - DLQ message count
- `router_intake_dlq_publish_failed_total` - DLQ publication failures

**Backpressure**:
- `router_intake_backpressure_active` - Backpressure status
- `router_jetstream_pending_messages` - Queue depth
- `router_intake_processing_latency_p95` - Processing latency

**NATS Connection**:
- `router_nats_connection_status` - Connection status
- `router_nats_connection_errors_total` - Connection errors
- `router_nats_reconnect_total` - Reconnection events

### Alert Rules

**See**: `apps/otp/router/docs/PROMETHEUS_ALERTS.md` for complete alert definitions.

**Key Alerts**:
- `RouterIntakeBackpressureActive` - Backpressure active
- `RouterIntakeQueueDepthHigh` - Queue depth high
- `RouterIntakeLatencyHigh` - Processing latency high
- `RouterIntakeDLQHigh` - DLQ messages high (if configured)

---

## Troubleshooting

### Issue: High DLQ Growth

**Symptoms**: DLQ messages accumulating rapidly

**Diagnosis**:
1. Check error code distribution
2. Inspect sample DLQ messages
3. Check audit logs

**Resolution**:
- Fix root cause (schema, version, tenant, etc.)
- Update client code if needed
- Adjust validation rules if appropriate

**Reference**: [Remediation: DLQ Growth](#remediation-dlq-growth)

### Issue: Validation Errors Spike

**Symptoms**: Validation errors increasing rapidly

**Diagnosis**:
1. Check error code distribution
2. Check message format
3. Check schema version

**Resolution**:
- Fix client message format
- Update schema version support
- Fix correlation field issues

**Reference**: [Remediation: Validation Errors](#remediation-validation-errors)

### Issue: NATS Connection Failures

**Symptoms**: NATS connection errors, reconnections

**Diagnosis**:
1. Check NATS server health
2. Check network connectivity
3. Check TLS/authentication

**Resolution**:
- Restart NATS server if needed
- Fix network issues
- Fix TLS/authentication configuration

**Reference**: [Remediation: NATS Failures](#remediation-nats-failures)

### Issue: Backpressure Active

**Symptoms**: Backpressure active, requests rejected

**Diagnosis**:
1. Check queue depth
2. Check processing latency
3. Check in-flight messages

**Resolution**:
- Scale Router instances
- Reduce incoming traffic
- Optimize processing

**Reference**: [Remediation: Backpressure](#remediation-backpressure)

---

## References

- **Operational Guide**: `apps/otp/router/docs/OPERATIONAL_GUIDE.md`
- **Prometheus Alerts**: `apps/otp/router/docs/PROMETHEUS_ALERTS.md`
- **Metrics Guide**: `docs/OBSERVABILITY_METRICS_MONITORING_GUIDE.md`
- **Backpressure Policy**: `docs/ARCHITECTURE/router-intake-backpressure-policy.md`
- **Intake Implementation**: `apps/otp/router/src/router_intake_validator.erl`
- **Error Handling**: `apps/otp/router/src/router_intake_error_handler.erl`
- **Fault Injection Tests**: `apps/otp/router/test/FAULT_INJECTION_TEST_SCENARIOS.md`
- **Smoke Tests**: `apps/otp/router/test/FAULT_INJECTION_SMOKE_TESTS.md`
- **Test Traceability**: `apps/otp/router/test/FAULT_INJECTION_REQUIREMENTS_TRACEABILITY.md`
- **Runbook Integration**: `apps/otp/router/test/FAULT_INJECTION_RUNBOOK_INTEGRATION.md`

