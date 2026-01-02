# Runbooks Implementation Report

**Date**: 2025-11-26  
**Status**: ✅ **Implementation Complete**  
**Purpose**: Implementation of operational runbooks for Router intake and Gateway rate limiting  
**Related**: `docs/OPS_RUNBOOK_ROUTER_INTAKE.md`, `docs/OPS_RUNBOOK_GATEWAY_RATE_LIMITING.md`

## Executive Summary

Operational runbooks for Router intake and Gateway rate limiting have been created, providing SRE/Ops teams with comprehensive troubleshooting and remediation procedures. Runbooks are integrated into CP/readiness documents and observability guides.

## Implementation

### C.1. Runbook по Intake Failures

**Document Created**: `docs/OPS_RUNBOOK_ROUTER_INTAKE.md`

**Key Sections**:
1. **Health Checks**:
   - Router process status
   - Intake metrics (Prometheus/Grafana queries)
   - JetStream consumer status (NATS CLI commands)

2. **Common Symptoms**:
   - DLQ Growth (indicators, possible causes)
   - Validation Errors Spike (indicators, possible causes)
   - NATS Connection Failures (indicators, possible causes)
   - Backpressure Active (indicators, possible causes)

3. **Diagnosis Procedures**:
   - **DLQ Growth**: Check DLQ metrics, error distribution, inspect DLQ messages, check audit logs
   - **Validation Errors**: Check error rate, message format, schema version, correlation fields
   - **NATS Failures**: Check connection status, NATS metrics, NATS server health, network connectivity
   - **Backpressure**: Check backpressure status, queue depth, processing latency, in-flight messages

4. **Remediation Steps**:
   - **DLQ Growth**: Identify root cause, fix root cause (schema/version/tenant/idempotency), monitor recovery
   - **Validation Errors**: Identify error pattern, fix client issues, temporary workaround (if needed)
   - **NATS Failures**: Check NATS server, restart NATS connection, fix TLS/auth issues, network troubleshooting
   - **Backpressure**: Identify overload cause, scale Router instances, reduce incoming traffic, optimize processing

5. **Emergency Procedures**:
   - Router not processing messages
   - DLQ flooding
   - NATS unavailable

6. **Monitoring & Alerts**:
   - Key metrics to monitor
   - Alert rules (references to PROMETHEUS_ALERTS.md)

7. **Troubleshooting**:
   - Common issues with quick references to remediation sections

### C.2. Runbook по Rate Limiting

**Document Created**: `docs/OPS_RUNBOOK_GATEWAY_RATE_LIMITING.md`

**Key Sections**:
1. **Health Checks**:
   - Rate limiting status (metrics, HTTP status codes, configuration)

2. **Configuration**:
   - Changing rate limits (env vars, config file, Kubernetes ConfigMap)
   - Verifying rate limit changes

3. **Common Symptoms**:
   - Mass 429 Responses (indicators, possible causes)
   - Inconsistent Rate Limiting (indicators, possible causes)
   - Rate Limiting Not Working (indicators, possible causes)

4. **Diagnosis Procedures**:
   - **Mass 429**: Check rate limit metrics, traffic patterns, current limits, client behavior
   - **Inconsistent Limits**: Check per-instance metrics, distributed rate limiting status, configuration consistency
   - **Rate Limiting Not Working**: Check rate limiting enabled, limits vs traffic, implementation

5. **Remediation Steps**:
   - **Mass 429**: Determine root cause, increase limits (temporary/permanent), fix client retry storms
   - **Inconsistent Limits**: Enable distributed rate limiting, fix Redis issues, standardize configuration
   - **Rate Limiting Not Working**: Verify rate limiting enabled, test rate limiting, check implementation

6. **Distinguishing Abuse vs Strict Limits**:
   - Indicators of real abuse (patterns, metrics)
   - Indicators of too strict limits (patterns, metrics)
   - Decision matrix (abuse vs too strict)
   - Actions based on diagnosis

7. **Emergency Procedures**:
   - Mass 429 responses
   - Distributed rate limiting failure

8. **Monitoring & Alerts**:
   - Key metrics to monitor
   - Alert rules (suggested)

9. **Troubleshooting**:
   - Common issues with quick references to remediation sections

### C.3. Интеграция с CP/READINESS

**Documents Updated**:

1. **`docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY_OVERVIEW.md`**:
   - Added runbooks to "Operational Guides" section:
     - `docs/OPS_RUNBOOK_ROUTER_INTAKE.md`
     - `docs/OPS_RUNBOOK_GATEWAY_RATE_LIMITING.md`

2. **`docs/OBSERVABILITY.md`**:
   - Added runbooks to "References" section:
     - `docs/OPS_RUNBOOK_ROUTER_INTAKE.md`
     - `docs/OPS_RUNBOOK_GATEWAY_RATE_LIMITING.md`

3. **`docs/OBSERVABILITY_METRICS_MONITORING_GUIDE.md`**:
   - Added runbooks to "References" section:
     - `docs/OPS_RUNBOOK_ROUTER_INTAKE.md`
     - `docs/OPS_RUNBOOK_GATEWAY_RATE_LIMITING.md`

4. **`apps/otp/router/docs/OPERATIONAL_GUIDE.md`**:
   - Added "Intake Operations" section with reference to `docs/OPS_RUNBOOK_ROUTER_INTAKE.md`
   - Quick reference commands for intake troubleshooting

5. **`docs/GATEWAY_RATE_LIMITING.md`**:
   - Updated "Operational Playbook" section with reference to `docs/OPS_RUNBOOK_GATEWAY_RATE_LIMITING.md`
   - Quick reference preserved

## Files Created/Modified

### New Files

1. **`docs/OPS_RUNBOOK_ROUTER_INTAKE.md`** - Router intake operations runbook
2. **`docs/OPS_RUNBOOK_GATEWAY_RATE_LIMITING.md`** - Gateway rate limiting operations runbook
3. **`docs/archive/dev/RUNBOOKS_IMPLEMENTATION_REPORT.md`** - This report

### Modified Files

1. **`docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY_OVERVIEW.md`** - Added runbooks to operational guides
2. **`docs/OBSERVABILITY.md`** - Added runbooks to references
3. **`docs/OBSERVABILITY_METRICS_MONITORING_GUIDE.md`** - Added runbooks to references
4. **`apps/otp/router/docs/OPERATIONAL_GUIDE.md`** - Added intake operations section
5. **`docs/GATEWAY_RATE_LIMITING.md`** - Updated operational playbook section

## Key Features

### Router Intake Runbook

**Comprehensive Coverage**:
- ✅ Health checks (process, metrics, JetStream)
- ✅ 4 common symptoms (DLQ, validation errors, NATS failures, backpressure)
- ✅ 4 diagnosis procedures (step-by-step troubleshooting)
- ✅ 4 remediation procedures (fixing root causes)
- ✅ 3 emergency procedures (Router not processing, DLQ flooding, NATS unavailable)
- ✅ Monitoring and alerts (key metrics, alert rules)
- ✅ Troubleshooting (common issues with references)

**Practical Commands**:
- Erlang shell commands for process checks
- Prometheus/Grafana queries for metrics
- NATS CLI commands for JetStream inspection
- Log search commands for troubleshooting

### Gateway Rate Limiting Runbook

**Comprehensive Coverage**:
- ✅ Health checks (rate limiting status, metrics, configuration)
- ✅ Configuration (3 methods: env vars, config file, K8s ConfigMap)
- ✅ 3 common symptoms (mass 429, inconsistent limits, not working)
- ✅ 3 diagnosis procedures (step-by-step troubleshooting)
- ✅ 3 remediation procedures (fixing root causes)
- ✅ Distinguishing abuse vs strict limits (decision matrix)
- ✅ 2 emergency procedures (mass 429, distributed rate limiting failure)
- ✅ Monitoring and alerts (key metrics, alert rules)
- ✅ Troubleshooting (common issues with references)

**Practical Commands**:
- Environment variable configuration
- Kubernetes ConfigMap updates
- Prometheus/Grafana queries for metrics
- HTTP header verification
- Redis connectivity checks

## Integration Points

### CP2 Readiness Overview

**Location**: `docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY_OVERVIEW.md`

**Added Section**:
```markdown
### Operational Guides

- `apps/otp/router/docs/OPERATIONAL_GUIDE.md` - Router operational guide (CP2-LC features)
- `docs/OPS_RUNBOOK_ROUTER_INTAKE.md` - Router intake operations runbook (DLQ, validation errors, NATS failures, backpressure)
- `docs/OPS_RUNBOOK_GATEWAY_RATE_LIMITING.md` - Gateway rate limiting operations runbook (configuration, mass 429, abuse detection)
- `apps/otp/router/docs/EXTENSIONS_RUNBOOK.md` - Extension Registry and Extensions operations runbook
```

### Observability Documents

**Locations**:
- `docs/OBSERVABILITY.md`
- `docs/OBSERVABILITY_METRICS_MONITORING_GUIDE.md`

**Added References**:
- Router intake runbook
- Gateway rate limiting runbook

### Operational Guides

**Locations**:
- `apps/otp/router/docs/OPERATIONAL_GUIDE.md`
- `docs/GATEWAY_RATE_LIMITING.md`

**Added Sections**:
- Intake operations section (Router)
- Operational playbook update (Gateway)

## Usage Examples

### Example 1: Troubleshooting DLQ Growth

**Scenario**: DLQ messages accumulating rapidly

**Steps from Runbook**:
1. Check DLQ metrics (Prometheus query)
2. Check error distribution (Prometheus query)
3. Inspect DLQ messages (NATS CLI)
4. Check audit logs (grep commands)
5. Identify root cause (decision tree)
6. Fix root cause (schema/version/tenant/idempotency)
7. Monitor recovery (metrics)

### Example 2: Changing Rate Limits

**Scenario**: Need to increase rate limits due to traffic growth

**Steps from Runbook**:
1. Check current limits (env vars)
2. Calculate appropriate limits (P95 * 1.5)
3. Update configuration (env/config/K8s)
4. Restart Gateway
5. Verify changes (HTTP headers, metrics)

### Example 3: Distinguishing Abuse vs Strict Limits

**Scenario**: Many 429 responses, need to determine if abuse or strict limits

**Steps from Runbook**:
1. Check request rate by tenant (Prometheus query)
2. Check hit ratio (Prometheus query)
3. Check traffic patterns (logs, metrics)
4. Apply decision matrix (abuse vs too strict)
5. Take appropriate action (block vs increase limits)

## Testing

### Runbook Validation

**Manual Review**:
- ✅ All sections present and complete
- ✅ Commands tested (syntax verified)
- ✅ Prometheus queries valid
- ✅ References to other documents correct
- ✅ Cross-references within runbooks working

### Integration Validation

**Link Checking**:
- ✅ All references to runbooks added
- ✅ Links in CP/readiness documents working
- ✅ Links in observability documents working
- ✅ Links in operational guides working

## Next Steps

### Immediate

1. **Review by SRE/Ops Teams**: Get feedback from operational teams
2. **Test Procedures**: Validate commands and procedures in staging
3. **Update Based on Feedback**: Refine based on real-world usage

### Short-Term

1. **Add More Examples**: Real-world scenarios from production
2. **Add Screenshots**: Grafana dashboard screenshots for visual reference
3. **Add Video Tutorials**: Short videos for common procedures

### Long-Term

1. **Automated Runbooks**: Integrate with runbook automation tools
2. **Interactive Troubleshooting**: Web-based troubleshooting wizard
3. **ML-Based Diagnosis**: Use metrics to suggest root causes

## References

- **Router Intake Runbook**: `docs/OPS_RUNBOOK_ROUTER_INTAKE.md`
- **Gateway Rate Limiting Runbook**: `docs/OPS_RUNBOOK_GATEWAY_RATE_LIMITING.md`
- **CP2 Readiness Overview**: `docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY_OVERVIEW.md`
- **Observability Guide**: `docs/OBSERVABILITY_METRICS_MONITORING_GUIDE.md`
- **Router Operational Guide**: `apps/otp/router/docs/OPERATIONAL_GUIDE.md`
- **Gateway Rate Limiting Spec**: `docs/GATEWAY_RATE_LIMITING.md`

