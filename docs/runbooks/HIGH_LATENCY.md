# Runbook: High Latency / Slow Responses

**Severity**: High / Warning
**Trigger**: `router_http_request_duration_seconds_bucket` (P99 > 1s) OR User Complaints

## 1. Triage
Determine if the latency is **Global** (all requests) or **Provider-Specific**.

### Check Metrics
```bash
# Run this PromQL in Grafana/Prometheus
rate(router_http_request_duration_seconds_sum[5m]) / rate(router_http_request_duration_seconds_count[5m])
```

*   **If Global High Latency**: Issue is likely in Router/Gateway or Network.
*   **If Specific Provider (e.g., OpenAI)**: Issue is downstream.

## 2. Diagnosis & Resolution

### Scenario A: Downstream Provider Slow (Most Common)
*   **Symptoms**: `router_provider_latency_seconds` is high for one provider.
*   **Action**:
    1.  Check Provider Status Page (e.g., status.openai.com).
    2.  **Mitigation**: The Router's `router_circuit_breaker` should automatically trip.
    3.  **Manual Override**: If CB fails, update config to disable provider temporarily:
        ```erlang
        %% In router console
        router_config:set_override(provider_enable_openai, false).
        ```

### Scenario B: Router CPU Saturation
*   **Symptoms**: High CPU on Router pods, `erlang_vm_process_limit_usage` high.
*   **Action**:
    1.  Scale up Router replicas.
    2.  Check for "Hot Loop" in logs (grep for `infinite_loop` or `process_limit`).

### Scenario C: NATS Congestion
*   **Symptoms**: `router_nats_publish_latency` high.
*   **Action**:
    1.  Check NATS JetStream dashboard.
    2.  If consumer lag is high, scale up Workers (CAF).

## 3. Verification
*   Monitor P99 latency for 5 minutes.
*   Verify `router_http_request_duration_seconds` returns to baseline (<200ms).

## 4. Escalation
If unresolved after 15 mins, page **SRE On-Call**.
