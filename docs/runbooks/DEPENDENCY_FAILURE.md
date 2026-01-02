# Runbook: Dependency Failure (LLM / NATS / Partial Outage)

Severity: High
Trigger: `router_dependency_errors_total` spike, publish/ack failures, or provider SLA breach

## 1. Classification
- Identify failing dependency: Provider (LLM), NATS/JetStream, Internal DB/cache.
- Scope: Global vs Tenant-specific vs Route-specific.

## 2. Provider Failure (LLM down/slow)
- Symptoms:
  - `router_provider_latency_seconds` elevated.
  - `router_assignment_publish_failures_total` increases.
  - Error logs with `caf_publish_failed` from `router_caf_adapter.erl`.
- Actions:
  1. Enable degraded mode: reduce priority, route to fallback provider.
  2. Trip circuit breaker for affected provider:
     ```erlang
     router_config:set_override(provider_enable_openai, false).
     router_config:set_override(provider_enable_providerX, false).
     ```
  3. Verify publish path health via JetStream:
     - Check consumer lag; scale CAF workers if needed.
- Verification:
  - Rate of errors falls; P99 duration returns to baseline.

## 3. NATS / JetStream Failure
- Symptoms:
  - `router_nats:publish_with_ack/3` returns `{error, Reason}`.
  - `router_nats.erl` logs: connection lost, reconnection exhausted.
  - Metrics: `router_retry_exhausted_total` increases.
- Actions:
  1. Attempt reconnect: `router_nats:reconnect/0`.
  2. If reconnection fails, switch to fail-open mode where appropriate.
  3. Communicate degraded mode to stakeholders.
- Verification:
  - `get_connection_status/0` returns healthy; pending operations drained.

## 4. Partial Outage (Tenant or Route)
- Symptoms:
  - Errors isolated to specific `tenant_id` or `route_id`.
  - Blocked assignments due to allowlist: `router_assignment_blocked_total`.
- Actions:
  1. Temporarily allow rollout for affected tenant if safe:
     ```erlang
     router_config:set_override(caf_tenant_allowlist, [<<"tenant_a">>, <<"tenant_b">>]).
     ```
  2. Apply rate limits or backpressure for hot tenants/routes.

## 5. Communication
- Announce degraded mode with expected time to recovery.
- Update run status board.

## 6. Exit Criteria
- Error rate normalizes for 15 minutes.
- JetStream healthy; provider latency back to SLO.

