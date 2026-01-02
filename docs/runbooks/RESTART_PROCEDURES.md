# Runbook: Restart Procedures (Rolling / Emergency / Config Reload)

Severity: Medium–High
Trigger: Planned deploy, memory leak, stuck processes, or config updates

## 1. Rolling Restart (Preferred)
- Goal: Zero/minimal impact; drain requests.
- Steps:
  1. Mark instance for drain (Gateway/Router):
     - Gateway: stop accepting new connections; finish in-flight responses.
     - Router: ensure supervisors stop workers gracefully.
  2. Send `SIGTERM` to process; wait grace period (e.g., 30s).
  3. Verify health endpoints; proceed to next instance.
- Validation:
  - No spike in `5xx` or `router_http_request_duration_seconds`.
  - No increase in `router_assignment_publish_failures_total`.

## 2. Emergency Restart
- Use when system is wedged or misbehaving.
- Steps:
  1. Announce impact; switch to degraded mode if available.
  2. `SIGTERM` with shortened grace; if no exit, `SIGKILL`.
  3. Verify after restart; check queues, consumer lag, error metrics.

## 3. Config Reload Semantics
- Prefer explicit restart after config changes to avoid drift.
- Steps:
  1. Apply config updates via `router_config:set_override/2`.
  2. If hot-reload not supported, perform rolling restart.
  3. Audit applied overrides; persist desired state to config store.

## 4. Observability Checks
- Logs: Structured JSON present; trace IDs propagated.
- Metrics: SLOs stable; error counters flat.
- Tracing: Spans correctly closed with statuses.

## 5. Exit Criteria
- Two consecutive health checks pass.
- Metrics baseline for 10–15 minutes.

