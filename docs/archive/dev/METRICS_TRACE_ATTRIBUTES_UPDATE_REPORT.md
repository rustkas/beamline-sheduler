# Metrics & Trace Attributes Update Report

Date: 2025-11-10

Scope: Router Result/ACK/CAF Adapter telemetry updates with tenant validation metrics and OpenTelemetry attributes, aligned with CP2.

## Summary
- Added tenant validation rejection metrics with `tenant_id` label:
  - `router_results_tenant_rejected_total{assignment_id,request_id,reason,tenant_id}`
  - `router_acks_tenant_rejected_total{assignment_id,reason,tenant_id}`
- Added alert: `RouterTenantRejectedHigh` — high rate of tenant rejections (> 5/min over 10m, warning).
- Extended OpenTelemetry trace attributes:
  - Assignment Publish (`router_caf_adapter.erl`): `assignment.publish_result`, `assignment.retries`, `assignment.error_kind`, `assignment.error`.
  - Result Process (`router_result_consumer.erl`): `result.status`, `result.job_type`, `result.provider_id`, `result.latency_ms`, `result.cost`, `result.tenant_id`.
  - ACK Process (`router_ack_consumer.erl`): `ack.status`, `ack.message`, `ack.tenant_id`.
  - Usage Emit (`router_result_consumer.erl`): initial attributes include `usage_subject`, `latency_ms`, `cost`; outcome attributes `usage.emit_result` and `usage.emit_error`, span status set accordingly.

## Code Changes
- `apps/otp/router/src/router_result_consumer.erl`
  - Enriched `with_span` for `beamline.router.emit.usage` with initial attributes `usage_subject`, `latency_ms`, `cost`.
  - After `emit_usage_event/2`, set `usage.emit_result` and `usage.emit_error` and adjust span status (`ok`/`error`).

## Metrics Emission Locations
- `router_result_consumer.erl`:
  - `router_results_tenant_rejected_total` on tenant validation failure.
  - `router_jetstream_redelivery_total` on NAK for controlled redelivery.
- `router_ack_consumer.erl`:
  - `router_acks_tenant_rejected_total` on tenant validation failure.
  - `router_jetstream_redelivery_total` on NAK for controlled redelivery.
- `router_caf_adapter.erl`:
  - `router_assignment_published_total`, `router_assignment_retry_total`, `router_retry_exhausted_total` with contextual labels.

## Alerting
- `PROMETHEUS_ALERTS.md` updated with `RouterTenantRejectedHigh` alert rule.

## Validation
- Compilation succeeds across modules.
- Manual inspection confirms attribute presence and correct span status setting.
- Next step: include these spans in tracing verification e2e and confirm attributes in collector/exporter.

## Notes
- `router_jetstream_maxdeliver_exhausted_total` remains dependent on JetStream server MaxDeliver exhaustion events; documented in alerts but emission requires server-side state.

