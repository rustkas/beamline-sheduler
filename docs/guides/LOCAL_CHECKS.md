# Local Checks Guide

## Proto/buf checks

- Install automatically: `bash scripts/check_proto.sh` will install `buf` to `tools/bin` if missing.
- Lint/build: `bash scripts/check_proto.sh` runs `buf lint` and `buf build` in `proto/`.
- Sync check: `bash scripts/check_proto_sync.sh` verifies `proto/beamline/flow/v1/flow.proto` matches `apps/otp/router/proto/...`.
- NATS compatibility: `bash scripts/check_proto_nats_compatibility.sh` validates JSON payload compatibility with protobuf types.

## CP2 Feature Validation

### Overview

The CP2 validation suite ensures all CP2+ features (idempotency, tracing, tenant validation, admin gRPC) are properly configured and functional.

### Usage

```bash
# Run full CP2 validation suite
bash scripts/validate_cp2.sh

# Expected output:
# ✅ [PASS] All CP2 validations passed (static + runtime)
# Exit code: 0
```

### What It Validates

**Static Checks**:
- Feature flags enabled in `beamline_router.app.src`
- Current checkpoint >= CP2-LC in `.trae/state.json`
- Required modules present (`router_idempotency.erl`, etc.)
- JetStream configuration complete
- Supervisor CP2+ gating logic

**Runtime Checks**:
- JetStream behavior checks via Router test suites (AS-IS: many run with `nats_mode=mock` and mock `router_nats`)
- Idempotency functionality via `router_idempotency_SUITE`
- OpenTelemetry tracing integration (code inspection)
- Tenant validation via `router_tenant_allowlist_SUITE`
- Admin gRPC service (optional)

AS-IS note:
- `apps/otp/router/test/router_jetstream_e2e_SUITE.erl` exists but is currently a placeholder (empty suite).
- JetStream-related scenarios currently present are in suites like `router_jetstream_e2e_integration_SUITE.erl` and `router_jetstream_unit_SUITE.erl`.

### Prerequisites

```bash
# NATS server must be running for JetStream tests
docker-compose up -d nats

# Erlang/OTP and rebar3 installed
rebar3 --version
```

### Troubleshooting

**JetStream tests skipped**:
- NATS server not running → `docker-compose up -d nats`
- NATS CLI not installed → `brew install nats-io/nats-tools/nats` (macOS) or see [NATS CLI docs](https://docs.nats.io/running-a-nats-service/nats_admin/natscli)

**Test suite failures**:
- Check test logs in `/tmp/jetstream_test.log`, `/tmp/idempotency_test.log`, etc.
- Run test suite directly: `cd apps/otp/router && rebar3 ct --suite test/router_idempotency_SUITE`

## Observability Smoke (CI)

Minimal CI checks:
- `/metrics` scrape returns Prometheus format:
  ```bash
  curl -fsS http://localhost:8081/metrics | head -n 5
  ```
- OTLP export endpoint reachable (optional):
  ```bash
  curl -fsS ${OTLP_ENDPOINT:-http://localhost:4318}/v1/traces -I || true
  ```

## Known issues

- None. All proto checks pass with current configuration.

## CI usage

- Use `scripts/check_proto.sh` in CI to ensure lint/build pass.
- Use `scripts/check_proto_sync.sh` to gate synchronization.

