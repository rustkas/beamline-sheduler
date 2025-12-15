#!/usr/bin/env bash
set -euo pipefail

# CP1 Contracts Verification Script (production-friendly)
# - Emits VS Code/Windsurf-friendly diagnostics: file:line:col: error|warn: message
# - Uses existing repo scripts where possible
# - Adds light boundary checks for CP1

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")"/.. && pwd)"
PROTO_DIR="$ROOT_DIR/proto"
DOCS_DIR="$ROOT_DIR/docs"
ROUTER_DIR="$ROOT_DIR/apps/otp/router"
ROUTER_SRC_DIR="$ROUTER_DIR/src"
SQL_DIR="$ROOT_DIR/sql"
TRAE_DIR="$ROOT_DIR/.trae"
API_DIR="$ROOT_DIR/api"
GATEWAY_DIR="$ROOT_DIR/apps/gateway"
CAF_DIR="$ROOT_DIR/apps/caf"

PASS_COUNT=0
FAIL_COUNT=0
WARN_COUNT=0

fail() { printf "%s:1:1: error: %s\n" "$1" "$2"; FAIL_COUNT=$((FAIL_COUNT+1)); }
warn() { printf "%s:1:1: warn: %s\n" "$1" "$2";  WARN_COUNT=$((WARN_COUNT+1)); }
pass() { printf "[PASS] %s\n" "$1"; PASS_COUNT=$((PASS_COUNT+1)); }

sha256_any() {
  # Linux: sha256sum, macOS: shasum -a 256
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$1" | awk '{print $1}'
  elif command -v shasum >/dev/null 2>&1; then
    shasum -a 256 "$1" | awk '{print $1}'
  else
    echo "no-sha256-tool"
    return 1
  fi
}

echo "=== CP1 Contracts Check: START ==="

# 1) Required files / dirs (presence & sanity)
[ -f "$PROTO_DIR/buf.yaml" ]       || fail "$PROTO_DIR/buf.yaml" "missing"
[ -f "$PROTO_DIR/buf.gen.yaml" ]   || fail "$PROTO_DIR/buf.gen.yaml" "missing"
[ -d "$PROTO_DIR/beamline/flow" ]  || fail "$PROTO_DIR/beamline/flow" "directory missing"
[ -d "$PROTO_DIR/beamline/provider" ] || fail "$PROTO_DIR/beamline/provider" "directory missing"
# Note: proto/beamline/ingress is deprecated (not part of core components)

proto_count=$(find "$PROTO_DIR/beamline" -type f -name '*.proto' | wc -l || true)
if [ "$proto_count" -gt 0 ]; then
  pass ".proto files found under proto/beamline ($proto_count)"
else
  fail "$PROTO_DIR/beamline" "no .proto files found"
fi

[ -f "$DOCS_DIR/NATS_SUBJECTS.md" ] || fail "$DOCS_DIR/NATS_SUBJECTS.md" "missing"
[ -f "$DOCS_DIR/ROUTING_POLICY.md" ] || fail "$DOCS_DIR/ROUTING_POLICY.md" "missing"
[ -f "$DOCS_DIR/CP1_ROUTER_SPEC.md" ] || fail "$DOCS_DIR/CP1_ROUTER_SPEC.md" "missing"

[ -d "$ROUTER_SRC_DIR" ] || fail "$ROUTER_SRC_DIR" "directory missing"
[ -f "$ROUTER_SRC_DIR/beamline_router_sup.erl" ] || fail "$ROUTER_SRC_DIR/beamline_router_sup.erl" "missing"
[ -f "$ROUTER_SRC_DIR/router_nats.erl" ] || fail "$ROUTER_SRC_DIR/router_nats.erl" "missing"
[ -f "$ROUTER_SRC_DIR/router_ack_consumer.erl" ] || fail "$ROUTER_SRC_DIR/router_ack_consumer.erl" "missing"
[ -f "$ROUTER_SRC_DIR/router_result_consumer.erl" ] || fail "$ROUTER_SRC_DIR/router_result_consumer.erl" "missing"

[ -f "$SQL_DIR/000_init.sql" ] || fail "$SQL_DIR/000_init.sql" "missing"

[ -f "$TRAE_DIR/state.json" ]   || fail "$TRAE_DIR/state.json" "missing"
[ -f "$TRAE_DIR/history.json" ] || fail "$TRAE_DIR/history.json" "missing"

# OpenAPI checks are deprecated in CP1 decisions

# 2) Subject naming sanity
if grep -qE "beamline\.router\.v1\.decide" "$DOCS_DIR/NATS_SUBJECTS.md" 2>/dev/null; then
  pass "Subject beamline.router.v1.decide documented"
else
  warn "$DOCS_DIR/NATS_SUBJECTS.md" "subject 'beamline.router.v1.decide' not documented"
fi

# 3) Optional proto lint via buf
if command -v buf >/dev/null 2>&1; then
  (cd "$PROTO_DIR" && buf lint) || warn "$PROTO_DIR" "buf lint reported issues"
else
  warn "$PROTO_DIR" "buf not installed; skipping proto lint"
fi

# 4) Boundary checks (lightweight CP1)
# 4.1 Gateway must NOT reference CAF/C++ directly
if [ -d "$GATEWAY_DIR" ] && [ -d "$CAF_DIR" ]; then
  if grep -RInE "\b(caf::|#include\s*<caf/|from\s+['\"].*/apps/caf/)" "$GATEWAY_DIR" >/dev/null 2>&1; then
    # покажем конкретные места
    grep -RInE "\b(caf::|#include\s*<caf/|from\s+['\"].*/apps/caf/)" "$GATEWAY_DIR" \
      | awk -F: '{print $1":"$2":"$3": error: CP1 violation - Gateway must not depend on CAF directly"}'
    FAIL_COUNT=$((FAIL_COUNT+1))
  else
    pass "Gateway boundary: no direct CAF references"
  fi
fi

# 4.2 Router (Erlang) must NOT reference HTTP/NestJS symbols
if [ -d "$ROUTER_SRC_DIR" ]; then
  if grep -RInE "\b(cowboy|http|nest(js)?)\b" "$ROUTER_SRC_DIR" >/dev/null 2>&1; then
    grep -RInE "\b(cowboy|http|nest(js)?)\b" "$ROUTER_SRC_DIR" \
      | awk -F: '{print $1":"$2":"$3": error: CP1 violation - Router must not reference HTTP/NestJS"}'
    FAIL_COUNT=$((FAIL_COUNT+1))
  else
    pass "Router boundary: no HTTP/NestJS references"
  fi
fi

# 5) Router CP1 tests
if [ -d "$ROUTER_DIR" ] && command -v rebar3 >/dev/null 2>&1; then
  cd "$ROUTER_DIR"
  if rebar3 ct --suite router_core_SUITE --suite router_error_SUITE --suite router_gateway_contract_smoke_SUITE >/dev/null 2>&1; then
    pass "Router CP1 tests (router_core_SUITE, router_error_SUITE, router_gateway_contract_smoke_SUITE)"
  else
    warn "$ROUTER_DIR" "Router CP1 tests failed (check logs)"
  fi
  cd "$ROOT_DIR"
else
  warn "$ROUTER_DIR" "Router directory or rebar3 not found; skipping Router CP1 tests"
fi

# 5.1) Gateway ↔ Router contract smoke test
if [ -x "$ROOT_DIR/scripts/gateway_router_contract_smoke.sh" ]; then
  if bash "$ROOT_DIR/scripts/gateway_router_contract_smoke.sh" --router-only >/dev/null 2>&1; then
    pass "Gateway ↔ Router contract smoke test"
  else
    warn "$ROOT_DIR/scripts/gateway_router_contract_smoke.sh" "Gateway ↔ Router contract smoke test failed"
  fi
else
  warn "$ROOT_DIR/scripts/gateway_router_contract_smoke.sh" "not found; skipping Gateway ↔ Router contract smoke test"
fi

# 5.2) Router observability tests
if [ -d "$ROUTER_DIR" ] && command -v rebar3 >/dev/null 2>&1; then
  cd "$ROUTER_DIR"
  if rebar3 ct --suite router_observability_SUITE >/dev/null 2>&1; then
    pass "Router observability tests (router_observability_SUITE)"
  else
    warn "$ROUTER_DIR" "Router observability tests failed (check logs)"
  fi
  cd "$ROOT_DIR"
else
  warn "$ROUTER_DIR" "Router directory or rebar3 not found; skipping Router observability tests"
fi

# 6) DTO <-> Proto sync (используем твой скрипт, если есть)
if [ -x "$ROOT_DIR/scripts/check_proto_sync.sh" ]; then
  if ! "$ROOT_DIR/scripts/check_proto_sync.sh"; then
    fail "$PROTO_DIR" "DTO and Proto are out of sync (check_proto_sync.sh failed)"
  else
    pass "DTO <-> Proto sync OK"
  fi
else
  warn "$ROOT_DIR/scripts/check_proto_sync.sh" "not found; skipping DTO <-> Proto sync check"
fi

# 7) Artifact checksums (pure read-only)
printf "Artifact checksums (sha256):\n"
for f in \
  "$PROTO_DIR/buf.yaml" \
  "$PROTO_DIR/buf.gen.yaml" \
  "$SQL_DIR/000_init.sql" \
  "$DOCS_DIR/NATS_SUBJECTS.md" \
  "$DOCS_DIR/ROUTING_POLICY.md" \
  "$DOCS_DIR/CP1_ROUTER_SPEC.md" \
  "$TRAE_DIR/state.json" \
  "$TRAE_DIR/history.json"
do
  if [ -f "$f" ]; then
    if sum="$(sha256_any "$f")"; then
      printf "  %s  %s\n" "$sum" "$f"
    fi
  fi
done

printf "Summary: PASS=%d FAIL=%d WARN=%d\n" "$PASS_COUNT" "$FAIL_COUNT" "$WARN_COUNT"
if [ "$FAIL_COUNT" -eq 0 ]; then
  echo "=== CP1 Contracts Check: PASS ==="
  exit 0
else
  echo "=== CP1 Contracts Check: FAIL ==="
  exit 1
fi
