#!/usr/bin/env bash
set -euo pipefail

# CP2 Behavior Verification Script (production-friendly)
# Emits diagnostics in "file:line:col: error|warn: message" format for IDE problem matchers.

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")"/.. && pwd)"
ROUTER_SRC_DIR="$ROOT_DIR/apps/otp/router/src"
DOCS_DIR="$ROOT_DIR/docs"

PASS_COUNT=0
FAIL_COUNT=0
WARN_COUNT=0

pass() { printf "[PASS] %s\n" "$1"; PASS_COUNT=$((PASS_COUNT+1)); }
fail_file() { # $1=file $2=msg [$3=line] [$4=col]
  local f="$1" m="$2" l="${3:-1}" c="${4:-1}"
  printf "%s:%s:%s: error: %s\n" "$f" "$l" "$c" "$m"
  FAIL_COUNT=$((FAIL_COUNT+1))
}
warn_file() { # $1=file $2=msg [$3=line] [$4=col]
  local f="$1" m="$2" l="${3:-1}" c="${4:-1}"
  printf "%s:%s:%s: warn: %s\n" "$f" "$l" "$c" "$m"
  WARN_COUNT=$((WARN_COUNT+1))
}

file_exists() { [ -f "$1" ]; }
grep_q() { # $1=file $2=pattern
  grep -qE "$2" "$1" 2>/dev/null
}
grep_emit_errors() { # $1=dir_or_file $2=pattern $3=message
  # печатает все совпадения как ошибки с точными координатами
  grep -RInE "$2" "$1" 2>/dev/null | awk -F: -v msg="$3" '{print $1":"$2":1: error: "msg}'
}

printf "=== CP2 Behavior Check: START ===\n"

# 1) Supervisor wiring
SUP="$ROUTER_SRC_DIR/beamline_router_sup.erl"
if file_exists "$SUP"; then
  pass "beamline_router_sup.erl present"
  grep_q "$SUP" '\brouter_nats\b'            && pass "router_nats supervised"            || fail_file "$SUP" "router_nats not found in supervisor children"
  grep_q "$SUP" '\brouter_result_consumer\b'  && pass "router_result_consumer supervised"|| fail_file "$SUP" "router_result_consumer not found in supervisor"
  grep_q "$SUP" '\brouter_ack_consumer\b'     && pass "router_ack_consumer supervised"   || fail_file "$SUP" "router_ack_consumer not found in supervisor"
else
  fail_file "$SUP" "beamline_router_sup.erl missing"
fi

# 2) NATS adapter: JetStream + headers + ack states
NATS="$ROUTER_SRC_DIR/router_nats.erl"
if file_exists "$NATS"; then
  pass "router_nats.erl present"
  grep_q "$NATS" '\bsubscribe_jetstream\s*\(' && pass "subscribe_jetstream/… implemented" || fail_file "$NATS" "subscribe_jetstream/… missing"
  grep_q "$NATS" '\back_message\s*\('         && pass "ack_message/… implemented"         || fail_file "$NATS" "ack_message/… missing"
  grep_q "$NATS" '\bnak_message\s*\('         && pass "nak_message/… implemented"         || warn_file "$NATS" "nak_message/… missing (consider adding NAK)"
  grep_q "$NATS" '\bin_progress_message\s*\(' && pass "in_progress_message/… implemented" || warn_file "$NATS" "in_progress_message/… missing"
  grep_q "$NATS" '\bparse_nats_message\s*\('  && pass "parse_nats_message/… implemented"  || fail_file "$NATS" "parse_nats_message/… missing"
  # parse_headers arity varies: allow /1 or /2
  grep_q "$NATS" '\bparse_headers\s*\(([^),]+,)?[^)]+\)' && pass "parse_headers implemented" || fail_file "$NATS" "parse_headers missing"
  grep_q "$NATS" '\bheaders_size\s*\('        && pass "headers_size/… implemented"         || warn_file "$NATS" "headers_size/… missing"
else
  fail_file "$NATS" "router_nats.erl missing"
fi

# 3) Consumers: subscriptions, header-priority, acking, tracing
for CONS in router_result_consumer.erl router_ack_consumer.erl; do
  FILE="$ROUTER_SRC_DIR/$CONS"
  if file_exists "$FILE"; then
    pass "$CONS present"
    grep_q "$FILE" '\bsubscribe_jetstream\s*\(' && pass "$CONS subscribes via JetStream" || fail_file "$FILE" "$CONS missing subscribe_jetstream call"
    grep_q "$FILE" '\bexplicit\b'               && pass "$CONS uses explicit ack policy" || warn_file "$FILE" "$CONS explicit ack policy not detected"
    grep_q "$FILE" '\{nats_message,'            && pass "$CONS matches {nats_message,Subject,Payload,Headers}" || fail_file "$FILE" "$CONS does not match nats_message tuple"
    grep_q "$FILE" '\bextract_header_or_payload\s*\(' && pass "$CONS prioritizes headers over payload" || warn_file "$FILE" "$CONS header priority function not found"
    grep_q "$FILE" '\brouter_nats:ack_message\s*\('   && pass "$CONS acknowledges messages"            || warn_file "$FILE" "$CONS ack_message usage not found"
    grep_q "$FILE" '\brouter_tracing:with_span\s*\('  && pass "$CONS integrates OpenTelemetry spans"  || warn_file "$FILE" "$CONS tracing span calls not detected"
  else
    fail_file "$FILE" "$CONS missing"
  fi
done

# 4) Tracing and Tenant Validator modules
TRACING="$ROUTER_SRC_DIR/router_tracing.erl"
TENANT="$ROUTER_SRC_DIR/router_tenant_validator.erl"

if file_exists "$TRACING"; then
  pass "router_tracing.erl present"
  grep_q "$TRACING" '\bwith_span\s*\(' && pass "router_tracing:with_span implemented" || warn_file "$TRACING" "with_span not found in router_tracing"
else
  warn_file "$TRACING" "router_tracing.erl missing"
fi

if file_exists "$TENANT"; then
  pass "router_tenant_validator.erl present"
  grep_q "$TENANT" '\bcheck_tenant_against_allowlist_internal\s*\(' && pass "tenant allowlist check implemented" || warn_file "$TENANT" "allowlist check function not found"
  grep_q "$TENANT" '\bemit_counter\s*\('                              && pass "tenant metrics emission present"   || warn_file "$TENANT" "emit_counter for metrics not found"
else
  warn_file "$TENANT" "router_tenant_validator.erl missing"
fi

# 5) Documentation: JetStream behavior markers (check files if exist)
NS="$DOCS_DIR/NATS_SUBJECTS.md"
if [ -f "$NS" ]; then
  grep -qi "JetStream" "$NS"           && pass "JetStream mentioned in NATS_SUBJECTS.md" || warn_file "$NS" "JetStream not mentioned"
else
  warn_file "$NS" "NATS_SUBJECTS.md missing"
fi

if [ -d "$DOCS_DIR" ]; then
  grep -Rqi "AckPolicy=Explicit" "$DOCS_DIR" && pass "AckPolicy=Explicit documented" || warn_file "$DOCS_DIR" "AckPolicy=Explicit not documented"
  grep -Rqi "DeliverGroup" "$DOCS_DIR"       && pass "DeliverGroup documented"       || warn_file "$DOCS_DIR" "DeliverGroup not documented"
  grep -Rqi "Backoff" "$DOCS_DIR"            && pass "Backoff behavior documented"   || warn_file "$DOCS_DIR" "Backoff not documented"
  grep -Rqi "MaxDeliver" "$DOCS_DIR"         && pass "MaxDeliver documented"         || warn_file "$DOCS_DIR" "MaxDeliver not documented"
else
  warn_file "$DOCS_DIR" "docs directory missing"
fi

# 6) Header keys presence in consumers (optional hints)
for CONS in router_result_consumer.erl router_ack_consumer.erl; do
  FILE="$ROUTER_SRC_DIR/$CONS"
  if file_exists "$FILE"; then
    for KEY in trace_id tenant_id version; do
      grep_q "$FILE" "\\b${KEY}\\b" && pass "$CONS references header key: $KEY" || warn_file "$FILE" "$CONS missing reference to $KEY"
    done
  fi
done

printf "Summary: PASS=%d FAIL=%d WARN=%d\n" "$PASS_COUNT" "$FAIL_COUNT" "$WARN_COUNT"
if [ "$FAIL_COUNT" -eq 0 ]; then
  echo "=== CP2 Behavior Check: PASS ==="
  exit 0
else
  echo "=== CP2 Behavior Check: FAIL ==="
  exit 1
fi
