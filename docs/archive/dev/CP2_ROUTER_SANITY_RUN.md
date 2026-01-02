# CP2 Router Sanity Run

**Purpose**: Minimal manual sanity check for CP2 Router features (JetStream + Idempotency)  
**Target**: Developers who want to verify CP2 features are working after implementation  
**Time**: ~5-10 minutes

## Prerequisites

- Erlang/OTP installed (tested with OTP 25+)
- NATS server running locally (default: `nats://localhost:4222`)
- Router application built (`rebar3 compile`)
- Prometheus metrics endpoint accessible (default: `http://localhost:9001/metrics`)

## Step 1: Start Local Stand (Router + NATS)

### Option A: Using Project Scripts (Recommended)

```bash
# From project root
cd /path/to/aigroup

# Start all services (NATS, Router, etc.)
./scripts/start-services.sh

# Verify NATS is running
curl http://localhost:8222/healthz
```

### Option B: Manual Docker Compose

```bash
# Start NATS with JetStream enabled
docker run -d --name nats-jetstream \
  -p 4222:4222 \
  -p 8222:8222 \
  -p 6222:6222 \
  nats:latest -js

# Verify NATS is running
curl http://localhost:8222/healthz
```

### Option C: Local NATS Installation

```bash
# Install NATS server (if not installed)
# macOS: brew install nats-server
# Linux: Download from https://github.com/nats-io/nats-server/releases

# Start NATS with JetStream
nats-server -js
```

### Start Router

```bash
# Navigate to router directory
cd apps/otp/router

# Compile (if needed)
rebar3 compile

# Start Router application
rebar3 shell

# In Erlang shell:
1> application:ensure_all_started(beamline_router).
{ok, [beamline_router]}

# Initialize CP2 modules
2> router_idem:init([]).
ok

3> router_jetstream:start_link().
{ok, <0.XXX.0>}

# Verify Router is running
4> whereis(router_jetstream).
<0.XXX.0>  % Should return a PID

5> whereis(router_idem).
undefined  % router_idem uses ETS, not gen_server

# Verify ETS tables exist
6> ets:info(router_idem).
[...]  % Should return table info

7> ets:info(router_jetstream_state).
[...]  % Should return table info
```

**Expected Result**: Router starts successfully, `router_jetstream` and `router_idem` processes are running.

## Step 2: Send Messages with Same Idempotency Key

### Using NATS CLI (if installed)

```bash
# Install NATS CLI (if not installed)
# macOS: brew install nats-io/nats-tools/nats
# Linux: Download from https://github.com/nats-io/natscli/releases

# Send first message with idempotency_key
echo '{"tenant_id":"test_tenant","request_id":"req-1","idempotency_key":"idem-key-123","task":{"type":"chat","payload":"test message 1"}}' | \
  nats pub beamline.router.v1.decide --header "trace_id=trace-123"

# Send second message with SAME idempotency_key (should be detected as duplicate)
echo '{"tenant_id":"test_tenant","request_id":"req-2","idempotency_key":"idem-key-123","task":{"type":"chat","payload":"test message 2"}}' | \
  nats pub beamline.router.v1.decide --header "trace_id=trace-123"

# Send third message with DIFFERENT idempotency_key (should be processed)
echo '{"tenant_id":"test_tenant","request_id":"req-3","idempotency_key":"idem-key-456","task":{"type":"chat","payload":"test message 3"}}' | \
  nats pub beamline.router.v1.decide --header "trace_id=trace-123"
```

### Using curl (alternative)

```bash
# Send messages via HTTP (if Router exposes HTTP endpoint)
curl -X POST http://localhost:9000/api/v1/routes/decide \
  -H "Content-Type: application/json" \
  -H "trace_id: trace-123" \
  -d '{"tenant_id":"test_tenant","request_id":"req-1","idempotency_key":"idem-key-123","task":{"type":"chat","payload":"test message 1"}}'

curl -X POST http://localhost:9000/api/v1/routes/decide \
  -H "Content-Type: application/json" \
  -H "trace_id: trace-123" \
  -d '{"tenant_id":"test_tenant","request_id":"req-2","idempotency_key":"idem-key-123","task":{"type":"chat","payload":"test message 2"}}'
```

**Expected Result**: 
- First message: processed (idempotency miss)
- Second message: detected as duplicate (idempotency hit)
- Third message: processed (idempotency miss)

## Step 3: Simulate Handler Failures (Redelivery + DLQ)

### Configure JetStream for Testing

In Erlang shell:

```erlang
% Configure JetStream: MaxDeliver = 3, DLQ enabled
router_jetstream:configure(#{
    max_deliver => 3,
    backoff_seconds => [1, 5, 15],
    dlq_subject_pattern => <<"~s.dlq">>,
    dlq_enabled => true
}).
```

### Simulate Message Processing with Errors

In Erlang shell:

```erlang
% Create test message
Msg = #{
    id => <<"msg-redelivery-test">>,
    subject => <<"beamline.router.v1.decide">>,
    payload => <<"{\"tenant_id\":\"test_tenant\",\"request_id\":\"req-redelivery\"}">>,
    headers => #{<<"Nats-Msg-Id">> => <<"msg-redelivery-test">>}
}.

% Simulate first delivery (will NAK due to error)
router_jetstream:handle(Msg, #{}).
timer:sleep(100).

% Simulate second delivery (will NAK again)
router_jetstream:handle(Msg, #{}).
timer:sleep(100).

% Simulate third delivery (will NAK again - MaxDeliver reached, should go to DLQ)
router_jetstream:handle(Msg, #{}).
timer:sleep(100).
```

### Verify DLQ Message

```bash
# Check DLQ subject (if using NATS CLI)
nats sub "beamline.router.v1.decide.dlq" --count=1

# Or check Router logs for DLQ publication
# Look for: "Message sent to DLQ" or "MaxDeliver reached"
```

**Expected Result**:
- First 3 deliveries: NAK (redelivery)
- After MaxDeliver (3): Message sent to DLQ
- DLQ message contains: `original_subject`, `error_code`, `delivery_count`, `trace_id`, `tenant_id`

## Step 4: Check Prometheus Metrics

### Access Metrics Endpoint

```bash
# Get all metrics
curl http://localhost:9001/metrics

# Filter JetStream metrics
curl http://localhost:9001/metrics | grep router_jetstream

# Filter Idempotency metrics
curl http://localhost:9001/metrics | grep router_idem
```

### Expected Metrics

**JetStream Metrics**:
```
router_jetstream_ack_total 3
router_redelivery_total 3
router_dlq_total 1
```

**Idempotency Metrics**:
```
router_idem_hits_total 1
router_idem_miss_total 2
```

### Quick Verification Script

```bash
#!/bin/bash
# quick_sanity_check.sh

METRICS_URL="http://localhost:9001/metrics"

echo "=== JetStream Metrics ==="
curl -s $METRICS_URL | grep router_jetstream

echo ""
echo "=== Idempotency Metrics ==="
curl -s $METRICS_URL | grep router_idem

echo ""
echo "=== Expected Values ==="
echo "router_jetstream_ack_total: >= 1"
echo "router_redelivery_total: >= 1"
echo "router_dlq_total: >= 1"
echo "router_idem_hits_total: >= 1"
echo "router_idem_miss_total: >= 1"
```

**Expected Result**: All metrics are present and have non-zero values (or at least 1 for sanity check).

## Troubleshooting

### Router Not Starting

```bash
# Check if NATS is accessible
telnet localhost 4222

# Check Router logs
tail -f apps/otp/router/log/router_*.log

# Verify ETS tables are created
erl -sname test -remsh router@localhost
1> ets:info(router_jetstream_state).
1> ets:info(router_idem).
```

### Metrics Not Available

```bash
# Check if metrics HTTP server is running
curl http://localhost:9001/health

# Check Router configuration
erl -sname test -remsh router@localhost
1> application:get_env(beamline_router, metrics_http_port).
```

### Idempotency Not Working

```bash
# Check idempotency table
erl -sname test -remsh router@localhost
1> ets:tab2list(router_idem).

# Verify TTL configuration
1> application:get_env(beamline_router, idempotency_ttl_seconds).
```

### JetStream Redelivery Not Working

```bash
# Check JetStream configuration
erl -sname test -remsh router@localhost
1> ets:lookup(router_jetstream_state, config).

# Verify NATS JetStream is enabled
nats stream ls
```

## Summary

After completing all steps, you should see:

1. ✅ Router and NATS running
2. ✅ Idempotency detecting duplicates (hits > 0)
3. ✅ JetStream redelivery working (redelivery_total > 0)
4. ✅ DLQ receiving messages after MaxDeliver (dlq_total > 0)
5. ✅ All metrics present and non-zero

**If all checks pass**: CP2 Router features (JetStream + Idempotency) are working correctly.

**If any check fails**: Review Router logs and verify configuration.

## Next Steps

- Run full test suite: `rebar3 ct --suite apps/otp/router/test/router_cp2_features_e2e_SUITE`
- Check integration tests: `rebar3 ct --suite apps/otp/router/test/router_jetstream_e2e_SUITE`
- Review detailed documentation: `docs/CP2_CHECKLIST.md`

