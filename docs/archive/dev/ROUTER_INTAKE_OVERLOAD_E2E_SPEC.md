# Router Intake Overload E2E Test Specification

**Date**: 2025-11-26  
**Status**: 📋 **Test Specification**  
**Purpose**: Define E2E test scenarios for Router intake overload and backpressure  
**Related**: `docs/ARCHITECTURE/router-intake-backpressure-policy.md`

## Overview

This document specifies E2E test scenarios for Router intake overload conditions. These tests verify that Router:
- ✅ Continues operating under overload (doesn't crash)
- ✅ Detects overload conditions (backpressure triggers)
- ✅ Applies backpressure actions (rejection, rate limiting adjustment)
- ✅ Emits correct metrics and alerts
- ✅ Recovers gracefully after overload resolved

## Test Scenarios

### Scenario 1: JetStream Backlog Overload

**Goal**: Verify Router handles high JetStream backlog

**Setup**:
1. Start Router with backpressure enabled
2. Configure overload thresholds:
   - `queue_overload = 1000`
   - `latency_overload_ms = 5000`
   - `inflight_overload = 500`
3. Simulate high backlog: Send 2000 valid `decide` messages rapidly
4. Simulate slow processing: Add 100ms delay per message

**Expected Behavior**:
- ✅ Router continues processing (doesn't crash)
- ✅ JetStream backlog grows (`pending_messages > 1000`)
- ✅ Backpressure triggered (`backpressure_active == 1`)
- ✅ Metrics emitted:
  - `router_jetstream_pending_messages > 1000`
  - `router_intake_backpressure_active == 1`
  - `router_intake_backpressure_triggered_total > 0`
- ✅ Alerts emitted (backpressure active, queue depth high)
- ✅ Logs show backpressure warnings

**Verification**:
```erlang
%% Check metrics
Pending = get_metric(router_jetstream_pending_messages, [{subject, <<"beamline.router.v1.decide">>}]),
?assert(Pending > 1000, "Backlog should exceed overload threshold"),

BackpressureActive = get_metric(router_intake_backpressure_active, [{subject, <<"beamline.router.v1.decide">>}]),
?assertEqual(1, BackpressureActive, "Backpressure should be active"),

Triggered = get_metric(router_intake_backpressure_triggered_total, [{subject, <<"beamline.router.v1.decide">>}, {trigger, <<"overload">>}]),
?assert(Triggered > 0, "Backpressure should be triggered"),
```

### Scenario 2: Processing Latency Overload

**Goal**: Verify Router handles high processing latency

**Setup**:
1. Start Router with backpressure enabled
2. Configure overload thresholds:
   - `latency_overload_ms = 5000`
3. Send 1000 valid `decide` messages
4. Simulate very slow processing: Add 2s delay per message

**Expected Behavior**:
- ✅ Router continues processing (doesn't crash)
- ✅ Processing latency grows (`latency_p95 > 5000ms`)
- ✅ Backpressure triggered (`backpressure_active == 1`)
- ✅ New requests rejected with HTTP 503 (if rejection enabled)
- ✅ Metrics emitted:
  - `router_intake_processing_latency_p95 > 5000`
  - `router_intake_backpressure_active == 1`
  - `router_intake_backpressure_rejections_total > 0` (if rejection enabled)
- ✅ Alerts emitted (latency high, backpressure active)

**Verification**:
```erlang
%% Check latency
LatencyP95 = get_metric(router_intake_processing_latency_p95, [{subject, <<"beamline.router.v1.decide">>}]),
?assert(LatencyP95 > 5000, "Latency should exceed overload threshold"),

BackpressureActive = get_metric(router_intake_backpressure_active, [{subject, <<"beamline.router.v1.decide">>}]),
?assertEqual(1, BackpressureActive, "Backpressure should be active"),

%% Check rejections (if enabled)
Rejections = get_metric(router_intake_backpressure_rejections_total, [{subject, <<"beamline.router.v1.decide">>}]),
?assert(Rejections > 0, "Rejections should occur"),
```

### Scenario 3: In-Flight Messages Overload

**Goal**: Verify Router handles high in-flight message count

**Setup**:
1. Start Router with backpressure enabled
2. Configure overload thresholds:
   - `inflight_overload = 500`
3. Send 1000 valid `decide` messages concurrently
4. Simulate slow processing: Add 1s delay per message

**Expected Behavior**:
- ✅ Router continues processing (doesn't crash)
- ✅ In-flight messages grow (`inflight_messages > 500`)
- ✅ Backpressure triggered (`backpressure_active == 1`)
- ✅ New requests rejected with HTTP 503 (if rejection enabled)
- ✅ Metrics emitted:
  - `router_intake_inflight_messages > 500`
  - `router_intake_backpressure_active == 1`
  - `router_intake_backpressure_rejections_total > 0` (if rejection enabled)
- ✅ Alerts emitted (in-flight high, backpressure active)

**Verification**:
```erlang
%% Check in-flight
InFlight = get_metric(router_intake_inflight_messages, [{subject, <<"beamline.router.v1.decide">>}]),
?assert(InFlight > 500, "In-flight should exceed overload threshold"),

BackpressureActive = get_metric(router_intake_backpressure_active, [{subject, <<"beamline.router.v1.decide">>}]),
?assertEqual(1, BackpressureActive, "Backpressure should be active"),
```

### Scenario 4: Combined Overload (Backlog + Latency + In-Flight)

**Goal**: Verify Router handles all overload conditions simultaneously

**Setup**:
1. Start Router with backpressure enabled
2. Configure overload thresholds (all)
3. Send 3000 valid `decide` messages rapidly
4. Simulate very slow processing: Add 3s delay per message

**Expected Behavior**:
- ✅ Router continues processing (doesn't crash)
- ✅ All overload triggers activated:
  - `pending_messages > 1000`
  - `latency_p95 > 5000`
  - `inflight_messages > 500`
- ✅ Backpressure triggered (`backpressure_active == 1`)
- ✅ Gateway rate limits reduced (if monitoring enabled)
- ✅ New requests rejected with HTTP 503
- ✅ All alerts emitted
- ✅ Metrics show overload state

**Verification**:
```erlang
%% Check all overload conditions
Pending = get_metric(router_jetstream_pending_messages, [{subject, <<"beamline.router.v1.decide">>}]),
LatencyP95 = get_metric(router_intake_processing_latency_p95, [{subject, <<"beamline.router.v1.decide">>}]),
InFlight = get_metric(router_intake_inflight_messages, [{subject, <<"beamline.router.v1.decide">>}]),

?assert(Pending > 1000, "Backlog should exceed overload threshold"),
?assert(LatencyP95 > 5000, "Latency should exceed overload threshold"),
?assert(InFlight > 500, "In-flight should exceed overload threshold"),

BackpressureActive = get_metric(router_intake_backpressure_active, [{subject, <<"beamline.router.v1.decide">>}]),
?assertEqual(1, BackpressureActive, "Backpressure should be active"),
```

### Scenario 5: Overload Recovery

**Goal**: Verify Router recovers after overload resolved

**Setup**:
1. Trigger overload (Scenario 4)
2. Wait for overload to resolve (stop sending messages, wait for processing)
3. Verify recovery

**Expected Behavior**:
- ✅ Backpressure deactivates (`backpressure_active == 0`)
- ✅ Metrics return to normal:
  - `pending_messages < 100`
  - `latency_p95 < 500ms`
  - `inflight_messages < 50`
- ✅ New requests accepted (HTTP 200)
- ✅ Gateway rate limits restored (if adjustment enabled)
- ✅ Alerts cleared

**Verification**:
```erlang
%% Wait for recovery
timer:sleep(10000),  %% Wait 10 seconds

%% Check recovery
Pending = get_metric(router_jetstream_pending_messages, [{subject, <<"beamline.router.v1.decide">>}]),
LatencyP95 = get_metric(router_intake_processing_latency_p95, [{subject, <<"beamline.router.v1.decide">>}]),
InFlight = get_metric(router_intake_inflight_messages, [{subject, <<"beamline.router.v1.decide">>}]),

?assert(Pending < 100, "Backlog should be below warning threshold"),
?assert(LatencyP95 < 500, "Latency should be below warning threshold"),
?assert(InFlight < 50, "In-flight should be below warning threshold"),

BackpressureActive = get_metric(router_intake_backpressure_active, [{subject, <<"beamline.router.v1.decide">>}]),
?assertEqual(0, BackpressureActive, "Backpressure should be inactive"),
```

## Test Implementation

### Test File

**Location**: `apps/otp/router/test/router_intake_e2e_SUITE.erl`

**Test Group**: `overload_tests`

**Test Cases**:
- `test_overload_jetstream_backlog/1`
- `test_overload_processing_latency/1`
- `test_overload_inflight_messages/1`
- `test_overload_combined/1`
- `test_overload_recovery/1`

### Helper Functions

**Slow Processing Simulation**:
```erlang
%% Add delay to simulate slow processing
simulate_slow_processing(DelayMs) ->
    timer:sleep(DelayMs),
    ok.
```

**Message Generation**:
```erlang
%% Generate valid decide request
generate_valid_decide_request(Index) ->
    #{
        <<"version">> => <<"1">>,
        <<"tenant_id">> => <<"tenant-overload-test">>,
        <<"request_id">> => list_to_binary("req-overload-" ++ integer_to_list(Index)),
        <<"task">> => #{
            <<"type">> => <<"text.generate">>,
            <<"payload">> => #{}
        }
    }.
```

**Metrics Verification**:
```erlang
%% Get metric value
get_metric(MetricName, Labels) ->
    %% Query metrics via telemetry or ETS
    %% Implementation depends on metrics storage
    ok.
```

## Test Execution

### Prerequisites

1. **NATS**: Running JetStream-enabled NATS server
2. **Router**: Router application started
3. **Configuration**: Backpressure thresholds configured

### Running Tests

**Single Test**:
```bash
cd apps/otp/router
rebar3 ct suite=router_intake_e2e_SUITE group=overload_tests
```

**All Overload Tests**:
```bash
cd apps/otp/router
rebar3 ct suite=router_intake_e2e_SUITE group=overload_tests
```

**With Specific Test**:
```bash
cd apps/otp/router
rebar3 ct suite=router_intake_e2e_SUITE testcase=test_overload_combined
```

## Expected Results

### Success Criteria

**All Tests Pass**:
- ✅ Router doesn't crash during overload
- ✅ Backpressure detected correctly
- ✅ Metrics emitted correctly
- ✅ Alerts emitted correctly
- ✅ Recovery works correctly

**Metrics Verification**:
- ✅ `router_jetstream_pending_messages` tracked
- ✅ `router_intake_processing_latency_p95` tracked
- ✅ `router_intake_inflight_messages` tracked
- ✅ `router_intake_backpressure_active` correct (0/1)
- ✅ `router_intake_backpressure_triggered_total` incremented
- ✅ `router_intake_backpressure_rejections_total` incremented (if rejection enabled)

### Failure Scenarios

**Router Crashes**:
- ❌ Test fails immediately
- ❌ Check logs for crash reason
- ❌ Verify process monitoring

**Backpressure Not Detected**:
- ❌ Metrics show overload but `backpressure_active == 0`
- ❌ Check threshold configuration
- ❌ Verify backpressure detection logic

**Metrics Not Emitted**:
- ❌ Metrics missing or zero during overload
- ❌ Check telemetry integration
- ❌ Verify metric definitions

## References

- **Backpressure Policy**: `docs/ARCHITECTURE/router-intake-backpressure-policy.md`
- **Metrics Guide**: `docs/OBSERVABILITY_METRICS_MONITORING_GUIDE.md`
- **Load Tests**: `docs/archive/dev/ROUTER_INTAKE_LOAD_TESTS_SPEC.md`
- **Test Implementation**: `apps/otp/router/test/router_intake_e2e_SUITE.erl`

