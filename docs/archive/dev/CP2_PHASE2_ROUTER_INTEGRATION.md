# Phase 2: Router Integration

⚠️ **LEGACY**: Early planning document. See `docs/archive/dev/EXTENSIONS_PIPELINE_IMPLEMENTATION_REPORT.md` for current implementation.

**Status**: ⚠️ **LEGACY** (Early Planning)  
**Duration**: 2 days  
**Prerequisites**: Phase 1 complete (Extension Registry operational)  
**Current Source of Truth**: `docs/archive/dev/EXTENSIONS_PIPELINE_IMPLEMENTATION_REPORT.md`

---

## Day 4: Extension Invoker

### Module: router_extension_invoker

**Purpose**: Invoke extensions via NATS request-reply with retry logic

**File**: `apps/otp/router/src/router_extension_invoker.erl`

**Key Features:**
- NATS request-reply pattern
- Configurable timeout/retry from Extension Registry
- Trace context propagation (OpenTelemetry)
- Health metrics collection
- Circuit breaker (optional, CP3+)

### Implementation Outline

```erlang
-module(router_extension_invoker).

-export([invoke/3, invoke_provider/3]).

%% Main invocation function
-spec invoke(ExtensionId :: binary(), Request :: map(), Context :: map()) ->
    {ok, Response :: map()} | {error, term()}.
invoke(ExtensionId, Request, Context) ->
    %% 1. Lookup extension from registry
    %% 2. Enrich request with trace context
    %% 3. Invoke via NATS with retry
    %% 4. Record metrics (success/failure)
    %% 5. Return response

%% Provider-specific wrapper
invoke_provider(ProviderId, Request, Context) ->
    invoke(ProviderId, Request, Context).
```

### Critical Functions

#### 1. NATS Request-Reply

```erlang
nats_request_reply(Subject, Payload, TimeoutMs) ->
    Connection = router_nats:get_connection(),
    case nats:request(Connection, Subject, Payload, TimeoutMs) of
        {ok, Response} -> {ok, Response};
        {error, timeout} -> {error, timeout};
        {error, Reason} -> {error, Reason}
    end.
```

#### 2. Retry Logic

```erlang
invoke_with_retry(_Subject, _Payload, _TimeoutMs, 0, _ExtId, LastError) ->
    {error, {max_retries_exceeded, LastError}};

invoke_with_retry(Subject, Payload, TimeoutMs, RetriesLeft, ExtId, _PrevError) ->
    case nats_request_reply(Subject, Payload, TimeoutMs) of
        {ok, Response} ->
            record_success(ExtId),
            {ok, Response};
        {error, timeout} = Error ->
            logger:warning("Extension timeout: ~p, retries left: ~p", [ExtId, RetriesLeft - 1]),
            timer:sleep(backoff_delay(RetriesLeft)),
            invoke_with_retry(Subject, Payload, TimeoutMs, RetriesLeft - 1, ExtId, Error);
        {error, Reason} = Error ->
            record_failure(ExtId, Reason),
            Error
    end.

backoff_delay(RetriesLeft) ->
    %% Exponential backoff: 100ms, 200ms, 400ms, ...
    100 * trunc(math:pow(2, 3 - RetriesLeft)).
```

#### 3. Health Tracking

```erlang
record_success(ExtensionId, Latency) ->
    SQL = "INSERT INTO extension_health (extension_id, last_success, success_count, avg_latency_ms) 
           VALUES ($1, NOW(), 1, $2)
           ON CONFLICT (extension_id) DO UPDATE 
           SET last_success = NOW(),
               success_count = extension_health.success_count + 1,
               avg_latency_ms = (extension_health.avg_latency_ms * extension_health.success_count + $2) / 
                                (extension_health.success_count + 1)",
    router_postgres:query(SQL, [ExtensionId, Latency]).

record_failure(ExtensionId, Reason) ->
    logger:error("Extension failure: ~p, reason: ~p", [ExtensionId, Reason]),
    SQL = "INSERT INTO extension_health (extension_id, last_failure, failure_count) 
           VALUES ($1, NOW(), 1)
           ON CONFLICT (extension_id) DO UPDATE 
           SET last_failure = NOW(),
               failure_count = extension_health.failure_count + 1",
    router_postgres:query(SQL, [ExtensionId]).
```

#### 4. OpenTelemetry Tracing

```erlang
invoke_with_tracing(ExtensionId, Subject, Request, Context) ->
    SpanName = <<"router.extension.invoke">>,
    Attributes = #{
        <<"extension.id">> => ExtensionId,
        <<"extension.subject">> => Subject,
        <<"trace_id">> => maps:get(<<"trace_id">>, Context, <<"unknown">>)
    },
    
    %% Start span
    SpanCtx = otel_tracer:start_span(SpanName, Attributes),
    
    %% Invoke
    Result = do_invoke(ExtensionId, Subject, Request, Context),
    
    %% End span with status
    case Result of
        {ok, _} -> otel_tracer:end_span(SpanCtx, #{status => ok});
        {error, Reason} -> otel_tracer:end_span(SpanCtx, #{status => error, error => Reason})
    end,
    
    Result.
```

### Tasks:
- [ ] Create `router_extension_invoker.erl`
- [ ] Implement NATS request-reply
- [ ] Add retry with exponential backoff
- [ ] OpenTelemetry span creation
- [ ] Prometheus metrics (counter, histogram)
- [ ] Unit tests with NATS mock
- [ ] Integration tests with real extension
- [ ] Load test: 1000 req/sec sustained

---

## Day 5: Policy Integration

### Update: router_decider.erl

**Purpose**: Integrate extensions into routing pipeline

### New Functions

#### 1. Extensions Pipeline

```erlang
apply_extensions_pipeline(Message, Policy, Context) ->
    Steps = maps:get(<<"steps">>, Policy, []),
    apply_steps(Message, Steps, Context, #{executed => []}).

apply_steps(Message, [], _Context, Acc) ->
    {ok, Message, Acc};

apply_steps(Message, [Step | Rest], Context, Acc) ->
    Action = maps:get(<<"action">>, Step),
    
    case handle_step(Action, Step, Message, Context, Acc) of
        {ok, UpdatedMessage, UpdatedAcc} ->
            apply_steps(UpdatedMessage, Rest, Context, UpdatedAcc);
        {error, Reason} ->
            {error, Reason, Acc}
    end.
```

#### 2. Step Handlers

```erlang
handle_step(<<"extension_pre">>, Step, Message, Context, Acc) ->
    ExtId = maps:get(<<"extension_id">>, Step),
    case router_extension_invoker:invoke(ExtId, Message, Context) of
        {ok, #{<<"message">> := UpdatedMessage}} ->
            NewAcc = add_executed(Acc, ExtId, <<"pre">>),
            {ok, UpdatedMessage, NewAcc};
        {error, Reason} ->
            {error, {extension_failed, ExtId, Reason}}
    end;

handle_step(<<"extension_validator">>, Step, Message, Context, Acc) ->
    ExtId = maps:get(<<"extension_id">>, Step),
    case router_extension_invoker:invoke(ExtId, Message, Context) of
        {ok, #{<<"status">> := <<"ok">>}} ->
            NewAcc = add_executed(Acc, ExtId, <<"validator">>),
            {ok, Message, NewAcc};
        {ok, #{<<"status">> := <<"reject">>, <<"reason">> := Reason}} ->
            {error, {validation_rejected, ExtId, Reason}};
        {error, Reason} ->
            {error, {extension_failed, ExtId, Reason}}
    end;

handle_step(<<"select_provider">>, Step, Message, Context, Acc) ->
    ProviderId = maps:get(<<"provider_id">>, Step),
    case router_extension_invoker:invoke_provider(ProviderId, Message, Context) of
        {ok, Response} ->
            NewAcc = add_executed(Acc, ProviderId, <<"provider">>),
            {ok, Response, NewAcc};
        {error, Reason} ->
            %% Try fallback
            handle_fallback(Step, Message, Context, Acc, Reason)
    end.

handle_fallback(Step, Message, Context, Acc, PrimaryError) ->
    Fallbacks = maps:get(<<"fallback">>, Step, []),
    try_fallbacks(Fallbacks, Message, Context, Acc, PrimaryError).

try_fallbacks([], _Message, _Context, _Acc, LastError) ->
    {error, {all_providers_failed, LastError}};

try_fallbacks([ProviderId | Rest], Message, Context, Acc, _PrevError) ->
    logger:info("Trying fallback provider: ~p", [ProviderId]),
    case router_extension_invoker:invoke_provider(ProviderId, Message, Context) of
        {ok, Response} ->
            NewAcc = add_executed(Acc, ProviderId, <<"provider_fallback">>),
            {ok, Response, NewAcc};
        {error, Reason} ->
            try_fallbacks(Rest, Message, Context, Acc, Reason)
    end.

add_executed(Acc, ExtensionId, Type) ->
    Executed = maps:get(executed, Acc, []),
    Acc#{executed => [{ExtensionId, Type} | Executed]}.
```

### Example Policy

```json
{
  "policy_id": "ai_chat_with_safety",
  "description": "AI chat with text normalization and PII protection",
  "steps": [
    {
      "action": "extension_pre",
      "extension_id": "normalize_text",
      "config": {
        "lowercase": true,
        "trim_whitespace": true
      }
    },
    {
      "action": "extension_validator",
      "extension_id": "pii_guard",
      "config": {
        "patterns": ["credit_card", "ssn", "email"],
        "block_on_match": true
      }
    },
    {
      "action": "select_provider",
      "provider_id": "openai_gpt4",
      "weight": 80,
      "fallback": ["anthropic_claude", "mock_provider"]
    }
  ],
  "timeout_ms": 35000,
  "retry_policy": {
    "max_retries": 2,
    "backoff_ms": 100
  }
}
```

### Tasks:
- [ ] Update `router_decider.erl`
- [ ] Implement `apply_extensions_pipeline/3`
- [ ] Add step handlers (pre/validator/provider)
- [ ] Fallback provider logic
- [ ] Policy schema validation (JSON Schema)
- [ ] Update `docs/ROUTING_POLICY.md` with examples
- [ ] Integration tests (full pipeline)
- [ ] Performance benchmarks (latency overhead < 10ms)

---

## Testing Strategy

### Unit Tests

**Extension Invoker:**
- [ ] NATS request-reply with mock
- [ ] Retry logic (timeout simulation)
- [ ] Metrics recording
- [ ] Trace context propagation

**Policy Integration:**
- [ ] Pre-processor step
- [ ] Validator step (accept/reject)
- [ ] Provider selection
- [ ] Fallback providers
- [ ] Pipeline execution order

### Integration Tests

**End-to-End:**
- [ ] HTTP request → Gateway → Router → Extension → Provider → Response
- [ ] Extension timeout → fallback provider
- [ ] Validator rejection → 403 response
- [ ] Multiple pre-processors in sequence
- [ ] Trace ID propagation through all steps

**Failure Scenarios:**
- [ ] Extension not found → error
- [ ] Extension timeout → retry → fallback
- [ ] All providers fail → error response
- [ ] PostgreSQL down → cache still works

### Performance Tests

- [ ] Baseline: Request without extensions (50ms p99)
- [ ] + 1 pre-processor: +5ms overhead
- [ ] + 1 validator: +5ms overhead
- [ ] + provider call: +200ms (external API)
- [ ] Total p99 latency: <270ms

---

## Monitoring & Observability

### Metrics (Prometheus)

```erlang
%% Extension invocation
router_extension_requests_total{extension_id, status} counter
router_extension_duration_seconds{extension_id} histogram
router_extension_failures_total{extension_id, reason} counter

%% Provider selection
router_provider_requests_total{provider_id, status} counter
router_provider_fallback_total{primary_provider, fallback_provider} counter

%% Pipeline execution
router_pipeline_steps_total{step_type} counter
router_pipeline_duration_seconds histogram
```

### Logs (JSON Structured)

```json
{
  "timestamp": "2025-11-22T10:00:00Z",
  "level": "info",
  "component": "router_extension_invoker",
  "extension_id": "pii_guard",
  "action": "invoke",
  "trace_id": "abc-123-def",
  "tenant_id": "tenant-1",
  "latency_ms": 45,
  "status": "success"
}
```

### Tracing (OpenTelemetry)

**Span Hierarchy:**
```
router.decide
├─ router.extension.pre (normalize_text)
├─ router.extension.validator (pii_guard)
└─ router.extension.provider (openai_gpt4)
   └─ openai.api.chat_completion (external)
```

---

## Documentation Updates

### Files to Update:

1. **docs/ROUTING_POLICY.md**
   - Add extensions section
   - Policy examples with extensions
   - Step types reference

2. **docs/NATS_SUBJECTS.md**
   - Document extension subjects pattern
   - Request/response contracts

3. **docs/ARCHITECTURE/extension-invocation-flow.md** (new)
   - Sequence diagrams
   - Error handling flows
   - Fallback logic

4. **apps/otp/router/docs/OPERATIONAL_GUIDE.md**
   - Extension troubleshooting
   - Common errors and solutions

---

## Deliverables Checklist

- [ ] `router_extension_invoker.erl` module
- [ ] Updated `router_decider.erl` with pipeline
- [ ] Policy schema updates
- [ ] Unit tests (>80% coverage)
- [ ] Integration tests (full pipeline)
- [ ] Performance benchmarks report
- [ ] Documentation updates
- [ ] Metrics dashboard config

---

**Next**: Proceed to Phase 3 (Reference Implementations)
