# Policy Applier Implementation Report

## Purpose

Реализация единого модуля применения policy (`router_policy_applier.erl`):
- Вход: запрос + tenant + policy_id (опционально)
- Выход: выбранный provider, список extensions, объяснение решения (для audit)

## Status

✅ **COMPLETED** - Модуль реализован и готов к использованию

## Implementation

### Module: `router_policy_applier.erl`

**Exports**:
- `apply_policy/3` - основная функция применения policy
- `apply_policy/4` - с дополнительным контекстом

**Input**:
- `Request` - route request (map или #route_request{})
- `TenantId` - tenant identifier (binary)
- `PolicyId` - policy identifier (binary, опционально, по умолчанию "default")
- `Context` - дополнительный контекст (map, опционально)

**Output**:
```erlang
{ok, #{
    provider_id => binary(),
    extensions => #{
        pre => [ExtensionConfig],
        validators => [ExtensionConfig],
        post => [ExtensionConfig]
    },
    explanation => #{
        reason => binary(),  % "sticky" | "weighted" | "fallback"
        provider_id => binary(),
        policy_id => binary(),
        policy_version => binary(),
        priority => integer(),
        steps => [binary()],  % Пошаговое объяснение решения
        context => map()  % Контекст решения
    }
}} | {error, {Reason, Context}}
```

### Features

#### 1. Provider Selection

Модуль использует `router_decider:decide/3` для выбора провайдера:
- Sticky routing (если enabled)
- Weighted distribution
- Fallback rules

#### 2. Extensions Extraction

Извлекает extensions из policy:
- `pre` - препроцессоры
- `validators` - валидаторы
- `post` - постпроцессоры

Поддерживает:
- Прямое поле `extensions` в policy
- Backward compatibility через `metadata.extensions`

#### 3. Decision Explanation

Формирует подробное объяснение решения для audit:
- **reason**: причина выбора провайдера
- **steps**: пошаговое описание процесса принятия решения
- **context**: контекст решения (tenant_id, policy_id, etc.)

**Example Explanation**:
```erlang
#{
    reason => <<"weighted">>,
    provider_id => <<"openai">>,
    policy_id => <<"default">>,
    policy_version => <<"1.0">>,
    priority => 50,
    steps => [
        <<"1. Checked sticky session: no existing session found">>,
        <<"2. Applied weighted distribution: 2 providers, total weight: 1.00">>,
        <<"3. Skipped fallbacks (provider selected via weighted)">>
    ],
    context => #{
        <<"tenant_id">> => <<"tenant_123">>,
        <<"message_id">> => <<"msg_456">>
    }
}
```

## Integration

### Usage Example

```erlang
%% Basic usage
case router_policy_applier:apply_policy(Request, TenantId, PolicyId) of
    {ok, Result} ->
        ProviderId = maps:get(provider_id, Result),
        Extensions = maps:get(extensions, Result),
        Explanation = maps:get(explanation, Result),
        %% Use provider, extensions, and explanation
        ok;
    {error, ErrorInfo} ->
        %% Handle error
        error
end.

%% With additional context
Context = #{
    <<"trace_id">> => <<"trace_123">>,
    <<"user_id">> => <<"user_456">>
},
case router_policy_applier:apply_policy(Request, TenantId, PolicyId, Context) of
    {ok, Result} ->
        %% Process result
        ok;
    {error, ErrorInfo} ->
        error
end.
```

### Integration with router_core

Модуль можно использовать в `router_core.erl`:

```erlang
%% In router_core:route/2
case router_policy_applier:apply_policy(RouteRequest, TenantId, FinalPolicyId, Context) of
    {ok, Result} ->
        ProviderId = maps:get(provider_id, Result),
        Extensions = maps:get(extensions, Result),
        Explanation = maps:get(explanation, Result),
        
        %% Create RouteDecision
        Decision = #route_decision{
            provider_id = ProviderId,
            reason = maps:get(reason, Explanation),
            priority = maps:get(priority, Explanation),
            metadata = maps:get(context, Explanation)
        },
        
        %% Log explanation for audit
        router_audit:log_decision(Explanation),
        
        {ok, Decision};
    {error, ErrorInfo} ->
        {error, ErrorInfo}
end.
```

## Policy Structure Updates

### Record: `#policy{}`

Добавлено поле `extensions`:
```erlang
-record(policy, {
    tenant_id :: tenant_id(),
    policy_id :: policy_id(),
    version = <<"1.0">> :: binary(),
    defaults = #{} :: map(),
    escalate_on = [] :: list(),
    weights = #{} :: map(),
    fallback = undefined :: map() | undefined,
    fallbacks = [] :: list(),
    sticky = undefined :: map() | undefined,
    extensions = #{} :: map(),  % NEW: Extensions configuration
    metadata = #{} :: map()
}).
```

### Policy JSON Format

Поддерживается формат extensions:
```json
{
  "version": "1.0",
  "providers": [...],
  "fallbacks": [...],
  "sticky": {...},
  "extensions": {
    "pre": [
      {
        "id": "normalize_text",
        "mode": "required",
        "config": {"lowercase": true}
      }
    ],
    "validators": [
      {
        "id": "pii_guard",
        "on_fail": "block"
      }
    ],
    "post": [
      {
        "id": "mask_pii",
        "mode": "required",
        "config": {"mask_email": true}
      }
    ]
  }
}
```

## Files Modified

1. `apps/otp/router/src/router_policy_applier.erl` - **NEW** - единый модуль применения policy
2. `apps/otp/router/include/beamline_router.hrl` - добавлено поле `extensions` в record `policy`
3. `apps/otp/router/src/router_policy_store.erl` - добавлен парсинг extensions

## Testing Recommendations

### Unit Tests

1. **Basic Policy Application**:
   - Применение policy с weights
   - Применение policy со sticky
   - Применение policy с fallbacks

2. **Extensions Extraction**:
   - Извлечение pre/validators/post из extensions
   - Backward compatibility с metadata.extensions
   - Обработка отсутствующих extensions

3. **Explanation Building**:
   - Формирование explanation для sticky
   - Формирование explanation для weighted
   - Формирование explanation для fallback

### Integration Tests

1. **End-to-End Policy Application**:
   - Полный цикл: request → policy → decision → explanation
   - Проверка всех шагов в explanation
   - Проверка extensions extraction

2. **Error Handling**:
   - Policy not found
   - Invalid policy format
   - Missing tenant_id

## Next Steps

1. **Integration**:
   - Интегрировать `router_policy_applier` в `router_core` (опционально)
   - Обновить тесты для использования нового модуля

2. **Audit Integration**:
   - Интегрировать explanation в audit trail
   - Сохранение explanation в базу данных

3. **Documentation**:
   - Обновить API документацию
   - Добавить примеры использования

## References

### Core Documentation
- `docs/ROUTING_POLICY.md` - **JSON-DSL спецификация** routing policies
- `docs/ARCHITECTURE/compatibility-rules.md` - **Compatibility rules** для Proto/NATS/JSON
- `docs/EXTENSIONS_API.md` - Extensions API specification

### Implementation Files
- `apps/otp/router/src/router_policy_applier.erl` - реализация модуля
- `apps/otp/router/src/router_decider.erl` - decision engine
- `apps/otp/router/src/router_policy_store.erl` - policy storage

### Related Documentation
- `docs/archive/dev/POLICY_DSL_SPEC_VS_IMPL_GAP.md` - анализ расхождений DSL ↔ реализация
- `docs/archive/dev/POLICY_ENGINE_JSON_DSL_IMPLEMENTATION.md` - отчет о реализации JSON-DSL
- `docs/archive/dev/POLICY_APPLIER_INTEGRATION.md` - интеграция в Router

## CP1/CP2 Invariants

### CP1 Invariants

**NATS Protocol**:
- ✅ **Subject**: `beamline.router.v1.decide` (Request-Reply pattern)
- ✅ **Payload Format**: JSON (NATS) → Protobuf (internal Router)
- ✅ **Timeout**: 5 seconds (configurable via policy)
- ✅ **Required Fields**: `tenant_id`, `message_id`, `message_type`
- ✅ **Correlation Fields**: `trace_id`, `run_id`, `flow_id`, `step_id` (CP1+)

**REST Protocol** (via Gateway):
- ✅ **Endpoint**: `POST /api/v1/router/decide`
- ✅ **Request Format**: JSON (REST) → NATS JSON (Gateway) → Protobuf (Router)
- ✅ **Response Format**: Protobuf (Router) → NATS JSON (Gateway) → JSON (REST)
- ✅ **Error Handling**: HTTP status codes (200, 400, 404, 500)

**Observability**:
- ✅ **Structured Logging**: JSON format (ISO-8601 timestamps, PII filtering)
- ✅ **Audit Trail**: All routing decisions logged with explanation
- ✅ **Telemetry**: Metrics and events via `router_telemetry_helper`

**References**:
- `docs/archive/dev/CP1_BOUNDARIES_AND_CONTRACTS.md` - CP1 module boundaries
- `docs/OBSERVABILITY_CP1_INVARIANTS.md` - CP1 observability invariants
- `docs/archive/dev/ROUTER_PROTO_NATS_CONSISTENCY.md` - Proto/NATS consistency

### CP2 Invariants

**Enhanced Features** (CP2-LC):
- ✅ **JetStream Integration**: Durable subscriptions, ACK/NAK, redelivery
- ✅ **Idempotency Layer**: ETS-based idempotency checks with TTL
- ✅ **OpenTelemetry Tracing**: Distributed tracing with span creation
- ✅ **Tenant Validation/ACL**: Tenant allowlist and policy registry validation
- ✅ **NAK on Errors**: Automatic NAK on validation failures

**References**:
- `docs/archive/dev/CP2_ROUTER_GATEWAY_SPEC.md` - CP2 Router/Gateway specification
- `docs/archive/dev/BEAMLINE_ROADMAP_AND_MAPPINGS.md` - CP2 roadmap

## Current Limitations and Known Discrepancies

### DSL ↔ Code Discrepancies

#### 1. Retry Logic (Critical)

**Status**: ❌ **NOT IMPLEMENTED**

**Specification** (`docs/ROUTING_POLICY.md`):
- Fallback rules support `retry` count field
- Retry should be applied when fallback provider fails

**Implementation**:
- ✅ `retry` field is **parsed** from JSON-DSL
- ❌ `retry` logic is **NOT applied** in decision making
- ❌ Retry state is **NOT tracked** between requests

**Impact**: Fallback rules with `retry > 1` are parsed but not used

**Workaround**: Use single fallback rule per condition

**Reference**: `docs/archive/dev/POLICY_DSL_SPEC_VS_IMPL_GAP.md#3-retry-logic`

#### 2. Backoff Strategy (Important)

**Status**: ❌ **NOT IMPLEMENTED**

**Specification** (`docs/ROUTING_POLICY.md`):
- Not explicitly specified, but expected for retry logic

**Implementation**:
- ❌ No backoff strategy implemented
- ❌ No `backoff` field in fallback rule JSON-DSL

**Impact**: Retry attempts happen immediately without delay

**Reference**: `docs/archive/dev/POLICY_DSL_SPEC_VS_IMPL_GAP.md#3-retry-logic`

#### 3. Explanation Format (Documentation Gap)

**Status**: ⚠️ **NOT SPECIFIED IN DOCS**

**Specification** (`docs/ROUTING_POLICY.md`):
- ❌ No explicit description of explanation format
- ❌ No description of explanation fields
- ❌ No description of steps format

**Implementation**:
- ✅ Explanation format is implemented in `router_policy_applier:build_explanation/3`
- ✅ Format: `#{reason, provider_id, policy_id, policy_version, priority, steps, context}`
- ✅ Steps: Array of binary strings describing decision steps

**Impact**: Explanation format is implementation-defined, not specification-defined

**Reference**: `docs/archive/dev/POLICY_DSL_SPEC_VS_IMPL_GAP.md#1-explanation-format`

#### 4. Extensions in Main DSL (Documentation Gap)

**Status**: ⚠️ **NOT IN ROUTING_POLICY.md**

**Specification** (`docs/ROUTING_POLICY.md`):
- ❌ Extensions not mentioned in main JSON-DSL example
- ❌ Extensions format not described

**Implementation**:
- ✅ Extensions are parsed from `pre`, `validators`, `post` fields
- ✅ Extensions format matches `EXTENSIONS_API.md`

**Impact**: Users may not know extensions can be configured in policy

**Reference**: `docs/EXTENSIONS_API.md` - Extensions API specification

#### 5. Metadata/Defaults Format (Documentation Gap)

**Status**: ⚠️ **NOT SPECIFIED**

**Specification** (`docs/ROUTING_POLICY.md`):
- ❌ `metadata` field format not specified
- ❌ `defaults` field format not specified
- ❌ `escalate_on` field format not specified

**Implementation**:
- ✅ Fields are parsed and stored in `#policy{}` record
- ✅ Format is implementation-defined (map/array)

**Impact**: These fields are parsed but format is not documented

**Reference**: `docs/archive/dev/POLICY_DSL_SPEC_VS_IMPL_GAP.md#4-additional-json-dsl-fields`

### Current Limitations

#### 1. Weight Normalization

**Status**: ⚠️ **PARTIAL**

**Behavior**:
- Weights are parsed from `providers` array (0-100) → converted to 0.0-1.0
- Weights are **NOT normalized** if sum ≠ 1.0
- Weights with sum < 1.0 or > 1.0 are used as-is

**Impact**: Inconsistent weight distribution if weights don't sum to 100

**Workaround**: Ensure weights sum to 100 in policy JSON

#### 2. Fallback Condition Matching

**Status**: ✅ **IMPLEMENTED** (with limitations)

**Behavior**:
- Fallback conditions are evaluated in order
- First matching condition is used
- Condition matching is **exact match** (no regex, no wildcards)

**Limitations**:
- No regex support in `when` conditions
- No wildcard support
- No complex boolean logic (AND/OR)

**Impact**: Limited flexibility in fallback condition matching

#### 3. Sticky Session TTL

**Status**: ✅ **IMPLEMENTED**

**Behavior**:
- TTL string ("10m", "5h") is parsed to seconds
- TTL is stored in sticky session
- Expired sessions are cleaned up

**Limitations**:
- No per-tenant TTL override
- No dynamic TTL adjustment
- No TTL extension on activity

**Impact**: TTL is fixed per policy, cannot be adjusted per request

## Integration Diagram

### router_policy_applier Integration in Router

```
┌─────────────────────────────────────────────────────────────────┐
│                         Router Core                              │
│  router_core:route/2                                            │
│    ├─ Extract tenant_id, policy_id from RouteRequest            │
│    ├─ Load context (trace_id, user_id, etc.)                    │
│    └─ Call router_policy_applier:apply_policy/4                 │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│              router_policy_applier:apply_policy/4               │
│    ├─ Load policy via router_policy:load_policy/2               │
│    ├─ Merge context (request + additional context)             │
│    ├─ Apply policy decision via router_decider:decide/3         │
│    ├─ Build explanation via build_explanation/3                  │
│    ├─ Extract extensions via extract_extensions/1                 │
│    └─ Return: {ok, #{provider_id, extensions, explanation}}      │
└─────────────────────────────────────────────────────────────────┘
                              │
        ┌─────────────────────┼─────────────────────┐
        ▼                     ▼                     ▼
┌──────────────┐    ┌──────────────┐    ┌──────────────┐
│ router_policy│    │router_decider│    │router_sticky │
│  :load_policy│    │   :decide    │    │    _store    │
└──────────────┘    └──────────────┘    └──────────────┘
        │                     │                     │
        ▼                     ▼                     ▼
┌──────────────┐    ┌──────────────┐    ┌──────────────┐
│router_policy│    │  Sticky      │    │  Weighted    │
│   _store    │    │  Routing     │    │  Routing     │
└──────────────┘    └──────────────┘    └──────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│              router_core:route/2 (continued)                    │
│    ├─ Extract provider_id, explanation from result              │
│    ├─ Log explanation via router_audit:log_decision/1          │
│    ├─ Convert to #route_decision{} record                       │
│    └─ Return: {ok, #route_decision{}}                           │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│              router_audit:log_decision/1                        │
│    ├─ Filter PII via router_logger:filter_pii/1                │
│    ├─ Log via router_logger:info/2 (structured JSON)            │
│    └─ Store audit trail via log_policy_action/5                 │
└─────────────────────────────────────────────────────────────────┘
```

### Integration Points

1. **Entry Point**: `router_core:route/2`
   - Extracts tenant_id, policy_id from RouteRequest
   - Calls `router_policy_applier:apply_policy/4`

2. **Policy Loading**: `router_policy:load_policy/2`
   - Loads policy from `router_policy_store`
   - Returns `#policy{}` record

3. **Decision Making**: `router_decider:decide/3`
   - Applies sticky routing (if enabled)
   - Applies weighted distribution
   - Applies fallback rules
   - Returns `#route_decision{}` record

4. **Explanation Building**: `router_policy_applier:build_explanation/3`
   - Builds step-by-step explanation
   - Includes context and metadata

5. **Audit Logging**: `router_audit:log_decision/1`
   - Filters PII from context
   - Logs structured JSON via `router_logger:info/2`
   - Stores audit trail in ETS

### Integration Code Flow

```erlang
%% router_core:route/2
route(RouteRequest, Context) ->
    #route_request{message = Message, policy_id = PolicyId, context = ReqContext} = RouteRequest,
    TenantId = maps:get(<<"tenant_id">>, Message),
    
    SafeContext = case Context of
        undefined -> #{};
        C when is_map(C) -> C;
        _ -> #{}
    end,
    
    %% Apply policy via router_policy_applier
    case router_policy_applier:apply_policy(RouteRequest, TenantId, FinalPolicyId, SafeContext) of
        {ok, ResultMap} ->
            ProviderId = maps:get(provider_id, ResultMap),
            Explanation = maps:get(explanation, ResultMap),
            Reason = maps:get(reason, Explanation),
            Priority = maps:get(priority, Explanation, 50),
            
            %% Log explanation for audit
            router_audit:log_decision(Explanation),
            
            Decision = #route_decision{
                provider_id = ProviderId,
                reason = Reason,
                priority = Priority,
                metadata = maps:get(context, Explanation, #{})
            },
            {ok, Decision};
        {error, ErrorInfo} ->
            ErrorInfo
    end.
```

**Reference**: `docs/archive/dev/POLICY_APPLIER_INTEGRATION.md` - detailed integration report

