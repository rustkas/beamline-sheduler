# Policy Applier Integration Report

## Purpose

Интеграция `router_policy_applier` в Router для обеспечения единого пути принятия решений о провайдере через единый модуль с единым форматом Explanation для логов/аудита.

## Status

✅ **COMPLETED** - Все пути принятия решения теперь идут через router_policy_applier

## Changes Summary

### 1. router_core.erl - Main Integration Point

**Before**:
```erlang
case router_policy:load_policy(TenantId, FinalPolicyId) of
    {ok, Policy} ->
        router_decider:decide(RouteRequest, Policy, SafeContext);
    ...
end
```

**After**:
```erlang
case router_policy_applier:apply_policy(RouteRequest, TenantId, FinalPolicyId, SafeContext) of
    {ok, Result} ->
        %% Extract provider and explanation
        ProviderId = maps:get(provider_id, Result),
        Explanation = maps:get(explanation, Result),
        
        %% Log explanation for audit
        router_audit:log_decision(Explanation),
        
        %% Convert to #route_decision{} for backward compatibility
        Decision = #route_decision{...},
        {ok, Decision};
    {error, ErrorInfo} ->
        ErrorInfo
end
```

**Benefits**:
- ✅ Единый путь через router_policy_applier
- ✅ Единый формат Explanation для всех решений
- ✅ Автоматическое логирование для audit
- ✅ Обратная совместимость через #route_decision{}

### 2. router_audit.erl - Decision Logging

**Added Function**:
```erlang
-spec log_decision(map()) -> ok.
log_decision(Explanation) ->
    %% Log routing decision explanation for audit
    %% Uses log_policy_action with "route" action
    ...
end
```

**Explanation Format**:
```erlang
#{
    reason => binary(),           % "sticky" | "weighted" | "fallback"
    provider_id => binary(),      % Selected provider
    policy_id => binary(),        % Policy ID used
    policy_version => binary(),   % Policy version
    priority => integer(),        % Decision priority
    steps => [binary()],         % Step-by-step explanation
    context => map()             % Context (tenant_id, trace_id, etc.)
}
```

**Audit Entry**:
- Action: `"route"`
- Resource Type: `"policy"`
- Details: includes provider_id, reason, steps, context

### 3. Integration Points

**All routing decisions now go through router_policy_applier**:

1. **router_core:route/2** ✅
   - Main entry point for routing
   - Uses router_policy_applier:apply_policy/4
   - Logs explanation via router_audit:log_decision/1

2. **router_decide_consumer.erl** ✅
   - Uses router_core:route/2
   - Indirectly uses router_policy_applier

3. **router_nats_subscriber.erl** ✅
   - Uses router_core:route/2
   - Indirectly uses router_policy_applier

4. **router_grpc.erl** ✅
   - Uses router_core:route/2
   - Indirectly uses router_policy_applier

5. **router_demo.erl** ✅
   - Uses router_core:route/2
   - Indirectly uses router_policy_applier

6. **router_admin_grpc.erl** ✅
   - Does NOT make routing decisions
   - Only manages policies (upsert, delete, get, list)
   - No changes needed

### 4. Backward Compatibility

**Maintained**:
- ✅ router_core:route/2 still returns {ok, #route_decision{}} | {error, ...}
- ✅ All callers continue to work without changes
- ✅ #route_decision{} structure unchanged

**Conversion**:
```erlang
%% router_policy_applier returns:
{ok, #{
    provider_id => ProviderId,
    extensions => Extensions,
    explanation => Explanation
}}

%% router_core converts to:
{ok, #route_decision{
    provider_id = ProviderId,
    reason = maps:get(reason, Explanation),
    priority = maps:get(priority, Explanation),
    metadata = maps:get(context, Explanation)
}}
```

## Decision Flow

### Before Integration

```
Request → router_core:route/2
  → router_policy:load_policy/2
  → router_decider:decide/3
  → #route_decision{}
```

### After Integration

```
Request → router_core:route/2
  → router_policy_applier:apply_policy/4
    → router_policy:load_policy/2
    → router_decider:decide/3
    → Build explanation
    → Extract extensions
  → router_audit:log_decision/1
  → Convert to #route_decision{}
```

## Explanation Format Standardization

### Unified Format

All routing decisions now use the same explanation format:

```erlang
#{
    reason => <<"sticky">> | <<"weighted">> | <<"fallback">>,
    provider_id => <<"openai">>,
    policy_id => <<"default">>,
    policy_version => <<"1.0">>,
    priority => 50 | 100 | 25,
    steps => [
        <<"1. Checked sticky session: no existing session found">>,
        <<"2. Applied weighted distribution: 2 providers, total weight: 1.00">>,
        <<"3. Skipped fallbacks (provider selected via weighted)">>
    ],
    context => #{
        <<"tenant_id">> => <<"tenant_123">>,
        <<"trace_id">> => <<"trace_456">>,
        <<"message_id">> => <<"msg_789">>
    }
}
```

### Audit Trail

All explanations are logged via `router_audit:log_decision/1`:
- Action: `"route"`
- Resource Type: `"policy"`
- Includes full explanation with steps and context
- Timestamped and traceable

## Files Modified

1. `apps/otp/router/src/router_core.erl`
   - Replaced router_decider:decide with router_policy_applier:apply_policy
   - Added explanation logging via router_audit:log_decision
   - Maintained backward compatibility with #route_decision{}

2. `apps/otp/router/src/router_audit.erl`
   - Added log_decision/1 function
   - Logs routing decision explanations for audit trail

## Testing Recommendations

### Unit Tests

1. **router_core Integration**:
   - Test that router_core uses router_policy_applier
   - Test explanation logging
   - Test backward compatibility (#route_decision{})

2. **Explanation Format**:
   - Test explanation structure
   - Test steps generation
   - Test context inclusion

3. **Audit Logging**:
   - Test log_decision/1 function
   - Test audit entry format
   - Test error handling

### Integration Tests

1. **End-to-End Flow**:
   - Request → router_core → router_policy_applier → decision → audit
   - Verify explanation in audit logs
   - Verify backward compatibility

2. **All Entry Points**:
   - router_decide_consumer
   - router_nats_subscriber
   - router_grpc
   - All use router_core:route/2 → router_policy_applier

## Verification Checklist

- ✅ router_core:route/2 uses router_policy_applier:apply_policy/4
- ✅ All routing decisions go through router_policy_applier
- ✅ Explanation logged via router_audit:log_decision/1
- ✅ Backward compatibility maintained (#route_decision{})
- ✅ All entry points (consumer, subscriber, grpc) use router_core:route/2
- ✅ Admin paths do NOT make routing decisions (correct behavior)
- ✅ Unified explanation format for all decisions

## Next Steps

1. **Testing**:
   - Add unit tests for router_core integration
   - Add integration tests for explanation logging
   - Verify audit trail completeness

2. **Documentation**:
   - Update API documentation with explanation format
   - Document audit trail structure
   - Add examples of explanation in different scenarios

3. **Monitoring**:
   - Add metrics for explanation generation
   - Monitor audit log performance
   - Track explanation format consistency

## References

- `apps/otp/router/src/router_core.erl` - main integration point
- `apps/otp/router/src/router_policy_applier.erl` - unified policy application
- `apps/otp/router/src/router_audit.erl` - audit logging
- `docs/archive/dev/POLICY_APPLIER_IMPLEMENTATION.md` - policy applier implementation

