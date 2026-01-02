---
version: 1.0
order_id: ORDER-WRK-3-CP2-004
from: mgr-2 (Architecture Manager)
to: wrk-3 (Router Core)
created_at: 2025-01-27T15:00:00Z
status: pending
priority: MEDIUM
rule_version: v10
message_protocol: v1
---

# ORDER: HEIR Policy Store Integration

## Order Information

**ORDER ID**: ORDER-WRK-3-CP2-004  
**From**: mgr-2 (Architecture Manager)  
**To**: wrk-3 (Router Core)  
**Priority**: 🟡 **MEDIUM** - Enhances policy management  
**Timeline**: 3 days  
**Dependencies**: ORDER-WRK-3-CP2-001 (Compilation fix)  
**Blocks**: None

## Task Description

Интегрировать HEIR Policy Store в Router для централизованного управления политиками с локальным fallback, согласно CP2 спецификации.

**Цель**: Обеспечить централизованное хранение политик через HEIR Policy Store с graceful degradation на локальное хранилище при недоступности HEIR.

## Expected Artifacts

### Modified Files

**File**: `apps/otp/router/src/router_policy_store.erl`

```erlang
-module(router_policy_store).
-behaviour(gen_server).

%% HEIR Policy Store Integration
-export([init_heir_store/1, get_heir_policy/2, store_heir_policy/3]).

%% API
-export([start_link/1, get_policy/2, store_policy/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    heir_enabled :: boolean(),
    heir_connection :: pid() | undefined,
    local_cache :: ets:tid(),
    config :: map()
}).

%% @doc Initialize HEIR policy store connection
-spec init_heir_store(map()) -> {ok, pid() | undefined} | {error, term()}.
init_heir_store(Config) ->
    case application:get_env(beamline_router, heir_policy_store_enabled, false) of
        true ->
            Host = maps:get(host, Config, "localhost"),
            Port = maps:get(port, Config, 8080),
            Timeout = maps:get(timeout, Config, 5000),
            case heir_client:connect(Host, Port, Timeout) of
                {ok, Conn} ->
                    {ok, Conn};
                {error, Reason} ->
                    logger:warning("Failed to connect to HEIR: ~p, falling back to local store", [Reason]),
                    {ok, undefined}
            end;
        false ->
            {ok, undefined}
    end.

%% @doc Get policy from HEIR store with local fallback
-spec get_heir_policy(tenant_id(), policy_id()) -> {ok, policy()} | {error, term()}.
get_heir_policy(TenantId, PolicyId) ->
    case gen_server:call(?MODULE, {get_heir_policy, TenantId, PolicyId}) of
        {ok, Policy} -> 
            {ok, Policy};
        {error, not_found} -> 
            % Fallback to local store
            get_policy(TenantId, PolicyId);
        Error -> 
            Error
    end.

%% @doc Store policy in both HEIR and local store
-spec store_heir_policy(tenant_id(), policy_id(), policy()) -> ok | {error, term()}.
store_heir_policy(TenantId, PolicyId, Policy) ->
    gen_server:call(?MODULE, {store_heir_policy, TenantId, PolicyId, Policy}).

%% gen_server implementation

init([Config]) ->
    {ok, HeirConn} = init_heir_store(Config),
    LocalCache = ets:new(policy_cache, [set, private, {keypos, 1}]),
    State = #state{
        heir_enabled = HeirConn =/= undefined,
        heir_connection = HeirConn,
        local_cache = LocalCache,
        config = Config
    },
    {ok, State}.

handle_call({get_heir_policy, TenantId, PolicyId}, _From, State) ->
    case State#state.heir_connection of
        undefined ->
            % HEIR disabled, use local store only
            case ets:lookup(State#state.local_cache, {TenantId, PolicyId}) of
                [{_, Policy}] -> {reply, {ok, Policy}, State};
                [] -> {reply, {error, not_found}, State}
            end;
        Conn ->
            case heir_client:get_policy(Conn, TenantId, PolicyId) of
                {ok, Policy} ->
                    % Cache locally for future requests
                    ets:insert(State#state.local_cache, {{TenantId, PolicyId}, Policy}),
                    {reply, {ok, Policy}, State};
                {error, not_found} ->
                    % Try local cache as fallback
                    case ets:lookup(State#state.local_cache, {TenantId, PolicyId}) of
                        [{_, Policy}] -> {reply, {ok, Policy}, State};
                        [] -> {reply, {error, not_found}, State}
                    end;
                Error ->
                    % HEIR error, try local cache
                    case ets:lookup(State#state.local_cache, {TenantId, PolicyId}) of
                        [{_, Policy}] -> 
                            logger:warning("HEIR error, using local cache: ~p", [Error]),
                            {reply, {ok, Policy}, State};
                        [] -> {reply, Error, State}
                    end
            end
    end;

handle_call({store_heir_policy, TenantId, PolicyId, Policy}, _From, State) ->
    % Store in both HEIR and local cache
    case State#state.heir_connection of
        undefined ->
            % HEIR disabled, store only locally
            ets:insert(State#state.local_cache, {{TenantId, PolicyId}, Policy}),
            {reply, ok, State};
        Conn ->
            case heir_client:store_policy(Conn, TenantId, PolicyId, Policy) of
                ok ->
                    ets:insert(State#state.local_cache, {{TenantId, PolicyId}, Policy}),
                    {reply, ok, State};
                Error ->
                    % Store locally even if HEIR fails
                    ets:insert(State#state.local_cache, {{TenantId, PolicyId}, Policy}),
                    logger:warning("HEIR store failed, stored locally: ~p", [Error]),
                    {reply, ok, State}
            end
    end;

handle_call({get_policy, TenantId, PolicyId}, _From, State) ->
    % Original local-only get_policy implementation
    case ets:lookup(State#state.local_cache, {TenantId, PolicyId}) of
        [{_, Policy}] -> {reply, {ok, Policy}, State};
        [] -> {reply, {error, not_found}, State}
    end;

handle_call({store_policy, TenantId, PolicyId, Policy}, _From, State) ->
    % Original local-only store_policy implementation
    ets:insert(State#state.local_cache, {{TenantId, PolicyId}, Policy}),
    {reply, ok, State}.

terminate(_Reason, State) ->
    case State#state.heir_connection of
        undefined -> ok;
        Conn -> heir_client:disconnect(Conn)
    end,
    ets:delete(State#state.local_cache),
    ok.
```

### New Files

**File**: `apps/otp/router/src/heir_client.erl`

```erlang
-module(heir_client).
-export([connect/3, disconnect/1, get_policy/3, store_policy/4]).

%% HEIR Policy Store HTTP/REST Client
%% Implements REST API calls to HEIR policy store service

-include_lib("kernel/include/logger.hrl").

-define(DEFAULT_TIMEOUT, 5000).
-define(HEIR_API_VERSION, "v1").

%% @doc Connect to HEIR policy store
-spec connect(string(), integer(), integer()) -> {ok, pid()} | {error, term()}.
connect(Host, Port, Timeout) ->
    % Create HTTP client connection pool
    % For now, return a simple connection identifier
    % In production, this would use httpc or hackney
    BaseURL = lists:flatten(io_lib:format("http://~s:~B", [Host, Port])),
    Conn = {heir_conn, BaseURL, Timeout},
    % Test connection
    case health_check(Conn) of
        ok -> {ok, Conn};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Disconnect from HEIR policy store
-spec disconnect(term()) -> ok.
disconnect(_Conn) ->
    ok.

%% @doc Get policy from HEIR store
-spec get_policy(term(), tenant_id(), policy_id()) -> {ok, policy()} | {error, term()}.
get_policy(Conn, TenantId, PolicyId) ->
    {heir_conn, BaseURL, Timeout} = Conn,
    URL = lists:flatten(io_lib:format("~s/api/~s/policies/~s/~s", 
        [BaseURL, ?HEIR_API_VERSION, TenantId, PolicyId])),
    
    case http_get(URL, Timeout) of
        {ok, {200, Policy}} -> {ok, Policy};
        {ok, {404, _}} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Store policy in HEIR store
-spec store_policy(term(), tenant_id(), policy_id(), policy()) -> ok | {error, term()}.
store_policy(Conn, TenantId, PolicyId, Policy) ->
    {heir_conn, BaseURL, Timeout} = Conn,
    URL = lists:flatten(io_lib:format("~s/api/~s/policies/~s/~s", 
        [BaseURL, ?HEIR_API_VERSION, TenantId, PolicyId])),
    
    case http_put(URL, Policy, Timeout) of
        {ok, {200, _}} -> ok;
        {ok, {201, _}} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% Internal functions

health_check(Conn) ->
    {heir_conn, BaseURL, Timeout} = Conn,
    URL = lists:flatten(io_lib:format("~s/health", [BaseURL])),
    case http_get(URL, Timeout) of
        {ok, {200, _}} -> ok;
        Error -> Error
    end.

http_get(URL, Timeout) ->
    % Placeholder for HTTP GET implementation
    % In production, use httpc or hackney
    % For now, return mock response
    {ok, {200, #{}}}.

http_put(URL, Body, Timeout) ->
    % Placeholder for HTTP PUT implementation
    % In production, use httpc or hackney
    % For now, return mock response
    {ok, {200, #{}}}.
```

### Configuration Files

**File**: `apps/otp/router/config/sys.config`

```erlang
[
  {beamline_router, [
    {heir_policy_store_enabled, true},
    {heir_policy_store_host, "localhost"},
    {heir_policy_store_port, 8080},
    {heir_policy_store_timeout, 5000}
  ]}
].
```

## Context and Purpose

### Why This Is Important

1. **Centralized Policy Management**: Единое хранилище политик для всех Router instances
2. **Scalability**: HEIR может масштабироваться независимо от Router
3. **Consistency**: Гарантия консистентности политик между instances
4. **Graceful Degradation**: Локальный fallback при недоступности HEIR

### Current State

**Missing**: 
- ❌ HEIR Policy Store integration
- ❌ Centralized policy management

**Existing**:
- ✅ Local ETS-based policy store
- ✅ Policy CRUD operations
- ✅ Policy caching

### Target State

- ✅ HEIR Policy Store integration с локальным fallback
- ✅ Configurable HEIR connection (enabled/disabled)
- ✅ Local caching для performance
- ✅ Graceful degradation при недоступности HEIR

## Technical Requirements

### HEIR Integration

**Connection Management**:
- Configurable connection (enabled/disabled via feature flag)
- Connection pooling для performance
- Health checks для connection validation
- Automatic reconnection при разрыве связи

**Policy Operations**:
- `get_policy`: HEIR → local cache fallback
- `store_policy`: Dual-write (HEIR + local cache)
- `delete_policy`: Dual-delete (HEIR + local cache)
- `list_policies`: HEIR → local cache fallback

**Performance Requirements**:
- HEIR latency < 50ms (P95)
- Local cache hit rate > 85%
- Graceful degradation без service interruption

### Error Handling

**HEIR Unavailable**:
- Automatic fallback to local cache
- Warning logs для monitoring
- Service continues operating normally

**HEIR Errors**:
- Retry logic с exponential backoff
- Circuit breaker для предотвращения cascading failures
- Clear error messages для debugging

## Acceptance Criteria

### Functional Criteria

- ✅ HEIR policy store integration работает
- ✅ Local fallback функционирует корректно
- ✅ Configurable connection (enabled/disabled)
- ✅ Local caching для performance optimization
- ✅ Graceful degradation при недоступности HEIR

### Performance Criteria

- ✅ HEIR latency < 50ms (P95)
- ✅ Local cache hit rate > 85%
- ✅ No performance regression > 5% от baseline
- ✅ Connection pooling эффективен

### Quality Criteria

- ✅ Comprehensive test coverage (> 80%)
- ✅ Unit tests для HEIR client
- ✅ Integration tests для online/offline scenarios
- ✅ Code review пройден
- ✅ Documentation обновлена

## Dependencies

### Required From

- **ORDER-WRK-3-CP2-001** (wrk-3): Router compilation fix - **MUST COMPLETE FIRST**
- HEIR Policy Store service must be available (или mock для testing)

### External Dependencies

- HTTP client library (httpc или hackney)
- HEIR Policy Store API specification
- Configuration management для HEIR connection

## Risks and Mitigations

### Risk 1: HEIR Service Unavailable

**Risk**: HEIR Policy Store может быть недоступен в production.

**Mitigation**:
- Graceful degradation на local cache
- Health checks и monitoring
- Clear error messages и logging
- Circuit breaker для предотвращения cascading failures

### Risk 2: Performance Impact

**Risk**: Дополнительный network hop может замедлить policy operations.

**Mitigation**:
- Local caching для performance
- Connection pooling
- Async operations где возможно
- Benchmark testing и optimization

### Risk 3: Integration Complexity

**Risk**: 3-day estimate может быть оптимистичным.

**Mitigation**:
- Phased implementation (basic → advanced)
- Fallback на local-only mode
- Defer to CP3 если critical path threatened

## Reporting Requirements

### Progress Report (Day 1-2)

**Status**: `in_progress`

**Summary**:
- HEIR client implementation progress
- Integration с router_policy_store progress
- Любые блокеры

### Final Report (Day 3)

**Status**: `done`

**Summary**:
- HEIR integration завершена
- Все тесты пройдены
- Документация обновлена

**Artifacts**:
- Все созданные/обновленные файлы
- Результаты тестирования
- Performance benchmarks
- Обновленная документация

## References

- `docs/archive/dev/CP2_WORKER_ASSIGNMENTS_DETAILED.md` - Детальный план CP2 задач
- `docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY_UPDATED.md` - CP2 readiness document
- `apps/otp/router/src/router_policy_store.erl` - Existing policy store
- HEIR Policy Store API specification (если доступна)

---

**ORDER ID**: ORDER-WRK-3-CP2-004  
**Status**: Pending (blocked by ORDER-WRK-3-CP2-001)  
**Priority**: 🟡 MEDIUM  
**Timeline**: 3 days  
**Rule Version**: v10  
**Message Protocol**: v1

