# Phase 1: Extension Registry Implementation

⚠️ **LEGACY**: Early planning document. See `docs/archive/dev/EXTENSION_REGISTRY_IMPLEMENTATION_REPORT.md` for current implementation.

**Status**: ⚠️ **LEGACY** (Early Planning)  
**Duration**: 3 days  
**Prerequisites**: PostgreSQL operational, Router code base  
**Current Source of Truth**: `docs/archive/dev/EXTENSION_REGISTRY_IMPLEMENTATION_REPORT.md`

---

## Day 1: PostgreSQL Schema

### SQL Migration File

**File**: `sql/011_extensions_registry.sql`

```sql
-- Extensions Registry Table
CREATE TABLE IF NOT EXISTS extensions (
    id VARCHAR(255) PRIMARY KEY,
    type VARCHAR(50) NOT NULL CHECK (type IN ('pre', 'validator', 'post', 'provider')),
    subject VARCHAR(255) NOT NULL UNIQUE,
    timeout_ms INTEGER NOT NULL DEFAULT 5000,
    retry INTEGER NOT NULL DEFAULT 1,
    enabled BOOLEAN NOT NULL DEFAULT true,
    config JSONB DEFAULT '{}',
    metadata JSONB DEFAULT '{}',
    created_at TIMESTAMP DEFAULT NOW(),
    updated_at TIMESTAMP DEFAULT NOW()
);

CREATE INDEX idx_extensions_type ON extensions(type) WHERE enabled = true;
CREATE INDEX idx_extensions_subject ON extensions(subject);

-- Audit Log
CREATE TABLE IF NOT EXISTS extension_audit_log (
    id SERIAL PRIMARY KEY,
    extension_id VARCHAR(255) NOT NULL REFERENCES extensions(id) ON DELETE CASCADE,
    action VARCHAR(50) NOT NULL CHECK (action IN ('created', 'updated', 'deleted', 'enabled', 'disabled')),
    changed_by VARCHAR(255),
    changes JSONB,
    timestamp TIMESTAMP DEFAULT NOW()
);

CREATE INDEX idx_audit_extension_id ON extension_audit_log(extension_id);
CREATE INDEX idx_audit_timestamp ON extension_audit_log(timestamp DESC);

-- Health Metrics
CREATE TABLE IF NOT EXISTS extension_health (
    extension_id VARCHAR(255) PRIMARY KEY REFERENCES extensions(id) ON DELETE CASCADE,
    last_success TIMESTAMP,
    last_failure TIMESTAMP,
    failure_count INTEGER DEFAULT 0,
    success_count INTEGER DEFAULT 0,
    avg_latency_ms FLOAT DEFAULT 0,
    updated_at TIMESTAMP DEFAULT NOW()
);

-- Seed Data
INSERT INTO extensions (id, type, subject, timeout_ms, retry, config) VALUES
    ('mock_provider', 'provider', 'beamline.provider.mock.v1', 1000, 0, 
     '{"mode": "echo", "latency_ms": 50}'::jsonb),
    ('openai_gpt4', 'provider', 'beamline.provider.openai_gpt4.v1', 30000, 2,
     '{"model": "gpt-4", "max_tokens": 2048}'::jsonb),
    ('anthropic_claude', 'provider', 'beamline.provider.anthropic_claude.v1', 30000, 2,
     '{"model": "claude-3-opus-20240229", "max_tokens": 4096}'::jsonb),
    ('normalize_text', 'pre', 'beamline.ext.pre.normalize_text.v1', 100, 0,
     '{"lowercase": true, "trim_whitespace": true}'::jsonb),
    ('pii_guard', 'validator', 'beamline.ext.validate.pii_guard.v1', 200, 1,
     '{"patterns": ["credit_card", "ssn", "email"], "block_on_match": true}'::jsonb),
    ('mask_sensitive_data', 'post', 'beamline.ext.post.mask_pii.v1', 100, 0,
     '{"mask_patterns": ["email", "phone"]}'::jsonb)
ON CONFLICT (id) DO NOTHING;

COMMENT ON TABLE extensions IS 'Central registry of all extensions';
COMMENT ON COLUMN extensions.subject IS 'NATS subject for this extension';
COMMENT ON COLUMN extensions.config IS 'Default configuration (can be overridden in policy)';
```

### Tasks:
- [ ] Create `sql/011_extensions_registry.sql`
- [ ] Test migration: `psql -U postgres -d beamline < sql/011_extensions_registry.sql`
- [ ] Verify seed data: `SELECT id, type, subject FROM extensions;`
- [ ] Add to CI migration scripts
- [ ] Document schema in `docs/ARCHITECTURE/extensions-registry-schema.md`

---

## Day 2: Mnesia Cache Layer

### Module: router_extension_registry

**File**: `apps/otp/router/src/router_extension_registry.erl`

```erlang
-module(router_extension_registry).
-behaviour(gen_server).

%% API
-export([start_link/0, lookup/1, lookup_by_type/1, reload/0, get_stats/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include("beamline_router.hrl").

-record(extension, {
    id :: binary(),
    type :: binary(),
    subject :: binary(),
    timeout_ms :: integer(),
    retry :: integer(),
    enabled :: boolean(),
    config :: map(),
    metadata :: map()
}).

-record(state, {
    last_reload :: integer(),
    load_count :: integer(),
    error_count :: integer()
}).

-define(TABLE, router_extensions).
-define(RELOAD_INTERVAL, 60000). % 1 minute

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec lookup(binary()) -> {ok, #extension{}} | {error, not_found}.
lookup(Id) ->
    case mnesia:dirty_read(?TABLE, Id) of
        [Extension] -> {ok, Extension};
        [] -> {error, not_found}
    end.

-spec lookup_by_type(binary()) -> {ok, [#extension{}]}.
lookup_by_type(Type) ->
    Pattern = #extension{type = Type, enabled = true, _ = '_'},
    Extensions = mnesia:dirty_match_object(Pattern),
    {ok, Extensions}.

-spec reload() -> ok | {error, term()}.
reload() ->
    gen_server:call(?MODULE, reload, 10000).

-spec get_stats() -> map().
get_stats() ->
    gen_server:call(?MODULE, get_stats).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Create Mnesia table if not exists
    case mnesia:create_table(?TABLE, [
        {attributes, record_info(fields, extension)},
        {record_name, extension},
        {type, set},
        {ram_copies, [node()]},
        {index, [type, subject]}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, ?TABLE}} -> ok;
        {aborted, Reason} ->
            logger:error("Failed to create extensions table: ~p", [Reason]),
            {stop, Reason}
    end,
    
    %% Initial load
    case load_from_postgres() of
        ok ->
            %% Schedule periodic reload
            erlang:send_after(?RELOAD_INTERVAL, self(), reload),
            {ok, #state{
                last_reload = erlang:system_time(millisecond),
                load_count = 1,
                error_count = 0
            }};
        {error, Reason} ->
            logger:error("Initial load failed: ~p", [Reason]),
            {stop, Reason}
    end.

handle_call(reload, _From, State) ->
    Result = load_from_postgres(),
    NewState = case Result of
        ok -> 
            State#state{
                last_reload = erlang:system_time(millisecond),
                load_count = State#state.load_count + 1
            };
        {error, _} ->
            State#state{error_count = State#state.error_count + 1}
    end,
    {reply, Result, NewState};

handle_call(get_stats, _From, State) ->
    Stats = #{
        last_reload => State#state.last_reload,
        load_count => State#state.load_count,
        error_count => State#state.error_count,
        total_extensions => mnesia:table_info(?TABLE, size),
        enabled_extensions => count_enabled()
    },
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(reload, State) ->
    load_from_postgres(),
    erlang:send_after(?RELOAD_INTERVAL, self(), reload),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

load_from_postgres() ->
    SQL = "SELECT id, type, subject, timeout_ms, retry, enabled, config, metadata 
           FROM extensions WHERE enabled = true ORDER BY type, id",
    
    case router_postgres:query(SQL, []) of
        {ok, _Columns, Rows} ->
            %% Clear cache
            mnesia:clear_table(?TABLE),
            
            %% Insert extensions
            InsertFun = fun(Row) ->
                Extension = row_to_extension(Row),
                mnesia:dirty_write(?TABLE, Extension)
            end,
            lists:foreach(InsertFun, Rows),
            
            logger:info("Extension registry reloaded: ~p extensions", [length(Rows)]),
            ok;
            
        {error, Reason} ->
            logger:error("Failed to load extensions from PostgreSQL: ~p", [Reason]),
            {error, Reason}
    end.

row_to_extension([Id, Type, Subject, TimeoutMs, Retry, Enabled, ConfigJson, MetadataJson]) ->
    #extension{
        id = Id,
        type = Type,
        subject = Subject,
        timeout_ms = TimeoutMs,
        retry = Retry,
        enabled = Enabled,
        config = decode_json(ConfigJson),
        metadata = decode_json(MetadataJson)
    }.

decode_json(null) -> #{};
decode_json(Json) when is_binary(Json) ->
    jsx:decode(Json, [return_maps]);
decode_json(Map) when is_map(Map) ->
    Map.

count_enabled() ->
    Pattern = #extension{enabled = true, _ = '_'},
    length(mnesia:dirty_match_object(Pattern)).
```

### Tasks:
- [ ] Create `router_extension_registry.erl`
- [ ] Add to `beamline_router_sup.erl` child specs
- [ ] Create `router_postgres` helper (if not exists)
- [ ] Unit tests: cache operations, reload logic
- [ ] Integration test: PostgreSQL sync
- [ ] Performance test: lookup latency < 1ms

---

## Day 3: Admin API

### Module: router_admin_extensions

**File**: `apps/otp/router/src/router_admin_extensions.erl`

```erlang
-module(router_admin_extensions).

-export([
    list/0, list/1,
    get/1,
    create/1,
    update/2,
    delete/1,
    enable/1,
    disable/1,
    health/1
]).

%% List all or filter by type
list() ->
    SQL = "SELECT id, type, subject, timeout_ms, retry, enabled, created_at 
           FROM extensions ORDER BY type, id",
    query_list(SQL, []).

list(Type) ->
    SQL = "SELECT id, type, subject, timeout_ms, retry, enabled, created_at 
           FROM extensions WHERE type = $1 ORDER BY id",
    query_list(SQL, [Type]).

query_list(SQL, Params) ->
    case router_postgres:query(SQL, Params) of
        {ok, _Cols, Rows} -> 
            {ok, [format_extension_row(R) || R <- Rows]};
        {error, Reason} -> 
            {error, Reason}
    end.

%% Get single extension with full details
get(Id) ->
    SQL = "SELECT id, type, subject, timeout_ms, retry, enabled, config, metadata 
           FROM extensions WHERE id = $1",
    case router_postgres:query(SQL, [Id]) of
        {ok, _Cols, [Row]} -> {ok, format_extension_full(Row)};
        {ok, _Cols, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% Create new extension
create(#{
    <<"id">> := Id,
    <<"type">> := Type,
    <<"subject">> := Subject
} = Params) ->
    TimeoutMs = maps:get(<<"timeout_ms">>, Params, 5000),
    Retry = maps:get(<<"retry">>, Params, 1),
    Config = maps:get(<<"config">>, Params, #{}),
    
    %% Validate type
    case lists:member(Type, [<<"pre">>, <<"validator">>, <<"post">>, <<"provider">>]) of
        false -> {error, {invalid_type, Type}};
        true ->
            SQL = "INSERT INTO extensions (id, type, subject, timeout_ms, retry, config) 
                   VALUES ($1, $2, $3, $4, $5, $6) RETURNING id",
            
            case router_postgres:query(SQL, [Id, Type, Subject, TimeoutMs, Retry, jsx:encode(Config)]) of
                {ok, _Cols, [{ReturnedId}]} ->
                    %% Audit log
                    audit_log(Id, <<"created">>, <<"system">>, Params),
                    %% Reload cache
                    router_extension_registry:reload(),
                    {ok, ReturnedId};
                {error, {error, error, _Code, unique_violation, _Msg, _}} ->
                    {error, already_exists};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% Update extension
update(Id, Params) ->
    {Fields, Values, N} = build_update_query(Params, 1, [], []),
    
    SQL = io_lib:format("UPDATE extensions SET ~s, updated_at = NOW() WHERE id = $~p",
                        [string:join(Fields, ", "), N]),
    
    case router_postgres:query(lists:flatten(SQL), Values ++ [Id]) of
        {ok, 1} ->
            audit_log(Id, <<"updated">>, <<"system">>, Params),
            router_extension_registry:reload(),
            ok;
        {ok, 0} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% Delete extension
delete(Id) ->
    SQL = "DELETE FROM extensions WHERE id = $1",
    case router_postgres:query(SQL, [Id]) of
        {ok, 1} ->
            audit_log(Id, <<"deleted">>, <<"system">>, #{}),
            router_extension_registry:reload(),
            ok;
        {ok, 0} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% Enable/Disable
enable(Id) ->
    set_enabled(Id, true).

disable(Id) ->
    set_enabled(Id, false).

set_enabled(Id, Enabled) ->
    SQL = "UPDATE extensions SET enabled = $1, updated_at = NOW() WHERE id = $2",
    Action = case Enabled of true -> <<"enabled">>; false -> <<"disabled">> end,
    
    case router_postgres:query(SQL, [Enabled, Id]) of
        {ok, 1} ->
            audit_log(Id, Action, <<"system">>, #{<<"enabled">> => Enabled}),
            router_extension_registry:reload(),
            ok;
        {ok, 0} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% Health status
health(Id) ->
    SQL = "SELECT extension_id, last_success, last_failure, failure_count, success_count, avg_latency_ms 
           FROM extension_health WHERE extension_id = $1",
    case router_postgres:query(SQL, [Id]) of
        {ok, _Cols, [Row]} -> {ok, format_health(Row)};
        {ok, _Cols, []} -> {ok, #{<<"status">> => <<"no_data">>}};
        {error, Reason} -> {error, Reason}
    end.

%%====================================================================
%% Internal
%%====================================================================

build_update_query(Params, N, Fields, Values) ->
    maps:fold(fun
        (<<"id">>, _, {F, V, I}) -> {F, V, I};
        (<<"type">>, _, {F, V, I}) -> {F, V, I};
        (Key, Value, {F, V, I}) ->
            Field = io_lib:format("~s = $~p", [Key, I]),
            {[Field | F], [Value | V], I + 1}
    end, {Fields, Values, N}, Params).

audit_log(ExtensionId, Action, ChangedBy, Changes) ->
    SQL = "INSERT INTO extension_audit_log (extension_id, action, changed_by, changes) 
           VALUES ($1, $2, $3, $4)",
    router_postgres:query(SQL, [ExtensionId, Action, ChangedBy, jsx:encode(Changes)]).

format_extension_row([Id, Type, Subject, TimeoutMs, Retry, Enabled, CreatedAt]) ->
    #{
        <<"id">> => Id,
        <<"type">> => Type,
        <<"subject">> => Subject,
        <<"timeout_ms">> => TimeoutMs,
        <<"retry">> => Retry,
        <<"enabled">> => Enabled,
        <<"created_at">> => CreatedAt
    }.

format_extension_full([Id, Type, Subject, TimeoutMs, Retry, Enabled, Config, Metadata]) ->
    #{
        <<"id">> => Id,
        <<"type">> => Type,
        <<"subject">> => Subject,
        <<"timeout_ms">> => TimeoutMs,
        <<"retry">> => Retry,
        <<"enabled">> => Enabled,
        <<"config">> => jsx:decode(Config, [return_maps]),
        <<"metadata">> => jsx:decode(Metadata, [return_maps])
    }.

format_health([ExtId, LastSuccess, LastFailure, FailCount, SuccessCount, AvgLatency]) ->
    #{
        <<"extension_id">> => ExtId,
        <<"last_success">> => LastSuccess,
        <<"last_failure">> => LastFailure,
        <<"failure_count">> => FailCount,
        <<"success_count">> => SuccessCount,
        <<"avg_latency_ms">> => AvgLatency,
        <<"health_score">> => calculate_health_score(SuccessCount, FailCount)
    }.

calculate_health_score(Success, Failure) when Success + Failure > 0 ->
    (Success / (Success + Failure)) * 100;
calculate_health_score(_, _) ->
    100.0.
```

### Tasks:
- [ ] Create `router_admin_extensions.erl`
- [ ] Add gRPC endpoints (or REST)
- [ ] RBAC authorization checks
- [ ] Unit tests for CRUD operations
- [ ] API documentation (OpenAPI/gRPC proto)
- [ ] Postman collection / curl examples

---

## Testing Checklist

### Unit Tests
- [ ] Extension record creation
- [ ] Mnesia cache lookup (<1ms)
- [ ] Cache reload logic
- [ ] Admin CRUD operations

### Integration Tests
- [ ] PostgreSQL → Mnesia sync
- [ ] Create extension via API → appears in cache
- [ ] Disable extension → removed from cache
- [ ] Periodic reload (wait 61s, verify reload)

### Load Tests
- [ ] 10,000 lookups/sec sustained
- [ ] Cache hit rate > 99%
- [ ] Reload doesn't block lookups

---

## Deliverables Checklist

- [ ] SQL migration file (`sql/011_extensions_registry.sql`)
- [ ] Schema documentation (`docs/ARCHITECTURE/extensions-registry-schema.md`)
- [ ] `router_extension_registry.erl` module
- [ ] `router_admin_extensions.erl` module
- [ ] Unit tests (>80% coverage)
- [ ] Integration tests
- [ ] API documentation
- [ ] Performance benchmarks report

---

**Next**: Proceed to Phase 2 (Router Integration)
