# Gen Server Lifecycle and Reset Pattern

**Version**: 1.0  
**Last Updated**: 2025-01-27  
**Purpose**: Define standard pattern for gen_servers with ETS tables that participate in tests

## Overview

This document defines the reset/lifecycle pattern for gen_servers with ETS tables. This pattern reduces the chance of repeating "ETS+CT+sup" problems by providing safe reset mechanisms without process termination.

## Pattern Requirements

### Module Structure

**For gen_servers with ETS tables that participate in tests:**

1. **Split `init/1` into `init/1` → `do_init/1`**:
   - `init/1` wraps `do_init/1` with error handling
   - All initialization logic goes in `do_init/1`
   - This prevents crashes in `init/1` from breaking supervisor

2. **Safe reset via `handle_call(reset_all, ...)`**:
   - Implement `handle_call(reset_all, _From, State)` callback
   - Clear ETS table using `ets:delete_all_objects/1` (NOT `ets:delete/1`)
   - Keep process and ETS table alive (don't kill process)
   - Return `{reply, ok, State}`

3. **Lifecycle helpers in `*_test_utils.erl`**:
   - Provide `reset_<module>/0` function
   - Use `gen_server:call/2` with `reset_all` message
   - Check `whereis/1` before calling (handle not-started case)
   - Handle `noproc` errors gracefully

## Example Implementation

### Gen Server Module

```erlang
-module(my_gen_server).

-behaviour(gen_server).

-export([start_link/0, reset_all/0]).
-export([init/1, handle_call/3, handle_cast/2]).

-define(TABLE, my_table).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

reset_all() ->
    gen_server:call(?MODULE, reset_all, 5000).

%% @doc Init wrapper with error handling
init(Args) ->
    process_flag(trap_exit, true),
    try
        do_init(Args)
    catch
        Class:Reason:Stack ->
            error_logger:error_msg(
                "my_gen_server init failed: ~p:~p~nStack: ~p~n",
                [Class, Reason, Stack]
            ),
            {stop, {init_failed, Class, Reason}}
    end.

%% @doc Internal initialization logic (safe, with error handling)
do_init(_Args) ->
    %% Ensure ETS table exists (safe creation/cleanup)
    Table = ensure_ets_table(?TABLE),
    
    State = #state{
        table = Table
    },
    
    {ok, State}.

%% @doc Safely ensure ETS table exists
ensure_ets_table(Name) when is_atom(Name) ->
    case ets:info(Name) of
        undefined ->
            %% Table doesn't exist - create it
            ets:new(Name, [
                set,
                named_table,
                public,
                {write_concurrency, true},
                {read_concurrency, true}
            ]);
        _Info ->
            %% Table already exists - delete and recreate (cleanup from previous run/test)
            ets:delete(Name),
            ets:new(Name, [
                set,
                named_table,
                public,
                {write_concurrency, true},
                {read_concurrency, true}
            ])
    end.

handle_call(reset_all, _From, State = #state{table = Table}) ->
    %% Safe reset: clear all states but keep process and ETS table alive
    %% This is called from test utilities, should not kill the process
    case ets:info(Table) of
        undefined ->
            %% Table lost - log warning but continue
            router_logger:warn(<<"my_gen_server reset_all: ETS table undefined">>, #{
                <<"event">> => <<"my_gen_server_reset_all">>
            }),
            {reply, ok, State};
        _ ->
            ets:delete_all_objects(Table),
            router_logger:info(<<"my_gen_server reset_all: table cleared">>, #{
                <<"event">> => <<"my_gen_server_reset_all">>
            }),
            {reply, ok, State}
    end;
```

### Test Utilities Module

```erlang
-module(my_test_utils).

-export([reset_my_gen_server/0]).

reset_my_gen_server() ->
    case whereis(my_gen_server) of
        undefined ->
            %% Not running, no need to reset
            ok;
        _Pid ->
            %% Process exists, call reset_all
            try
                my_gen_server:reset_all()
            catch
                exit:{noproc, _} ->
                    %% Process died between whereis and call
                    ok;
                _:Error ->
                    %% Other error - log but don't fail
                    error_logger:warning_msg("reset_my_gen_server failed: ~p", [Error]),
                    ok
            end
    end.
```

## Key Principles

### 1. Safe Reset (No Process Termination)

**DO**:
- Use `ets:delete_all_objects/1` to clear table contents
- Keep process and table alive
- Return `{reply, ok, State}` from `handle_call(reset_all, ...)`

**DON'T**:
- Use `ets:delete/1` (deletes table, breaks process)
- Call `exit/2` or `application:stop/1` in reset
- Kill the gen_server process

### 2. Error Handling in init/1

**DO**:
- Wrap `do_init/1` in `try-catch` in `init/1`
- Log errors with stack trace
- Return `{stop, Reason}` on initialization failure

**DON'T**:
- Put initialization logic directly in `init/1` without error handling
- Let crashes propagate to supervisor

### 3. Test Utility Pattern

**DO**:
- Check `whereis/1` before calling reset
- Handle `noproc` errors gracefully
- Return `ok` even if process not running (not an error in tests)

**DON'T**:
- Call reset without checking if process exists
- Fail test if process not started
- Use direct ETS access in test utilities

## Migration Checklist

When applying this pattern to existing gen_servers:

- [ ] Split `init/1` into `init/1` → `do_init/1` with error handling
- [ ] Implement `handle_call(reset_all, ...)` that clears ETS table
- [ ] Add `reset_<module>/0` function to `*_test_utils.erl`
- [ ] Update tests to use `reset_<module>/0` instead of direct ETS access
- [ ] Remove direct `ets:delete_all_objects` and `ets:delete` calls from tests
- [ ] Verify reset works correctly (table cleared, process alive)
- [ ] Document reset function in module documentation

## Examples in Codebase

### Working Examples

1. **`router_circuit_breaker.erl`**:
   - ✅ Implements `init/1` → `do_init/1` pattern
   - ✅ Implements `handle_call(reset_all, ...)` with safe table clearing
   - ✅ Uses `ets:delete_all_objects/1` (NOT `ets:delete/1`)

2. **`router_test_utils.erl`**:
   - ✅ Provides `reset_circuit_breaker/0` function
   - ✅ Checks `whereis/1` before calling
   - ✅ Handles `noproc` errors gracefully

### Modules That Should Adopt This Pattern

- `router_quota` (if it's a gen_server with ETS)
- `router_audit` (if it's a gen_server with ETS)
- `router_rbac` (already has `reset/0`, verify pattern)
- Any other gen_server with ETS tables used in tests

## Benefits

1. **Reduces ETS+CT+sup problems**:
   - Safe reset without process termination
   - No table recreation needed
   - No race conditions between test cleanup and process restart

2. **Improved test reliability**:
   - Tests can reset state independently
   - No need to stop/restart application
   - Faster test execution

3. **Better error handling**:
   - Initialization errors don't crash supervisor
   - Clear error messages with stack traces
   - Graceful handling of missing processes

## References

- `apps/otp/router/src/router_circuit_breaker.erl` - Reference implementation
- `apps/otp/router/test/router_test_utils.erl` - Test utility pattern
- `docs/OBSERVABILITY_CONVENTIONS.md` - Metrics access layer pattern

