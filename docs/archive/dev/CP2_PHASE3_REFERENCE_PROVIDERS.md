# Phase 3: Reference Provider Implementations

⚠️ **LEGACY**: Early planning document. See `docs/archive/dev/EXTENSIONS_E2E_IMPLEMENTATION_REPORT.md` for current implementation.

**Status**: ⚠️ **LEGACY** (Early Planning)  
**Duration**: 5 days  
**Prerequisites**: Phase 1 & 2 complete (Extension Registry + Router Integration)  
**Current Source of Truth**: `docs/archive/dev/EXTENSIONS_E2E_IMPLEMENTATION_REPORT.md`

---

## Provider 1: Mock Provider (Erlang) — Days 6-7

### Purpose
- **Testing**: Development and integration tests
- **Demo**: Showcase extensions without external dependencies
- **Template**: Reference implementation for Erlang providers

### Directory Structure

```
providers/mock-provider-erlang/
├── src/
│   ├── mock_provider.app.src
│   ├── mock_provider_app.erl
│   ├── mock_provider_sup.erl
│   └── mock_provider_server.erl
├── test/
│   └── mock_provider_SUITE.erl
├── Dockerfile
├── rebar.config
├── rebar.lock
└── README.md
```

### Implementation

**File**: `providers/mock-provider-erlang/src/mock_provider_server.erl`

```erlang
-module(mock_provider_server).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_info/2, terminate/2]).

-record(state, {
    connection :: pid(),
    stats :: map()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    %% NATS configuration from environment
    NatsHost = os:getenv("NATS_HOST", "localhost"),
    NatsPort = list_to_integer(os:getenv("NATS_PORT", "4222")),
    Subject = os:getenv("NATS_SUBJECT", "beamline.provider.mock.v1"),
    
    %% Connect to NATS
    {ok, Connection} = nats:connect(#{
        host => NatsHost,
        port => NatsPort
    }),
    
    %% Subscribe to provider subject
    {ok, _Sub} = nats:subscribe(Connection, list_to_binary(Subject), [
        {msg_callback, fun handle_nats_msg/2}
    ]),
    
    logger:info("Mock provider started: ~s:~p, subject: ~s", 
                [NatsHost, NatsPort, Subject]),
    
    {ok, #state{
        connection = Connection,
        stats = #{requests => 0, successes => 0, errors => 0}
    }}.

handle_info({nats_msg, _Subject, ReplyTo, Payload}, State) ->
    NewState = handle_request(ReplyTo, Payload, State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{connection = Connection}) ->
    nats:close(Connection),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

handle_request(ReplyTo, Payload, State) ->
    StartTime = erlang:monotonic_time(millisecond),
    
    try
        Request = jsx:decode(Payload, [return_maps]),
        TraceId = maps:get(<<"trace_id">>, Request, <<"unknown">>),
        
        logger:info("Processing request, trace_id: ~p", [TraceId]),
        
        %% Simulate processing delay
        LatencyMs = maps:get(<<"latency_ms">>, Request, 50),
        timer:sleep(LatencyMs),
        
        %% Build mock response
        Response = build_mock_response(Request),
        ResponsePayload = jsx:encode(Response),
        
        %% Send response
        nats:publish(State#state.connection, ReplyTo, ResponsePayload),
        
        Duration = erlang:monotonic_time(millisecond) - StartTime,
        logger:info("Request completed, trace_id: ~p, duration: ~pms", [TraceId, Duration]),
        
        %% Update stats
        Stats = State#state.stats,
        State#state{
            stats = Stats#{
                requests => maps:get(requests, Stats) + 1,
                successes => maps:get(successes, Stats) + 1
            }
        }
    catch
        Class:Reason:Stacktrace ->
            logger:error("Request failed: ~p:~p~n~p", [Class, Reason, Stacktrace]),
            
            %% Send error response
            ErrorResponse = #{
                <<"error">> => #{
                    <<"code">> => <<"internal_error">>,
                    <<"message">> => iolist_to_binary(io_lib:format("~p", [Reason]))
                }
            },
            nats:publish(State#state.connection, ReplyTo, jsx:encode(ErrorResponse)),
            
            %% Update stats
            Stats = State#state.stats,
            State#state{
                stats = Stats#{
                    requests => maps:get(requests, Stats) + 1,
                    errors => maps:get(errors, Stats) + 1
                }
            }
    end.

build_mock_response(Request) ->
    Prompt = maps:get(<<"prompt">>, Request, <<"">>),
    ProviderId = maps:get(<<"provider_id">>, Request, <<"mock">>),
    Parameters = maps:get(<<"parameters">>, Request, #{}),
    
    %% Mock output
    Output = iolist_to_binary([
        <<"Mock response to: ">>,
        binary:part(Prompt, 0, min(byte_size(Prompt), 100)),
        <<" [provider=">>, ProviderId, <<"]">>
    ]),
    
    %% Mock usage
    PromptTokens = byte_size(Prompt) div 4,
    CompletionTokens = byte_size(Output) div 4,
    
    #{
        <<"provider_id">> => ProviderId,
        <<"output">> => Output,
        <<"usage">> => #{
            <<"prompt_tokens">> => PromptTokens,
            <<"completion_tokens">> => CompletionTokens,
            <<"total_tokens">> => PromptTokens + CompletionTokens
        },
        <<"metadata">> => #{
            <<"model">> => <<"mock-1.0">>,
            <<"finish_reason">> => <<"stop">>,
            <<"latency_ms">> => maps:get(<<"latency_ms">>, Parameters, 50)
        }
    }.
```

### Dockerfile

```dockerfile
FROM erlang:26-alpine

WORKDIR /app

# Copy rebar3 and source
COPY rebar.config rebar.lock ./
COPY src ./src

# Build release
RUN rebar3 release

# Runtime
FROM erlang:26-alpine
COPY --from=0 /app/_build/default/rel/mock_provider /app
WORKDIR /app

CMD ["bin/mock_provider", "foreground"]
```

### Tasks:
- [ ] Create Erlang project structure
- [ ] Implement mock_provider_server.erl
- [ ] Add to docker-compose.yml
- [ ] Integration tests with Router
- [ ] Load test: 1000 req/sec
- [ ] Documentation (README.md)

---

## Provider 2: OpenAI Provider (Go) — Days 8-9

### Purpose
- **Production-ready**: Real LLM integration
- **Performance**: Go concurrency model
- **Template**: Reference for Go providers

### Directory Structure

```
providers/openai-provider-go/
├── cmd/
│   └── server/
│       └── main.go
├── internal/
│   ├── handler/
│   │   └── provider_handler.go
│   ├── client/
│   │   └── openai_client.go
│   └── metrics/
│       └── prometheus.go
├── Dockerfile
├── go.mod
├── go.sum
├── k8s/
│   ├── deployment.yaml
│   ├── service.yaml
│   └── secret.yaml
└── README.md
```

### Implementation

**File**: `providers/openai-provider-go/cmd/server/main.go`

```go
package main

import (
    "context"
    "encoding/json"
    "log"
    "os"
    "os/signal"
    "syscall"
    "time"

    "github.com/nats-io/nats.go"
    openai "github.com/sashabaranov/go-openai"
    "github.com/prometheus/client_golang/prometheus"
    "github.com/prometheus/client_golang/prometheus/promhttp"
    "net/http"
)

var (
    requestsTotal = prometheus.NewCounterVec(
        prometheus.CounterOpts{
            Name: "openai_provider_requests_total",
            Help: "Total number of requests",
        },
        []string{"status"},
    )
    requestDuration = prometheus.NewHistogramVec(
        prometheus.HistogramOpts{
            Name: "openai_provider_request_duration_seconds",
            Help: "Request duration",
            Buckets: prometheus.DefBuckets,
        },
        []string{},
    )
    tokensUsed = prometheus.NewCounterVec(
        prometheus.CounterOpts{
            Name: "openai_provider_tokens_total",
            Help: "Total tokens used",
        },
        []string{"type"},
    )
)

func init() {
    prometheus.MustRegister(requestsTotal, requestDuration, tokensUsed)
}

func main() {
    // Configuration
    natsURL := getEnv("NATS_URL", nats.DefaultURL)
    subject := getEnv("NATS_SUBJECT", "beamline.provider.openai_gpt4.v1")
    apiKey := os.Getenv("OPENAI_API_KEY")
    if apiKey == "" {
        log.Fatal("OPENAI_API_KEY not set")
    }

    // NATS connection
    nc, err := nats.Connect(natsURL)
    if err != nil {
        log.Fatal(err)
    }
    defer nc.Close()

    // OpenAI client
    client := openai.NewClient(apiKey)

    // Subscribe to NATS
    log.Printf("OpenAI provider listening on: %s", subject)
    _, err = nc.Subscribe(subject, func(msg *nats.Msg) {
        handleRequest(client, msg)
    })
    if err != nil {
        log.Fatal(err)
    }

    // Prometheus metrics endpoint
    http.Handle("/metrics", promhttp.Handler())
    go http.ListenAndServe(":9090", nil)

    // Graceful shutdown
    sigChan := make(chan os.Signal, 1)
    signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)
    <-sigChan
    log.Println("Shutting down...")
}

func handleRequest(client *openai.Client, msg *nats.Msg) {
    start := time.Now()
    
    var req struct {
        TraceID    string                 `json:"trace_id"`
        TenantID   string                 `json:"tenant_id"`
        ProviderID string                 `json:"provider_id"`
        Prompt     string                 `json:"prompt"`
        Parameters map[string]interface{} `json:"parameters"`
    }

    if err := json.Unmarshal(msg.Data, &req); err != nil {
        log.Printf("Error decoding request: %v", err)
        sendError(msg, err)
        requestsTotal.WithLabelValues("decode_error").Inc()
        return
    }

    log.Printf("[%s] Processing request, tenant: %s", req.TraceID, req.TenantID)

    // OpenAI API call
    ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
    defer cancel()

    model := getParamString(req.Parameters, "model", "gpt-4")
    maxTokens := getParamInt(req.Parameters, "max_tokens", 2048)
    temperature := getParamFloat(req.Parameters, "temperature", 0.7)

    resp, err := client.CreateChatCompletion(ctx, openai.ChatCompletionRequest{
        Model:       model,
        Messages:    []openai.ChatCompletionMessage{{Role: "user", Content: req.Prompt}},
        MaxTokens:   maxTokens,
        Temperature: float32(temperature),
    })

    if err != nil {
        log.Printf("[%s] OpenAI error: %v", req.TraceID, err)
        sendError(msg, err)
        requestsTotal.WithLabelValues("api_error").Inc()
        return
    }

    // Build response
    response := map[string]interface{}{
        "provider_id": "openai_gpt4",
        "output":      resp.Choices[0].Message.Content,
        "usage": map[string]int{
            "prompt_tokens":     resp.Usage.PromptTokens,
            "completion_tokens": resp.Usage.CompletionTokens,
            "total_tokens":      resp.Usage.TotalTokens,
        },
        "metadata": map[string]interface{}{
            "model":         resp.Model,
            "finish_reason": resp.Choices[0].FinishReason,
            "trace_id":      req.TraceID,
        },
    }

    data, _ := json.Marshal(response)
    msg.Respond(data)

    // Metrics
    duration := time.Since(start).Seconds()
    requestDuration.WithLabelValues().Observe(duration)
    requestsTotal.WithLabelValues("success").Inc()
    tokensUsed.WithLabelValues("prompt").Add(float64(resp.Usage.PromptTokens))
    tokensUsed.WithLabelValues("completion").Add(float64(resp.Usage.CompletionTokens))

    log.Printf("[%s] Completed in %.2fs, tokens: %d", req.TraceID, duration, resp.Usage.TotalTokens)
}

func sendError(msg *nats.Msg, err error) {
    errorResp := map[string]interface{}{
        "error": map[string]interface{}{
            "code":    "provider_error",
            "message": err.Error(),
        },
    }
    data, _ := json.Marshal(errorResp)
    msg.Respond(data)
}

func getEnv(key, defaultVal string) string {
    if val := os.Getenv(key); val != "" {
        return val
    }
    return defaultVal
}

func getParamString(params map[string]interface{}, key, defaultVal string) string {
    if val, ok := params[key].(string); ok {
        return val
    }
    return defaultVal
}

func getParamInt(params map[string]interface{}, key string, defaultVal int) int {
    if val, ok := params[key].(float64); ok {
        return int(val)
    }
    return defaultVal
}

func getParamFloat(params map[string]interface{}, key string, defaultVal float64) float64 {
    if val, ok := params[key].(float64); ok {
        return val
    }
    return defaultVal
}
```

### Kubernetes Deployment

**File**: `providers/openai-provider-go/k8s/deployment.yaml`

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: openai-provider
  labels:
    app: openai-provider
spec:
  replicas: 3
  selector:
    matchLabels:
      app: openai-provider
  template:
    metadata:
      labels:
        app: openai-provider
    spec:
      containers:
      - name: openai-provider
        image: beamline/openai-provider:v1.0.0
        ports:
        - containerPort: 9090
          name: metrics
        env:
        - name: NATS_URL
          value: "nats://nats:4222"
        - name: NATS_SUBJECT
          value: "beamline.provider.openai_gpt4.v1"
        - name: OPENAI_API_KEY
          valueFrom:
            secretKeyRef:
              name: openai-secret
              key: api-key
        resources:
          requests:
            memory: "128Mi"
            cpu: "100m"
          limits:
            memory: "256Mi"
            cpu: "500m"
        livenessProbe:
          httpGet:
            path: /metrics
            port: 9090
          initialDelaySeconds: 30
          periodSeconds: 10
---
apiVersion: v1
kind: Service
metadata:
  name: openai-provider
spec:
  selector:
    app: openai-provider
  ports:
  - port: 9090
    name: metrics
```

### Tasks:
- [ ] Go project setup (go mod init)
- [ ] Implement provider server
- [ ] Add Prometheus metrics
- [ ] Dockerfile multi-stage build
- [ ] K8s manifests (deployment, service, secret)
- [ ] Integration tests with Router
- [ ] Load test: 500 req/sec (OpenAI rate limits)
- [ ] Documentation (README, API key setup)

---

## Provider 3: Anthropic Provider (Rust) — Day 10 (Optional)

### Purpose
- **Showcase multi-language**: Rust for safety/performance
- **Alternative LLM**: Claude integration
- **Optional**: Can defer to CP3+

### Quick Implementation Notes

```rust
// main.rs
use nats;
use serde_json::{json, Value};
use anthropic_sdk::{Client, Message};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let nc = nats::connect("nats://localhost:4222")?;
    let subject = "beamline.provider.anthropic_claude.v1";
    
    let sub = nc.subscribe(subject)?;
    println!("Anthropic provider listening on: {}", subject);
    
    for msg in sub.messages() {
        tokio::spawn(async move {
            handle_request(msg).await;
        });
    }
    
    Ok(())
}

async fn handle_request(msg: nats::Message) {
    // Parse request
    // Call Anthropic API
    // Send response
}
```

### Tasks (If Implementing):
- [ ] Rust project setup (cargo new)
- [ ] Anthropic SDK integration
- [ ] NATS client (async-nats)
- [ ] Dockerfile
- [ ] Basic tests

---

## Testing All Providers

### Integration Test Suite

**File**: `tests/integration/providers_test.erl`

```erlang
-module(providers_test).
-include_lib("eunit/include/eunit.hrl").

all_providers_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         fun test_mock_provider/0,
         fun test_openai_provider/0,
         fun test_provider_fallback/0,
         fun test_provider_timeout/0
     ]
    }.

setup() ->
    %% Start Router and extensions
    application:ensure_all_started(beamline_router),
    ok.

cleanup(_) ->
    application:stop(beamline_router).

test_mock_provider() ->
    Request = #{
        <<"tenant_id">> => <<"test-tenant">>,
        <<"provider_id">> => <<"mock">>,
        <<"prompt">> => <<"Hello, mock provider!">>
    },
    
    {ok, Response} = router_extension_invoker:invoke_provider(<<"mock">>, Request, #{}),
    
    ?assert(maps:is_key(<<"output">>, Response)),
    ?assert(maps:is_key(<<"usage">>, Response)),
    ?assertEqual(<<"mock">>, maps:get(<<" provider_id">>, Response)).

test_openai_provider() ->
    %% Requires OPENAI_API_KEY
    case os:getenv("OPENAI_API_KEY") of
        false -> {skip, "OPENAI_API_KEY not set"};
        _ ->
            Request = #{
                <<"tenant_id">> => <<"test-tenant">>,
                <<"provider_id">> => <<"openai_gpt4">>,
                <<"prompt">> => <<"Say 'Hello'">>
            },
            
            {ok, Response} = router_extension_invoker:invoke_provider(<<"openai_gpt4">>, Request, #{}),
            
            ?assert(maps:is_key(<<"output">>, Response)),
            ?assert(maps:is_key(<<"usage">>, Response))
    end.

test_provider_fallback() ->
    %% Primary provider fails → fallback to mock
    Policy = #{
        <<"steps">> => [
            #{
                <<"action">> => <<"select_provider">>,
                <<"provider_id">> => <<"invalid_provider">>,
                <<"fallback">> => [<<"mock">>]
            }
        ]
    },
    
    {ok, Response, _Acc} = router_decider:apply_extensions_pipeline(#{}, Policy, #{}),
    ?assertEqual(<<"mock">>, maps:get(<<"provider_id">>, Response)).

test_provider_timeout() ->
    %% Test timeout handling
    %% Mock provider with very high latency
    ok.
```

---

## Deliverables Checklist

### Mock Provider (Erlang)
- [ ] Source code (mock_provider_server.erl)
- [ ] Dockerfile
- [ ] docker-compose integration
- [ ] Integration tests
- [ ] README with usage examples

### OpenAI Provider (Go)
- [ ] Source code (main.go, handler, client)
- [ ] Prometheus metrics
- [ ] Dockerfile (multi-stage)
- [ ] K8s manifests (deployment, service, secret)
- [ ] Integration tests
- [ ] README with API key setup

### Optional: Anthropic Provider (Rust)
- [ ] Source code (main.rs)
- [ ] Dockerfile
- [ ] Basic tests
- [ ] README

### General
- [ ] Provider Development Guide (`docs/archive/dev/PROVIDER_DEVELOPMENT_GUIDE.md`)
- [ ] Example policies with multiple providers
- [ ] Load test results
- [ ] Deployment documentation

---

**Completion**: Phase 3 complete → Extensions fully operational! 🎉
