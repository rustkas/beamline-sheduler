# Custom Provider Extensions Implementation Guide

⚠️ **LEGACY**: This document is superseded by `docs/EXTENSIONS_DEVELOPER_GUIDE.md`. See that document for current information.

**Status**: ⚠️ **LEGACY** (Superseded)  
**Last Updated**: 2025-11-22  
**Replaces**: `apps/otp/provider` (удалён в пользу Extensions API)  
**Current Source of Truth**: `docs/EXTENSIONS_DEVELOPER_GUIDE.md`

---

## Обзор

**Custom Provider Extensions** — это способ интеграции внешних LLM/API провайдеров (OpenAI, Anthropic, CRM, RAG) без изменения кода Router/Gateway/Worker.

### Ключевые принципы

1. **Отдельный NATS-сервис** для каждого провайдера
2. **Регистрация в Extension Registry** по уникальному `provider_id`
3. **Управление через Routing Policy** (без изменений кода)
4. **Любой язык**: Go, Rust, Erlang, Python, C, etc.

---

## Архитектура

```
┌──────────────┐
│   Gateway    │ HTTP клиенты
└──────┬───────┘
       │
       ↓ NATS: beamline.router.v1.decide
┌──────────────┐
│    Router    │ Extension Registry lookup
└──────┬───────┘
       │
       ├→ beamline.provider.openai.v1
       ├→ beamline.provider.anthropic.v1
       └→ beamline.provider.custom_crm.v1
          │
          ↓
   ┌─────────────────┐
   │ Custom Provider │ Отдельный NATS-сервис
   │   Extension     │ (Go/Rust/Erlang/Python)
   └─────────────────┘
```

---

## Пример: OpenAI Provider Extension

### 1. Extension Registry

Регистрация в PostgreSQL или config файле:

```json
{
  "openai_gpt4": {
    "type": "provider",
    "subject": "beamline.provider.openai_gpt4.v1",
    "timeout_ms": 30000,
    "retry": 2
  }
}
```

### 2. NATS Subject

Провайдер подписывается на:
```
beamline.provider.openai_gpt4.v1
```

### 3. Request Contract (JSON over NATS)

```json
{
  "trace_id": "uuid",
  "tenant_id": "tenant-123",
  "provider_id": "openai_gpt4",
  "prompt": "User message + context",
  "parameters": {
    "model": "gpt-4",
    "max_tokens": 512,
    "temperature": 0.7
  },
  "context": {
    "conversation_id": "conv-1"
  }
}
```

### 4. Response Contract

```json
{
  "provider_id": "openai_gpt4",
  "output": "AI generated response...",
  "usage": {
    "prompt_tokens": 120,
    "completion_tokens": 80,
    "total_tokens": 200
  },
  "metadata": {
    "model": "gpt-4-0613",
    "finish_reason": "stop"
  }
}
```

### 5. Routing Policy

Выбор провайдера через политику:

```json
{
  "policy_id": "default",
  "steps": [
    {
      "action": "select_provider",
      "provider_id": "openai_gpt4",
      "weight": 80
    },
    {
      "action": "select_provider",
      "provider_id": "anthropic_claude",
      "weight": 20
    }
  ],
  "fallback": ["anthropic_claude", "openai_gpt3"]
}
```

---

## Реализация на разных языках

### Go Example

```go
package main

import (
    "encoding/json"
    "github.com/nats-io/nats.go"
    "log"
)

type ProviderRequest struct {
    TraceID    string                 `json:"trace_id"`
    TenantID   string                 `json:"tenant_id"`
    ProviderID string                 `json:"provider_id"`
    Prompt     string                 `json:"prompt"`
    Parameters map[string]interface{} `json:"parameters"`
}

type ProviderResponse struct {
    ProviderID string                 `json:"provider_id"`
    Output     string                 `json:"output"`
    Usage      map[string]int         `json:"usage"`
    Metadata   map[string]interface{} `json:"metadata"`
}

func main() {
    nc, _ := nats.Connect(nats.DefaultURL)
    defer nc.Close()

    nc.Subscribe("beamline.provider.openai_gpt4.v1", func(m *nats.Msg) {
        var req ProviderRequest
        json.Unmarshal(m.Data, &req)

        // Call OpenAI API
        response := callOpenAI(req)

        // Send response
        data, _ := json.Marshal(response)
        m.Respond(data)
    })

    select {}
}

func callOpenAI(req ProviderRequest) ProviderResponse {
    // OpenAI API integration
    return ProviderResponse{
        ProviderID: "openai_gpt4",
        Output:     "Response from GPT-4",
        Usage: map[string]int{
            "prompt_tokens":     100,
            "completion_tokens": 50,
        },
    }
}
```

### Erlang Example

```erlang
-module(openai_provider_extension).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_info/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, Connection} = nats:connect(),
    {ok, _Sub} = nats:subscribe(Connection, <<"beamline.provider.openai_gpt4.v1">>),
    {ok, #{connection => Connection}}.

handle_info({msg, Subject, ReplyTo, Payload}, State) ->
    Request = jsx:decode(Payload, [return_maps]),
    Response = handle_openai_request(Request),
    ResponseData = jsx:encode(Response),
    nats:publish(maps:get(connection, State), ReplyTo, ResponseData),
    {noreply, State}.

handle_openai_request(Request) ->
    %% OpenAI API call logic
    #{
        <<"provider_id">> => <<"openai_gpt4">>,
        <<"output">> => <<"Response from GPT-4">>,
        <<"usage">> => #{
            <<"prompt_tokens">> => 100,
            <<"completion_tokens">> => 50
        }
    }.
```

---

## Преимущества перед `apps/otp/provider`

| Аспект | apps/otp/provider | Custom Provider Extensions |
|--------|-------------------|---------------------------|
| **Масштабируемость** | Монолитный OTP-проект | Независимые сервисы |
| **Изоляция** | Shared процесс | Полная изоляция |
| **Развертывание** | Вместе с Router | Независимое |
| **Язык** | Только Erlang | Любой язык |
| **Добавление провайдера** | Изменение кода | Только конфигурация |
| **NATS subject** | Универсальный | Уникальный per-provider |
| **Управление** | Код | Extension Registry + Policy |

---

## Migration Path (для будущих провайдеров)

### Старый подход (УДАЛЁН)
```
apps/otp/provider/
├── src/
│   ├── provider_core.erl
│   ├── provider_openai.erl
│   └── provider_anthropic.erl
└── rebar.config
```

### Новый подход (Extensions API)
```
Отдельные репозитории или сервисы:

providers/
├── openai-extension/      (Go/Rust/Erlang)
├── anthropic-extension/   (Go/Rust/Erlang)
└── custom-crm-extension/  (Python/Go)

Каждый:
- Свой NATS subject: beamline.provider.{id}.v1
- Регистрация в Extension Registry
- Независимое развертывание
```

---

## Тестирование

### Integration Tests

```erlang
%% Test Custom Provider Extension
test_openai_extension(_Config) ->
    %% Arrange: Start extension mock
    {ok, _Pid} = start_provider_mock(<<"beamline.provider.openai_gpt4.v1">>),
    
    %% Act: Send request via Router
    Request = #{
        <<"tenant_id">> => <<"test-tenant">>,
        <<"provider_id">> => <<"openai_gpt4">>,
        <<"prompt">> => <<"Hello">>
    },
    {ok, Response} = router_client:decide(Request),
    
    %% Assert: Provider selected and responded
    ?assertEqual(<<"openai_gpt4">>, maps:get(<<"provider_id">>, Response)).
```

---

## Observability

### Metrics
- `provider_requests_total{provider_id="openai_gpt4"}`
- `provider_latency_seconds{provider_id="openai_gpt4"}`
- `provider_errors_total{provider_id="openai_gpt4"}`

### Logs (JSON)
```json
{
  "timestamp": "2025-11-22T10:00:00Z",
  "level": "info",
  "provider_id": "openai_gpt4",
  "trace_id": "abc-123",
  "tenant_id": "tenant-1",
  "latency_ms": 450,
  "tokens_used": 150
}
```

---

## Ссылки

- **Extensions API**: [docs/EXTENSIONS_API.md](../../EXTENSIONS_API.md)
- **Extensions API (RU)**: [docs/EXTENSIONS_API_RU.md](../../EXTENSIONS_API_RU.md)
- **Extension Registry**: Section 6 in Extensions API
- **Routing Policy**: [docs/ROUTING_POLICY.md](../../ROUTING_POLICY.md)
- **NATS Subjects**: [docs/NATS_SUBJECTS.md](../../NATS_SUBJECTS.md)

---

## FAQ

### Q: Где хранится Extension Registry?
A: PostgreSQL (таблица `extensions`) + кеш в Mnesia/ETS для быстрого lookup.

### Q: Как добавить новый провайдер?
A: 
1. Создать NATS-сервис на любом языке
2. Зарегистрировать в Extension Registry (SQL INSERT)
3. Обновить Routing Policy для использования нового `provider_id`
4. Деплой сервиса (независимо от Router)

### Q: Можно ли использовать несколько языков?
A: Да! OpenAI на Go, Anthropic на Rust, CRM на Python — всё через NATS.

### Q: Как обрабатываются ошибки?
A: Router получает timeout/error, применяет retry (из Registry), затем fallback (из Policy).

---

**Note**: Этот подход обеспечивает максимальную гибкость и масштабируемость для интеграции любых внешних сервисов.
