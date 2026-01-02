# Extensions API

> **In short:** An extension is a separate service that communicates with BeamLine **only through NATS using a fixed contract**, and enabling/disabling extensions is done **only through configuration/policy**, without changing Gateway, Router, or CAF-workers code.

---

## 0. Definition

**Extension** is a *separate NATS service* that:

- has its own NATS subject (e.g., `beamline.ext.pre.normalize_text.v1`);
- receives and returns messages according to a fixed contract (CP-Ext or CP2);
- registers in **Extension Registry** by logical `extension_id`;
- connects to the pipeline through **Routing Policy**, without changing core code.

Gateway, Router, and CAF-workers remain unchanged when adding new extensions.

---

## 1. Extension Types

Four types of extensions are supported:

1. **Pre-processor** — modifies/enriches incoming message *before* routing.
2. **Validator** — decides whether processing can continue (accept / reject).
3. **Post-processor** — modifies provider response *after* routing.
4. **Custom provider** — acts as another provider (LLM, RAG, CRM, etc.).

Router only knows about the extension:

- logical `extension_id` (e.g., `normalize_text`);
- its type (`pre` / `validator` / `post` / `provider`);
- NATS subject, timeout and retry (from Extension Registry);
- how to interpret the result in its pipeline.

---

## 2. Common CP-Ext Contract (pre / validator / post)

### 2.1. NATS subjects

Each extension subscribes to **its own** NATS subject, for example:

- `beamline.ext.pre.normalize_text.v1`
- `beamline.ext.validate.pii_guard.v1`
- `beamline.ext.post.mask_pii.v1`

Versioning is mandatory (`.v1`, `.v2`, …).

### 2.2. CP-Ext Request (JSON over NATS)

**Implementation**: `router_extension_invoker:build_request_payload/2`

**Actual Request Format** (as sent over NATS):

```json
{
  "trace_id": "uuid",
  "tenant_id": "tenant-123",
  "payload": {
    "message_id": "m-1",
    "message_type": "chat",
    "payload": "Original text",
    "metadata": {
      "channel": "telegram"
    }
  },
  "metadata": {
    "lang": "en",
    "policy_id": "policy-456"
  }
}
```

**Internal Request Structure** (passed to `router_extension_invoker:invoke/3`):

```erlang
Request = #{
    <<"payload">> => Message,      % Message to process
    <<"config">> => Config,        % Extension-specific config from policy
    <<"metadata">> => Context      % Request context
}
```

**Fields**:

- `trace_id` — end-to-end trace for logs/tracing (optional, extracted from Context, added by `build_request_payload`).
- `tenant_id` — current tenant (optional, extracted from Context, added by `build_request_payload`).
- `payload` — message payload (from Request, contains the message to process).
- `metadata` — arbitrary context (language, channel, customer_id, policy_id, etc., from Request).

**Note**: 
- Extension ID is not sent in request payload (known from Registry lookup).
- Extension config from policy is passed in internal Request structure but not in NATS payload (config is per-policy, not per-request).

### 2.3. CP-Ext Response (pre / post)

**Implementation**: `router_decider:execute_pre_processors/3`, `router_decider:execute_post_processors/3`

**Response Format** (JSON over NATS):

```json
{
  "payload": {
    "message_id": "m-1",
    "message_type": "chat",
    "payload": "normalized text",
    "metadata": {
      "channel": "telegram",
      "normalized": "true"
    }
  },
  "metadata": {
    "lang": "en",
    "detected_lang": "en"
  }
}
```

**Current Implementation**:
- Response is a map (JSON object) returned from extension via NATS.
- Router extracts `payload` and `metadata` from response:
  - `ProcessedPayload = maps:get(<<"payload">>, Response, Message)`
  - `ProcessedContext = maps:merge(Context, maps:get(<<"metadata">>, Response, #{}))`
- For pre-processors: `payload` becomes new message, `metadata` enriches context.
- For post-processors: `payload` becomes new response, `metadata` enriches context.

**Requirements**:

- Response must be a valid JSON object (map).
- Extension should return:
  - `payload` (optional, defaults to original message/response if not present)
  - `metadata` (optional, merged into context if present)
- Extension can:
  - modify `payload` (message/response content);
  - add/change `metadata` (context enrichment);
  - return any structure (Router extracts `payload` and `metadata` if present, uses defaults if missing).

**Router Behavior**:
- Router substitutes updated `payload` and merges `metadata` into context.
- Pipeline continues executing next policy step with enriched context.
- If `payload` is missing, original message/response is used.
- If `metadata` is missing, context is unchanged.

---

## 3. Validator Extensions

Validator is called **after pre-processing and/or before provider selection**.

### 3.1. Request

Request is the same **CP-Ext Request** as pre-processor.

### 3.2. Response

**Implementation**: `router_decider:execute_validators/3`

**Success:**

```json
{
  "status": "ok"
}
```

**Block/Reject:**

```json
{
  "status": "reject",
  "reason": "pii_detected",
  "details": {
    "field": "payload",
    "pattern": "credit_card"
  }
}
```

**Current Implementation**:
- Router expects response to be a map (JSON object).
- Router checks for `status` field:
  - `status = "ok"` or missing → Router continues pipeline.
  - `status = "reject"` → Router applies `on_fail` behavior.

**Semantics**:

- `status = "ok"` (or missing) — Router continues pipeline.
- `status = "reject"` — behavior is defined by policy:
  - `on_fail = "block"` — Router stops request and returns error/special response to client (fail-closed);
  - `on_fail = "warn"` — Router logs event but doesn't block (fail-open with logging);
  - `on_fail = "ignore"` — Router ignores result and continues (fail-open).

**Error Handling**:
- On timeout or network error Router treats result as `reject` and applies `on_fail`.
- Error format: `{error, {Reason, Metadata}}` with `extension_id`, `reason`, and `context`.

---

## 4. Post-processor Extensions

Post-processor is called **after provider response**, before returning to client.

### 4.1. Request

```json
{
  "trace_id": "uuid",
  "tenant_id": "tenant-123",
  "extensions": {
    "id": "mask_pii",
    "config": {
      "mask_email": true,
      "mask_phone": true
    }
  },
  "message": {
    "message_id": "m-1",
    "message_type": "chat",
    "payload": "Model reply text",
    "metadata": {
      "provider_id": "openai:gpt-4.1-mini"
    }
  },
  "context": {
    "lang": "en"
  }
}
```

### 4.2. Response

```json
{
  "message": {
    "message_id": "m-1",
    "message_type": "chat",
    "payload": "Reply with [masked] PII",
    "metadata": {
      "provider_id": "openai:gpt-4.1-mini",
      "pii_masked": "true"
    }
  },
  "context": {
    "lang": "en"
  }
}
```

Router substitutes result and returns it to client. Error behavior is defined by `mode` in policy (`required` or `optional`).

---

## 5. Custom Provider Extensions

Custom provider is an extension-provider that looks like a regular CP2 provider but implemented as a separate NATS service.

### 5.1. NATS subject

Example subject:

- `beamline.provider.my_crm_summarizer.v1`

### 5.2. Provider Request (CP2-style, JSON over NATS)

Simplified example:

```json
{
  "trace_id": "uuid",
  "tenant_id": "tenant-123",
  "provider_id": "my_crm_summarizer",
  "prompt": "User message + context",
  "parameters": {
    "max_tokens": 512
  },
  "context": {
    "conversation_id": "conv-1",
    "customer_id": "cust-42"
  }
}
```

### 5.3. Provider Response

```json
{
  "provider_id": "my_crm_summarizer",
  "output": "Summarized answer...",
  "usage": {
    "prompt_tokens": 120,
    "completion_tokens": 80
  },
  "metadata": {
    "source": "crm"
  }
}
```

Router and CAF-workers treat such provider equally with others (OpenAI, Anthropic, etc.), selection is made by policy via `provider_id`.

---

## 6. Extension Registry

**Extension Registry** is the source of truth about extensions for Router. Can be stored in DB or as config, with cache in Mnesia/ETS.

### 6.1. Record Structure

Example JSON:

```json
{
  "normalize_text": {
    "type": "pre",
    "subject": "beamline.ext.pre.normalize_text.v1",
    "timeout_ms": 80,
    "retry": 0
  },
  "mask_pii": {
    "type": "post",
    "subject": "beamline.ext.post.mask_pii.v1",
    "timeout_ms": 100,
    "retry": 0
  },
  "pii_guard": {
    "type": "validator",
    "subject": "beamline.ext.validate.pii_guard.v1",
    "timeout_ms": 100,
    "retry": 0
  },
  "my_crm_summarizer": {
    "type": "provider",
    "subject": "beamline.provider.my_crm_summarizer.v1",
    "timeout_ms": 5000,
    "retry": 1
  }
}
```

Router uses registry to:

- by logical `id` from policy find:
  - `type`;
  - NATS `subject`;
  - `timeout_ms` and `retry`;
- understand **where** to send request and **how** to handle errors.

---

## 7. Routing Policy: Connecting Extensions

Routing Policy describes **which** extensions and providers are used for specific scenario/tenant and **in what order**.

### 7.1. Policy Example

```json
{
  "policy_id": "support_en",
  "pre": [
    {
      "id": "normalize_text",
      "mode": "required",
      "config": {
        "lowercase": true
      }
    }
  ],
  "validators": [
    {
      "id": "pii_guard",
      "on_fail": "block"
    }
  ],
  "providers": [
    "openai:gpt-4.1-mini",
    "my_crm_summarizer"
  ],
  "post": [
    {
      "id": "mask_pii",
      "mode": "required",
      "config": {
        "mask_email": true
      }
    }
  ]
}
```

Semantics:

- `pre[]` — sequence of pre-processors:
  - `id` — logical ID (lookup in Registry);
  - `mode`:
    - `required` — on timeout/error policy is considered unsuccessful (Router acts according to default error rules);
    - `optional` — on error Router skips extension and continues;
  - `config` — per-policy config, goes into `extensions.config`.

- `validators[]` — sequence of validators:
  - `id` — logical ID;
  - `on_fail` — `block` / `warn` / `ignore`.

- `providers[]` — list of `provider_id` in priority/weights order (detailed format described in `ROUTING_POLICY.md`).

- `post[]` — sequence of post-processors, with same `mode` and `config` rules as `pre[]`.

---

## 8. Code vs Config

### 8.1. "As Code" (platform core)

Changes rarely, requires releases:

1. **CP1/CP2/CP-Ext Schemas**
   - Protobuf/JSON types `RouteRequest`, `RouteDecision`, `ExtRequest`, `ExtResponse`, `ProviderRequest`, `ProviderResponse`.
   - Router/Workers code that works with these types.

2. **Extension Calling Mechanics in Router**
   - Pipeline: `pre[] → validators[] → routing → provider → post[]`.
   - Adapter `extension_id → subject/timeout → NATS request/response`.
   - Unified error rules (timeouts, retries, circuit breaker).
   - Logging and metrics.

3. **Extension Registry loader**
   - Reading registry from DB/config → Mnesia/ETS.
   - Reading and applying Routing Policies.

4. **Gateway / Router / CAF-workers**
   - Don't change for specific client scenarios, only during platform evolution.

### 8.2. "As Config" (dynamics without core restart)

Changes frequently:

1. **Routing Policies**
   - Chain composition `pre/validators/post`.
   - Provider list and their weights/priorities.
   - Error behavior (`mode`, `on_fail`).
   - Per-policy `config` for extensions.

2. **Extension Registry**
   - Adding new extension:
     - spin up NATS service;
     - add record to registry (`id → type/subject/timeout`);
     - Router after reload already knows new subject.

3. **Tenant-level overrides**
   - Which `policy` to use for specific tenant/channel.
   - Enabling/disabling extensions for tenant (feature-flags).
   - **Extension Version Routing**: Different extension versions for different tenants/environments (see `docs/ARCHITECTURE/EXTENSION_ROUTING_STRATEGY.md`).

Storage:

- Postgres + cache in Mnesia/ETS; or
- config files with hot-reload.

**Config change = new business case without core recompilation.**

### 8.3. Where Client Custom Code Lives

Client custom logic is implemented only in **separate NATS extension services** (`pre`, `validator`, `post`, `provider`):

- can be written in any language (Go, Rust, Erlang, C, Python, etc.);
- must comply with CP-Ext/CP2 contract.

Adding new behavior:

1. Spin up new NATS service (subscribe to own subject).
2. Add record to Extension Registry.
3. Connect extension in Routing Policy (by `id`).
4. Apply config.

At the same time:

- **Gateway** doesn't change (HTTP ↔ NATS CP1);
- **Router** doesn't change code (only rereads Registry/Policies);
- **CAF-workers** don't change (see provider through CP2 as regular).

---

## 9. Extension Lifecycle (for developer)

1. **Create Extension Service**
   - choose type (`pre` / `validator` / `post` / `provider`);
   - implement CP-Ext/CP2 Request → Response handler;
   - subscribe to own NATS subject.

2. **Register in Extension Registry**
   - add record `{id → type, subject, timeout_ms, retry}`.

3. **Connect in Routing Policy**
   - specify `id` in `pre[]` / `validators[]` / `post[]` or in `providers[]` list;
   - if needed specify mode (`mode` / `on_fail`) and `config`.

4. **Reload Config / Update Cache**
   - Router rereads Registry and Policies;
   - new extension starts participating in pipeline without core code changes.

---

## 10. C-Gateway and Extensions

This section clarifies how the **C HTTP Gateway** relates to extensions.

- C-Gateway:
  - exposes HTTP API (`/api/v1/messages`, `/api/v1/routes/decide`, etc.) according to API Registry;
  - converts HTTP requests into NATS requests for **Router** using CP1/CP2 contracts;
  - **does not know** about individual extensions or their NATS subjects.

- Router:
  - receives requests from C-Gateway over NATS (e.g. `beamline.router.v1.decide`);
  - looks up **Routing Policy** and **Extension Registry** to decide which extensions to call;
  - calls extensions via CP-Ext contracts (`beamline.ext.*`, `beamline.provider.*`), as described above;
  - combines extension results and provider responses into RouteDecision / final message for Gateway.

Implications:

- Adding, changing, or removing an extension **never requires changes** in C-Gateway code:
  - C-Gateway stays a pure HTTP↔NATS adapter on CP1/CP2 layer.
  - All extension-specific logic lives in Router + Extension Registry + Routing Policies.
- C-Gateway only needs to:
  - respect API Registry DTOs;
  - send/receive NATS messages for Router;
  - propagate `trace_id` / `tenant_id` so Router and extensions can correlate logs and traces.

This keeps extension behavior entirely configuration-driven (policies/registry), while C-Gateway remains stable and focused on transport and basic validation.

---

## 11. Extension Version Routing

Extension version routing allows different versions of the same extension to be used based on tenant, environment, or policy context.

**For complete routing strategy, see**: `docs/ARCHITECTURE/EXTENSION_ROUTING_STRATEGY.md`

**Quick Reference**:
- Routing rules format: `{"tenant_id": ["tenant1"], "environment": "prod"}`
- Empty rules (`{}`): Matches by default (fallback)
- List values: Uses "IN" semantics (tenant_id in list)
- String values: Uses exact match (environment = "prod")
- All conditions: AND logic (all keys must match)

**SQL Examples**: See `sql/012_extension_routing_examples.sql`
