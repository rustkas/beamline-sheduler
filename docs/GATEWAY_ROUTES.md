# Gateway Routes

REST and optional SSE endpoints exposed by the Gateway with correlation fields and NATS subject mapping. Universal, provider-agnostic contracts.

## Error Codes
- `200 OK`  success
- `400 Bad Request`  validation error
- `401 Unauthorized`  invalid/absent credentials
- `403 Forbidden`  not allowed
- `404 Not Found`  resource missing
- `429 Too Many Requests`  rate limit exceeded
- `5xx`  internal/transient errors

## Correlation Fields
Every request carries:
- `tenant_id`  current tenant identifier
- `trace_id`  end-to-end trace id
- `message_id`  idempotency key for message operations

## Endpoints  NATS Subjects (Universal)
- `POST /api/v1/messages`  create & publish message (returns ack)
- `GET /api/v1/messages/:id`  fetch stored message by id
- `GET /api/v1/messages`  list messages
- `POST /api/v1/routes/decide`  request route decision for a message
- `GET /api/v1/routes/decide/:messageId`  get cached decision for message id
- `GET /api/v1/policies/:tenantId`  list policies for tenant
- `GET /api/v1/policies/:tenantId/:policyId`  fetch a policy
- `POST /api/v1/policies/:tenantId`  create policy
- `PUT /api/v1/policies/:tenantId/:policyId`  update policy
- `DELETE /api/v1/policies/:tenantId/:policyId`  delete policy
- `POST /api/v1/flows/validate`  Flow DSL validation (JSON Schema)
- `POST /api/v1/flows/dry-run`  Flow compile + latency simulation
- `GET /api/v1/registry/blocks`  list available block types (read-only)
- `GET /api/v1/registry/blocks/:type`  get block manifest
- `GET /health`  health probe

### Request/Response Examples

`POST /api/v1/messages`

Request body (`CreateMessageDto`):

```
{
  "tenant_id": "tenant_abc123",
  "message_type": "chat",
  "payload": "{\"text\": \"Hello, world!\"}",
  "trace_id": "trace_abc123",
  "metadata": { "source": "api", "version": "1.0" }
}
```

Response (`MessageAckDto`):

```
{
  "message_id": "msg_1700000000000_xxxxx",
  "ack_timestamp_ms": 1700000000000,
  "status": "published"
}
```

`GET /api/v1/messages/:id`

Response (`MessageDto`):

```
{
  "message_id": "msg_1700000000000_xxxxx",
  "tenant_id": "tenant_abc123",
  "trace_id": "trace_abc123",
  "message_type": "chat",
  "payload": "{\"text\": \"Hello, world!\"}",
  "metadata": { "source": "api", "version": "1.0" },
  "timestamp_ms": 1700000000000
}
```

`POST /api/v1/routes/decide`

Request body (`RouteRequestDto`):

```
{
  "message": {
    "message_id": "msg_123",
    "tenant_id": "tenant_abc123",
    "message_type": "chat",
    "payload": "{\"text\": \"Hello\"}"
  },
  "policy_id": "default",
  "context": { "user_id": "user_001" }
}
```

Response (`RouteDecisionDto`):

```
{
  "provider_id": "provider-sticky",
  "reason": "sticky",
  "priority": 100,
  "expected_latency_ms": 200,
  "expected_cost": 0.001,
  "metadata": { "session_key": "user_001" }
}
```

## Rate Limiting
- Per tenant & per endpoint quotas
- Sliding window counters in `rate_counters`
- `429` on exceeded limits with `Retry-After` hint
 - See `docs/GATEWAY_RATE_LIMITING.md` for universal specification and configuration

## References
- `docs/GATEWAY_UNIVERSAL_SPEC.md`  Universal Gateway specification (CP1)
- `docs/GATEWAY_PURPOSE.md`  Gateway design and responsibilities
- `docs/NATS_SUBJECTS.md`  subjects naming
- `docs/ARCHITECTURE/api-registry.md`  REST API DTO specifications (JSON format)
- `apps/c-gateway/src/http_server.c`  C-Gateway HTTP server implementation
- `docs/GATEWAY_RATE_LIMITING.md`  Rate limiting spec (CP1/CP2)
- `docs/ADR/ADR-016-c-gateway-migration.md`  C-Gateway migration decision
