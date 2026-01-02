---
title: UI Routes
description: Mapping of UI events to Gateway endpoints and NATS subjects, SSE usage, error handling, auth flow, and state management.
---

# UI Routes

Defines how the UI interacts with the Gateway and internal messaging via NATS, including real-time streams and error handling.

## UI  Gateway  NATS

UI events do not call services directly; they go through the Gateway:

```
UI Event  Gateway Endpoint  NATS Subject
```

Example:
```
message:send  POST /api/v1/messages  beamline.router.v1.decide
```

## Real-time Updates (SSE)

The UI subscribes to SSE streams via the Gateway:

- Message Status: `GET /api/v1/stream/:message_id`
- Provider Stream: receive provider-side streaming data

## Error Handling

The UI handles normalized errors returned by the Gateway:

```typescript
interface ErrorResponse {
  error: {
    code: string;
    message: string;
    trace_id: string;
    timestamp: string;
  }
}
```

UI behavior:
- Show toast notification for errors
- Log errors to console in dev mode
- Display `trace_id` for support/troubleshooting
- Retry transient errors (5xx)

## Authentication Flow

1. Login: `/app/login`
   - Enter API key
   - Save to localStorage/sessionStorage
   - Redirect to `/app`

2. Token Refresh: automatic refresh
   - Check token expiry
   - Refresh via Gateway
   - Continue without re-login

3. Logout: `/app/logout`
   - Clear tokens
   - Redirect to `/app/login`

## State Management

The UI uses SvelteKit stores for:
- Authentication state
- Current tenant
- Active messages (SSE streams)
- Policies cache
- Usage statistics cache

## References

- `docs/GATEWAY_ROUTES.md`: Gateway REST/SSE endpoints
- `docs/NATS_SUBJECTS.md`: NATS subjects catalog
- `docs/ROUTING_POLICY.md`: JSON-DSL policies
