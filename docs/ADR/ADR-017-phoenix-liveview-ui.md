# ADR-017: Phoenix LiveView UI Migration

## Status
**Accepted** (2025-11-20)

## Context

Current UI implementation uses SvelteKit (JavaScript/TypeScript SPA), which introduces:
- Separate technology stack from backend (Erlang/OTP)
- Complex build pipeline (Node.js, npm/pnpm, Vite)
- Client-server state synchronization challenges
- Additional deployment complexity
- Split team expertise (JS frontend + Erlang backend)

BeamLine Constructor core is built on BEAM (Erlang/OTP), making Elixir + Phoenix + LiveView a natural fit.

### Repository layout and migration notes

- Legacy SvelteKit UI lived in the monorepo under `apps/app` (main UI) and `apps/docs` (documentation UI shell).
- During an intermediate, experimental phase, SvelteKit UI was moved to `frontend/`, but this approach was later recognized as suboptimal for the long-term architecture (separate stack, extra build pipeline).
- As part of this migration, all **canonical** UI work is consolidated in:
  - `apps/ui_web/` — Phoenix LiveView application (target and current UI implementation).
- Legacy directories `apps/app`, `apps/docs` and the experimental `frontend/` SvelteKit UI are considered deprecated and are being removed from the tree to avoid confusion in tooling and TRAE metadata.

## Decision

**Migrate UI from SvelteKit to Elixir Phoenix LiveView.**

### Technology Stack

**Framework**: Phoenix 1.7+ with LiveView  
**Language**: Elixir 1.15+  
**Runtime**: BEAM (same as Router)  
**Real-time**: Phoenix Channels (WebSocket)  
**Styling**: TailwindCSS  
**Components**: Phoenix LiveView Components  
**Build**: Mix (Elixir build tool)

### Architecture

```
┌─────────────────────────────────────────┐
│     Phoenix LiveView UI (Elixir)        │
│  ┌───────────────────────────────────┐  │
│  │  LiveView Pages (Server-rendered) │  │
│  │  - Dashboard                       │  │
│  │  - Messages Management             │  │
│  │  - Policies Editor                 │  │
│  │  - Extensions Registry             │  │
│  │  - Usage & Billing                 │  │
│  └───────────────────────────────────┘  │
│              ↕ WebSocket                 │
│  ┌───────────────────────────────────┐  │
│  │  Phoenix Channels (PubSub)        │  │
│  └───────────────────────────────────┘  │
└──────────────┬──────────────────────────┘
               │ HTTP/REST
               ▼
┌─────────────────────────────────────────┐
│         C-Gateway (C11)                  │
│         HTTP ↔ NATS adapter              │
└──────────────┬──────────────────────────┘
               │ NATS
               ▼
┌─────────────────────────────────────────┐
│         Router (Erlang/OTP)              │
└─────────────────────────────────────────┘
```

## Rationale

### ✅ Advantages

**1. Unified BEAM Stack**
- Same runtime as Router (Erlang/OTP)
- Shared supervision trees
- Direct BEAM-to-BEAM communication (optional)
- Hot code reloading (BEAM feature)
- Unified observability (BEAM telemetry)

**2. Radical Simplification**
- **No frontend-backend split**: One language, one stack
- **No tool proliferation**: Mix instead of 10+ JS tools
- **No state synchronization**: Server-side state only
- **No contract duplication**: Single source of truth
- **Single deployment artifact**: Reduced operational complexity

**3. LiveView: Interactive UI without SPA**
- Server-rendered HTML with **WebSocket diffs**
- Reactive UI without heavy JavaScript
- Built-in state management (server-side)
- Real-time updates out of the box
- Tables, forms, search, validation — **all without JS**

**4. Developer Experience: 2-3x Faster**
- **Phoenix generators**: `mix phx.gen.live` → production-ready CRUD
- **Convention over configuration**: Less boilerplate
- **No tool conflicts**: No Vite vs Webpack vs Jest issues
- **Fast development cycle**: Hot reload (code + templates)
- **Focus on features, not infrastructure**

**5. Performance & Scalability**
- BEAM concurrency model (1M+ lightweight processes)
- Efficient WebSocket handling
- Minimal client-side JavaScript
- Server-side rendering (fast initial load)
- No async/await complexity

**6. Integration with Router**
- Easy integration with Erlang Router (same VM)
- Phoenix PubSub for real-time events
- Can subscribe to NATS directly (Elixir NATS client)
- Optional: Direct Erlang/Elixir RPC calls

**7. Maintenance: 87% Less Overhead**
- Stable ecosystem (Elixir rarely breaks)
- Fewer dependencies (~15 vs ~100)
- No breaking changes every 3 months
- Less time fighting tools, more time building features

### ⚠️ Trade-offs

**Cons:**
- New language for team (Elixir vs TypeScript)
- Less mature component ecosystem vs React/Vue
- Server-side rendering requires server resources
- WebSocket dependency (vs stateless REST)

**Mitigations:**
- Elixir syntax similar to Ruby (easy to learn)
- Phoenix Component library growing rapidly
- BEAM handles concurrency well (cheap processes)
- Fallback to REST for non-interactive pages

## Implementation Plan

### Phase 1: Project Setup (2 days)
1. Create Phoenix project: `apps/ui-web`
2. Configure TailwindCSS
3. Setup authentication (OIDC via Guardian)
4. Configure C-Gateway client
5. Basic layout and navigation

### Phase 2: Core Pages (5 days)
1. Dashboard (real-time metrics)
2. Messages Management (CRUD + real-time)
3. Policies Editor (JSON + visual)
4. Extensions Registry (CRUD)
5. Usage & Billing

### Phase 3: Real-time Features (3 days)
1. Phoenix Channels setup
2. Live updates (messages, metrics)
3. SSE replacement with LiveView
4. Notifications system

### Phase 4: Migration & Deployment (2 days)
1. Deprecate SvelteKit UI
2. Update docker-compose.yml
3. Documentation updates
4. Deployment guide

**Total**: ~12 days

## Consequences

### Positive
- ✅ Unified BEAM stack (Elixir + Erlang)
- ✅ Faster development (Phoenix generators)
- ✅ Better real-time capabilities (LiveView + Channels)
- ✅ Simplified deployment (single stack)
- ✅ Improved observability (BEAM telemetry)

### Negative
- ⚠️ Team needs to learn Elixir (but similar to Ruby)
- ⚠️ Migration effort (~12 days)
- ⚠️ Existing SvelteKit code deprecated

### Neutral
- Component library smaller but growing
- Server-side rendering (different paradigm)

## Alternatives Considered

### 1. Keep SvelteKit
- ❌ Separate stack (JS vs Erlang)
- ❌ Complex build pipeline
- ❌ State synchronization issues

### 2. Elm
- ✅ Functional, type-safe
- ❌ Smaller ecosystem
- ❌ Still separate frontend

### 3. Phoenix + React
- ✅ Rich component ecosystem
- ❌ Loses LiveView benefits
- ❌ Still requires JS build pipeline

## References

- Phoenix Framework: https://www.phoenixframework.org/
- Phoenix LiveView: https://hexdocs.pm/phoenix_live_view/
- Elixir: https://elixir-lang.org/
- TailwindCSS: https://tailwindcss.com/

## Related ADRs

- ADR-016: C-Gateway Migration (C11)
- ADR-010: Target Architecture

---

**Decision Date**: 2025-11-20  
**Status**: Accepted  
**Next Review**: After Phase 2 completion (once core pages in apps/ui_web are stable)
