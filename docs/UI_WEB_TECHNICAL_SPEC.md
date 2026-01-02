# UI-Web Technical Specification
## Phoenix LiveView UI for BeamLine Constructor

**Version**: 1.0  
**Date**: 2025-11-20  
**Status**: Approved

---

## 1. Overview

### 1.1 Purpose

Replace SvelteKit UI with Phoenix LiveView to:
- Unify stack (Elixir + Erlang on BEAM)
- Simplify architecture (no separate frontend)
- Improve real-time capabilities (LiveView + Channels)
- Accelerate development (Phoenix generators)

### 1.2 Scope

**In Scope:**
- Dashboard (real-time metrics)
- Messages Management (CRUD + live updates)
- Routing Policies Editor (JSON + visual)
- Extensions Registry UI (CRUD + health monitoring)
- Usage & Billing
- Authentication (OIDC)
- Real-time notifications

**Out of Scope:**
- Mobile apps (future)
- Public API documentation UI (future)
- Admin CLI (separate tool)

---

## 2. Architecture

### 2.1 Technology Stack

| Component | Technology | Version |
|-----------|-----------|---------|
| Framework | Phoenix | 1.7+ |
| Language | Elixir | 1.15+ |
| Runtime | BEAM/OTP | 26+ |
| LiveView | Phoenix LiveView | 0.20+ |
| Styling | TailwindCSS | 3.4+ |
| Auth | Guardian | 2.3+ |
| HTTP Client | Tesla | 1.8+ |
| JSON | Jason | 1.4+ |
| Database | PostgreSQL | 15+ (shared with Router) |

### 2.2 Project Structure

```
apps/ui-web/
├── assets/
│   ├── css/
│   │   └── app.css              # TailwindCSS
│   ├── js/
│   │   └── app.js               # Minimal JS (LiveView hooks)
│   └── vendor/
├── lib/
│   ├── ui_web/
│   │   ├── channels/            # Phoenix Channels
│   │   ├── components/          # LiveView Components
│   │   ├── controllers/         # HTTP Controllers
│   │   ├── live/                # LiveView Pages
│   │   │   ├── dashboard_live.ex
│   │   │   ├── messages_live/
│   │   │   ├── policies_live/
│   │   │   ├── extensions_live/
│   │   │   └── usage_live/
│   │   ├── router.ex            # Phoenix Router
│   │   ├── endpoint.ex          # Phoenix Endpoint
│   │   └── telemetry.ex         # Telemetry
│   ├── ui_web.ex
│   └── ui_web_application.ex    # OTP Application
├── priv/
│   ├── static/                  # Compiled assets
│   └── gettext/                 # I18n
├── test/
├── config/
│   ├── config.exs
│   ├── dev.exs
│   ├── prod.exs
│   └── test.exs
├── mix.exs                      # Dependencies
└── README.md
```

### 2.3 Communication Flow

```
Browser (LiveView Client)
    ↕ WebSocket (Phoenix Channel)
Phoenix LiveView Server (Elixir)
    ↕ HTTP/REST
C-Gateway (C11)
    ↕ NATS
Router (Erlang/OTP)
```

**Alternative (Future):**
```
Phoenix LiveView (Elixir)
    ↕ Direct BEAM call (Distributed Erlang)
Router (Erlang/OTP)
```

---

## 3. Core Features

### 3.1 Authentication

**Technology**: Guardian (JWT) + OIDC

**Flow:**
1. User clicks "Login"
2. Redirect to OIDC provider (Keycloak, Auth0, etc.)
3. OIDC callback with authorization code
4. Exchange code for JWT token
5. Store JWT in httpOnly cookie
6. Guardian validates JWT on each request

**Implementation:**
```elixir
# lib/ui_web/auth/guardian.ex
defmodule UiWeb.Auth.Guardian do
  use Guardian, otp_app: :ui_web
  
  def subject_for_token(user, _claims), do: {:ok, to_string(user.id)}
  def resource_from_claims(%{"sub" => id}), do: {:ok, get_user(id)}
end

# lib/ui_web/auth/pipeline.ex
defmodule UiWeb.Auth.Pipeline do
  use Guardian.Plug.Pipeline,
    otp_app: :ui_web,
    module: UiWeb.Auth.Guardian,
    error_handler: UiWeb.Auth.ErrorHandler
    
  plug Guardian.Plug.VerifySession
  plug Guardian.Plug.VerifyHeader
  plug Guardian.Plug.LoadResource, allow_blank: true
end
```

### 3.2 Dashboard (Real-time Metrics)

**URL**: `/app/dashboard`

**Features:**
- System health (C-Gateway, Router, Worker CAF)
- Real-time metrics (throughput, latency, errors)
- Active connections count
- Recent alerts
- Quick stats (messages today, policies count, extensions count)

**LiveView Implementation:**
```elixir
defmodule UiWeb.DashboardLive do
  use UiWeb, :live_view
  
  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      # Subscribe to real-time updates
      Phoenix.PubSub.subscribe(UiWeb.PubSub, "metrics")
      :timer.send_interval(5000, self(), :update_metrics)
    end
    
    {:ok, assign(socket, metrics: fetch_metrics())}
  end
  
  @impl true
  def handle_info(:update_metrics, socket) do
    {:noreply, assign(socket, metrics: fetch_metrics())}
  end
  
  @impl true
  def handle_info({:metric_update, metric}, socket) do
    {:noreply, update(socket, :metrics, &Map.put(&1, metric.key, metric.value))}
  end
end
```

**Template:**
```heex
<div class="dashboard">
  <.header>Dashboard</.header>
  
  <div class="grid grid-cols-4 gap-4">
    <.stat_card title="Throughput" value={@metrics.throughput} unit="req/s" />
    <.stat_card title="Latency p95" value={@metrics.latency_p95} unit="ms" />
    <.stat_card title="Error Rate" value={@metrics.error_rate} unit="%" />
    <.stat_card title="Active Connections" value={@metrics.connections} />
  </div>
  
  <.live_component module={UiWeb.Components.MetricsChart} id="metrics-chart" data={@metrics.history} />
</div>
```

### 3.3 Messages Management

**URL**: `/app/messages`

**Features:**
- List messages (paginated, filterable)
- Create new message
- View message details (payload, routing decision, trace)
- Real-time status updates
- Retry/cancel actions

**LiveView Implementation:**
```elixir
defmodule UiWeb.MessagesLive.Index do
  use UiWeb, :live_view
  
  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      Phoenix.PubSub.subscribe(UiWeb.PubSub, "messages")
    end
    
    {:ok, assign(socket, messages: list_messages(), page: 1)}
  end
  
  @impl true
  def handle_event("create_message", %{"message" => params}, socket) do
    case create_message(params) do
      {:ok, message} ->
        {:noreply, 
         socket
         |> put_flash(:info, "Message created")
         |> push_navigate(to: ~p"/app/messages/#{message.id}")}
      {:error, changeset} ->
        {:noreply, assign(socket, changeset: changeset)}
    end
  end
  
  @impl true
  def handle_info({:message_updated, message}, socket) do
    {:noreply, update(socket, :messages, &update_message(&1, message))}
  end
end
```

### 3.4 Routing Policies Editor ⭐ **КРИТИЧНО**

**URL**: `/app/policies`

**Features:**
- List policies (per tenant)
- Create/Edit policy
- **Visual Pipeline Builder** (drag-and-drop)
- JSON Editor (fallback)
- Dry-run testing
- Version history
- Rollback

**Visual Pipeline Builder:**
```elixir
defmodule UiWeb.PoliciesLive.Editor do
  use UiWeb, :live_view
  
  @impl true
  def handle_event("add_extension", %{"type" => type, "id" => id}, socket) do
    policy = socket.assigns.policy
    updated_policy = add_extension_to_pipeline(policy, type, id)
    
    {:noreply, assign(socket, policy: updated_policy)}
  end
  
  @impl true
  def handle_event("remove_extension", %{"type" => type, "index" => index}, socket) do
    policy = socket.assigns.policy
    updated_policy = remove_extension_from_pipeline(policy, type, index)
    
    {:noreply, assign(socket, policy: updated_policy)}
  end
  
  @impl true
  def handle_event("dry_run", _params, socket) do
    case dry_run_policy(socket.assigns.policy) do
      {:ok, result} ->
        {:noreply, assign(socket, dry_run_result: result)}
      {:error, error} ->
        {:noreply, put_flash(socket, :error, error)}
    end
  end
end
```

**Template (Visual Builder):**
```heex
<div class="policy-editor">
  <.header>Edit Policy: <%= @policy.policy_id %></.header>
  
  <div class="pipeline-builder">
    <!-- Pre-processors -->
    <div class="pipeline-stage" phx-drop="drop_extension" phx-value-type="pre">
      <h3>Pre-processors</h3>
      <%= for {ext, index} <- Enum.with_index(@policy.pre) do %>
        <.extension_card extension={ext} type="pre" index={index} />
      <% end %>
      <.add_extension_button type="pre" extensions={@available_extensions.pre} />
    </div>
    
    <!-- Validators -->
    <div class="pipeline-stage" phx-drop="drop_extension" phx-value-type="validators">
      <h3>Validators</h3>
      <%= for {ext, index} <- Enum.with_index(@policy.validators) do %>
        <.extension_card extension={ext} type="validators" index={index} />
      <% end %>
      <.add_extension_button type="validators" extensions={@available_extensions.validators} />
    </div>
    
    <!-- Providers -->
    <div class="pipeline-stage">
      <h3>Providers</h3>
      <%= for {provider, index} <- Enum.with_index(@policy.providers) do %>
        <.provider_card provider={provider} index={index} />
      <% end %>
    </div>
    
    <!-- Post-processors -->
    <div class="pipeline-stage" phx-drop="drop_extension" phx-value-type="post">
      <h3>Post-processors</h3>
      <%= for {ext, index} <- Enum.with_index(@policy.post) do %>
        <.extension_card extension={ext} type="post" index={index} />
      <% end %>
      <.add_extension_button type="post" extensions={@available_extensions.post} />
    </div>
  </div>
  
  <div class="actions">
    <.button phx-click="dry_run">Dry Run</.button>
    <.button phx-click="save">Save Policy</.button>
  </div>
</div>
```

### 3.5 Extensions Registry UI ⭐ **КРИТИЧНО**

**URL**: `/app/extensions`

**Features:**
- List extensions (filterable by type)
- Register new extension
- Edit extension config
- Health status monitoring
- Enable/disable toggle
- Delete extension

**LiveView Implementation:**
```elixir
defmodule UiWeb.ExtensionsLive.Index do
  use UiWeb, :live_view
  
  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      Phoenix.PubSub.subscribe(UiWeb.PubSub, "extensions")
      :timer.send_interval(10000, self(), :check_health)
    end
    
    {:ok, assign(socket, extensions: list_extensions(), filter: "all")}
  end
  
  @impl true
  def handle_event("register", %{"extension" => params}, socket) do
    case register_extension(params) do
      {:ok, extension} ->
        {:noreply,
         socket
         |> put_flash(:info, "Extension registered")
         |> push_navigate(to: ~p"/app/extensions/#{extension.id}")}
      {:error, changeset} ->
        {:noreply, assign(socket, changeset: changeset)}
    end
  end
  
  @impl true
  def handle_event("toggle", %{"id" => id}, socket) do
    extension = Enum.find(socket.assigns.extensions, &(&1.id == id))
    {:ok, updated} = toggle_extension(extension)
    
    {:noreply, update(socket, :extensions, &update_extension(&1, updated))}
  end
  
  @impl true
  def handle_info(:check_health, socket) do
    health_statuses = check_extensions_health(socket.assigns.extensions)
    {:noreply, assign(socket, health: health_statuses)}
  end
end
```

### 3.6 Usage & Billing

**URL**: `/app/usage`

**Features:**
- Per-tenant usage statistics
- Cost estimation
- Charts (time series)
- Quota management
- Billing reports (CSV/PDF export)

---

## 4. Real-time Features

### 4.1 Phoenix Channels

**Setup:**
```elixir
# lib/ui_web/channels/user_socket.ex
defmodule UiWeb.UserSocket do
  use Phoenix.Socket
  
  channel "metrics:*", UiWeb.MetricsChannel
  channel "messages:*", UiWeb.MessagesChannel
  channel "extensions:*", UiWeb.ExtensionsChannel
  
  def connect(%{"token" => token}, socket, _connect_info) do
    case Guardian.decode_and_verify(token) do
      {:ok, claims} ->
        {:ok, assign(socket, :user_id, claims["sub"])}
      {:error, _} ->
        :error
    end
  end
end
```

**Metrics Channel:**
```elixir
defmodule UiWeb.MetricsChannel do
  use Phoenix.Channel
  
  def join("metrics:dashboard", _params, socket) do
    send(self(), :after_join)
    {:ok, socket}
  end
  
  def handle_info(:after_join, socket) do
    push(socket, "metrics", %{data: fetch_metrics()})
    {:noreply, socket}
  end
  
  # Broadcast from other parts of the system
  def broadcast_metric(metric) do
    UiWeb.Endpoint.broadcast("metrics:dashboard", "metric_update", metric)
  end
end
```

### 4.2 LiveView PubSub Integration

**Subscribe to NATS events (via Elixir NATS client):**
```elixir
defmodule UiWeb.NatsSubscriber do
  use GenServer
  
  def start_link(_) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end
  
  def init(_) do
    {:ok, conn} = Gnat.start_link(%{host: "localhost", port: 4222})
    
    # Subscribe to message updates
    {:ok, _} = Gnat.sub(conn, self(), "beamline.messages.updates.v1")
    
    {:ok, %{conn: conn}}
  end
  
  def handle_info({:msg, %{body: body}}, state) do
    message = Jason.decode!(body)
    
    # Broadcast to LiveView via PubSub
    Phoenix.PubSub.broadcast(
      UiWeb.PubSub,
      "messages",
      {:message_updated, message}
    )
    
    {:noreply, state}
  end
end
```

---

## 5. API Client

### 5.1 C-Gateway Client

**HTTP Client (Tesla):**
```elixir
defmodule UiWeb.Gateway.Client do
  use Tesla
  
  plug Tesla.Middleware.BaseUrl, gateway_url()
  plug Tesla.Middleware.JSON
  plug Tesla.Middleware.Headers, [{"content-type", "application/json"}]
  plug Tesla.Middleware.Timeout, timeout: 30_000
  
  def create_message(params) do
    post("/api/v1/messages", params)
  end
  
  def get_message(id) do
    get("/api/v1/messages/#{id}")
  end
  
  def decide_route(params) do
    post("/api/v1/routes/decide", params)
  end
  
  defp gateway_url do
    Application.get_env(:ui_web, :gateway_url, "http://localhost:8081")
  end
end
```

### 5.2 Admin API Client (gRPC)

**For Extensions Registry:**
```elixir
defmodule UiWeb.Admin.ExtensionsClient do
  # Use grpc-elixir library
  
  def list_extensions(tenant_id) do
    # gRPC call to RouterAdmin.ListExtensions
  end
  
  def register_extension(params) do
    # gRPC call to RouterAdmin.RegisterExtension
  end
end
```

---

## 6. Deployment

### 6.1 Docker

**Dockerfile:**
```dockerfile
FROM elixir:1.15-alpine AS builder

WORKDIR /app

# Install build dependencies
RUN apk add --no-cache build-base git nodejs npm

# Install hex and rebar
RUN mix local.hex --force && \
    mix local.rebar --force

# Copy mix files
COPY mix.exs mix.lock ./
COPY config config

# Install dependencies
RUN mix deps.get --only prod
RUN mix deps.compile

# Copy assets
COPY assets assets
RUN cd assets && npm install && npm run deploy

# Copy source
COPY lib lib
COPY priv priv

# Compile and digest assets
RUN mix phx.digest
RUN mix compile

# Build release
RUN mix release

# Runtime stage
FROM alpine:3.18

RUN apk add --no-cache openssl ncurses-libs libstdc++

WORKDIR /app

COPY --from=builder /app/_build/prod/rel/ui_web ./

ENV HOME=/app
ENV MIX_ENV=prod

CMD ["bin/ui_web", "start"]
```

### 6.2 docker-compose.yml

```yaml
services:
  ui-web:
    build:
      context: ./apps/ui-web
      dockerfile: Dockerfile
    ports:
      - "4000:4000"
    environment:
      - SECRET_KEY_BASE=${SECRET_KEY_BASE}
      - DATABASE_URL=postgresql://postgres:postgres@postgres:5432/beamline_dev
      - GATEWAY_URL=http://c-gateway:8080
      - NATS_URL=nats://nats:4222
    depends_on:
      - postgres
      - c-gateway
      - nats
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:4000/health"]
      interval: 30s
      timeout: 10s
      retries: 3
```

---

## 7. Testing

### 7.1 LiveView Tests

```elixir
defmodule UiWeb.DashboardLiveTest do
  use UiWeb.ConnCase
  import Phoenix.LiveViewTest
  
  test "displays dashboard metrics", %{conn: conn} do
    {:ok, view, html} = live(conn, "/app/dashboard")
    
    assert html =~ "Dashboard"
    assert html =~ "Throughput"
    assert html =~ "Latency"
  end
  
  test "updates metrics in real-time", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/app/dashboard")
    
    # Simulate metric update
    send(view.pid, {:metric_update, %{key: :throughput, value: 1500}})
    
    assert render(view) =~ "1500"
  end
end
```

### 7.2 Integration Tests

```elixir
defmodule UiWeb.MessagesIntegrationTest do
  use UiWeb.ConnCase
  import Phoenix.LiveViewTest
  
  test "creates message and shows in list", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/app/messages")
    
    view
    |> form("#message-form", message: %{
      tenant_id: "test",
      message_type: "chat",
      payload: "Hello"
    })
    |> render_submit()
    
    assert render(view) =~ "Hello"
  end
end
```

---

## 8. Migration Plan

### 8.1 Phase 1: Setup (2 days)
- ✅ Create Phoenix project
- ✅ Configure TailwindCSS
- ✅ Setup authentication (Guardian + OIDC)
- ✅ Configure C-Gateway client
- ✅ Basic layout

### 8.2 Phase 2: Core Pages (5 days)
- ✅ Dashboard
- ✅ Messages Management
- ✅ Policies Editor
- ✅ Extensions Registry
- ✅ Usage & Billing

### 8.3 Phase 3: Real-time (3 days)
- ✅ Phoenix Channels
- ✅ NATS subscriber
- ✅ Live updates
- ✅ Notifications

### 8.4 Phase 4: Deployment (2 days)
- ✅ Docker setup
- ✅ docker-compose integration
- ✅ Documentation
- ✅ Deprecate SvelteKit

---

## 9. Success Criteria

### Functional
- ✅ All pages from SvelteKit UI replicated
- ✅ Real-time updates working
- ✅ Authentication functional
- ✅ Policies Editor (visual + JSON)
- ✅ Extensions Registry UI

### Non-Functional
- ✅ Initial load < 2s
- ✅ Real-time updates < 100ms latency
- ✅ Mobile responsive
- ✅ Accessibility (WCAG 2.1 AA)

### Technical
- ✅ Hot reload working
- ✅ Tests passing (>80% coverage)
- ✅ Docker build successful
- ✅ Documentation complete

---

## 10. References

- Phoenix Framework: https://www.phoenixframework.org/
- Phoenix LiveView Guide: https://hexdocs.pm/phoenix_live_view/
- Elixir School: https://elixirschool.com/
- ADR-017: Phoenix LiveView UI Migration
