# UI-Web Implementation Plan
## Phoenix LiveView UI - Step-by-Step Guide

**Version**: 1.0  
**Date**: 2025-11-20  
**Estimated**: 12 days (96 hours)

---

## Phase 1: Project Setup (Day 1-2, 16h)

### Day 1: Phoenix Project Creation (8h)

#### Step 1.1: Create Phoenix Project (2h)
```bash
cd apps
mix phx.new ui_web --no-ecto --live
cd ui_web

# Answer prompts:
# Fetch and install dependencies? [Yn] Y
```

**Verify:**
```bash
mix phx.server
# Visit http://localhost:4000
```

#### Step 1.2: Configure TailwindCSS (2h)
```bash
# Already included in Phoenix 1.7+
# Customize: assets/tailwind.config.js
```

**Edit `assets/tailwind.config.js`:**
```javascript
module.exports = {
  content: [
    "./js/**/*.js",
    "../lib/ui_web.ex",
    "../lib/ui_web/**/*.*ex"
  ],
  theme: {
    extend: {
      colors: {
        brand: "#FD4F00",
      }
    },
  },
  plugins: [
    require("@tailwindcss/forms"),
    require("@tailwindcss/typography"),
  ]
}
```

#### Step 1.3: Add Dependencies (2h)

**Edit `mix.exs`:**
```elixir
defp deps do
  [
    {:phoenix, "~> 1.7.10"},
    {:phoenix_live_view, "~> 0.20.2"},
    {:phoenix_html, "~> 4.0"},
    {:phoenix_live_dashboard, "~> 0.8.3"},
    {:telemetry_metrics, "~> 0.6"},
    {:telemetry_poller, "~> 1.0"},
    {:jason, "~> 1.4"},
    {:dns_cluster, "~> 0.1.1"},
    {:bandit, "~> 1.0"},
    
    # Auth
    {:guardian, "~> 2.3"},
    {:ueberauth, "~> 0.10"},
    {:ueberauth_oidc, "~> 0.1"},
    
    # HTTP Client
    {:tesla, "~> 1.8"},
    {:hackney, "~> 1.18"},
    
    # NATS Client (optional)
    {:gnat, "~> 1.8"},
    
    # Utils
    {:timex, "~> 3.7"},
    {:number, "~> 1.0"}
  ]
end
```

```bash
mix deps.get
```

#### Step 1.4: Configure Environment (2h)

**Create `config/dev.exs` additions:**
```elixir
config :ui_web, UiWeb.Endpoint,
  http: [ip: {0, 0, 0, 0}, port: 4000],
  check_origin: false,
  code_reloader: true,
  debug_errors: true,
  secret_key_base: "YOUR_SECRET_KEY_BASE",
  watchers: [
    esbuild: {Esbuild, :install_and_run, [:ui_web, ~w(--sourcemap=inline --watch)]},
    tailwind: {Tailwind, :install_and_run, [:ui_web, ~w(--watch)]}
  ]

# C-Gateway client
config :ui_web, :gateway,
  url: "http://localhost:8081",
  timeout: 30_000

# NATS
config :ui_web, :nats,
  url: "nats://localhost:4222"
```

**Create `.env` file:**
```bash
export SECRET_KEY_BASE=$(mix phx.gen.secret)
export GATEWAY_URL=http://localhost:8081
export NATS_URL=nats://localhost:4222
export DATABASE_URL=postgresql://postgres:postgres@localhost:5432/beamline_dev
```

---

### Day 2: Authentication & Layout (8h)

#### Step 2.1: Setup Guardian (3h)

**Create `lib/ui_web/auth/guardian.ex`:**
```elixir
defmodule UiWeb.Auth.Guardian do
  use Guardian, otp_app: :ui_web

  def subject_for_token(%{id: id}, _claims) do
    {:ok, to_string(id)}
  end

  def resource_from_claims(%{"sub" => id}) do
    {:ok, %{id: id}}
  end
end
```

**Create `lib/ui_web/auth/pipeline.ex`:**
```elixir
defmodule UiWeb.Auth.Pipeline do
  use Guardian.Plug.Pipeline,
    otp_app: :ui_web,
    module: UiWeb.Auth.Guardian,
    error_handler: UiWeb.Auth.ErrorHandler

  plug Guardian.Plug.VerifySession, claims: %{"typ" => "access"}
  plug Guardian.Plug.VerifyHeader, claims: %{"typ" => "access"}
  plug Guardian.Plug.LoadResource, allow_blank: true
end
```

**Create `lib/ui_web/auth/error_handler.ex`:**
```elixir
defmodule UiWeb.Auth.ErrorHandler do
  import Plug.Conn
  import Phoenix.Controller

  def auth_error(conn, {type, _reason}, _opts) do
    conn
    |> put_flash(:error, "Authentication required")
    |> redirect(to: "/login")
    |> halt()
  end
end
```

#### Step 2.2: OIDC Integration (3h)

**Create `lib/ui_web/controllers/auth_controller.ex`:**
```elixir
defmodule UiWeb.AuthController do
  use UiWeb, :controller
  plug Ueberauth

  def login(conn, _params) do
    render(conn, :login)
  end

  def callback(%{assigns: %{ueberauth_auth: auth}} = conn, _params) do
    user = %{
      id: auth.uid,
      email: auth.info.email,
      name: auth.info.name
    }

    conn
    |> UiWeb.Auth.Guardian.Plug.sign_in(user)
    |> put_flash(:info, "Successfully authenticated")
    |> redirect(to: "/app/dashboard")
  end

  def callback(%{assigns: %{ueberauth_failure: _fails}} = conn, _params) do
    conn
    |> put_flash(:error, "Failed to authenticate")
    |> redirect(to: "/login")
  end

  def logout(conn, _params) do
    conn
    |> UiWeb.Auth.Guardian.Plug.sign_out()
    |> put_flash(:info, "Logged out")
    |> redirect(to: "/login")
  end
end
```

#### Step 2.3: Create Layout (2h)

**Create `lib/ui_web/components/layouts/app.html.heex`:**
```heex
<!DOCTYPE html>
<html lang="en" class="h-full bg-gray-100">
  <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <meta name="csrf-token" content={get_csrf_token()} />
    <.live_title suffix=" · BeamLine">
      <%= assigns[:page_title] || "UI" %>
    </.live_title>
    <link phx-track-static rel="stylesheet" href={~p"/assets/app.css"} />
    <script defer phx-track-static type="text/javascript" src={~p"/assets/app.js"}>
    </script>
  </head>
  <body class="h-full">
    <div class="min-h-full">
      <nav class="bg-gray-800">
        <div class="mx-auto max-w-7xl px-4 sm:px-6 lg:px-8">
          <div class="flex h-16 items-center justify-between">
            <div class="flex items-center">
              <div class="flex-shrink-0">
                <h1 class="text-white text-xl font-bold">BeamLine Constructor</h1>
              </div>
              <div class="ml-10 flex items-baseline space-x-4">
                <.link navigate={~p"/app/dashboard"} class="nav-link">Dashboard</.link>
                <.link navigate={~p"/app/messages"} class="nav-link">Messages</.link>
                <.link navigate={~p"/app/policies"} class="nav-link">Policies</.link>
                <.link navigate={~p"/app/extensions"} class="nav-link">Extensions</.link>
                <.link navigate={~p"/app/usage"} class="nav-link">Usage</.link>
              </div>
            </div>
            <div>
              <.link href={~p"/logout"} class="text-gray-300 hover:text-white">
                Logout
              </.link>
            </div>
          </div>
        </div>
      </nav>

      <main class="py-10">
        <div class="mx-auto max-w-7xl px-4 sm:px-6 lg:px-8">
          <.flash_group flash={@flash} />
          <%= @inner_content %>
        </div>
      </main>
    </div>
  </body>
</html>
```

---

## Phase 2: Core Pages (Day 3-7, 40h)

### Day 3: Dashboard (8h)

#### Step 3.1: Create Dashboard LiveView (4h)

**Create `lib/ui_web/live/dashboard_live.ex`:**
```elixir
defmodule UiWeb.DashboardLive do
  use UiWeb, :live_view

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      Phoenix.PubSub.subscribe(UiWeb.PubSub, "metrics")
      :timer.send_interval(5000, self(), :update_metrics)
    end

    {:ok,
     socket
     |> assign(:page_title, "Dashboard")
     |> assign(:metrics, fetch_metrics())
     |> assign(:components_health, fetch_components_health())}
  end

  @impl true
  def handle_info(:update_metrics, socket) do
    {:noreply,
     socket
     |> assign(:metrics, fetch_metrics())
     |> assign(:components_health, fetch_components_health())}
  end

  @impl true
  def handle_info({:metric_update, metric}, socket) do
    {:noreply, update(socket, :metrics, &Map.put(&1, metric.key, metric.value))}
  end

  defp fetch_metrics do
    # Call C-Gateway /metrics endpoint
    %{
      throughput: 1250,
      latency_p50: 8,
      latency_p95: 45,
      latency_p99: 120,
      error_rate: 0.5,
      connections: 42
    }
  end

  defp fetch_components_health do
    # Call health endpoints
    %{
      c_gateway: :healthy,
      router: :healthy,
      worker_caf: :healthy,
      nats: :healthy
    }
  end
end
```

#### Step 3.2: Create Dashboard Template (2h)

**Create `lib/ui_web/live/dashboard_live.html.heex`:**
```heex
<div class="dashboard">
  <.header>
    Dashboard
    <:subtitle>Real-time system metrics and health</:subtitle>
  </.header>

  <!-- Component Health -->
  <div class="mt-8">
    <h2 class="text-lg font-medium">System Health</h2>
    <div class="mt-4 grid grid-cols-4 gap-4">
      <.health_card name="C-Gateway" status={@components_health.c_gateway} />
      <.health_card name="Router" status={@components_health.router} />
      <.health_card name="Worker CAF" status={@components_health.worker_caf} />
      <.health_card name="NATS" status={@components_health.nats} />
    </div>
  </div>

  <!-- Metrics -->
  <div class="mt-8">
    <h2 class="text-lg font-medium">Metrics</h2>
    <div class="mt-4 grid grid-cols-3 gap-4">
      <.metric_card
        title="Throughput"
        value={@metrics.throughput}
        unit="req/s"
        trend="up"
      />
      <.metric_card
        title="Latency p95"
        value={@metrics.latency_p95}
        unit="ms"
        trend="stable"
      />
      <.metric_card title="Error Rate" value={@metrics.error_rate} unit="%" trend="down" />
    </div>
  </div>
</div>
```

#### Step 3.3: Create Components (2h)

**Create `lib/ui_web/components/dashboard_components.ex`:**
```elixir
defmodule UiWeb.DashboardComponents do
  use Phoenix.Component

  attr :name, :string, required: true
  attr :status, :atom, required: true

  def health_card(assigns) do
    ~H"""
    <div class="bg-white overflow-hidden shadow rounded-lg">
      <div class="p-5">
        <div class="flex items-center">
          <div class="flex-shrink-0">
            <.health_icon status={@status} />
          </div>
          <div class="ml-5 w-0 flex-1">
            <dl>
              <dt class="text-sm font-medium text-gray-500 truncate">
                <%= @name %>
              </dt>
              <dd class="flex items-baseline">
                <div class="text-2xl font-semibold text-gray-900">
                  <%= status_text(@status) %>
                </div>
              </dd>
            </dl>
          </div>
        </div>
      </div>
    </div>
    """
  end

  attr :title, :string, required: true
  attr :value, :any, required: true
  attr :unit, :string, default: ""
  attr :trend, :string, default: "stable"

  def metric_card(assigns) do
    ~H"""
    <div class="bg-white overflow-hidden shadow rounded-lg">
      <div class="p-5">
        <dt class="text-sm font-medium text-gray-500 truncate">
          <%= @title %>
        </dt>
        <dd class="mt-1 flex items-baseline justify-between">
          <div class="flex items-baseline text-2xl font-semibold text-gray-900">
            <%= @value %> <span class="ml-2 text-sm text-gray-500"><%= @unit %></span>
          </div>
          <.trend_indicator trend={@trend} />
        </dd>
      </div>
    </div>
    """
  end

  defp health_icon(%{status: :healthy}) do
    ~H"""
    <svg class="h-6 w-6 text-green-400" fill="currentColor" viewBox="0 0 20 20">
      <path
        fill-rule="evenodd"
        d="M10 18a8 8 0 100-16 8 8 0 000 16zm3.707-9.293a1 1 0 00-1.414-1.414L9 10.586 7.707 9.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l4-4z"
        clip-rule="evenodd"
      />
    </svg>
    """
  end

  defp health_icon(%{status: _}) do
    ~H"""
    <svg class="h-6 w-6 text-red-400" fill="currentColor" viewBox="0 0 20 20">
      <path
        fill-rule="evenodd"
        d="M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z"
        clip-rule="evenodd"
      />
    </svg>
    """
  end

  defp status_text(:healthy), do: "Healthy"
  defp status_text(:degraded), do: "Degraded"
  defp status_text(:unhealthy), do: "Unhealthy"

  defp trend_indicator(%{trend: "up"}) do
    ~H"""
    <span class="text-green-600">↑</span>
    """
  end

  defp trend_indicator(%{trend: "down"}) do
    ~H"""
    <span class="text-red-600">↓</span>
    """
  end

  defp trend_indicator(_) do
    ~H"""
    <span class="text-gray-400">→</span>
    """
  end
end
```

---

### Day 4-5: Messages Management (16h)

#### Step 4.1: Create Messages Context (4h)

**Create `lib/ui_web/gateway/client.ex`:**
```elixir
defmodule UiWeb.Gateway.Client do
  use Tesla

  plug Tesla.Middleware.BaseUrl, gateway_url()
  plug Tesla.Middleware.JSON
  plug Tesla.Middleware.Headers, [{"content-type", "application/json"}]
  plug Tesla.Middleware.Timeout, timeout: 30_000

  def list_messages(params \\ %{}) do
    get("/api/v1/messages", query: params)
  end

  def get_message(id) do
    get("/api/v1/messages/#{id}")
  end

  def create_message(params) do
    post("/api/v1/messages", params)
  end

  defp gateway_url do
    Application.get_env(:ui_web, :gateway)[:url]
  end
end
```

#### Step 4.2: Create Messages LiveView (6h)

**Create `lib/ui_web/live/messages_live/index.ex`:**
```elixir
defmodule UiWeb.MessagesLive.Index do
  use UiWeb, :live_view
  alias UiWeb.Gateway.Client

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      Phoenix.PubSub.subscribe(UiWeb.PubSub, "messages")
    end

    {:ok,
     socket
     |> assign(:page_title, "Messages")
     |> assign(:messages, [])
     |> assign(:page, 1)
     |> assign(:filter, %{})
     |> load_messages()}
  end

  @impl true
  def handle_event("filter", %{"filter" => filter}, socket) do
    {:noreply,
     socket
     |> assign(:filter, filter)
     |> assign(:page, 1)
     |> load_messages()}
  end

  @impl true
  def handle_event("next_page", _, socket) do
    {:noreply,
     socket
     |> update(:page, &(&1 + 1))
     |> load_messages()}
  end

  @impl true
  def handle_info({:message_created, message}, socket) do
    {:noreply, update(socket, :messages, &[message | &1])}
  end

  @impl true
  def handle_info({:message_updated, message}, socket) do
    {:noreply, update(socket, :messages, &update_message(&1, message))}
  end

  defp load_messages(socket) do
    params = Map.merge(socket.assigns.filter, %{page: socket.assigns.page})

    case Client.list_messages(params) do
      {:ok, %{body: messages}} ->
        assign(socket, :messages, messages)

      {:error, _} ->
        put_flash(socket, :error, "Failed to load messages")
    end
  end

  defp update_message(messages, updated) do
    Enum.map(messages, fn msg ->
      if msg.id == updated.id, do: updated, else: msg
    end)
  end
end
```

#### Step 4.3: Create Messages Template (4h)

**Create `lib/ui_web/live/messages_live/index.html.heex`:**
```heex
<div class="messages">
  <.header>
    Messages
    <:actions>
      <.link navigate={~p"/app/messages/new"} class="button">
        New Message
      </.link>
    </:actions>
  </.header>

  <!-- Filters -->
  <div class="mt-4">
    <.form for={%{}} phx-change="filter" class="flex gap-4">
      <.input type="text" name="filter[tenant_id]" placeholder="Tenant ID" />
      <.input type="select" name="filter[status]" options={["all", "pending", "completed", "failed"]} />
      <.input type="text" name="filter[trace_id]" placeholder="Trace ID" />
    </.form>
  </div>

  <!-- Messages Table -->
  <div class="mt-8">
    <table class="min-w-full divide-y divide-gray-300">
      <thead>
        <tr>
          <th>Message ID</th>
          <th>Tenant</th>
          <th>Type</th>
          <th>Status</th>
          <th>Created</th>
          <th>Actions</th>
        </tr>
      </thead>
      <tbody>
        <%= for message <- @messages do %>
          <tr>
            <td><%= message.message_id %></td>
            <td><%= message.tenant_id %></td>
            <td><%= message.message_type %></td>
            <td>
              <.badge status={message.status} />
            </td>
            <td><%= format_datetime(message.created_at) %></td>
            <td>
              <.link navigate={~p"/app/messages/#{message.id}"}>View</.link>
            </td>
          </tr>
        <% end %>
      </tbody>
    </table>
  </div>

  <!-- Pagination -->
  <div class="mt-4 flex justify-between">
    <button phx-click="prev_page" disabled={@page == 1}>Previous</button>
    <span>Page <%= @page %></span>
    <button phx-click="next_page">Next</button>
  </div>
</div>
```

#### Step 4.4: Create Message Detail Page (2h)

**Create `lib/ui_web/live/messages_live/show.ex`** (similar structure)

---

### Day 6-7: Policies & Extensions (16h)

**(Continue with similar pattern for Policies Editor and Extensions Registry)**

---

## Phase 3: Real-time Features (Day 8-10, 24h)

### Day 8: Phoenix Channels (8h)

#### Step 8.1: Setup Channels (4h)

**Create `lib/ui_web/channels/user_socket.ex`:**
```elixir
defmodule UiWeb.UserSocket do
  use Phoenix.Socket

  channel "metrics:*", UiWeb.MetricsChannel
  channel "messages:*", UiWeb.MessagesChannel

  def connect(%{"token" => token}, socket, _connect_info) do
    case UiWeb.Auth.Guardian.decode_and_verify(token) do
      {:ok, claims} ->
        {:ok, assign(socket, :user_id, claims["sub"])}

      {:error, _} ->
        :error
    end
  end

  def id(socket), do: "user_socket:#{socket.assigns.user_id}"
end
```

#### Step 8.2: Create Channels (4h)

**Create `lib/ui_web/channels/metrics_channel.ex`:**
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
end
```

---

### Day 9-10: NATS Integration & Live Updates (16h)

#### Step 9.1: NATS Subscriber (8h)

**Create `lib/ui_web/nats/subscriber.ex`:**
```elixir
defmodule UiWeb.Nats.Subscriber do
  use GenServer
  require Logger

  def start_link(_) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init(_) do
    {:ok, conn} = Gnat.start_link(%{
      host: nats_host(),
      port: nats_port()
    })

    # Subscribe to message updates
    {:ok, _} = Gnat.sub(conn, self(), "beamline.messages.updates.v1")

    # Subscribe to metrics
    {:ok, _} = Gnat.sub(conn, self(), "beamline.metrics.v1")

    {:ok, %{conn: conn}}
  end

  def handle_info({:msg, %{body: body, topic: "beamline.messages.updates.v1"}}, state) do
    message = Jason.decode!(body)

    Phoenix.PubSub.broadcast(
      UiWeb.PubSub,
      "messages",
      {:message_updated, message}
    )

    {:noreply, state}
  end

  def handle_info({:msg, %{body: body, topic: "beamline.metrics.v1"}}, state) do
    metric = Jason.decode!(body)

    Phoenix.PubSub.broadcast(
      UiWeb.PubSub,
      "metrics",
      {:metric_update, metric}
    )

    {:noreply, state}
  end

  defp nats_host, do: Application.get_env(:ui_web, :nats)[:host] || "localhost"
  defp nats_port, do: Application.get_env(:ui_web, :nats)[:port] || 4222
end
```

#### Step 9.2: Add to Supervision Tree (2h)

**Edit `lib/ui_web/application.ex`:**
```elixir
def start(_type, _args) do
  children = [
    UiWeb.Telemetry,
    {Phoenix.PubSub, name: UiWeb.PubSub},
    UiWeb.Endpoint,
    UiWeb.Nats.Subscriber  # Add this
  ]

  opts = [strategy: :one_for_one, name: UiWeb.Supervisor]
  Supervisor.start_link(children, opts)
end
```

---

## Phase 4: Deployment (Day 11-12, 16h)

### Day 11: Docker & docker-compose (8h)

#### Step 11.1: Create Dockerfile (4h)

**(See Technical Spec section 6.1)**

#### Step 11.2: Update docker-compose.yml (4h)

**(See Technical Spec section 6.2)**

---

### Day 12: Documentation & Migration (8h)

#### Step 12.1: Update Documentation (4h)

- Update `README.md`
- Update `docs/CORE_COMPONENTS.md`
- Create `apps/ui-web/README.md`
- Update `.trae/state.json`

#### Step 12.2: Deprecate SvelteKit (2h)

- Move `frontend/` to `deprecated-solutions/frontend-sveltekit/`
- Update `apps/ui/` placeholder
- Update references in docs

#### Step 12.3: Final Testing (2h)

```bash
# Run tests
mix test

# Build Docker
docker build -t beamline/ui-web .

# Test docker-compose
docker-compose up ui-web
```

---

## Success Checklist

### Phase 1 ✅
- [ ] Phoenix project created
- [ ] TailwindCSS configured
- [ ] Authentication working (Guardian + OIDC)
- [ ] C-Gateway client configured
- [ ] Basic layout complete

### Phase 2 ✅
- [ ] Dashboard page working
- [ ] Messages Management complete
- [ ] Policies Editor (JSON mode)
- [ ] Extensions Registry UI
- [ ] Usage & Billing pages

### Phase 3 ✅
- [ ] Phoenix Channels setup
- [ ] NATS subscriber working
- [ ] Real-time updates functional
- [ ] Notifications system

### Phase 4 ✅
- [ ] Docker build successful
- [ ] docker-compose integration
- [ ] Documentation updated
- [ ] SvelteKit deprecated
- [ ] Tests passing (>80% coverage)

---

## Next Steps After Completion

1. **Visual Policy Editor** (drag-and-drop) - 3 days
2. **Advanced Extensions Features** - 2 days
3. **Performance Optimization** - 2 days
4. **Production Deployment** - 1 day

**Total**: 12 days base + 8 days advanced = **20 days to full feature parity**
