# Phase 1: Project Setup (Day 1–2, 16 hours)

## Day 1: Foundation (8 hours)

### Step 1.1: Install Elixir & Erlang (1h)

#### Ubuntu/Debian

```bash
# 1) Add Erlang Solutions repository
wget https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb
sudo dpkg -i erlang-solutions_2.0_all.deb
sudo apt-get update

# 2) Install Erlang/OTP 26
sudo apt-get install -y esl-erlang

# 3) Install Elixir 1.15+
sudo apt-get install -y elixir

# 4) Verify installation
elixir --version
# Expected:
# Erlang/OTP 26 [erts-14.x] [...]
# Elixir 1.15.x (compiled with Erlang/OTP 26)

erl -version
# Expected: Erlang (BEAM) emulator version 14.x
```

#### macOS

```bash
# Using Homebrew
brew install elixir

# Verify
elixir --version
```

#### Troubleshooting

```bash
# If elixir command not found:
export PATH="$PATH:/usr/local/bin"
echo 'export PATH="$PATH:/usr/local/bin"' >> ~/.bashrc
source ~/.bashrc

# Check Erlang is installed:
which erl
# Should output: /usr/bin/erl or /usr/local/bin/erl
```

---

### Step 1.2: Install Phoenix Framework (30min)

```bash
# 1) Install Hex package manager
mix local.hex --force

# 2) Install Rebar (Erlang build tool)
mix local.rebar --force

# 3) Install Phoenix project generator
mix archive.install hex phx_new --force

# 4) Verify Phoenix installation
mix phx.new --version
# Expected: Phoenix installer v1.7.10
```

What is Mix?
- Mix = Elixir's build tool (like npm/cargo/maven)
- Commands:
  - `mix deps.get` — install dependencies
  - `mix compile` — compile project
  - `mix test` — run tests
  - `mix phx.server` — start Phoenix server

---

### Step 1.3: Create Phoenix Project (1h)

```bash
# 1) Navigate to apps directory
cd /home/rustkas/aigroup/apps

# 2) Create Phoenix project
mix phx.new ui_web --no-ecto --live

# Flags:
# --no-ecto  → no DB (use Gateway API)
# --live     → include Phoenix LiveView

# 3) Answer prompts:
# Fetch and install dependencies? [Yn] Y
```

What gets created:
```
apps/ui_web/
├── assets/
│   ├── css/
│   │   └── app.css
│   ├── js/
│   │   └── app.js
│   └── vendor/
├── lib/
│   ├── ui_web/
│   │   ├── components/
│   │   ├── controllers/
│   │   ├── endpoint.ex
│   │   ├── router.ex
│   │   └── telemetry.ex
│   ├── ui_web.ex
│   └── ui_web_application.ex
├── test/
├── config/
│   ├── config.exs
│   ├── dev.exs
│   ├── prod.exs
│   └── test.exs
├── mix.exs
└── README.md
```

---

### Step 1.4: Initial Configuration (1h)

#### A. Configure Environment Variables

```bash
# 1) Create .env file
cd /home/rustkas/aigroup/apps/ui_web
cat > .env << 'EOF'
# Phoenix
export SECRET_KEY_BASE=$(mix phx.gen.secret)
export PHX_HOST=localhost
export PHX_PORT=4000

# C-Gateway
export GATEWAY_URL=http://localhost:8081

# NATS
export NATS_URL=nats://localhost:4222

# OIDC (later)
export OIDC_DISCOVERY_URL=https://your-oidc-provider.com/.well-known/openid-configuration
export OIDC_CLIENT_ID=your-client-id
export OIDC_CLIENT_SECRET=your-client-secret
export OIDC_REDIRECT_URI=http://localhost:4000/auth/callback
EOF

# 2) Generate secret key (optional manual replacement)
mix phx.gen.secret
# Copy output to .env → export SECRET_KEY_BASE=<generated_secret>

# 3) Load environment
source .env
```

#### B. Update `config/dev.exs`

```elixir
import Config

# Endpoint
config :ui_web, UiWeb.Endpoint,
  http: [ip: {0, 0, 0, 0}, port: String.to_integer(System.get_env("PHX_PORT") || "4000")],
  check_origin: false,
  code_reloader: true,
  debug_errors: true,
  secret_key_base: System.get_env("SECRET_KEY_BASE"),
  watchers: [
    esbuild: {Esbuild, :install_and_run, [:ui_web, ~w(--sourcemap=inline --watch)]},
    tailwind: {Tailwind, :install_and_run, [:ui_web, ~w(--watch)]}
  ],
  live_reload: [
    patterns: [
      ~r"priv/static/(?!uploads/).*(js|css|png|jpeg|jpg|gif|svg)$",
      ~r"priv/gettext/.*(po)$",
      ~r"lib/ui_web/(controllers|live|components)/.*(ex|heex)$"
    ]
  ]

# C-Gateway client
config :ui_web, :gateway,
  url: System.get_env("GATEWAY_URL") || "http://localhost:8081",
  timeout: 30_000

# NATS
config :ui_web, :nats,
  url: System.get_env("NATS_URL") || "nats://localhost:4222"

# Logger
config :logger, :console, format: "[$level] $message\n"

# Phoenix dev
config :phoenix, :stacktrace_depth, 20
config :phoenix, :plug_init_mode, :runtime
```

---

### Step 1.5: First Run & Verification (30min)

```bash
# 1) Install deps
cd /home/rustkas/aigroup/apps/ui_web
mix deps.get

# 2) Install Node assets
cd assets && npm install && cd ..

# 3) Start Phoenix server
source .env
mix phx.server

# Expected:
# [info] Running UiWeb.Endpoint with Bandit at 0.0.0.0:4000
# [info] Access UiWeb.Endpoint at http://localhost:4000
# [watch] build finished, watching for changes...
```

Open: `http://localhost:4000` — Phoenix welcome page.

Hot reload test:
```bash
# Edit lib/ui_web/controllers/page_html/home.html.heex → change text
# Browser auto-reloads
```

---

## Day 2: Authentication & Layout (8 hours)

### Step 2.1: Add Dependencies (30min)

Edit `mix.exs` deps:

```elixir
defp deps do
  [
    {:phoenix, "~> 1.7.10"},
    {:phoenix_html, "~> 4.0"},
    {:phoenix_live_reload, "~> 1.4", only: :dev},
    {:phoenix_live_view, "~> 0.20.2"},
    {:floki, ">= 0.30.0", only: :test},
    {:phoenix_live_dashboard, "~> 0.8.3"},
    {:esbuild, "~> 0.8", runtime: Mix.env() == :dev},
    {:tailwind, "~> 0.2", runtime: Mix.env() == :dev},
    {:heroicons, github: "tailwindlabs/heroicons", tag: "v2.1.1", sparse: "optimized", app: false, compile: false, depth: 1},
    {:telemetry_metrics, "~> 0.6"},
    {:telemetry_poller, "~> 1.0"},
    {:gettext, "~> 0.20"},
    {:jason, "~> 1.4"},
    {:dns_cluster, "~> 0.1.1"},
    {:bandit, "~> 1.0"},

    # Authentication
    {:guardian, "~> 2.3"},
    {:ueberauth, "~> 0.10"},
    {:ueberauth_oidc, "~> 0.1"},
    {:jose, "~> 1.11"},

    # HTTP Client
    {:tesla, "~> 1.8"},
    {:hackney, "~> 1.18"},
    {:mint, "~> 1.5"},

    # NATS Client (optional for Phase 3)
    {:gnat, "~> 1.8"},

    # Utilities
    {:timex, "~> 3.7"},
    {:number, "~> 1.0"}
  ]
end
```

Install & compile:
```bash
mix deps.get
mix deps.compile
```

---

### Step 2.2: Setup Guardian (JWT) (2h)

Create `lib/ui_web/auth/guardian.ex`:

```elixir
defmodule UiWeb.Auth.Guardian do
  use Guardian, otp_app: :ui_web

  def subject_for_token(%{id: id}, _claims), do: {:ok, to_string(id)}
  def subject_for_token(_, _), do: {:error, :no_id_provided}

  def resource_from_claims(%{"sub" => id}), do: {:ok, %{id: id, email: "user@example.com"}}
  def resource_from_claims(_), do: {:error, :no_claims_sub}
end
```

Create `lib/ui_web/auth/pipeline.ex`:

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

Create `lib/ui_web/auth/error_handler.ex`:

```elixir
defmodule UiWeb.Auth.ErrorHandler do
  import Plug.Conn
  import Phoenix.Controller
  @behaviour Guardian.Plug.ErrorHandler

  @impl Guardian.Plug.ErrorHandler
  def auth_error(conn, {type, _reason}, _opts) do
    body = Jason.encode!(%{error: to_string(type)})
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(401, body)
  end
end
```

Config in `config/config.exs`:

```elixir
config :ui_web, UiWeb.Auth.Guardian,
  issuer: "ui_web",
  secret_key: System.get_env("GUARDIAN_SECRET_KEY") || "dev-only-secret-key"
```

Generate secret:
```bash
mix guardian.gen.secret
# Add to .env → export GUARDIAN_SECRET_KEY=<secret>
```

---

### Step 2.3: Setup OIDC (Ueberauth) (2h)

Create `lib/ui_web/controllers/auth_controller.ex`:

```elixir
defmodule UiWeb.AuthController do
  use UiWeb, :controller
  plug Ueberauth
  alias UiWeb.Auth.Guardian

  def login(conn, _params), do: render(conn, :login)

  def callback(%{assigns: %{ueberauth_auth: auth}} = conn, _params) do
    user_params = %{id: auth.uid, email: auth.info.email, name: auth.info.name}
    conn
    |> Guardian.Plug.sign_in(user_params)
    |> put_flash(:info, "Successfully authenticated")
    |> redirect(to: ~p"/app/dashboard")
  end

  def callback(%{assigns: %{ueberauth_failure: _fails}} = conn, _params) do
    conn
    |> put_flash(:error, "Failed to authenticate")
    |> redirect(to: ~p"/login")
  end

  def logout(conn, _params) do
    conn
    |> Guardian.Plug.sign_out()
    |> put_flash(:info, "Logged out successfully")
    |> redirect(to: ~p"/login")
  end
end
```

Create `lib/ui_web/controllers/auth_html/login.html.heex`:

```heex
<div class="flex min-h-full flex-col justify-center py-12 sm:px-6 lg:px-8">
  <div class="sm:mx-auto sm:w-full sm:max-w-md">
    <h2 class="mt-6 text-center text-3xl font-bold tracking-tight text-gray-900">
      Sign in to BeamLine Constructor
    </h2>
  </div>
  <div class="mt-8 sm:mx-auto sm:w-full sm:max-w-md">
    <div class="bg-white py-8 px-4 shadow sm:rounded-lg sm:px-10">
      <a href={~p"/auth/oidc"} class="flex w-full justify-center rounded-md bg-indigo-600 px-3 py-2 text-sm font-semibold text-white shadow-sm hover:bg-indigo-500 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-indigo-600">
        Sign in with OIDC
      </a>
    </div>
  </div>
</div>
```

Config in `config/config.exs`:

```elixir
config :ueberauth, Ueberauth,
  providers: [
    oidc: {Ueberauth.Strategy.OIDC, [
      default: [
        client_id: System.get_env("OIDC_CLIENT_ID"),
        client_secret: System.get_env("OIDC_CLIENT_SECRET"),
        discovery_document_uri: System.get_env("OIDC_DISCOVERY_URL"),
        redirect_uri: System.get_env("OIDC_REDIRECT_URI"),
        response_type: "code",
        scope: "openid profile email"
      ]
    ]}
  ]
```

---

### Step 2.4: Update Router (1h)

Edit `lib/ui_web/router.ex`:

```elixir
defmodule UiWeb.Router do
  use UiWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, html: {UiWeb.Layouts, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :auth do
    plug UiWeb.Auth.Pipeline
  end

  pipeline :ensure_auth do
    plug Guardian.Plug.EnsureAuthenticated
  end

  scope "/", UiWeb do
    pipe_through :browser
    get "/", PageController, :home
    get "/login", AuthController, :login
  end

  scope "/auth", UiWeb do
    pipe_through :browser
    get "/:provider", AuthController, :request
    get "/:provider/callback", AuthController, :callback
    post "/:provider/callback", AuthController, :callback
    get "/logout", AuthController, :logout
  end

  scope "/app", UiWeb do
    pipe_through [:browser, :auth, :ensure_auth]
    live "/dashboard", DashboardLive, :index
  end

  if Application.compile_env(:ui_web, :dev_routes) do
    import Phoenix.LiveDashboard.Router
    scope "/dev" do
      pipe_through :browser
      live_dashboard "/dashboard", metrics: UiWeb.Telemetry
    end
  end
end
```

---

### Step 2.5: Create Basic Layout (2h)

Update `lib/ui_web/components/layouts/root.html.heex`:

```heex
<!DOCTYPE html>
<html lang="en" class="h-full bg-gray-100">
  <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <meta name="csrf-token" content={get_csrf_token()} />
    <.live_title suffix=" · BeamLine Constructor">
      <%= assigns[:page_title] || "UI" %>
    </.live_title>
    <link phx-track-static rel="stylesheet" href={~p"/assets/app.css"} />
    <script defer phx-track-static type="text/javascript" src={~p"/assets/app.js"}></script>
  </head>
  <body class="h-full">
    <%= @inner_content %>
  </body>
</html>
```

Create `lib/ui_web/components/layouts/app.html.heex`:

```heex
<div class="min-h-full">
  <nav class="bg-gray-800">
    <div class="mx-auto max-w-7xl px-4 sm:px-6 lg:px-8">
      <div class="flex h-16 items-center justify-between">
        <div class="flex items-center">
          <div class="flex-shrink-0">
            <h1 class="text-white text-xl font-bold">BeamLine Constructor</h1>
          </div>
          <div class="ml-10 flex items-baseline space-x-4">
            <.link navigate={~p"/app/dashboard"} class="text-gray-300 hover:bg-gray-700 hover:text-white rounded-md px-3 py-2 text-sm font-medium">Dashboard</.link>
            <.link navigate={~p"/app/messages"} class="text-gray-300 hover:bg-gray-700 hover:text-white rounded-md px-3 py-2 text-sm font-medium">Messages</.link>
            <.link navigate={~p"/app/policies"} class="text-gray-300 hover:bg-gray-700 hover:text-white rounded-md px-3 py-2 text-sm font-medium">Policies</.link>
            <.link navigate={~p"/app/extensions"} class="text-gray-300 hover:bg-gray-700 hover:text-white rounded-md px-3 py-2 text-sm font-medium">Extensions</.link>
            <.link navigate={~p"/app/usage"} class="text-gray-300 hover:bg-gray-700 hover:text-white rounded-md px-3 py-2 text-sm font-medium">Usage</.link>
          </div>
        </div>
        <div>
          <.link href={~p"/auth/logout"} class="text-gray-300 hover:bg-gray-700 hover:text-white rounded-md px-3 py-2 text-sm font-medium">Logout</.link>
        </div>
      </div>
    </div>
  </nav>
  <.flash_group flash={@flash} />
  <main class="py-10">
    <div class="mx-auto max-w-7xl px-4 sm:px-6 lg:px-8">
      <%= @inner_content %>
    </div>
  </main>
</div>
```

---

### Step 2.6: Configure TailwindCSS (1h)

Update `assets/tailwind.config.js`:

```javascript
module.exports = {
  content: [
    "./js/**/*.js",
    "../lib/ui_web.ex",
    "../lib/ui_web/**/*.*ex"
  ],
  theme: {
    extend: {
      colors: { brand: "#FD4F00" }
    }
  },
  plugins: [
    require("@tailwindcss/forms"),
    require("@tailwindcss/typography"),
  ]
}
```

Install plugins:
```bash
cd assets
npm install -D @tailwindcss/forms @tailwindcss/typography
cd ..
```

Update `assets/css/app.css`:

```css
@import "tailwindcss/base";
@import "tailwindcss/components";
@import "tailwindcss/utilities";

/* Custom styles */
.nav-link { @apply text-gray-300 hover:bg-gray-700 hover:text-white rounded-md px-3 py-2 text-sm font-medium; }
.button { @apply inline-flex items-center rounded-md bg-indigo-600 px-3 py-2 text-sm font-semibold text-white shadow-sm hover:bg-indigo-500 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-indigo-600; }
```

---

### Step 2.7: Final Verification (30min)

```bash
mix compile
source .env
mix phx.server
# Routes:
# - http://localhost:4000 (home)
# - http://localhost:4000/login (login)
# - http://localhost:4000/dev/dashboard (LiveDashboard)
```

Hot reload: edit any `.heex` → browser auto‑reloads.

---

## ✅ Phase 1 Complete Checklist

- [ ] Elixir/Erlang installed (`elixir --version`)
- [ ] Phoenix installed (`mix phx.new --version`)
- [ ] Project created (`apps/ui_web/`)
- [ ] Dependencies installed (`mix deps.get`)
- [ ] Environment configured (`apps/ui_web/.env`)
- [ ] Guardian setup (JWT)
- [ ] OIDC configured (Ueberauth)
- [ ] Router updated (auth pipelines)
- [ ] Layouts created (root + app)
- [ ] TailwindCSS configured
- [ ] Server runs (`mix phx.server`)
- [ ] Hot reload works
- [ ] Login page accessible
- [ ] Navigation visible

Estimated time: 16 hours (2 days)