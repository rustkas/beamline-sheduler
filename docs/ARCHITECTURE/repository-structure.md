---
version: 1.1
authors:
  - Agent 2: Architecture/Tech Lead
last_update: 2025-11-18T11:30:00Z
status: approved
changelog:
  - 2025-11-18: Updated structure to reflect C-Gateway (C11) instead of TypeScript/NestJS Gateway
---

# Repository Structure

## Purpose

Detailed repository structure for BeamLine Constructor with description of each directory and module purpose.

## Full Structure

```
aigroup/
├── apps/                          # Applications
│   ├── otp/                       # Erlang/OTP applications
│   │   ├── router/                # Router core (Erlang)
│   │   │   ├── src/
│   │   │   │   ├── router_core.erl        # Core routing logic
│   │   │   │   ├── router_decider.erl     # Decision making
│   │   │   │   ├── router_policy.erl       # Policy evaluation
│   │   │   │   ├── router_grpc.erl        # gRPC service
│   │   │   │   └── router_nats.erl         # NATS client
│   │   │   ├── test/
│   │   │   │   ├── router_core_test.erl
│   │   │   │   └── router_policy_test.erl
│   │   │   ├── rebar.config
│   │   │   └── README.md
│   │   ├── provider/              # Provider adapters (Erlang)
│   │   │   ├── src/
│   │   │   │   ├── provider_core.erl
│   │   │   │   ├── provider_adapters/
│   │   │   │   │   ├── openai_adapter.erl
│   │   │   │   │   ├── anthropic_adapter.erl
│   │   │   │   │   └── google_adapter.erl
│   │   │   │   └── provider_grpc.erl
│   │   │   └── rebar.config
│   │   └── usage/                 # Usage metrics (Erlang)
│   │       ├── src/
│   │       │   ├── usage_collector.erl
│   │       │   ├── usage_aggregator.erl
│   │       │   └── usage_nats.erl
│   │       └── rebar.config
│   ├── c-gateway/                 # C11 HTTP Gateway
│   │   ├── src/
│   │   │   ├── main.c
│   │   │   ├── http_server.c
│   │   │   ├── nats_client_real.c
│   │   │   └── nats_client_stub.c
│   │   ├── tests/
│   │   ├── CMakeLists.txt
│   │   └── Dockerfile
│   ├── caf/                       # C++ Actor Framework
│   │   └── processor/             # Message processor (C++)
│   │       ├── src/
│   │       │   ├── processor_main.cpp
│   │       │   └── processor_actor.cpp
│   │       └── CMakeLists.txt
│   └── ui_web/                    # Phoenix LiveView UI
│       ├── assets/
│       │   ├── css/
│       │   │   └── app.css        # TailwindCSS
│       │   └── js/
│       │       └── app.js         # JS entry (esbuild)
│       └── lib/
│           ├── ui_web/            # domain layer (auth, application)
│           └── ui_web_web/        # web layer (controllers, components, live)
│
├── proto/                         # Protocol buffer definitions (Source of Truth)
│   ├── beamline/
│   │   ├── flow/
│   │   │   └── v1/
│   │   │       └── flow.proto     # Router ABI
│   │   └── provider/
│   │       └── v1/
│   │           └── provider.proto # Provider ABI
│   └── buf.yaml                   # Buf configuration
│
├── sql/                           # Database schemas (Source of Truth)
│   ├── 000_init.sql               # Initial DDL
│   ├── 001_policies.sql           # Policies tables
│   └── migrations/                # Migration scripts
│
├── docs/                          # Documentation
│   ├── BEAMLINE_VISION_AND_ARCHITECTURE.md # BeamLine vision & architecture overview
│   ├── ROADMAP.md                 # Project roadmap (CP3+, Infra, Ops)
│   ├── specs/                     # Core specifications and contracts
│   ├── guides/                    # Developer guides and checklists
│   ├── archive/                   # Archived documentation and reports
│   │   ├── dev/                   # Historical development reports
│   │   └── ...                    # Legacy specs
│   ├── ADR/                       # Architecture Decision Records
│   │   ├── TEMPLATE.md
│   │   ├── ADR-001-monorepo-structure.md
│   │   ├── ADR-002-state-management.md
│   │   ├── ADR-004-erlang-otp-router.md
│   │   ├── ADR-005-mnesia-caching.md
│   │   ├── ADR-006-nats-inter-service-communication.md
│   │   └── ADR-010-target-architecture.md
│   ├── ARCHITECTURE/              # Architecture specifications
│   │   ├── context-maps.md        # Context maps and diagrams
│   │   ├── api-registry.md        # API endpoint registry
│   │   └── repository-structure.md # This file
│   ├── ROUTING_POLICY.md          # Routing policy JSON-DSL
│   ├── CI_CD.md                   # CI/CD pipelines
│   ├── OBSERVABILITY.md           # Metrics, tracing, SLO
│   ├── NATS_SUBJECTS.md           # NATS subjects catalog
│   ├── GATEWAY_ROUTES.md          # Gateway API endpoints
│   ├── UI_ROUTES.md               # UI routes specification
│   ├── STATE.schema.json          # State validation schema
│   ├── HISTORY.schema.json         # History validation schema
│   └── CP_TRANSITION_GUIDE.md     # CP transition process
│
├── scripts/                       # Utility scripts
│   ├── validate_state.sh          # State validation
│   ├── verify_hmac_chain.py        # HMAC chain verification
│   ├── check_schema_changes.sh    # Schema changes validation
│   ├── check_hmac_masking.sh      # HMAC masking check
│   └── dry_run_ci.sh              # CI dry-run scenarios
│
├── .trae/                         # State and audit (Source of Truth)
│   ├── state.json                 # Current project state
│   ├── history.json               # Append-only audit trail
│   ├── manifest.json              # Schema versions and validation rules
│   └── drafts/                    # Draft entries (if no audit_secret)
│
├── .cursor/                       # Cursor IDE rules
│   └── rules/
│       ├── agents/                # Agent-specific rules
│       │   ├── agent-2-architecture-tech-lead.mdc
│       │   └── state-and-history-management.mdc
│       └── core/                  # Shared rules
│
├── .github/                       # GitHub workflows
│   └── workflows/
│       ├── build.yml.template
│       ├── test.yml.template
│       └── docker.yml.template
│
├── tests/                         # Cross-component tests
│   ├── unit/                      # Unit tests
│   ├── integration/               # Integration tests
│   └── e2e/                       # End-to-end tests
│
├── docker-compose.yml             # Local development stack
├── README.md                      # Project overview
└── LICENSE                        # License file
```

## Modules and Their Purpose

### apps/otp/router/

**Purpose**: Routing core on Erlang/OTP

**Key modules**:
- `router_core.erl`: Core routing logic
- `router_decider.erl`: Routing decision making
- `router_policy.erl`: Routing policy evaluation
- `router_grpc.erl`: gRPC service Router.Decide
- `router_nats.erl`: NATS client for communication with Gateway

**Dependencies**:
- PostgreSQL (epgsql/pgo) for loading policies
- Mnesia for policy caching
- NATS for communication with Gateway and Extensions

**Failure Handling**: Behavior during NATS publish/publish_with_ack errors is explicitly specified in `apps/otp/router/docs/NATS_PUBLISH_FAILURE_BEHAVIOR.md` and `apps/otp/router/docs/NATS_PUBLISH_FAILURE_MONITORING.md`.

> **Note**: Provider adapters (OpenAI, Anthropic, etc.) are implemented as **Custom Provider Extensions** (separate NATS services registered in Extension Registry). Router communicates with them via NATS subjects like `beamline.provider.{id}.v1`. See `docs/EXTENSIONS_API.md`.

### apps/c-gateway/

**Purpose**: HTTP REST API Gateway on C11

**Key modules**:
- `main.c`: Entry point
- `http_server.c`: HTTP server implementation with REST endpoints
- `nats_client_real.c`: Real NATS C client integration
- `nats_client_stub.c`: Stub NATS client for testing

**Dependencies**:
- NATS C client (libnats) for communication with Router
- Jansson for JSON DTO parsing
- Rate limiting and health checks built-in

**Build modes**:
- Default: stub mode (no external deps except jansson)
- `-DUSE_NATS_LIB=ON`: real NATS client mode

### apps/ui_web/

**Purpose**: Phoenix LiveView UI for user interface

**Key modules**:
- `lib/ui_web_web/controllers/*`: HTTP controllers & HTML views
- `lib/ui_web_web/live/*`: LiveView pages
- `lib/ui_web_web/components/*`: Reusable components & layouts

**Assets**:
- TailwindCSS + esbuild with `mix assets.build` / `mix assets.deploy`

## Source of Truth

### proto/

**Purpose**: Protocol buffer definitions (ABI)

**Files**:
- `proto/beamline/flow/v1/flow.proto`: Router ABI
- `proto/beamline/provider/v1/provider.proto`: Provider ABI

**Validation**: Buf (breaking changes, lint)

### sql/

**Purpose**: Database schemas (DDL)

**Files**:
- `sql/000_init.sql`: Initial DDL
- `sql/migrations/*.sql`: Migration scripts

**Validation**: SQL syntax, migration compatibility

### .trae/

**Purpose**: State and audit

**Files**:
- `.trae/state.json`: Current project state
- `.trae/history.json`: Append-only audit trail
- `.trae/manifest.json`: Schema versions and validation rules

**Validation**: JSON-Schema, HMAC chain integrity

## Integration with STATE/HISTORY

All modules must:
1. Update `.trae/state.json` when creating/modifying artifacts
2. Add entries to `.trae/history.json` for significant changes
3. Support artifact checksums for No-Drift principle
4. Follow rules from `.cursor/rules/agents/state-and-history-management.mdc`

## References

- `docs/ADR/ADR-001-monorepo-structure.md`: Monorepo structure decision
- `docs/ADR/ADR-010-target-architecture.md`: Target architecture
- `README.md`: Project overview
- `.trae/manifest.json`: Schema versions and validation


