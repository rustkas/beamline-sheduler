---
version: 1.0
last_update: 2025-01-27T20:30:00Z
status: current
rule_version: v10
message_protocol: v1
---

# BeamLine Constructor: System Metadata

## Project Overview

**Project Name**: BeamLine Constructor  
**Version**: 1.0  
**Current Checkpoint**: CP3-LC  
**Project Type**: Monorepo  
**License**: [Specify license]

## Core Vision

**BeamLine Constructor** is an **operating system for AI factories** (AI Production Line).

> We are creating a unified orchestrator platform where AI agents, API services, and humans work as one cohesive system — with clear data exchange rules, understandable results for each step, and security "out of the box".

### Project Essence

- **Orchestration of live executors** (AI agents, APIs, humans), not static BPMN diagrams
- **AI agents as production machines** with known characteristics and SLA
- **Unified contracts** through gRPC + Protobuf with versioning without breaking changes
- **Security and observability** as core principles, not optional features

## System Architecture

### High-Level Split

The system is split into two main concerns:

1. **Orchestration & Control Plane** – built on **Erlang/OTP** + NATS
2. **Execution & Heavy Compute** – built on **CAF/C++** and language-specific workers

### Communication Protocols

- **gRPC + Protobuf**: Normalized contracts between orchestration and workers (ABI layer)
- **NATS Subjects**: Messaging inside the platform (versioned subjects)
- **HTTP/REST**: External API interface (C-Gateway)

## Core Components (5 Components)

### 1. C-Gateway

**Path**: `apps/c-gateway/`  
**Status**: ✅ **Completed (CP1-LC)**  
**Technology**: C11 + libnats + jansson  
**Role**: HTTP REST API Gateway

**Key Features**:
- Entry point for all HTTP clients
- HTTP ↔ NATS adapter (CP1 contract)
- JSON DTO parsing and validation
- Rate limiting and health checks
- Lightweight, native performance (10x less memory vs TypeScript/NestJS)

**Build Modes**:
- Stub mode (testing without NATS)
- Real NATS mode (production)

**ADR**: `docs/ADR/ADR-016-c-gateway-migration.md`

---

### 2. Router

**Path**: `apps/otp/router/`  
**Status**: ✅ **Completed (CP1-LC)**  
**Technology**: Erlang/OTP  
**Role**: Central routing orchestrator

**Key Features**:
- Extension pipeline management: `pre → validator → provider → post`
- Routing Policy application (weights, sticky, fallback)
- Extension Registry lookup
- NATS orchestration
- RBAC and tenant validation
- Admin gRPC service (RouterAdmin)

**CP2+ Features** (enabled by default):
- ✅ JetStream integration with durable subscriptions
- ✅ Idempotency layer (ETS-based with TTL)
- ✅ OpenTelemetry distributed tracing
- ✅ Tenant validation/ACL enforcement
- ✅ Admin gRPC service

**Key Modules**:
- `router_nats.erl` - NATS/JetStream client
- `router_idempotency.erl` - Idempotency service
- `router_tenant_validator.erl` - Tenant ACL validation
- `router_tracing.erl` - OpenTelemetry integration
- `router_admin_grpc.erl` - Admin gRPC service
- `router_policy_store.erl` - Policy management

---

### 3. Worker CAF

**Path**: `apps/caf/processor/`  
**Status**: ✅ **Completed (CP3-LC)**  
**Technology**: C++/CAF (Actor Framework)  
**Role**: High-performance compute engine

**Key Features**:
- Actor-based runtime for parallel computations
- BlockExecutor interface: HTTP, FS, SQL, Human approval blocks
- Resource pools (CPU/GPU/IO)
- Comprehensive observability:
  - Prometheus metrics export
  - OpenTelemetry tracing
  - Structured JSON logging
- Sandbox mode for security
- Multi-tenant isolation
- Retry/timeout handling
- Dead Letter Queue (DLQ) support

**Phase 1 Blocks**:
- HTTP block (REST API calls)
- FS block (file system operations)
- SQL block (database queries)
- Human approval block (workflow pauses)

---

### 4. UI

**Path**: `apps/ui_web/`  
**Status**: 🚧 **Migration in Progress** (SvelteKit → Phoenix LiveView)  
**Technology**: Elixir + Phoenix + LiveView  
**Role**: Web interface for management and monitoring

**Migration Rationale**:
- Unify stack (Elixir + Erlang on BEAM)
- Simplify architecture (no separate frontend)
- Improve real-time capabilities (LiveView + Channels)
- Accelerate development (Phoenix generators)
- 2-3x faster development, 5x less complexity, 87% less maintenance

**Migration Status**:
- **Phase 1: Setup** ✅ (2 days) - Project creation, TailwindCSS, dependencies, environment configuration
- **Phase 2: Core Pages** 🔄 (5 days) - Dashboard, Messages Management, Routing Policies Editor
- **Phase 3: Real-time** 📅 (3 days) - LiveView updates, SSE streaming, notifications
- **Phase 4: Deployment** 📅 (2 days) - Production configuration, CI/CD integration

**Migration Timeline**: 12 days estimated (96 hours)

**Planned Features**:
- Dashboard (real-time metrics)
- Messages Management (CRUD + live updates)
- Routing Policies Editor (JSON + visual)
- Extensions Registry UI (CRUD + health monitoring)
- Usage & Billing
- Authentication (OIDC)
- Real-time notifications

**Related Documentation**:
- `docs/UI_WEB_TECHNICAL_SPEC.md` - Technical specification
- `docs/UI_WEB_IMPLEMENTATION_PLAN.md` - Step-by-step implementation plan
- `apps/ui_web/INTEGRATION_TESTING_GUIDE.md` - Integration testing guide (UI-Web ↔ C-Gateway)

**ADR**: `docs/ADR/ADR-017-phoenix-liveview-ui.md`

---

### 5. Extensions

**Type**: NATS services (pluggable architecture)  
**Status**: Architecture defined, API documented  
**Languages**: Any (Go, Rust, Erlang, Python, C, etc.)  
**Role**: Extensibility without core code changes

**Extension Types**:
- **Pre-processors**: Modify incoming messages
- **Validators**: Validation and blocking (PII guard, rate limits)
- **Post-processors**: Modify provider responses
- **Custom Providers**: Custom providers (RAG, CRM, etc.)

**Key Feature**: Add new functionality through configuration (Extension Registry + Routing Policy), without changing C-Gateway/Router/Worker

**Documentation**: 
- `docs/EXTENSIONS_API.md`
- `docs/EXTENSIONS_API_RU.md`

---

## Supporting Components

All other subprojects serve **development process** and are **not part of the product**:

### Adapters and Services

- **Usage Service** (`apps/otp/usage/`) - Metrics collection and billing

> **Note**: Adapters to external LLM/API (OpenAI, Anthropic, CRM, RAG) are implemented as **Custom Provider Extensions** (see `docs/EXTENSIONS_API.md`). These are separate NATS services registered in Extension Registry.

### Contracts and Schemas

- **Proto Definitions** (`proto/beamline/`) - gRPC/Protobuf contracts
  - `proto/beamline/flow/v1/flow.proto` - Routing messages and services
  - `proto/beamline/provider/v1/provider.proto` - Provider contracts
- **SQL Schemas** (`sql/`) - PostgreSQL schemas and migrations
  - `sql/000_init.sql` - Initial database schema

### Development Tooling

- **DevState** (`devstate/`) - Development state management (.trae)
- **Scripts** (`scripts/`) - CI/CD, validation, checks
- **Tools** (`tools/`) - MCP servers, wrappers, utilities

### Infrastructure

- **Infrastructure** (`infra/`) - Docker, Kubernetes configs
- **Backend Shared** (`backend/`) - Shared libraries
- **Documentation** (`docs/`) - Architectural documentation

### Archive

- **Deprecated Solutions** (`deprecated-solutions/`) - Old implementations (TypeScript Gateway, etc.)

---

## Technology Stack

### Core Technologies

| Component | Technology | Version | Purpose |
|-----------|-----------|---------|---------|
| **Gateway** | C11 | C11 standard | HTTP REST API entry point |
| **Router** | Erlang/OTP | OTP 26+ | Orchestration and routing |
| **Worker** | C++/CAF | CAF 0.19+ | High-performance compute |
| **UI** | Elixir/Phoenix | Phoenix 1.7+ | Web interface |
| **Messaging** | NATS | NATS 2.10+ | Internal messaging |
| **Database** | PostgreSQL | 15+ | Persistent storage |
| **Contracts** | Protocol Buffers | proto3 | API contracts |

### Key Libraries and Frameworks

**C-Gateway**:
- `libnats` - NATS client library
- `jansson` - JSON parsing and generation
- `libmicrohttpd` or custom HTTP server

**Router (Erlang/OTP)**:
- `nats_erlang` - NATS client for Erlang
- `grpcbox` - gRPC server framework
- `telemetry` - Metrics and observability
- `opentelemetry_erlang` - Distributed tracing

**Worker CAF**:
- `CAF (C++ Actor Framework)` - Actor-based runtime
- `prometheus-cpp` - Metrics export
- `opentelemetry-cpp` - Distributed tracing

**UI (Phoenix LiveView)**:
- `Phoenix` - Web framework
- `Phoenix LiveView` - Real-time UI
- `Guardian` - Authentication
- `TailwindCSS` - Styling

---

## Checkpoints (CP) Status

### CP0-LC: Repo/State Bootstrap ✅

**Status**: Completed  
**Goal**: Specification of repository structure, state, contracts, DDL

**Acceptance Criteria**:
- ✅ Monorepo structure described
- ✅ Development state management configured (TRAE tooling)
- ✅ ABI v1alpha skeletons created
- ✅ DDL draft ready
- ✅ NATS subjects catalog created
- ✅ Documentation in `docs/*` aligned

---

### CP1-LC: Router Core + C-Gateway ✅

**Status**: Completed  
**Goal**: Routing core implementation (Erlang) + HTTP Gateway (C11)

**Components Completed**:
- ✅ **C-Gateway**: HTTP REST API Gateway (C11)
- ✅ **Router**: Routing core with RBAC, Policy Enforcement, Rate Limiting

**Key Features**:
- gRPC service Router.Decide implementation
- Routing policy application (weights, sticky, fallback)
- NATS and Mnesia integration
- RBAC system (router_rbac, router_permissions, router_audit)
- Policy Enforcement (router_quota)
- Rate Limiting (router_rate_limiter)
- Admin gRPC service
- Metrics and tracing
- Tests (unit and integration)

**Specification**: `docs/archive/dev/CP1_ROUTER_SPEC.md`

---

### CP2-LC: Enhanced Features (In Progress)

**Status**: 🔄 In Progress  
**Goal**: CP2 features enabled by default, validation suite, observability enhancements

**CP2 Features** (enabled by default):
- ✅ JetStream integration with durable subscriptions
- ✅ Idempotency layer (ETS-based with TTL)
- ✅ OpenTelemetry distributed tracing
- ✅ Tenant validation/ACL enforcement
- ✅ Admin gRPC service

**Current Tasks**:
- 🔄 CP2 validation suite creation (wrk-2)
- 📋 Gateway Prometheus/OTLP export (wrk-4)
- 📋 HEIR Policy Store integration (wrk-3)

**Documentation**: `docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY_UPDATED.md`

---

### CP3-LC: Worker CAF ✅

**Status**: Completed  
**Goal**: High-performance compute engine with actor-based runtime

**Key Features**:
- ✅ Actor-based runtime with resource pools (CPU/GPU/IO)
- ✅ BlockExecutor interface
- ✅ Phase 1 blocks (HTTP, FS, SQL, Human approval)
- ✅ Comprehensive observability (Prometheus metrics, OpenTelemetry tracing, JSON logs)
- ✅ Sandbox mode
- ✅ Multi-tenant isolation
- ✅ Retry/timeout handling
- ✅ DLQ support

**Ready For**: Throughput testing

---

### CP4: UI Integration

**Status**: 🚧 In Development  
**Goal**: Phoenix LiveView UI integration

**Current Status**: 
- ✅ Testing Infrastructure Complete (Priority 1 & 2)
  - Mock Gateway for isolated testing
  - Contract tests and schema validators
  - Async helpers for flaky test prevention
  - CI/CD workflow ready
- 🔄 Core Pages Development (Phase 2) - In Progress
- 📋 Migration from SvelteKit to Phoenix LiveView

**Planned Features**:
- Dashboard (real-time metrics)
- Messages Management
- Routing Policies Editor
- Extensions Registry UI
- Usage & Billing
- Authentication (OIDC)

**Testing Infrastructure**:
- Mock Gateway: `test/support/mock_gateway.ex`
- Contract Tests: `test/ui_web/integration/gateway_contract_test.exs`
- API Documentation: `docs/API_CONTRACTS.md`
- CI/CD: `.github/workflows/ui-web-test.yml`

---

### CP5: Production Ready

**Status**: Planned  
**Goal**: Full production readiness

---

## Project Structure

### Monorepo Organization

```
aigroup/
├── apps/
│   ├── c-gateway/          # C11 HTTP Gateway (CORE)
│   ├── otp/
│   │   ├── router/         # Erlang/OTP Router (CORE)
│   │   └── usage/          # Usage metrics (Supporting)
│   ├── caf/
│   │   └── processor/      # C++/CAF Worker (CORE)
│   └── ui_web/             # Phoenix LiveView UI (CORE)
├── proto/                   # Protocol Buffers contracts
│   └── beamline/
│       ├── flow/v1/        # Routing messages
│       └── provider/v1/     # Provider contracts
├── sql/                     # PostgreSQL schemas
├── docs/                    # Documentation
├── scripts/                 # CI/CD and validation scripts
├── infra/                   # Infrastructure configs
├── tools/                   # Development tools
├── devstate/                # DevState tooling (external)
└── .trae/                   # Development state (external)
```

---

## Communication Patterns

### Request Flow

```
Client (HTTP)
    ↓
C-Gateway (C11)
    ↓ (NATS)
Router (Erlang/OTP)
    ↓ (Extension Pipeline)
Extensions (NATS services)
    ↓
Worker CAF (C++/CAF)
    ↓
Response back through chain
```

### NATS Subjects

**Versioned Subjects** (see `docs/NATS_SUBJECTS.md`):
- `beamline.router.v1.decide` - Routing decisions
- `beamline.router.v1.result` - Execution results
- `beamline.router.v1.ack` - Acknowledgments
- `beamline.usage.v1.metered` - Usage events

**Extension Subjects**:
- `beamline.ext.pre.v1.{extension_id}` - Pre-processors
- `beamline.ext.validator.v1.{extension_id}` - Validators
- `beamline.ext.provider.v1.{extension_id}` - Providers
- `beamline.ext.post.v1.{extension_id}` - Post-processors

---

## Observability

### Metrics

**Router**:
- Prometheus metrics via Telemetry
- Custom metrics for routing decisions, policy enforcement, rate limiting

**Worker CAF**:
- Prometheus metrics export (`/metrics` endpoint)
- Resource pool metrics (CPU/GPU/IO utilization)
- Block execution metrics

**Gateway**:
- HTTP request metrics
- Rate limiting metrics
- Idempotency metrics

### Tracing

**OpenTelemetry Integration**:
- Router: Distributed tracing spans for routing decisions
- Worker CAF: Tracing spans for block execution
- Trace context propagation via W3C Trace Context format

### Logging

**Structured JSON Logging**:
- Router: JSONL format with correlation IDs
- Worker CAF: Structured JSON logs
- Gateway: Structured JSON logs

---

## Security

### Authentication & Authorization

- **RBAC System**: Role-based access control in Router
- **Tenant Validation**: ACL enforcement per tenant
- **API Keys**: Managed through Router Admin gRPC
- **OIDC**: Planned for UI authentication

### Data Protection

- **PII Filtering**: Validator extensions for PII detection
- **Secret Masking**: HMAC values masked in documentation (16+... format)
- **TLS**: NATS TLS support (configurable)

---

## Development Principles

### No-Drift Principle

All artifacts are synchronized by versions, checksums are validated before checkpoint transitions.

**Validation**:
- `scripts/validate_state.sh` - State validation
- `scripts/verify_hmac_chain.py` - HMAC chain verification
- `scripts/check_schema_changes.sh` - Schema version validation

### Source of Truth

- **ABI**: Protobuf definitions (`proto/beamline/`)
- **DDL**: SQL schemas (`sql/`)
- **NATS Subjects**: Catalog in `docs/NATS_SUBJECTS.md`
- **Manifest**: `.trae/manifest.json` - Single source of truth for schema versions

### Audit

All operations are logged through Erlang-managed audit system (separate from TRAE development tooling).

---

## Development State Management

**Important**: Development state management (`.trae` files, DevState) is **external development tooling** and **not part of BeamLine Constructor product logic**.

**TRAE IDE**:
- Website: https://www.trae.ai/
- DevState: `devstate/` - Validates and exports `.trae` files to enforce No-Drift
- Recommended: Add `bash devstate/scripts/devstate_verify.sh` to pre-commit/pre-push locally

---

## Key Documentation

### Architecture

- `docs/BEAMLINE_VISION_AND_ARCHITECTURE.md` - Product vision and architecture overview
- `docs/CORE_COMPONENTS.md` - Detailed breakdown of 5 core components
- `docs/ARCHITECTURE/` - Architectural specifications
- `docs/ADR/` - Architecture Decision Records

### Contracts

- `docs/NATS_SUBJECTS.md` - NATS subjects catalog
- `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` - Proto to NATS mapping
- `docs/ROUTING_POLICY.md` - JSON-DSL routing policies
- `proto/README.md` - Protobuf definitions documentation

### Operations

- `docs/OBSERVABILITY.md` - Metrics, tracing, SLO
- `apps/otp/router/docs/OPERATIONAL_GUIDE.md` - Router operational guide
- `docs/archive/dev/LOCAL_CHECKS.md` - Local validation guide
- `docs/archive/dev/PR_CHECKLIST.md` - PR checklist

### Development

- `docs/WORKERS_CONTRACT.md` - Worker contract and responsibilities
- `docs/SCHEMA_VERSIONING.md` - Schema versioning policy
- `docs/CI_VALIDATION.md` - CI validation guide

---

## Current Implementation Status

### Completed Components

- ✅ **C-Gateway** (CP1-LC): HTTP REST API Gateway in C11
- ✅ **Router** (CP1-LC): Erlang/OTP routing orchestrator with RBAC, policies, rate limiting
- ✅ **Worker CAF** (CP3-LC): C++/CAF compute engine with actor-based runtime

### In Progress

- 🔄 **UI** (CP4): Migration from SvelteKit to Phoenix LiveView
- 🔄 **CP2 Features**: Validation suite, observability enhancements

### Planned

- 📅 **Extensions Registry**: Full extension management UI
- 📅 **Production Readiness** (CP5): Full production deployment

---

## Version Information

- **Project Version**: 1.0
- **ABI Version**: v1
- **State Schema Version**: 1.0.0
- **History Schema Version**: 1.0.0
- **Manifest Version**: 1.0.0
- **Last Update**: 2025-01-27T20:30:00Z

---

## References

- `README.md` - General project overview
- `docs/BEAMLINE_VISION_AND_ARCHITECTURE.md` - Product vision
- `docs/CORE_COMPONENTS.md` - Core components breakdown
- `.trae/manifest.json` - Manifest (single source of truth)
- `.trae/state.json` - Current project state

---

**Document Version**: 1.0  
**Last Updated**: 2025-01-27T20:30:00Z  
**Rule Version**: v10  
**Message Protocol**: v1

