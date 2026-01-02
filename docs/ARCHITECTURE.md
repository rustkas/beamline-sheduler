# BeamLine Architecture Overview

> **Reference Documentation**: For comprehensive architecture details, see [BEAMLINE_VISION_AND_ARCHITECTURE.md](BEAMLINE_VISION_AND_ARCHITECTURE.md)

## Quick Architecture Overview

BeamLine Scheduler is a distributed, polyglot workflow orchestration platform built on a modular architecture.

### System Architecture

```
┌─────────────┐
│   Client    │
└──────┬──────┘
       │ HTTP/gRPC
       ▼
┌─────────────────┐
│   C-Gateway     │  ← High-performance API Gateway (C11)
│  (Port 8080)    │
└────────┬────────┘
         │ NATS
         ▼
┌──────────────────┐
│  Erlang Router   │  ← Intelligent routing & orchestration (Erlang/OTP 26)
│  (gRPC: 9000)    │
└────────┬─────────┘
         │ NATS
    ┌────┴────┬──────────┐
    ▼         ▼          ▼
┌────────┐ ┌────────┐ ┌─────────┐
│ Worker │ │ Worker │ │  CAF    │
│ (Rust) │ │ (C++)  │ │Processor│
└────────┘ └────────┘ └─────────┘
    │
    ▼
┌──────────────┐
│  Phoenix UI  │  ← Management interface (Phoenix LiveView)
│ (Port 4000)  │
└──────────────┘
```

## Core Components

### 1. C-Gateway (API Gateway)
- **Technology**: C11, libjansson, libnats
- **Purpose**: High-performance HTTP/gRPC ingress
- **Features**:
  - HTTP REST API with NATS integration
  - Rate limiting and request validation
  - Health checks and metrics
  - Minimal latency overhead

**Documentation**: [C-Gateway README](../apps/c-gateway/README.md)

### 2. Erlang Router (Orchestration Layer)
- **Technology**: Erlang/OTP 26, NATS, gRPC
- **Purpose**: Intelligent request routing and workflow orchestration
- **Features**:
  - Policy-based routing (weights, sticky sessions, fallback)
  - RBAC and tenant isolation
  - JetStream integration with durable subscriptions
  - Idempotency layer (ETS-based with TTL)
  - OpenTelemetry distributed tracing
  - Comprehensive NATS resilience patterns

**Documentation**: [Router README](../apps/otp/router/README.md)

### 3. CAF Processor (Compute Engine)
- **Technology**: C++20, CAF (C++ Actor Framework)
- **Purpose**: High-performance compute engine with actor-based runtime
- **Features**:
  - Actor-based architecture with resource pools (CPU/GPU/IO)
  - Multi-tenant isolation with per-tenant quotas
  - Block executors (HTTP, FS, SQL, Human approval)
  - Sandbox mode for safe execution
  - Dead Letter Queue (DLQ) support

**Documentation**: [CAF Processor README](../apps/caf/processor/README.md)

### 4. Rust Worker (Execution Runtime)
- **Technology**: Rust, Tokio, NATS
- **Purpose**: Modular, high-performance job execution runtime
- **Features**:
  - Async execution with Tokio
  - Modular handlers (HTTP, Scripting, Database, File System, Human interaction)
  - NATS protocol integration
  - Graceful shutdown and DLQ rotation

**Documentation**: [Rust Worker README](../apps/worker/README.md)

### 5. Phoenix UI (Web Interface)
- **Technology**: Elixir, Phoenix LiveView, TailwindCSS
- **Purpose**: Real-time web management interface
- **Features**:
  - Real-time workflow monitoring (LiveView)
  - Extension management
  - Message system with pagination
  - SSE for live updates
  - OIDC/OAuth2 authentication

**Documentation**: [Phoenix UI README](../apps/ui_web/README.md)

## Communication Patterns

### NATS-Based Messaging
- **Asynchronous Communication**: All inter-component communication via NATS
- **Versioned Subjects**: `beamline.{service}.v{version}.{action}`
- **JetStream**: Durable messaging for critical workflows
- **Subject Documentation**: [NATS_SUBJECTS.md](NATS_SUBJECTS.md)

### Protocol Buffers (ABI Layer)
- **Contract Definition**: [PROTO_NATS_MAPPING.md](ARCHITECTURE/PROTO_NATS_MAPPING.md)
- **Versioning**: `v1alpha` → `v1` with strict compatibility rules
- **Message Fields**: All messages include `run_id`, `flow_id`, `step_id`, `idempotency_key`, `trace_id`

## Key Architecture Principles

### 1. Polyglot Design
Each component uses the best language for its purpose:
- **C**: High-performance gateway with minimal overhead
- **Erlang/OTP**: Fault-tolerant routing and orchestration
- **C++/CAF**: Actor-based compute engine
- **Rust**: Safe, concurrent worker execution
- **Elixir/Phoenix**: Real-time web interface

### 2. Fault Tolerance
- **Circuit Breakers**: Prevent cascade failures
- **Retry Logic**: Exponential backoff with jitter
- **Idempotency**: ETS-based deduplication
- **Health Checks**: All components expose health endpoints
- **Dead Letter Queues**: Failed messages preserved for analysis

### 3. Observability
- **Structured Logging**: JSON logs across all components
- **Distributed Tracing**: OpenTelemetry with trace propagation
- **Metrics**: Prometheus-compatible metrics
- **Correlation IDs**: Request tracking across services

**Documentation**: [OBSERVABILITY.md](OBSERVABILITY.md)

### 4. Security
- **mTLS**: Service-to-service encryption
- **RBAC**: Role-based access control
- **Tenant Isolation**: Multi-tenancy support with quotas
- **Input Validation**: All inputs validated before processing
- **Audit Logging**: Comprehensive audit trail

**Documentation**: [SECURITY_GUIDE.md](SECURITY_GUIDE.md)

## Data Flow Example

1. **Client Request** → HTTP REST API
2. **C-Gateway** → Validates request, publishes to NATS
3. **Erlang Router** → Routes based on policy, creates workflow
4. **Worker** (Rust/CAF) → Executes task, publishes result
5. **Phoenix UI** → Real-time updates via LiveView/SSE

## Scalability

### Horizontal Scaling
- **Stateless Components**: Gateway, Workers scale independently
- **NATS Queue Groups**: Load distribution across worker pool
- **JetStream Clustering**: Message durability and replication

### Vertical Scaling
- **Resource Pools**: CAF processor manages CPU/GPU/IO pools
- **Concurrency**: Configurable worker concurrency limits
- **Backpressure**: Flow control at every layer

## Development Lifecycle (Checkpoints)

- **CP0**: Repository structure and specifications ✅
- **CP1**: Router core + C-Gateway ✅
- **CP2**: Enhanced features (JetStream, idempotency, tracing) ✅
- **CP3**: Worker CAF & Rust Worker + Ops readiness ✅
- **CP4**: Phoenix UI integration 🚧
- **CP5**: Full production deployment 📅

**Detailed Roadmap**: [ROADMAP.md](ROADMAP.md)

## Architecture Diagrams

### Component Interaction
For detailed component interaction diagrams, see:
- [PROTO_NATS_MAPPING.md](ARCHITECTURE/PROTO_NATS_MAPPING.md)
- [Repository Structure](ARCHITECTURE/repository-structure.md)

### Context Maps
For domain boundaries and context mapping:
- [Context Maps](ARCHITECTURE/context-maps.md)

## Related Documentation

- **[BEAMLINE_VISION_AND_ARCHITECTURE.md](BEAMLINE_VISION_AND_ARCHITECTURE.md)** - Complete architecture vision
- **[CORE_COMPONENTS.md](CORE_COMPONENTS.md)** - Detailed component specifications
- **[API_CONTRACTS.md](API_CONTRACTS.md)** - API contract specifications
- **[ROUTING_POLICY.md](ROUTING_POLICY.md)** - Routing policy details
- **[NATS_SUBJECTS.md](NATS_SUBJECTS.md)** - NATS subject conventions

## Summary

BeamLine Scheduler implements a **microservices architecture** with:
- **High-performance ingress** (C-Gateway)
- **Fault-tolerant orchestration** (Erlang Router)
- **Flexible execution** (Rust/C++ Workers)
- **Real-time monitoring** (Phoenix UI)
- **Asynchronous messaging** (NATS)
- **Production-grade observability** (OpenTelemetry, Prometheus)

The architecture is designed for **production readiness**, **horizontal scalability**, and **operational excellence**.
