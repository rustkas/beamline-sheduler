# Beamline Scheduler

> Distributed workflow orchestration platform with high-performance execution engines

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![Erlang/OTP](https://img.shields.io/badge/Erlang-26+-red.svg)](https://www.erlang.org/)
[![Rust](https://img.shields.io/badge/Rust-1.70+-orange.svg)](https://www.rust-lang.org/)
[![Phoenix](https://img.shields.io/badge/Phoenix-1.8-orange.svg)](https://phoenixframework.org/)

## 🚀 Overview

**Beamline Scheduler** is a distributed, polyglot workflow orchestration platform designed for high-performance task execution and intelligent routing. Built with production-grade reliability, observability, and scalability in mind.

### What is Beamline Scheduler?

Beamline Scheduler coordinates complex workflows across distributed systems by intelligently routing requests, executing tasks with specialized workers, and providing comprehensive monitoring and management capabilities.

**Key Characteristics:**
- **Polyglot Architecture**: Erlang/OTP for routing, C for high-performance gateway, C++ and Rust for worker execution, Phoenix LiveView for UI
- **NATS-based Messaging**: Asynchronous, scalable communication between components
- **Production-Ready**: Comprehensive observability, fault tolerance, and operational tooling
- **Modular Design**: Each component can be deployed and scaled independently

## 🏗️ Architecture

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

## 📦 Components

### Core Services

| Component | Technology | Status | Repository |
|-----------|-----------|--------|------------|
| **[C-Gateway](apps/c-gateway)** | C11 | ✅ Production Ready | [beamline-c-gateway](https://github.com/rustkas/beamline-c-gateway) |
| **[Router](apps/otp/router)** | Erlang/OTP 26 | ✅ Production Ready | [beamline-otp-router](https://github.com/rustkas/beamline-otp-router) |
| **[CAF Processor](apps/caf/processor)** | C++/CAF | ✅ Production Ready | [beamline-caf](https://github.com/rustkas/beamline-caf) |
| **[Rust Worker](apps/worker)** | Rust | ✅ Production Ready | [beamline-worker](https://github.com/rustkas/beamline-worker) |
| **[UI Web](apps/ui_web)** | Phoenix LiveView | 🚧 In Development | [beamline-ui-web](https://github.com/rustkas/beamline-ui-web) |

### Component Details

#### 🚪 C-Gateway
**Purpose**: High-performance HTTP/gRPC API Gateway  
**Features**:
- HTTP REST API with NATS integration
- Rate limiting and request validation
- Health checks and metrics
- Minimal latency overhead

**Tech**: C11, libjansson, libnats, CMake

#### 🧭 Erlang Router
**Purpose**: Intelligent request routing and workflow orchestration  
**Features**:
- Policy-based routing (weights, sticky sessions, fallback)
- RBAC and tenant isolation
- JetStream integration with durable subscriptions
- Idempotency layer (ETS-based with TTL)
- Admin gRPC service
- OpenTelemetry distributed tracing
- Comprehensive NATS resilience patterns

**Tech**: Erlang/OTP 26, NATS, gRPC, Prometheus

#### ⚙️ CAF Processor
**Purpose**: High-performance compute engine with actor-based runtime  
**Features**:
- Actor-based architecture with resource pools (CPU/GPU/IO)
- Multi-tenant isolation with per-tenant quotas
- Block executors (HTTP, FS, SQL, Human approval)
- Sandbox mode for safe execution
- Dead Letter Queue (DLQ) support

**Tech**: C++20, CAF (C++ Actor Framework), Prometheus, OpenTelemetry

#### 🦀 Rust Worker
**Purpose**: Modular, high-performance job execution runtime  
**Features**:
- Async execution with Tokio
- Modular handlers:
  - HTTP (REST, GraphQL with retries)
  - Scripting (JavaScript via Boa, JMESPath)
  - Database (PostgreSQL with sqlx)
  - File System (secure blob operations)
  - Human interaction (approval workflows)
- NATS protocol integration
- Graceful shutdown and DLQ rotation

**Tech**: Rust, Tokio, NATS, PostgreSQL

#### 🎨 Phoenix UI
**Purpose**: Real-time web management interface  
**Features**:
- Real-time workflow monitoring (LiveView)
- Extension management
- Message system with pagination
- SSE for live updates
- OIDC/OAuth2 authentication

**Tech**: Elixir, Phoenix LiveView, TailwindCSS, DaisyUI

## 🚀 Getting Started

### Prerequisites

```bash
# Required
- Docker & Docker Compose
- Git with submodules support

# For local development
- Erlang/OTP 26+
- Rust 1.70+
- C++20 compiler (g++ or clang)
- C11 compiler
- Elixir 1.15+ (for UI)
- NATS Server 2.9+
```

### Quick Start

```bash
# Clone with submodules
git clone --recursive https://github.com/rustkas/beamline-sheduler.git
cd beamline-sheduler

# Or if already cloned, initialize submodules
git submodule init
git submodule update --recursive

# Start with Docker Compose
docker-compose up -d

# Verify services
curl http://localhost:8080/_health  # C-Gateway
curl http://localhost:4000/health   # Phoenix UI (if running)
```

### Building Components Individually

#### C-Gateway
```bash
cd apps/c-gateway
make
make test
./build/c-gateway
```

#### Erlang Router
```bash
cd apps/otp/router
rebar3 get-deps
rebar3 compile
rebar3 ct  # Run tests
```

#### CAF Processor
```bash
cd apps/caf/processor
mkdir build && cd build
cmake ..
make -j$(nproc)
ctest  # Run tests
```

#### Rust Worker
```bash
cd apps/worker
cargo build --release
cargo test
cargo run --release
```

#### Phoenix UI
```bash
cd apps/ui_web
mix setup
mix phx.server
```

## 📊 Service Endpoints

| Service | Port | Health Check | Metrics |
|---------|------|--------------|---------|
| C-Gateway | 8080 | `GET /_health` | `GET /metrics` |
| Router (gRPC) | 9000 | gRPC health probe | Prometheus metrics |
| CAF Processor | 9090 | `GET /health` | `GET /metrics` |
| Rust Worker | 9091 | `GET /health` | `GET /metrics` |
| Phoenix UI | 4000 | `GET /health` | Built-in LiveDashboard |
| NATS | 4222 | - | HTTP monitoring :8222 |

## 🔧 Configuration

### Environment Variables

**C-Gateway:**
```bash
GATEWAY_BASE_URL=http://localhost:8080
NATS_URL=nats://localhost:4222
ROUTER_DECIDE_SUBJECT=beamline.router.v1.decide
```

**Router:**
```bash
NATS_URL=nats://localhost:4222
ROUTER_GRPC_PORT=9000
```

**Workers:**
```bash
NATS_URL=nats://localhost:4222
WORKER_MAX_CONCURRENCY=8
```

**Phoenix UI:**
```bash
PORT=4000
GATEWAY_BASE_URL=http://localhost:8080
AUTH_ENABLED=false
```

See individual component READMEs for full configuration options.

## 🧪 Testing

### Run All Tests

```bash
# Gateway tests
cd apps/c-gateway && make test

# Router tests
cd apps/otp/router && rebar3 ct

# CAF tests
cd apps/caf/processor && ctest

# Worker tests
cd apps/worker && cargo test

# UI tests
cd apps/ui_web && mix test
```

### Integration Tests

```bash
# Full integration test suite (requires running services)
bash scripts/run_integration_tests.sh
```

## 📈 Observability

### Metrics (Prometheus)

All components expose Prometheus-compatible metrics:
- Request/response rates and latencies
- Resource utilization (CPU, memory, goroutines)
- Queue depths and backlog
- Error rates and types
- Business metrics (tasks executed, jobs completed)

### Tracing (OpenTelemetry)

Distributed tracing across all components with span propagation via NATS headers.

### Logging

- **Structured JSON logs** across all components
- **Correlation IDs** for request tracking
- **Log levels** configurable per component

### Health Checks

All services implement health and readiness probes for Kubernetes deployments.

## 🚢 Deployment

### Docker Compose (Development)

```bash
docker-compose up -d
```

### Kubernetes (Production)

```bash
# Apply Kubernetes manifests
kubectl apply -f infra/k8s/

# Or use Helm (if charts available)
helm install beamline ./charts/beamline
```

### Systemd (Bare Metal)

Each component can run as a systemd service. See individual component READMEs.

## 🔐 Security

- **RBAC**: Role-based access control in Router
- **Tenant Isolation**: Multi-tenancy support with quotas
- **TLS Support**: NATS connections support TLS
- **Input Validation**: All inputs validated before processing
- **Audit Logging**: Comprehensive audit trail
- **Path Sandboxing**: File operations restricted to safe directories
- **SQL Injection Prevention**: Parameterized queries only

## 📚 Documentation

### Core Documentation
- [Architecture Overview](docs/ARCHITECTURE.md)
- [API Contracts](docs/API_CONTRACTS.md)
- [Deployment Guide](docs/DEPLOYMENT.md)
- [Operations Guide](docs/OPERATIONS_GUIDE_RU.md)
- [Contributing Guide](CONTRIBUTING.md)

### Component Documentation
- [C-Gateway README](apps/c-gateway/README.md)
- [Router README](apps/otp/router/README.md)
- [CAF Processor README](apps/caf/processor/README.md)
- [Rust Worker README](apps/worker/README.md)
- [Phoenix UI README](apps/ui_web/README.md)

## 🗺️ Roadmap

### Current Status (January 2025)

- ✅ **CP0**: Repository structure and specifications
- ✅ **CP1**: Router core + C-Gateway
- ✅ **CP2**: Enhanced features (JetStream, idempotency, tracing)
- ✅ **CP3**: Worker CAF & Rust Worker + Ops readiness
- 🚧 **CP4**: Phoenix UI integration (in progress)
- 📅 **CP5**: Full production deployment

### Planned Features

- **Advanced Scheduling**: Priority-based and deadline-aware scheduling
- **Streaming Support**: Real-time data streaming workflows
- **Plugin System**: Dynamic extension loading
- **Multi-cluster**: Cross-cluster workflow orchestration
- **Cost Optimization**: Resource usage tracking and optimization
- **Auto-scaling**: Dynamic worker scaling based on load

## 🤝 Contributing

We welcome contributions! See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

### Development Setup

1. Fork the repository
2. Clone with submodules: `git clone --recursive`
3. Create a feature branch
4. Make changes and add tests
5. Run full test suite
6. Submit a pull request

## 📄 License

This project is licensed under the Apache License 2.0 - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

Built with:
- [Erlang/OTP](https://www.erlang.org/) - Robust distributed systems
- [NATS](https://nats.io/) - High-performance messaging
- [C++ Actor Framework (CAF)](https://actor-framework.org/) - Actor-based concurrency
- [Phoenix Framework](https://www.phoenixframework.org/) - Real-time web
- [Rust](https://www.rust-lang.org/) - Systems programming

## 📞 Support

- **Issues**: [GitHub Issues](https://github.com/rustkas/beamline-sheduler/issues)
- **Discussions**: [GitHub Discussions](https://github.com/rustkas/beamline-sheduler/discussions)

---

**Built with ❤️ using Erlang, Rust, C++, C, and Phoenix**
