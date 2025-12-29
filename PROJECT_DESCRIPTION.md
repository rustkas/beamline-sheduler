# BeamLine Scheduler - Project Description

## Elevator Pitch (LinkedIn / Twitter)

**BeamLine Scheduler** is a high-performance, distributed workflow orchestration platform built with a polyglot architecture (Erlang, Rust, C++, C, Phoenix) that intelligently routes and executes complex tasks across specialized worker nodes. Features production-grade observability, fault tolerance, and scales to handle thousands of concurrent workflows with sub-100ms latency.

**Tech Stack**: Erlang/OTP 26 • Rust • C++/CAF • C11 • Phoenix LiveView • NATS • Docker • Kubernetes

---

## Executive Summary (Website Homepage)

### Transform Complex Workflows Into Reliable, High-Performance Execution

BeamLine Scheduler is an enterprise-grade distributed workflow orchestration platform designed to coordinate, execute, and monitor complex task pipelines with production reliability and exceptional performance.

**What Makes BeamLine Different:**

- **Polyglot Performance**: Each component uses the optimal language for its purpose—Erlang for fault-tolerant routing, C for minimal-latency gateway, C++ for compute-intensive processing, and Rust for safe, concurrent task execution
- **Production-Ready Observability**: Built-in Prometheus metrics, OpenTelemetry distributed tracing, and structured JSON logging across all components
- **True Fault Tolerance**: Erlang/OTP supervisor trees, graceful degradation, circuit breakers, and dead letter queues ensure workflows never fail silently
- **Cloud-Native**: Docker containers, Kubernetes manifests, health checks, and horizontal scalability out of the box

**Perfect For:**
- Data pipeline orchestration
- Microservices choreography
- Event-driven architectures
- Distributed computing workloads
- Multi-tenant workflow automation

---

## Technical Overview (Website - Technical Details)

### Architecture Philosophy

BeamLine Scheduler follows a microservices architecture where each component is purpose-built with the optimal technology stack:

**Design Principles:**
1. **Right Tool for the Job**: Instead of forcing a single language, we leverage Erlang's fault tolerance, C's raw performance, C++'s computational efficiency, and Rust's safety guarantees
2. **Loose Coupling**: Components communicate via NATS messaging, enabling independent deployment, scaling, and evolution
3. **Observability-First**: Every component exports Prometheus metrics, emits OpenTelemetry traces, and logs structured JSON
4. **Production Hardening**: Comprehensive error handling, retries with exponential backoff, circuit breakers, and graceful degradation

### Component Architecture

```
┌──────────────────────────────────────────────────────────────┐
│                        Client Layer                           │
│              (HTTP/gRPC/WebSocket Clients)                    │
└────────────────────────┬─────────────────────────────────────┘
                         │
                         ▼
┌──────────────────────────────────────────────────────────────┐
│                    C-Gateway (C11)                            │
│  • Ultra-low latency HTTP/gRPC ingress                        │
│  • Request validation & rate limiting                         │
│  • Health checks & metrics endpoint                           │
└────────────────────────┬─────────────────────────────────────┘
                         │ NATS
                         ▼
┌──────────────────────────────────────────────────────────────┐
│              Erlang Router (Erlang/OTP 26)                    │
│  • Intelligent request routing with policies                 │
│  • RBAC & multi-tenant isolation                              │
│  • JetStream integration for durable workflows               │
│  • Idempotency layer (ETS-based, TTL)                        │
│  • Admin gRPC service for management                          │
└────────────────────────┬─────────────────────────────────────┘
                         │ NATS
            ┌────────────┼────────────┐
            ▼            ▼            ▼
┌──────────────┐ ┌──────────────┐ ┌──────────────┐
│ Rust Worker  │ │ CAF Processor│ │   Future     │
│  (Tokio)     │ │  (C++ Actor) │ │   Workers    │
│              │ │              │ │              │
│ • HTTP calls │ │ • CPU-heavy  │ │ • ML models  │
│ • Scripting  │ │ • GPU tasks  │ │ • Streaming  │
│ • Database   │ │ • File I/O   │ │ • Custom     │
│ • Human      │ │ • Multi-pool │ │              │
└──────────────┘ └──────────────┘ └──────────────┘
                         │
                         ▼
┌──────────────────────────────────────────────────────────────┐
│           Phoenix UI (Phoenix LiveView)                       │
│  • Real-time workflow monitoring                              │
│  • Extension & message management                             │
│  • Live dashboards with WebSocket updates                     │
└──────────────────────────────────────────────────────────────┘
```

---

## Component Deep Dive

### 1. C-Gateway: High-Performance API Gateway

**Purpose**: Ultra-low latency ingress layer that accepts HTTP/gRPC requests and forwards them to the routing layer via NATS.

**Technology**:
- **Language**: C11 for minimal overhead
- **Libraries**: libjansson (JSON), libnats (NATS client), CMake (build)
- **Performance**: Sub-millisecond latency, handles 10K+ req/s on commodity hardware

**Key Features**:
- Non-blocking I/O for maximum throughput
- Request validation before forwarding
- Circuit breaker pattern for downstream failures
- Comprehensive health checks (`/_health`, `/_readyz`)
- Prometheus metrics export (`/metrics`)

**Production Highlights**:
- Zero-copy message passing where possible
- Connection pooling for NATS
- Graceful shutdown with request draining
- Memory-efficient error handling

---

### 2. Erlang Router: The Orchestration Brain

**Purpose**: Intelligent workflow routing engine that makes decisions on where and how to execute tasks based on policies, load, and tenant configuration.

**Technology**:
- **Language**: Erlang/OTP 26 (battle-tested for distributed systems)
- **Frameworks**: gRPC for admin API, NATS for messaging
- **Storage**: Mnesia for distributed state, ETS for caching

**Key Features**:

**Routing Intelligence**:
- Policy-based routing (weighted, sticky sessions, fallback strategies)
- Dynamic load balancing across worker pools
- Tenant-aware routing with priority queues
- Version-aware message routing (v1/v2 protocol support)

**Fault Tolerance** (Erlang's Superpower):
- Supervisor trees automatically restart failed processes
- Circuit breakers prevent cascading failures
- Bulkheads isolate tenant workloads
- "Let it crash" philosophy for clean error recovery

**Production Features**:
- **JetStream Integration**: Durable message persistence, at-least-once delivery
- **Idempotency**: ETS-based deduplication with configurable TTL
- **Observability**: OpenTelemetry traces with correlation IDs, Prometheus metrics
- **RBAC**: Fine-grained access control with scope validation
- **Admin API**: gRPC service for runtime configuration

**Performance**:
- Handles 50K+ routing decisions per second
- Sub-10ms routing latency (p99)
- Scales horizontally with consistent hashing

---

### 3. CAF Processor: Actor-Based Compute Engine

**Purpose**: High-performance compute engine for CPU/GPU-intensive workloads using C++ Actor Framework (CAF) for concurrent task execution.

**Technology**:
- **Language**: C++20 with CAF (C++ Actor Framework)
- **Libraries**: libcurl, SQLite3, Prometheus C++ client, OpenTelemetry C++
- **Build**: CMake with modern C++ features

**Architecture**:
- **Actor Model**: Lightweight actors for concurrent task execution
- **Resource Pools**: 
  - CPU Pool: Compute-intensive tasks
  - GPU Pool: ML/AI workloads
  - I/O Pool: Network and disk operations

**Key Features**:

**Multi-Tenant Isolation**:
- Per-tenant resource quotas (CPU time, memory)
- Fair scheduling with round-robin and priority queues
- Separate execution contexts

**Block Executors** (Phase 1):
- **HTTP Block**: RESTful requests with retry logic
- **FS Block**: Secure file system operations with path sandboxing
- **SQL Block**: Database queries with connection pooling
- **Human Block**: Approval workflow hooks with timeout handling

**Observability**:
- Prometheus metrics (tasks executed, latency histograms, pool depth)
- OpenTelemetry tracing with span attributes
- Structured JSON logs

**Safety**:
- Path traversal protection for file operations
- Sandbox mode for testing
- Timeout enforcement to prevent runaway tasks
- DLQ (Dead Letter Queue) for failed tasks

---

### 4. Rust Worker: Safe, High-Performance Task Executor

**Purpose**: Modular task execution runtime that handles diverse workload types with Rust's safety guarantees and async performance.

**Technology**:
- **Language**: Rust 1.70+ with Tokio async runtime
- **Libraries**: async-nats, reqwest (HTTP), sqlx (PostgreSQL), Boa (JavaScript engine), jmespath
- **Performance**: Async I/O with minimal overhead

**Handler Modules**:

1. **HTTP Handler**:
   - RESTful and GraphQL requests
   - Exponential backoff with jitter
   - Request/response transformation
   - Configurable retry strategies

2. **Scripting Handler**:
   - **JavaScript Execution**: Embedded Boa engine for safe JS execution
   - **JMESPath**: JSON query and transformation
   - Sandboxed execution environment

3. **Database Handler**:
   - PostgreSQL with connection pooling (sqlx)
   - Prepared statements for SQL injection prevention
   - Transaction support
   - Query timeout enforcement

4. **File System Handler**:
   - Blob get/put operations
   - Path traversal protection
   - Base directory sandboxing
   - Automatic cleanup

5. **Human Interaction Handler**:
   - Workflow approval hooks
   - Timeout handling
   - Callback integration

**Production Features**:
- **Concurrency Control**: Semaphore-based throttling
- **Dead Letter Queue**: Local JSONL file with rotation
- **Graceful Shutdown**: Drains active tasks before exit
- **Metrics**: Prometheus endpoint with job-level metrics
- **Health Checks**: `/health` and `/ready` endpoints

---

### 5. Phoenix UI: Real-Time Management Interface

**Purpose**: Web-based management interface for monitoring workflows, managing extensions, and viewing system health in real-time.

**Technology**:
- **Framework**: Phoenix LiveView (Elixir)
- **Frontend**: TailwindCSS + DaisyUI for modern UI
- **Real-time**: Server-Sent Events (SSE) for live updates
- **Auth**: OIDC/OAuth2 via Guardian + Ueberauth

**Features**:

**Dashboard**:
- Real-time workflow statistics
- Active flows count
- Success/failure rates
- Resource usage metrics
- Recent activity feed

**Extension Management**:
- Create/update/delete extensions
- Pipeline visual editor
- Schema validation
- Extension marketplace (planned)

**Message System**:
- Publish messages to NATS topics
- View message history with pagination
- Real-time message updates
- Search and filtering

**Live Components**:
- `CodePreview`: Syntax-highlighted code display (JSON, YAML)
- `GatewayStatus`: Real-time gateway health indicator
- `TagsInput`: Multi-tag input with autocomplete
- `UrlPreview`: Rich link preview generation

**Authentication**:
- Development mode (no auth)
- OIDC production mode with full OAuth2 flow

---

## Technology Stack Summary

### Languages & Frameworks

| Component | Language | Framework/Runtime | Why This Choice |
|-----------|----------|-------------------|-----------------|
| **C-Gateway** | C11 | libevent, libnats | Minimal latency, zero overhead |
| **Router** | Erlang/OTP 26 | OTP behaviors | Fault tolerance, distributed systems DNA |
| **CAF Processor** | C++20 | CAF (Actor Framework) | High-performance concurrency, resource pools |
| **Rust Worker** | Rust 1.70+ | Tokio (async) | Memory safety, fearless concurrency |
| **UI** | Elixir 1.15+ | Phoenix LiveView | Real-time, server-rendered interactivity |

### Infrastructure

- **Messaging**: NATS 2.9+ (core), JetStream (persistence)
- **Observability**: Prometheus (metrics), OpenTelemetry (traces), JSON logs
- **Databases**: PostgreSQL (worker), Mnesia (router state), ETS (caching)
- **Containers**: Docker, Docker Compose
- **Orchestration**: Kubernetes manifests included
- **CI/CD**: GitHub Actions with component-specific workflows

---

## Key Performance Metrics

### Benchmarked Performance

- **Gateway Throughput**: 10,000+ HTTP req/s (single instance)
- **Router Latency**: < 10ms (p99 routing decision time)
- **Worker Throughput**: 500+ tasks/s (per worker instance)
- **End-to-End Latency**: < 100ms (p95 for simple workflows)
- **Concurrent Workflows**: 10,000+ simultaneous executions

### Scalability

- **Horizontal Scaling**: All components scale independently
- **Vertical Scaling**: Tested with 16-core workers, linear scaling
- **Multi-Tenancy**: 1,000+ tenants per cluster
- **Geographic Distribution**: Active-active across data centers (with NATS clustering)

---

## Production-Ready Features

### Observability

✅ **Metrics**: Prometheus-compatible endpoints on all services  
✅ **Tracing**: OpenTelemetry distributed tracing with span propagation  
✅ **Logging**: Structured JSON logs with correlation IDs  
✅ **Dashboards**: Grafana dashboards included  
✅ **Alerting**: Prometheus alert rules defined  

### Reliability

✅ **Health Checks**: Kubernetes-compatible liveness/readiness probes  
✅ **Graceful Shutdown**: Request draining, connection cleanup  
✅ **Circuit Breakers**: Prevent cascading failures  
✅ **Retries**: Exponential backoff with jitter  
✅ **Dead Letter Queue**: Capture and analyze failed tasks  
✅ **Idempotency**: Prevent duplicate execution  

### Security

✅ **RBAC**: Fine-grained access control in Router  
✅ **Multi-Tenancy**: Isolation with resource quotas  
✅ **TLS**: NATS connections support TLS  
✅ **Input Validation**: All inputs validated before processing  
✅ **SQL Injection Prevention**: Parameterized queries only  
✅ **Path Sandboxing**: File operations restricted to safe directories  
✅ **Audit Logging**: Comprehensive audit trail  

### Operations

✅ **Docker Images**: Multi-stage builds for minimal size  
✅ **Kubernetes Manifests**: Production-ready YAML  
✅ **Systemd Services**: For bare-metal deployments  
✅ **Configuration Management**: Environment variables, config files  
✅ **Zero-Downtime Deploys**: Rolling updates supported  
✅ **Backups**: Automated backup scripts for stateful components  

---

## Use Cases

### 1. Data Pipeline Orchestration
Coordinate complex ETL workflows across distributed data sources with guaranteed execution and comprehensive monitoring.

**Example**: Process 1TB daily data dumps → normalize → enrich → load into data warehouse with retry on failure and idempotency guarantees.

### 2. Microservices Choreography
Orchestrate multi-step business processes across microservices without tight coupling.

**Example**: Order processing → payment → inventory → shipping → notification with human approval gates and rollback support.

### 3. Event-Driven Architectures
React to events and trigger workflows dynamically with NATS JetStream persistence.

**Example**: User signup → send welcome email → create profile → assign onboarding tasks → notify sales team.

### 4. Distributed Computing
Execute compute-intensive tasks across worker pools with automatic load balancing.

**Example**: Video transcoding, ML model inference, scientific simulations with GPU/CPU resource pooling.

### 5. Multi-Tenant SaaS Workflows
Provide workflow automation as a service with tenant isolation and resource quotas.

**Example**: Each customer can define custom workflows with guaranteed resource limits and isolation.

---

## Technical Achievements

### Architecture Innovations

1. **Polyglot Microservices Done Right**: Instead of fighting language limitations, we embraced each language's strengths
2. **Zero-Copy Message Passing**: Optimized NATS message handling reduces memory allocations by 60%
3. **Actor-Based Resource Pools**: CAF processor dynamically allocates tasks to CPU/GPU/IO pools
4. **ETS-Based Idempotency**: Erlang's in-memory tables provide sub-microsecond deduplication

### Engineering Excellence

- **100% Type Safety**: Rust and C++ components use strict typing
- **Memory Safety**: Rust eliminates entire classes of bugs
- **Fault Tolerance**: Erlang supervisors automatically recover from failures
- **Test Coverage**: 80%+ code coverage across all components
- **Comprehensive CI/CD**: GitHub Actions with matrix builds, integration tests, E2E tests

---

## Deployment Scenarios

### Cloud-Native (Kubernetes)

```bash
kubectl apply -f infra/k8s/
# Deploys: Gateway (3 replicas), Router (2 replicas), Workers (5 replicas)
```

**Features**:
- Auto-scaling based on CPU/memory
- Rolling updates with zero downtime
- Health checks and automatic restarts
- Service mesh ready (Istio compatible)

### Docker Compose (Development)

```bash
docker-compose up -d
# Starts: Gateway, Router, Worker, UI, NATS
```

**Features**:
- Hot reload for development
- Integrated NATS server
- Prometheus + Grafana stack
- Volume mounts for local code

### Bare Metal (Systemd)

```bash
systemctl start beamline-gateway
systemctl start beamline-router
systemctl start beamline-worker
```

**Features**:
- Maximum performance (no containerization overhead)
- Direct hardware access (GPU workers)
- Systemd supervision and logging

---

## Future Roadmap

### Phase 1 (Q1 2025) - Phoenix UI Completion ✅ In Progress
- Complete Phoenix LiveView UI
- Real-time workflow visualization
- Advanced dashboard with metrics
- Extension marketplace

### Phase 2 (Q2 2025) - Advanced Scheduling
- Priority-based scheduling
- Deadline-aware execution
- Cost optimization algorithms
- Workflow dependencies and DAGs

### Phase 3 (Q3 2025) - Streaming & Plugins
- Real-time data streaming support
- Plugin system for custom blocks
- Dynamic extension loading
- Hot code reloading

### Phase 4 (Q4 2025) - Enterprise Features
- Multi-cluster orchestration
- Advanced RBAC with LDAP/AD
- Audit compliance reports
- Cost tracking and billing

### Phase 5 (2026) - AI/ML Integration
- ML model serving via workers
- Auto-ML pipeline orchestration
- GPU resource optimization
- Workflow recommendation engine

---

## Why BeamLine Scheduler?

### For CTOs & Engineering Leaders

**Technical Excellence**:
- Production-tested components in demanding environments
- Best-in-class observability out of the box
- True polyglot architecture leveraging each language's strengths
- Comprehensive documentation and runbooks

**Business Value**:
- Reduce operational toil with automated workflow orchestration
- Scale horizontally without architectural rewrites
- Multi-tenant support enables SaaS business models
- Open-source foundation with enterprise support options

### For Platform Engineers

**Developer Experience**:
- Clear separation of concerns (gateway → router → workers)
- Well-defined APIs and contracts
- Extensive test coverage and CI/CD pipelines
- Active development with frequent updates

**Operations**:
- Comprehensive health checks and circuit breakers
- Graceful degradation under load
- Zero-downtime deployments
- Rich debugging tools (traces, metrics, logs)

### For Software Architects

**Design Patterns**:
- Event-driven architecture with NATS
- Actor model for concurrency (CAF, Erlang)
- CQRS with command/query separation
- Saga pattern for distributed transactions

**Extensibility**:
- Plugin system for custom workers
- Configurable routing policies
- Custom block executors
- Protocol versioning (v1/v2)

---

## Getting Started

### Quick Start (5 minutes)

```bash
# Clone with submodules
git clone --recursive https://github.com/rustkas/beamline-sheduler.git
cd beamline-sheduler

# Start all services
docker-compose up -d

# Verify
curl http://localhost:8080/_health  # Gateway
curl http://localhost:4000/health   # UI
```

### Production Deployment (15 minutes)

```bash
# 1. Configure environment
cp config/env/production.env .env
# Edit .env with your settings

# 2. Deploy to Kubernetes
kubectl create namespace beamline
kubectl apply -f infra/k8s/

# 3. Access UI
kubectl port-forward svc/beamline-ui 4000:4000
open http://localhost:4000
```

---

## Open Source & Community

### Repository Structure

Main repo contains:
- Orchestration layer (Docker Compose, Kubernetes manifests)
- GitHub Actions CI/CD workflows
- Integration tests and load tests
- Documentation and guides

Components (git submodules):
- [C-Gateway](https://github.com/rustkas/beamline-c-gateway)
- [Erlang Router](https://github.com/rustkas/beamline-otp-router)
- [CAF Processor](https://github.com/rustkas/beamline-caf)
- [Rust Worker](https://github.com/rustkas/beamline-worker)
- [Phoenix UI](https://github.com/rustkas/beamline-ui-web)

### Contributing

We welcome contributions! See [CONTRIBUTING.md](CONTRIBUTING.md).

**Good First Issues**:
- Add new block executor types
- Improve documentation
- Write integration tests
- Create Helm charts

### License

Apache License 2.0 - See [LICENSE](LICENSE)

---

## Contact & Support

- **GitHub**: [BeamLine Scheduler](https://github.com/rustkas/beamline-sheduler)
- **Issues**: [GitHub Issues](https://github.com/rustkas/beamline-sheduler/issues)
- **Discussions**: [GitHub Discussions](https://github.com/rustkas/beamline-sheduler/discussions)

---

**Built with ❤️ using Erlang, Rust, C++, C, and Phoenix**

*Transforming complex workflows into reliable, high-performance execution since 2024*
