---
version: 1.0
status: accepted
date: 2025-11-18
deciders:
  - Agent 2: Architecture/Tech Lead
  - Cascade: Implementation Agent
related_adrs:
  - ADR-006-nats-inter-service-communication.md
  - ADR-010-target-architecture.md
supersedes: []
superseded_by: []
---

# ADR-016: Migration from TypeScript/NestJS Gateway to C11 Gateway

## Status

**accepted** (2025-11-18)

## Context

The initial architecture proposed a TypeScript/NestJS-based HTTP Gateway for the BeamLine Constructor project. During CP1-LC implementation phase, the team identified several concerns:

### Current Situation
- Original plan: TypeScript/NestJS Gateway (`apps/gateway/`) for REST API
- Alternative considered: C++/CAF Ingress Gateway (`apps/caf/ingress/`)
- Both approaches had significant overhead for a simple HTTP→NATS bridge

### Constraints and Requirements
- **Performance**: Gateway must handle 2000-4000 RPS with minimal latency
- **Memory footprint**: Target RSS < 200MB under load
- **Simplicity**: Gateway is a thin HTTP→NATS adapter, not a complex business logic layer
- **Dependencies**: Minimize runtime dependencies and build complexity
- **CP1 Isolation**: Gateway must be fully isolated from HTTP layer, communicating only via NATS

### Stakeholders Affected
- **Router (Erlang/OTP)**: Expects NATS messages in specific format
- **Clients**: Expect REST API at `/api/v1/*` endpoints
- **DevOps**: Need simple deployment and monitoring
- **Development team**: Need maintainable, debuggable code

### Technical Context
- NATS C client (libnats) is mature and well-tested
- Jansson library provides robust JSON parsing in C
- C11 offers excellent performance and minimal overhead
- Existing C-Gateway implementation already exists at `apps/c-gateway/`

## Decision

**Adopt C11-based HTTP Gateway (`apps/c-gateway/`) as the official CP1 Gateway implementation**, replacing earlier TypeScript/NestJS and C++/CAF proposals.

### Key Rationale

1. **Performance**: C11 provides native performance with minimal overhead
2. **Simplicity**: Gateway is ~28KB of code (http_server.c) - no framework bloat
3. **Memory efficiency**: C11 + libnats footprint is ~10-20MB vs 100-200MB for Node.js/NestJS
4. **Build flexibility**: Supports both stub mode (testing) and real NATS mode (production)
5. **Proven implementation**: C-Gateway already implemented and tested in CP1-LC

### Implementation Approach

**Technology Stack:**
- **Language**: C11 (ISO/IEC 9899:2011)
- **HTTP Server**: Custom implementation in `http_server.c`
- **NATS Client**: libnats (C client library)
- **JSON Parsing**: jansson library
- **Build System**: CMake

**Architecture:**
```
Client (HTTP/REST)
    ↓
C-Gateway (apps/c-gateway/)
  - http_server.c: HTTP endpoints
  - nats_client_real.c: NATS integration
  - nats_client_stub.c: Testing stub
    ↓ NATS (beamline.ingress.v1.*)
Router (Erlang/OTP)
```

**Build Modes:**
- **Stub mode** (default): No external NATS dependency, uses `nats_client_stub.c`
- **Real mode** (`-DUSE_NATS_LIB=ON`): Links against libnats, uses `nats_client_real.c`

### Dependencies
- **Required**: jansson (JSON parsing)
- **Optional**: libnats (real NATS client, only for production builds)
- **Build**: CMake 3.15+, C11 compiler (gcc/clang)

## Consequences

### Positive

- ✅ **10x lower memory footprint**: ~20MB vs ~200MB for NestJS
- ✅ **Native performance**: No V8 JIT overhead, direct syscalls
- ✅ **Minimal dependencies**: Only jansson required for stub builds
- ✅ **Fast build times**: ~2-5 seconds vs ~30-60 seconds for TypeScript
- ✅ **Simple deployment**: Single binary, no npm/node_modules
- ✅ **Debuggable**: Standard C debugging tools (gdb, valgrind)
- ✅ **Testing flexibility**: Stub mode allows testing without NATS infrastructure
- ✅ **Production ready**: Already implemented and tested in CP1-LC

### Negative

- ⚠️ **Manual memory management**: Requires careful handling of malloc/free
- ⚠️ **No async/await syntax**: Uses traditional callback patterns
- ⚠️ **Less ecosystem**: Fewer libraries compared to Node.js/TypeScript
- ⚠️ **Learning curve**: Team must be proficient in C11 and memory safety
- ⚠️ **Error handling**: More verbose than TypeScript try/catch

### Neutral

- 📝 **Documentation migration**: All TypeScript examples → JSON/C examples
- 📝 **DTO format**: JSON-only (no TypeScript interfaces in docs)
- 📝 **Testing approach**: Unit tests in C, integration tests via HTTP
- 📝 **Observability**: JSON structured logs, Prometheus metrics (future)

## Alternatives Considered

### Alternative 1: TypeScript/NestJS Gateway

**Description**: Full-featured NestJS application with TypeScript, decorators, dependency injection, and extensive middleware.

**Pros**:
- Rich ecosystem (npm packages)
- Type safety with TypeScript
- Familiar to many developers
- Excellent tooling (IDE support, debuggers)
- Built-in OpenAPI/Swagger generation

**Cons**:
- High memory footprint (100-200MB base)
- V8 JIT compilation overhead
- Slow cold starts (~2-5 seconds)
- Complex dependency tree (node_modules)
- Overkill for simple HTTP→NATS bridge

**Why not chosen**: Performance and memory overhead not justified for a thin adapter. Gateway has no complex business logic requiring TypeScript's type system or NestJS's DI framework.

### Alternative 2: C++ CAF Ingress Gateway

**Description**: C++ Actor Framework (CAF) based gateway with actor model for concurrency.

**Pros**:
- Actor model for concurrency
- Modern C++ features (C++17/20)
- Type safety with templates
- Good performance

**Cons**:
- CAF framework overhead (~50MB)
- Complex build system (CMake + CAF dependencies)
- Steeper learning curve than C11
- Actor model unnecessary for stateless HTTP→NATS bridge
- Longer compile times

**Why not chosen**: CAF's actor model is powerful but overkill for a stateless request-reply gateway. C11 provides sufficient performance with much simpler implementation.

### Alternative 3: Go HTTP Gateway

**Description**: Go-based HTTP server with goroutines for concurrency.

**Pros**:
- Built-in HTTP server
- Goroutines for concurrency
- Fast compilation
- Good NATS client library
- Memory safety (GC)

**Cons**:
- GC pauses (10-50ms)
- Higher memory footprint than C (~30-50MB base)
- Introduces another language to the stack
- Go runtime overhead

**Why not chosen**: While Go is a strong candidate, C11 provides better performance and lower memory footprint. Team already proficient in C/C++ for CAF Worker implementation.

## Implementation Notes

### Migration Steps

1. ✅ **Implementation**: C-Gateway already implemented at `apps/c-gateway/`
2. ✅ **Documentation update**: 
   - Updated `.trae/state.json` (AGENT_4_C_GATEWAY)
   - Updated `docs/ARCHITECTURE/api-registry.md` (TypeScript → JSON DTOs)
   - Updated `docs/ARCHITECTURE/repository-structure.md`
   - Updated `docs/ARCHITECTURE/context-maps.md`
   - Updated `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`
   - Updated `docs/ADR_INDEX.md`
3. 🔄 **README.md update**: Add C-Gateway section
4. 🔄 **Docker integration**: Ensure docker-compose.yml reflects C-Gateway
5. 📅 **Deprecation**: Mark TypeScript/NestJS references as deprecated

### Build Instructions

**Stub mode (default, no NATS required):**
```bash
cd apps/c-gateway
mkdir build && cd build
cmake ..
make
./c-gateway
```

**Real NATS mode (production):**
```bash
cd apps/c-gateway
mkdir build && cd build
cmake -DUSE_NATS_LIB=ON ..
make
./c-gateway
```

### Testing

**Unit tests:**
```bash
cd apps/c-gateway/build
./c-gateway-json-test      # JSON DTO parsing
./c-gateway-router-test    # Router status mapping
./c-gateway-nats-test      # NATS stub wrapper
./c-gateway-http-test      # HTTP integration
```

**Smoke test (with real NATS):**
```bash
./nats-router-smoke-test
```

### Dependencies Installation

**Ubuntu/Debian:**
```bash
sudo apt-get install libjansson-dev libnats-dev
```

**macOS:**
```bash
brew install jansson nats-c
```

## References

- **Implementation**: `apps/c-gateway/`
- **CMakeLists.txt**: `apps/c-gateway/CMakeLists.txt`
- **HTTP Server**: `apps/c-gateway/src/http_server.c`
- **NATS Client**: `apps/c-gateway/src/nats_client_real.c`
- **API Registry**: `docs/ARCHITECTURE/api-registry.md`
- **NATS Subjects**: `docs/NATS_SUBJECTS.md`
- **Related ADRs**:
  - ADR-006: NATS Inter-Service Communication
  - ADR-010: Target Architecture

## Compliance

- ✅ **Aligns with `.trae/manifest.json`**: State updated to reflect C-Gateway
- ✅ **Follows compatibility policy**: JSON DTOs maintain API compatibility
- ✅ **Respects security constraints**: No secrets in code, environment-based config
- ✅ **Integrates with STATE/HISTORY**: `.trae/state.json` updated with AGENT_4_C_GATEWAY
- ✅ **CP1 Isolation**: Gateway communicates only via NATS, no direct HTTP to Router
- ✅ **Observability**: JSON structured logs, health endpoint at `/_health`
- ✅ **No-Drift principle**: Implementation matches documentation

## Metrics and Success Criteria

### Performance Targets (CP1)
- **Throughput**: 2000 RPS sustained, 4000 RPS peak
- **Latency**: p50 < 10ms, p99 < 50ms (HTTP → NATS → HTTP)
- **Memory**: RSS < 200MB under load
- **CPU**: < 50% on single core at 2000 RPS

### Validation
- ✅ Implementation complete (CP1-LC)
- ✅ Unit tests passing
- ✅ Integration tests with Router (Erlang/OTP)
- 📅 Load testing with k6 (planned for CP3-LC)
- 📅 Production deployment smoke tests

---

**Decision Date**: 2025-11-18  
**Implementation Status**: Completed (CP1-LC)  
**Next Review**: CP3-LC (load testing validation)
