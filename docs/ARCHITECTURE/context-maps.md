---
version: 1.1
authors:
  - Agent 2: Architecture/Tech Lead
last_update: 2025-11-18T11:30:00Z
status: approved
changelog:
  - 2025-11-18: Updated NestJS Gateway references to C-Gateway (C11 implementation)
---

# Context Maps

## ??????????

????? ?????????? (Context Maps) ??? ???????????? ??????????? BeamLine Constructor, ?????? ??????? ? ?? ??????????????.

## System Context Diagram

```
+-------------------------------------------------------------+
¦                    External World                            ¦
¦  +--------------+  +--------------+  +--------------+     ¦
¦  ¦   Clients    ¦  ¦   Webhooks   ¦  ¦   Partners   ¦     ¦
¦  ¦  (HTTP/REST) ¦  ¦  (HTTP)      ¦  ¦  (gRPC)      ¦     ¦
¦  +--------------+  +--------------+  +--------------+     ¦
+---------+------------------+------------------+-------------+
          ¦                  ¦                  ¦
          ¦ HTTP/REST        ¦ HTTP             ¦ gRPC
          ?                  ?                  ?
+-------------------------------------------------------------+
¦              BeamLine Constructor                            ¦
¦                                                              ¦
¦  +------------------------------------------------------+  ¦
¦  ¦         C-Gateway (apps/c-gateway/)                  ¦  ¦
¦  ¦  - REST API (/api/v1/*)                              ¦  ¦
¦  ¦  - NATS integration (libnats)                        ¦  ¦
¦  ¦  - JSON DTO parsing (jansson)                        ¦  ¦
¦  +------------------------------------------------------+  ¦
¦                      ¦                                      ¦
¦                      ¦ NATS                                 ¦
¦                      ?                                      ¦
¦  +------------------------------------------------------+  ¦
¦  ¦         Erlang/OTP Layer (apps/otp/*)                ¦  ¦
¦  ¦                                                       ¦  ¦
¦  ¦  +--------------+  +--------------+  +----------+ ¦  ¦
¦  ¦  ¦   Router     ¦  ¦   Provider   ¦  ¦  Usage   ¦ ¦  ¦
¦  ¦  ¦  (Core)      ¦  ¦  (Adapters)  ¦  ¦ (Metrics)¦ ¦  ¦
¦  ¦  +--------------+  +--------------+  +----------+ ¦  ¦
¦  +---------+------------------+----------------+--------+  ¦
¦            ¦                  ¦                ¦            ¦
¦            ¦ gRPC             ¦ gRPC          ¦            ¦
¦            ?                  ?                ¦            ¦
¦  +---------------------------------------------+          ¦
¦  ¦    C++ CAF Layer (apps/caf/*)               ¦          ¦
¦  ¦                                             ¦          ¦
¦  ¦  +--------------+                          ¦          ¦
¦  ¦  ¦   Processor  ¦                          ¦          ¦
¦  ¦  ¦  (Compute)   ¦                          ¦          ¦
¦  ¦  +--------------+                          ¦          ¦
¦  +---------------------------------------------+          ¦
¦                                                              ¦
¦  +------------------------------------------------------+  ¦
¦  ¦              Data Layer                               ¦  ¦
¦  ¦                                                       ¦  ¦
¦  ¦  +--------------+          +--------------+          ¦  ¦
¦  ¦  ¦  PostgreSQL  ¦          ¦   Mnesia/ETS ¦          ¦  ¦
¦  ¦  ¦  (Source of  ¦          ¦  (Hot Path) ¦          ¦  ¦
¦  ¦  ¦   Truth)     ¦          ¦             ¦          ¦  ¦
¦  ¦  +--------------+          +--------------+          ¦  ¦
¦  +------------------------------------------------------+  ¦
¦                                                              ¦
¦  +------------------------------------------------------+  ¦
¦  ¦         UI-Web (Phoenix LiveView) (apps/ui_web/)    ¦  ¦
¦  ¦  - Dashboard, Messages, Policies, Usage             ¦  ¦
¦  +------------------------------------------------------+  ¦
¦                      ¦                                      ¦
¦                      ¦ HTTP/REST                            ¦
¦                      ?                                      ¦
¦              [C-Gateway]                                    ¦
+-------------------------------------------------------------+
```

## Container Diagram

### C-Gateway Container

```
+-----------------------------------------+
¦      C-Gateway Container (C11)          ¦
¦                                         ¦
¦  +-----------------------------------+ ¦
¦  ¦   HTTP Server (http_server.c)      ¦ ¦
¦  ¦   - POST /api/v1/messages          ¦ ¦
¦  ¦   - GET  /api/v1/messages/:id      ¦ ¦
¦  ¦   - GET  /_health                  ¦ ¦
¦  +-----------------------------------+ ¦
¦                 ¦                       ¦
¦  +--------------?--------------------+ ¦
¦  ¦   NATS Client                     ¦ ¦
¦  ¦   - nats_client_real.c            ¦ ¦
¦  ¦   - nats_client_stub.c            ¦ ¦
¦  +-----------------------------------+ ¦
¦                 ¦                       ¦
¦  +--------------?--------------------+ ¦
¦  ¦   NATS Client                     ¦ ¦
¦  ¦   - Request-Reply                 ¦ ¦
¦  ¦   - Pub-Sub                       ¦ ¦
¦  ¦   - Streaming                     ¦ ¦
¦  +-----------------------------------+ ¦
+-----------------------------------------+
```

### Erlang/OTP Router Container

```
+-----------------------------------------+
¦    Erlang/OTP Router Container          ¦
¦                                         ¦
¦  +-----------------------------------+ ¦
¦  ¦   Router Core                      ¦ ¦
¦  ¦   - router_core.erl                ¦ ¦
¦  ¦   - router_decider.erl              ¦ ¦
¦  +-----------------------------------+ ¦
¦                 ¦                       ¦
¦  +--------------?--------------------+ ¦
¦  ¦   Policy Manager                  ¦ ¦
¦  ¦   - router_policy.erl             ¦ ¦
¦  ¦   - Mnesia Cache                  ¦ ¦
¦  +-----------------------------------+ ¦
¦                 ¦                       ¦
¦  +--------------?--------------------+ ¦
¦  ¦   NATS Client                     ¦ ¦
¦  ¦   - beamline.router.v1.decide     ¦ ¦
¦  +-----------------------------------+ ¦
¦                                         ¦
¦  +-----------------------------------+ ¦
¦  ¦   gRPC Service                    ¦ ¦
¦  ¦   - Router.Decide                 ¦ ¦
¦  +-----------------------------------+ ¦
+-----------------------------------------+
```

### C++ CAF Provider Container

```
+-----------------------------------------+
¦    C++ CAF Provider Container           ¦
¦                                         ¦
¦  +-----------------------------------+ ¦
¦  ¦   Provider Broker                  ¦ ¦
¦  ¦   - Fan-out/Fan-in                 ¦ ¦
¦  ¦   - Retry Logic                    ¦ ¦
¦  +-----------------------------------+ ¦
¦                 ¦                       ¦
¦  +--------------?--------------------+ ¦
¦  ¦   Provider Adapters                ¦ ¦
¦  ¦   - OpenAI Adapter                 ¦ ¦
¦  ¦   - Anthropic Adapter              ¦ ¦
¦  ¦   - Google Adapter                 ¦ ¦
¦  +-----------------------------------+ ¦
¦                 ¦                       ¦
¦  +--------------?--------------------+ ¦
¦  ¦   gRPC Client                     ¦ ¦
¦  ¦   - Provider.Invoke               ¦ ¦
¦  ¦   - Provider.InvokeStream         ¦ ¦
¦  +-----------------------------------+ ¦
+-----------------------------------------+
```

## Component Interaction Diagram

```
+-------------+
¦   Client    ¦
+-------------+
       ¦ HTTP POST /api/v1/messages
       ?
+-----------------+
¦ C-Gateway       ¦
¦                 ¦
¦ 1. Parse JSON   ¦
¦ 2. Validate     ¦
¦ 3. NATS publish ¦
+-----------------+
       ¦ NATS: beamline.router.v1.decide
       ?
+-----------------+
¦ Erlang Router   ¦
¦                 ¦
¦ 1. Load Policy  ¦
¦ 2. Apply Rules  ¦
¦ 3. Decide       ¦
+-----------------+
       ¦ NATS: beamline.provider.v1.invoke
       ?
+-----------------+
¦ Erlang Provider ¦
¦                 ¦
¦ 1. Invoke       ¦
¦ 2. Stream       ¦
¦ 3. Return       ¦
+-----------------+
       ¦ Response
       ?
+-----------------+
¦ C-Gateway       ¦
¦                 ¦
¦ 1. Format JSON  ¦
¦ 2. HTTP response¦
+-----------------+
       ¦ HTTP SSE
       ?
+-------------+
¦   Client    ¦
+-------------+
```

## Module Boundaries

### Boundary 1: Gateway ? OTP

**Protocol**: NATS  
**Subjects**: `beamline.router.v1.decide`  
**Data Format**: JSON (NATS) ? Protobuf (internal)  
**Responsibilities**:
- Gateway: HTTP ? NATS transformation
- Router: NATS ? gRPC processing

### Boundary 2: Router ⇄ Providers

**Protocol**: NATS  
**Subjects**: `beamline.provider.{id}.v1`  
**Data Format**: JSON/Protobuf  
**Responsibilities**:
- Router: Decision making & delegation
- Provider: Adapter execution (External/Extension)

### Boundary 3: Gateway ? UI

**Protocol**: HTTP/REST, SSE  
**Endpoints**: `/api/v1/*`  
**Data Format**: JSON  
**Responsibilities**:
- Gateway: API provision
- UI: User interface

### Boundary 4: All ? Data Layer

**Protocol**: SQL (PostgreSQL), Native (Mnesia/ETS)  
**Responsibilities**:
- PostgreSQL: Source of Truth
- Mnesia/ETS: Hot path caching

## References

- `docs/NATS_SUBJECTS.md`: NATS subjects catalog
- `docs/GATEWAY_ROUTES.md`: Gateway API endpoints
- `proto/beamline/flow/v1/flow.proto`: gRPC ABI
- Architecture overview: see `docs/ADR/ADR-010-target-architecture.md`




