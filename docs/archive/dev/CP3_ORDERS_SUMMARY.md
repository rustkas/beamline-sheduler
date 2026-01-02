---
version: 1.0
created_at: 2025-11-17T00:00:00Z
status: draft
rule_version: v1
message_protocol: v1
---

# CP3 ORDERS Summary - Execution Plan

## Executive Summary

This document provides an overview of planned **CP3** ORDERS related to ingress/gateway and orchestration evolution.
In this repository, CP3 work is focused primarily on:

- Preparing **Ingress / HTTP Gateway infrastructure** as part of CP3-INGRESS.
- Laying the foundation for future Orchestrator integration (see ABI bridge docs).

This summary is intentionally smaller than CP2's and will grow as new CP3 ORDERS are defined.

---

## ORDER Dependency Graph (CP3 Scope)

```
ORDER-WRK-4-OPENRESTY-001 (MEDIUM) [wrk-4]
    │
    └─→ ORDER-WRK-4-OPENRESTY-002 (MEDIUM) [wrk-4]

(Additional CP3 ORDERS for Orchestrator/Workers may be added later)
```

- `ORDER-WRK-4-OPENRESTY-001`: Implement HTTP Gateway (migration path from NestJS).
- `ORDER-WRK-4-OPENRESTY-002`: Decommission NestJS Gateway after successful migration.

---

## ORDERS Overview (CP3-relevant Work)

### 🟡 MEDIUM PRIORITY

#### ORDER-WRK-4-OPENRESTY-001: HTTP Gateway (Migration)

**Owner**: wrk-4 (Gateway Lead)  
**Priority**: 🟡 **MEDIUM**  
**Status**: Planned (CP3/CP4 split)  
**CP Mapping**:
- **CP3-INGRESS**: Infrastructure and core Lua modules (IDs `CP3-GW-001..003`).
- **CP4-INTEGRATION**: HTTP contracts, behaviour parity, tests and traffic switch (IDs `CP4-GW-001..004`).

**Scope (CP3 portion only):**

- `CP3-GW-001`: Skeleton for gateway configuration (nginx.conf, conf.d, Lua layout, Docker).
- `CP3-GW-002`: `beamline.util` module (headers/json/response helpers) with unit tests.
- `CP3-GW-003`: `beamline.router_client` (NATS/Router integration) with tests/mocks.

**Dependencies:**
- CP1-ROUTER completed.
- CP2 foundation in place (Router/Gateway messaging core).

**Blocks:**
- CP4-level tasks for HTTP behaviour parity and UI integration.

**Acceptance (for CP3 portion):**
- HTTP Gateway service builds and starts locally.
- `/healthz` endpoint works.
- Lua utility/router_client modules exist and are covered by unit tests.

---

#### ORDER-WRK-4-OPENRESTY-002: NestJS Gateway De-commissioning

**Owner**: wrk-4 (Gateway Lead)  
**Priority**: 🟡 **MEDIUM**  
**Status**: Planned (depends on ORDER-WRK-4-OPENRESTY-001)  
**CP Mapping**:
- Primarily **CP4-INTEGRATION**, but listed here for visibility as a follow-up step once CP3/CP4 work is stable.

**Scope:**

- Inventory of all NestJS Gateway dependencies (`CP4-GW-DC-001`).
- Ensuring all environments use only the new HTTP Gateway (`CP4-GW-DC-002`).
- Removal of `apps/gateway/` and related CI/infra artefacts (`CP4-GW-DC-003`).
- Documentation and ADR updates (`CP4-GW-DC-004`).
- Final CP/CI validation after removal (`CP4-GW-DC-005`).

**Dependencies:**
- Successful completion and stabilisation of ORDER-WRK-4-OPENRESTY-001.

**Blocks:**
- None beyond migration readiness.

**Acceptance:**
- NestJS Gateway fully removed from the monorepo and infrastructure.
- ADR statuses updated (ADR-008 `superseded`, ADR-016 `accepted`).
- All CI/CP checks pass without NestJS.

---

## Execution Timeline (High-Level)

### Phase 1 (CP3-INGRESS Focus)

| Task ID       | ORDER ID                      | Owner | Status  |
|---------------|-------------------------------|-------|---------|
| CP3-GW-001    | ORDER-WRK-4-OPENRESTY-001     | wrk-4 | Planned |
| CP3-GW-002    | ORDER-WRK-4-OPENRESTY-001     | wrk-4 | Planned |
| CP3-GW-003    | ORDER-WRK-4-OPENRESTY-001     | wrk-4 | Planned |

Goals:

- Bring up HTTP Gateway skeleton.
- Implement base Lua utility and Router client modules.
- Ensure integration with Router/NATS exists at a minimal, testable level.

### Phase 2 (CP4-INTEGRATION Focus, Gateway & UI)

CP4-level tasks continue the work from CP3 and are listed here only for continuity:

| Task ID       | ORDER ID                             | Owner | Status  |
|---------------|--------------------------------------|-------|---------|
| CP4-GW-001    | ORDER-WRK-4-OPENRESTY-001           | wrk-4 | Planned |
| CP4-GW-002    | ORDER-WRK-4-OPENRESTY-001           | wrk-4 | Planned |
| CP4-GW-003    | ORDER-WRK-4-OPENRESTY-001           | wrk-4 | Planned |
| CP4-GW-004    | ORDER-WRK-4-OPENRESTY-001           | wrk-4 | Planned |
| CP4-GW-DC-001 | ORDER-WRK-4-OPENRESTY-002           | wrk-4 | Planned |
| CP4-GW-DC-002 | ORDER-WRK-4-OPENRESTY-002           | wrk-4 | Planned |
| CP4-GW-DC-003 | ORDER-WRK-4-OPENRESTY-002           | wrk-4 | Planned |
| CP4-GW-DC-004 | ORDER-WRK-4-OPENRESTY-002           | wrk-4 | Planned |
| CP4-GW-DC-005 | ORDER-WRK-4-OPENRESTY-002           | wrk-4 | Planned |

---

## Success Criteria for CP3 Scope

- ✅ HTTP Gateway service exists and can be started independently.
- ✅ Core Lua modules (`util`, `router_client`) реализованы и покрыты тестами.
- ✅ Интеграция с Router/NATS доступна на уровне CP3 (ingress ready).
- ✅ CP3-INGRESS в Router/Gateway части может ссылаться на этот документ и соответствующие ORDER-файлы как на исполнение плана.

---

**Document Version**: 1.0  
**Last Updated**: 2025-11-17T00:00:00Z  
**Rule Version**: v1  
**Message Protocol**: v1
