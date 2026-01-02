---
version: 1.0
authors:
  - Agent 1: Repo/State Bootstrap (CP0-LC)
last_update: 2025-11-05T13:31:22Z
status: approved
---

# Agent Assignment Guide

## Overview

This document describes how agents are assigned to checkpoints and their responsibilities.

## Agent Roles

### Agent 1: Repo/State Bootstrap (CP0-LC)

**Status**: [COMPLETED] Completed  
**Scope**: Design and specification only

**Responsibilities**:
- Monorepo structure specification
- State management artifacts (`.trae/state.json`, `.trae/history.json`)
- Contract specifications (protobuf, NATS, DDL)
- Documentation catalog

**Rules**: `.cursor/rules/agents/wrk-1-schemas-manifest-ci-gates.mdc`

### Agent 2: Router Core (CP1-ROUTER)

**Status**: ?? Ready for assignment  
**Scope**: Implementation (Erlang/OTP)

**Responsibilities**:
- Implement router core logic
- gRPC service implementation
- NATS integration
- Mnesia/ETS integration
- Policy management
- Metrics and tracing

**Specification**: `docs/CP1_ROUTER_SPEC.md`  
**Rules**: `.cursor/rules/agents/agent-2-router-core.mdc` (to be created)

### Agent 3: Provider Adapters (CP2-PROVIDER)

**Status**: ?? Planned  
**Scope**: Implementation (Erlang/OTP)

**Responsibilities**:
- Provider adapter implementations
- Provider invocation logic
- Cost tracking
- Error handling

### Agent 4: C-Gateway (CP1-LC)

**Status**: ✅ Completed  
**Scope**: C11 HTTP Gateway implementation

**Responsibilities**:
- C11 HTTP REST API Gateway (`apps/c-gateway/`)
- NATS integration (libnats)
- JSON DTO parsing (jansson)
- Rate limiting and health checks
- ADR-016: Migration from TypeScript to C11

### Agent 5: UI Integration (CP4-INTEGRATION)

**Status**: 📅 Planned  
**Scope**: Implementation (SvelteKit)

**Responsibilities**:
- SvelteKit UI implementation
- Integration testing
- End-to-end workflows

## Assignment Process

1. **Orchestrator** reviews current CP status
2. **Orchestrator** assigns task to appropriate agent
3. **Agent** receives task specification
4. **Agent** implements according to specification
5. **Agent** validates against acceptance criteria
6. **Orchestrator** verifies and advances CP

## Agent Communication

Agents communicate through:
- `.trae/state.json`: Current state and agent status
- `.trae/history.json`: Audit trail with HMAC
- `docs/CP*_SPEC.md`: Detailed specifications
- Artifact checksums: Verification of work

## Rules Isolation

Each agent has isolated rules:
- `.cursor/rules/agents/agent-{N}-{name}.mdc`
- No conflicts between agents
- Shared rules in `.cursor/rules/core/` and `.cursor/rules/development/`

## References

- Orchestrator rules: see `docs/ADR/ADR-010-target-architecture.md`
- `docs/CP_TRANSITION_GUIDE.md`: CP transition process
- `docs/CP1_ROUTER_SPEC.md`: CP1-ROUTER specification
- `docs/CP1_CHECKLIST.md`: CP1-ROUTER acceptance checklist

## ???-???? ??????? CP1

??? ???????? ?????????? CP1-ROUTER ??. ???-???? ???????: **[docs/CP1_CHECKLIST.md](../archive/dev/CP1_CHECKLIST.md)**



