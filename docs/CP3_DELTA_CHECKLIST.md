# CP3 Delta Checklist

**Purpose**: Items explicitly deferred from CP2 to CP3/Pre-Release  
**Date**: 2025-01-27  
**Status**: 📋 **CP3 DELTA** (not started)

## Overview

This document lists all items that were **explicitly deferred** from CP2 to CP3/Pre-Release. These items are **not part of CP2 scope** but are planned for future implementation.

**Key Principle**: CP3 items are **additive** to CP2. CP2 baseline must remain green when CP3 items are implemented.

---

## Deferred Items

### 1. DevState Pre-commit/Pre-push Hooks

**Scope**: Automated Git hooks for DevState verification before commits/pushes  
**Status**: ✅ **IMPLEMENTED** (CP3)  
**Reason**: Documentation exists, but hooks require manual setup (not automated in CP2)  
**Reference**: 
- `docs/CP2_CHECKLIST.md` (CI DevState gates section)
- `devstate-tools/devstate/docs/IDE_INTEGRATION.md` (lines 154-181 - pre-push hook example)
- `docs/archive/dev/CP2_CHECKLIST_GAP_REPORT.md` (CI DevState Gates section)

**CP3 Implementation**: ✅ **COMPLETED** (2025-01-27)
- ✅ Pre-commit hook: `scripts/hooks/pre-commit` - Fast validation when DevState files are staged
- ✅ Pre-push hook: `scripts/hooks/pre-push` - Comprehensive validation before push (strict No-Drift)
- ✅ Installation script: `scripts/install_devstate_hooks.sh` - Automated installation with conflict handling
- ✅ Makefile target: `make install-devstate-hooks` - One-command installation
- ✅ Documentation: `devstate-tools/devstate/docs/HOOKS.md` - Complete hook documentation

**Installation**:
```bash
make install-devstate-hooks
# Or: bash scripts/install_devstate_hooks.sh --backup --force
```

**Documentation**: `devstate-tools/devstate/docs/HOOKS.md`

---

### 11. DevState CLI UX and Auto-Repair

**Scope**: Improve developer UX for DevState CLI and add safe auto-repair helpers

**Status**: 📋 **PLANNED (CP3 increment after hooks)**

**Rationale**: After hooks, developers need faster recovery from common issues (broken HMAC chain, checksum drift) with clear guidance.

**Deliverables**:
- `scripts/devstate_auto_repair.sh` — guided fix for HMAC/Checksums with backup and diff preview
- `devstate-tools/devstate/docs/CLI_UX.md` — compact usage and recovery flows
- Integration in CI: optional non-blocking hint step to suggest auto-repair when drift detected

**Key Behaviors**:
- Always create backups before mutation
- Print exact deltas and impacted artifacts
- Respect No-Drift: prefer export/import via DevState API over local mutation; local mutation allowed only with explicit confirmation flags

**References**:
- `docs/archive/dev/CP3_DEVSTATE_HOOKS_PLAN.md` (post-hooks UX)
- `devstate-tools/devstate/docs/HOOKS.md` (messages/hints)

---

### 12. IDE Plugins (DevState HMAC/Schema Hints)

**Scope**: Lightweight IDE hints (VS Code) for `.trae/*` edits — schema validation, HMAC chain warning, quick actions to export/import

**Status**: 📋 **PLANNED (CP3 increment)**

**Deliverables**:
- `tools/vscode-devstate/` — minimal extension skeleton with diagnostics
- `devstate-tools/devstate/docs/IDE_PLUGIN_UX.md` — short guide

**Notes**:
- Non-blocking hints; hooks remain the enforcement layer
- Reuse existing scripts (`devstate_verify.sh`) for diagnostics

---

### 2. Full E2E Headers Propagation Tests

**Scope**: Complete end-to-end test for headers propagation: REST → NATS → CAF → Usage  
**Status**: ⚠️ **DEFERRED TO CP3/Pre-Release**  
**Reason**: Partial tests exist, but full chain (REST → NATS → CAF → Usage) not fully covered  
**Reference**: 
- `docs/CP2_CHECKLIST.md` (Headers propagation section)
- `docs/archive/dev/CP2_CHECKLIST_GAP_REPORT.md` (Priority Actions section)

**CP3 Action**: Create `router_headers_propagation_e2e_SUITE.erl` with full chain test

---

### 3. Dashboards and Alerts

**Scope**: Production-ready Grafana dashboards and Alertmanager rules for Router/Gateway/Worker  
**Status**: ⚠️ **DEFERRED TO CP3/Pre-Release**  
**Reason**: Deferred to release infrastructure (not in CP2 scope)  
**Reference**: 
- `docs/CP2_CHECKLIST.md` (Observability expansion section)
- `docs/archive/dev/CP2_CHECKLIST_GAP_REPORT.md` (Low Priority section)
- `docs/archive/dev/CP2_OBSERVABILITY_BACKLOG.md` (Wave 2+)

**CP3 Action**: 
- Create Grafana dashboards for Router/Gateway/Worker metrics
- Define Alertmanager rules for production monitoring
- Integrate with release infrastructure

---

### 4. Global and Multi-level Rate Limiting

**Scope**: Global scope rate limiting and multi-level checks (global → tenant → policy)  
**Status**: ⚠️ **DEFERRED TO CP2+**  
**Reason**: Per-policy and per-tenant rate limiting implemented; global scope and multi-level checks deferred  
**Reference**: 
- `docs/CP2_CHECKLIST.md` (Rate Limiting section)
- `docs/archive/dev/RATE_LIMIT_POLICY_DSL_DESIGN.md`

**CP3 Action**: 
- Implement global scope rate limiting
- Implement multi-level checks (global → tenant → policy hierarchy)

---

### 5. Full Backpressure Gateway → Router Integration

**Scope**: Complete end-to-end backpressure protocol between Gateway and Router  
**Status**: ⚠️ **DEFERRED TO CP3/Pre-Release**  
**Reason**: Design complete, but full integration deferred to CP3  
**Reference**: 
- `docs/archive/dev/CP2_WORKER_BACKPRESSURE_DESIGN.md`
- `docs/archive/dev/CP2_WORKER_RELIABILITY_BACKLOG.md` (Wave 2)
- `docs/archive/dev/CP2_CHECKLIST_GAP_REPORT.md` (Deferred to CP3 section)

**CP3 Action**: 
- Implement full Gateway → Router backpressure integration
- Add end-to-end overload scenarios tests
- Verify backpressure protocol in production-like environment

---

### 6. Distributed Rate Limiting

**Scope**: Connection pooling, retry logic, circuit breaker integration for distributed rate limiting  
**Status**: ⚠️ **DEFERRED TO CP3/Pre-Release**  
**Reason**: PoC → Production transition deferred to CP3  
**Reference**: 
- `docs/archive/dev/CP2_WORKER_RELIABILITY_BACKLOG.md` (Wave 2)
- `docs/archive/dev/CP2_CHECKLIST_GAP_REPORT.md` (Deferred to CP3 section)

**CP3 Action**: 
- Implement connection pooling for distributed rate limiting
- Add retry logic and circuit breaker integration
- Production-ready distributed rate limiting

---

### 7. Abuse Detection

**Scope**: Production alerting integration, dashboard definitions for abuse detection  
**Status**: ⚠️ **DEFERRED TO CP3/Pre-Release**  
**Reason**: Production alerting integration and dashboard definitions deferred  
**Reference**: 
- `docs/archive/dev/CP2_WORKER_RELIABILITY_BACKLOG.md` (Wave 2)
- `docs/archive/dev/CP2_CHECKLIST_GAP_REPORT.md` (Deferred to CP3 section)

**CP3 Action**: 
- Integrate abuse detection with production alerting
- Define dashboard definitions for abuse detection
- Add abuse detection metrics and alerts

---

### 8. SLO/SLI Gates

**Scope**: Blocking mode in CI, production SLO monitoring  
**Status**: ⚠️ **DEFERRED TO CP3/Pre-Release**  
**Reason**: Blocking mode in CI and production SLO monitoring deferred  
**Reference**: 
- `docs/archive/dev/CP2_WORKER_RELIABILITY_BACKLOG.md` (Wave 2)
- `docs/archive/dev/CP2_CHECKLIST_GAP_REPORT.md` (Deferred to CP3 section)

**CP3 Action**: 
- Implement blocking mode in CI for SLO/SLI gates
- Add production SLO monitoring
- Define SLO/SLI thresholds and alerting rules

---

### 9. Additional Observability Metrics

**Scope**: Additional metrics for all components beyond Wave 1 base contract  
**Status**: ⚠️ **DEFERRED TO CP3/Pre-Release**  
**Reason**: Wave 1 base contract complete; additional metrics deferred to Wave 2+  
**Reference**: 
- `docs/archive/dev/CP2_OBSERVABILITY_BACKLOG.md` (Wave 2+)
- `docs/archive/dev/CP2_WAVE1_PRODUCT_SUMMARY.md` (Out of Scope section)

**CP3 Action**: 
- Implement additional metrics for Router/Gateway/Worker
- Add advanced analytics metrics
- Extend metrics contract beyond Wave 1

---

### 10. Full Cross-component Tracing

**Scope**: End-to-end trace collection and visualization across all components  
**Status**: ⚠️ **DEFERRED TO CP3/Pre-Release**  
**Reason**: Minimal OTel spans implemented (Router); full cross-component tracing deferred  
**Reference**: 
- `docs/archive/dev/CP2_OBSERVABILITY_BACKLOG.md` (Wave 2+)
- `docs/archive/dev/CP2_ROUTER_OTEL_MINIMAL_SCOPE.md`
- `docs/archive/dev/CP2_WAVE1_PRODUCT_SUMMARY.md` (Out of Scope section)

**CP3 Action**: 
- Implement full OTel spans for Gateway and Worker
- Add end-to-end trace collection
- Integrate with trace visualization (Jaeger/Tempo)

---

## Summary Table

| Item | Component | Scope | Reason for Deferral | Reference |
|------|-----------|-------|---------------------|-----------|
| Pre-commit/pre-push hooks | DevState | Automated Git hooks | Manual setup required (not automated in CP2) | `docs/CP2_CHECKLIST.md` (CI DevState gates) |
| Full E2E headers propagation | Router/Gateway | REST → NATS → CAF → Usage | Partial tests exist, full chain not covered | `docs/CP2_CHECKLIST.md` (Headers propagation) |
| Dashboards/alerts | Router/Gateway/Worker | Grafana dashboards, Alertmanager rules | Deferred to release infrastructure | `docs/CP2_CHECKLIST.md` (Observability expansion) |
| Global/multi-level rate limiting | Router | Global scope, multi-level checks | Per-policy/tenant implemented; global deferred | `docs/CP2_CHECKLIST.md` (Rate Limiting) |
| Full backpressure Gateway→Router | Worker | End-to-end backpressure | Design complete, integration deferred | `docs/archive/dev/CP2_WORKER_BACKPRESSURE_DESIGN.md` |
| Distributed rate limiting | Worker | Connection pooling, retry logic, CB | PoC → Production transition deferred | `docs/archive/dev/CP2_WORKER_RELIABILITY_BACKLOG.md` |
| Abuse detection | Worker | Production alerting, dashboards | Production integration deferred | `docs/archive/dev/CP2_WORKER_RELIABILITY_BACKLOG.md` |
| SLO/SLI gates | Worker | Blocking CI, production SLO | Blocking mode and production monitoring deferred | `docs/archive/dev/CP2_WORKER_RELIABILITY_BACKLOG.md` |
| Additional observability metrics | Observability | Beyond Wave 1 base contract | Wave 1 complete; additional metrics deferred | `docs/archive/dev/CP2_OBSERVABILITY_BACKLOG.md` |
| Full cross-component tracing | Observability | End-to-end trace collection | Minimal spans implemented; full tracing deferred | `docs/archive/dev/CP2_OBSERVABILITY_BACKLOG.md` |

---

## CP3 Planning Notes

### Priority Order (Suggested)

1. **High Priority** (Critical for production):
   - Dashboards and alerts (required for production monitoring)
   - Full backpressure Gateway → Router integration (required for production reliability)
   - SLO/SLI gates (required for production SLO monitoring)

2. **Medium Priority** (Important for production quality):
   - Pre-commit/pre-push hooks (improves developer experience and prevents errors)
   - Full E2E headers propagation tests (improves test coverage)
   - Additional observability metrics (improves monitoring capabilities)

3. **Low Priority** (Nice to have):
   - Global/multi-level rate limiting (enhancement to existing rate limiting)
   - Distributed rate limiting (enhancement to existing rate limiting)
   - Abuse detection (enhancement to existing reliability features)
   - Full cross-component tracing (enhancement to existing tracing)

### Dependencies

- **Dashboards/alerts** depend on: Additional observability metrics (some metrics may be needed for dashboards)
- **Full backpressure Gateway → Router** depends on: Backpressure design (already complete)
- **SLO/SLI gates** depend on: Additional observability metrics (SLO/SLI require metrics)

---

## References

- **`docs/CP2_CHECKLIST.md`** - Complete CP2 checklist (shows what was completed and what was deferred)
- **`docs/archive/dev/CP2_OVERALL_COMPLETION_SUMMARY.md`** - CP2 overall completion summary
- **`docs/archive/dev/CP2_CHECKLIST_GAP_REPORT.md`** - Gap analysis report (includes deferred items)
- **`docs/archive/dev/CP2_WORKER_RELIABILITY_BACKLOG.md`** - Worker reliability backlog (Wave 2 items)
- **`docs/archive/dev/CP2_OBSERVABILITY_BACKLOG.md`** - Observability backlog (Wave 2+ items)

---

**Status**: 📋 **CP3 DELTA CHECKLIST** (ready for CP3 planning)  
**Last Updated**: 2025-01-27
