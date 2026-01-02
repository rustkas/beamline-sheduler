# Ops Readiness Canon (CP3)

**Status**: DRAFT
**Owner**: Ops/SRE Team
**Last Updated**: 2025-12-22

## 1. Core Philosophy: "The Manageable System"
A production-ready system must not rely on "tribal knowledge" or "magic".
*   **Auditability**: If it happened, it must be in the logs (structured).
*   **Observability**: If it's slow or broken, metrics must show *why*.
*   **Predictability**: Failure modes must be defined and tested.
*   **Security**: Secrets are managed, not hardcoded. PII is protected.

## 2. Observability Contract

### 2.1 Structured Logging
All application logs MUST be machine-parsable (JSONL).
*   **Format**: JSON Lines (`.jsonl`).
*   **Required Fields**: `timestamp`, `level`, `component`, `message`, `trace_id`.
*   **Implementation**: `router_logger.erl` uses `jsx` for encoding.
*   **PII Policy**: Fields matching PII patterns (e.g., `phone`, `email`, `card`) MUST be masked before logging.
    *   *Reference*: `router_logger:mask_pii/1`.

### 2.2 Metrics (RED Method)
All critical paths MUST emit RED metrics (Rate, Errors, Duration).
*   **Storage**: In-memory ETS tables with read/write concurrency optimization.
*   **Label Normalization**: High-cardinality labels (e.g., user input) MUST be normalized to prevent metric explosion.
    *   *Reference*: `router_metrics:normalize_labels/1`.
*   **Naming**: Snake_case, hierarchical (e.g., `router_http_request_duration_seconds`).

## 3. Security Posture

### 3.1 Secret Management
*   **No Hardcoded Secrets**: Source code MUST NOT contain actual secrets.
*   **Configuration**: Secrets are injected via Environment Variables (`.env` or orchestration).
*   **Validation**:
    *   `admin_api_key`: MUST be binary, minimum 8 bytes.
    *   *Warning*: Default template uses `CHANGE_ME`. Production checks must flag this.
    *   *Reference*: `router_config_validator:validate_config_value/2`.

### 3.2 Access Control
*   **Least Privilege**: Processes should run as non-root users inside containers.
*   **Admin API**: Protected by `admin_api_key`.

## 4. Runbook Index (To Be Created)
Canonical procedures for operational incidents.

| Incident | Runbook File | Status |
| :--- | :--- | :--- |
| High Latency / Slow Responses | `docs/runbooks/HIGH_LATENCY.md` | ⏳ Pending |
| Dependency Failure (LLM/DB) | `docs/runbooks/DEPENDENCY_FAILURE.md` | ⏳ Pending |
| Process Crash / Restart Loop | `docs/runbooks/RESTART_PROCEDURES.md` | ⏳ Pending |

## 5. Failure Semantics
*   **Degraded Mode**: System should serve cached/fallback responses if dependencies fail (Variant B goal).
*   **Crash Recovery**: Supervision trees must restart workers cleanly without dropping active requests if possible.
