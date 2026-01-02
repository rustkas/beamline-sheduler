# CP3 Execution Environment Requirements

## 1. Executive Summary
CP3 Execution is currently **PARTIALLY BLOCKED** due to environment limitations. To achieve full execution (Infra validation, Full Performance profiling, Chaos Testing), the execution environment must meet the requirements defined in this document.

## 2. Container Runtime (Docker)
**Status**: CRITICAL MISSING DEPENDENCY
**Requirement**:
*   **Docker Daemon**: Must be accessible (`/var/run/docker.sock` or TCP).
*   **Docker Compose**: Version v2.0+ required for orchestration.
*   **Privileges**: CI/User must have permission to spawn containers, create networks, and mount volumes.

**Justification**:
*   **Infra**: `validate-env.sh` and `up.sh` strictly depend on Docker.
*   **Chaos**: Fault injection (Pumba/Chaos Mesh) requires container manipulation (kill, pause, network isolate).
*   **Perf**: Full system load testing requires NATS and Database to be running in known configurations (containers).

## 3. Network Requirements
**Status**: BLOCKED (for Chaos)
**Requirement**:
*   **Bridge Networking**: Ability to create isolated bridge networks.
*   **Traffic Control (tc)**: Kernel modules `sch_netem` (or equivalent) for network simulation (latency, packet loss).
*   **Port Availability**: Ports 8080 (Gateway), 4222 (NATS), 5432 (Postgres) must be free or mappable.

**Justification**:
*   **Chaos**: Simulating NATS partition requires network manipulation impossible on shared host localhost.

## 4. Storage & State
**Status**: PARTIAL
**Requirement**:
*   **Volume Mounts**: Ability to mount local directories into containers (for config, logs).
*   **Tmpfs**: Support for tmpfs mounts for high-performance ephemeral storage (NATS JetStream).

## 5. CI/CD Parity
**Status**: GAP IDENTIFIED
**Requirement**:
*   The CI environment must match the local dev environment's capabilities (Docker-in-Docker or accessible Daemon).
*   If CI cannot support Docker, the project must formally downgrade CP3 scope to "In-Process Only" (ADR required).

## 6. Current Gap Analysis
| Component | CP3 Requirement | Current State | Impact |
|-----------|-----------------|---------------|--------|
| **Docker** | Present & Running | **MISSING** | Infra Scripts Fail, Chaos Blocked |
| **NATS** | Managed Container | Host/Mock | Chaos Blocked, Perf Limited (crashes) |
| **Postgres**| Managed Container | Missing | Infra Validation Fails |
| **Network** | Isolated Bridge | Localhost | No Network Fault Injection |

## 7. Recommendation
1.  **Provision Docker**: Ensure the execution environment has a running Docker daemon.
2.  **Or**: **Descope CP3**: If Docker is impossible, officially remove Chaos Testing and Full Integration Testing from CP3 Scope via ADR.
