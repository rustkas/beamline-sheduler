# Infrastructure Environment Contract

## 1. Overview
This document defines the environment requirements, configuration contracts, and validation procedures for the BeamLine Constructor project. It serves as the single source of truth for infrastructure expectations in CP3.

## 2. Tooling & Runtime Dependencies
Strict versioning is enforced to ensure reproducibility across environments (Dev, CI, Prod).

### 2.1. Infrastructure Tools
| Tool | Version Requirement | Purpose |
|------|---------------------|---------|
| **Docker Engine** | `24.0+` | Container runtime. |
| **Docker Compose** | `V2+` | Service orchestration. |
| **Bash** | `4.0+` | Script execution (`scripts/local/*`). |

### 2.2. Application Runtimes (Host-Native Mode)
Required only if building/running services outside Docker.

| Component | Language | Version | Note |
|-----------|----------|---------|------|
| **Router** | Erlang/OTP | `26` | Based on `erlang:26-alpine`. |
| **UI Web** | Elixir | `1.15+` | Must be compatible with Erlang 26. |
| **DevState** | Node.js | `20` | Based on `node:20-alpine`. |
| **Scripts** | Python | `3.10+` | For utility scripts (`infra/*`). |
| **Gateway** | C | `gcc`/`make` | Built on Alpine 3.20. |

## 3. Environment Variables Contract
The application relies on the following environment variables.

| Variable Name | Required | Default (Dev) | Description |
|--------------|----------|---------------|-------------|
| `NATS_URL` | Yes | `nats://nats:4222` | NATS connection URL. |
| `DB_NAME` | Yes | `beamline_dev` | PostgreSQL database name. |
| `DB_USER` | Yes | `postgres` | PostgreSQL user. |
| `DB_PASSWORD` | Yes | `postgres` | PostgreSQL password. |
| `GATEWAY_PORT` | Yes | `8080` | HTTP port for the C-Gateway (internal). |
| `BEAMLINE_HMAC_SECRET` | Yes (Prod) | `dev-secret` | HMAC secret for signing. |

### 3.1. Discrepancies & Notes
- **Variable Naming**: `docker-compose.yml` maps host ports using `C_GATEWAY_HOST_PORT` and `UI_WEB_PORT`. Internal services use standard ports.
- **Validation**: `scripts/validate-env.sh` enforces the presence of these variables.

## 4. Execution Modes

### 4.1. Full Stack (Docker - Primary)
*   **Command**: `./scripts/local/up.sh`
*   **Scope**: Runs all services (Gateway, Router, UI, NATS, DB) in containers.
*   **Validation**: `./scripts/validate-env.sh` (Full check).
*   **Guarantee**: This mode is the primary target for CP3 Infra Execution.

### 4.2. Host-Native (Secondary)
*   **Status**: Partial Support / Advanced.
*   **Use Case**: Rapid development of specific components (e.g., `iex -S mix` for UI).
*   **Requirement**: User must manually provide dependencies (NATS, Postgres) and match Runtime versions defined in Section 2.2.

## 5. Validation Procedures
*   **Script**: `scripts/validate-env.sh`
*   **Checks**:
    1. `.env` file existence.
    2. Docker daemon status.
    3. Docker Compose availability.
    4. Mandatory environment variables.
