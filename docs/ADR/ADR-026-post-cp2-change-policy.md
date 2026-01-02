# ADR-026: Post-CP2 Change Policy

> **Status**: Accepted
> **Date**: 2025-12-21
> **Author**: System Architect
> **Applies to**: CP3 Phase

## Context

The project has successfully reached **CP2-LC (Checkpoint 2 - Limited Capability)**. The architecture and core feature set have been baselined in the [Architecture Snapshot CP2-LC](../ARCHITECTURE_SNAPSHOT_CP2_LC.md).

As we move into **CP3 (Production Readiness)**, there is a risk of "definition drift" where architectural changes or new feature requests might destabilize the system or invalidate the validation work being done. Without a formal change policy, it is unclear which changes are safe to make and which require governance oversight.

## Decision

We adopt a **Strict Change Policy** for the post-CP2 lifecycle. This policy classifies all proposed changes into three categories: **Allowed**, **Restricted (Requires ADR)**, and **Forbidden**.

### 1. Allowed Changes (Green Lane)
These changes can be made via standard PR review process without architectural approval.

*   **Bug Fixes**: Corrections to behavior that deviates from the CP2 specification.
*   **CP3 Scope Implementation**: Work explicitly defined in [CP3 Scope Definition](../CP3_SCOPE_DEFINITION.md) (e.g., observability, deployment scripts, chaos tests).
*   **Documentation**: Updates to guides, runbooks, and comments that do not alter system behavior.
*   **Refactoring (Internal)**: Code cleanup within a module that does not change its external API, contracts, or performance characteristics.

### 2. Restricted Changes (Yellow Lane - Requires ADR)
These changes require a new **Architecture Decision Record (ADR)** and formal approval before implementation.

*   **Architecture Changes**: Any modification to the [Architecture Snapshot](../ARCHITECTURE_SNAPSHOT_CP2_LC.md) (e.g., adding new services, changing NATS subjects, altering the supervision tree).
*   **New Features**: Functionality not present in CP2 and not explicitly listed in CP3 Scope.
*   **Contract Changes**: Modifications to gRPC Protobuf definitions or NATS subject schemas.
*   **Dependency Changes**: Adding major new dependencies or changing core libraries (e.g., OTP version, Cowboy version).

### 3. Forbidden Changes (Red Lane)
These changes are strictly prohibited during the CP3 phase.

*   **Retroactive CP2 Scope Changes**: Attempting to "sneak in" features that were cut from CP2.
*   **Breaking Changes without Migration**: Modifying APIs/ABIs in a way that breaks existing clients without a documented migration path.
*   **Unauthorized Architectural Refactoring**: "Rewrite" of core components (Router/Gateway) without an ADR.

## Consequences

### Positive
*   **Stability**: The system remains stable for performance and chaos testing.
*   **Clarity**: Developers know exactly what requires approval.
*   **Focus**: The team stays focused on CP3 goals (hardening) rather than new feature creep.

### Negative
*   **Overhead**: Small but "architectural" changes require documentation (ADR).
*   **Friction**: Rapid iteration on features is intentionally slowed down to favor stability.

## References

*   [Architecture Snapshot CP2-LC](../ARCHITECTURE_SNAPSHOT_CP2_LC.md)
*   [CP3 Scope Definition](../CP3_SCOPE_DEFINITION.md)
*   [Project Freeze CP2-LC](../PROJECT_FREEZE_CP2_LC.md)
