# Architecture Snapshot: CP2-LC

> **Status**: Frozen (CP2-LC Baseline)
> **Date**: 2025-12-21
> **Snapshot ID**: cp2-lc-2025-12

## Overview

This document defines the architectural snapshot for **Checkpoint 2 - Limited Capability (CP2-LC)**.
It serves as the authoritative baseline for the system architecture at the completion of CP2.
Any deviations in CP3 must be documented as deltas against this snapshot.

## Core Architectural Documents

The following documents constitute the CP2-LC architectural definition:

### 1. High-Level Vision & Architecture
*   [**BeamLine Vision & Architecture**](./BEAMLINE_VISION_AND_ARCHITECTURE.md)
    *   Defines the separation of Control Plane (Erlang/OTP) and Execution Plane (CAF/C++).
    *   Establishes the "AI Factory" metaphor.

### 2. System Status & Feature Matrix
*   [**CP2 Readiness Summary**](./CP2_READINESS_SUMMARY.md)
    *   Detailed feature matrix for CP2 completion.
    *   Confirms status of JetStream, Idempotency, and Circuit Breakers.
*   [**Roadmap**](./ROADMAP.md)
    *   Contextualizes CP2 within the broader project phases.

### 3. Component Architecture (Router)
*   [**Router Architecture Documentation**](../apps/otp/router/docs/ARCHITECTURE_DOCUMENTATION.md)
    *   **Crucial Reference**: Detailed process tree, supervisor hierarchy, and data flow.
    *   Includes NATS/gRPC interaction flows and internal state management (ETS/Mnesia).

### 4. Transport & Integration
*   [**CAF NATS/JetStream Transport**](./TECHSPEC_CAF_NATS_TRANSPORT.md)
    *   Defines the contract between Router and CAF workers via NATS.
    *   Specifies `caf.exec.assign.v1` and `caf.exec.result.v1` subjects.

## Key Invariants (CP2-LC)

At the CP2-LC stage, the system guarantees:

1.  **Durable Routing**: All routing decisions are idempotent and durable via JetStream.
2.  **Control/Execution Split**: Router never executes heavy compute; it delegates to CAF.
3.  **Strict Contracts**: Communication adheres to defined gRPC and NATS schemas.
4.  **Resilience**: Circuit breakers protect against cascading failures; supervisors handle process crashes.

## Usage

*   **Reference**: Use this snapshot to understand the system state as of CP2 completion.
*   **Diffing**: When proposing CP3 changes, verify if they violate the invariants listed above.
