# WRK‑2 REPORT — Task G.1: Cross-link Beamline Vision ↔ CP1/CP2/ABI Bridge

**Worker:** wrk‑2  
**Task ID:** G.1  
**Date:** 2025-11-17  
**Status:** completed

---

## 1. Objective

Create a cross-linking network between the high-level Beamline Vision document and the CP1/CP2/ABI bridge documents, so that a reader can easily navigate:

- from the product vision and roadmap (Vision) to implementation-level specs (CP2 readiness, ABI bridge), and
- from CP1/CP2/ABI documents back to the Vision as the single source of truth for roadmap and architecture.

Target documents:

- docs/BEAMLINE_VISION_AND_ARCHITECTURE.md
- docs/archive/dev/CP1_ARCHITECTURE_CHECKLIST.md
- docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY.md
- docs/archive/dev/ORCHESTRATOR_ROUTER_ABI_BRIDGE.md

---

## 2. Changes

### 2.1 BEAMLINE_VISION_AND_ARCHITECTURE.md → CP2 & ABI bridge

**File:** docs/BEAMLINE_VISION_AND_ARCHITECTURE.md  
**Section:** Roadmap & Checkpoints (near 9.2)

- Added a bilingual subsection:
  - 9.2.x CP2 and ABI bridge details / Детали CP2 и ABI bridge
- The subsection includes navigation links:
  - docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY.md — CP2 readiness and feature inventory for the Router/Gateway.
  - docs/archive/dev/ORCHESTRATOR_ROUTER_ABI_BRIDGE.md — ABI evolution bridge between the Orchestrator and the Router.

### 2.2 CP1_ARCHITECTURE_CHECKLIST.md → Vision

**File:** docs/archive/dev/CP1_ARCHITECTURE_CHECKLIST.md  
**Location:** Intro / Purpose section

- Added a bilingual Note block that:
  - Clarifies CP1-ROUTER as one checkpoint within the broader Beamline roadmap.
  - Points to docs/BEAMLINE_VISION_AND_ARCHITECTURE.md as the source of:
    - high-level product vision,
    - component map,
    - CP0–CP8 lifecycle.

### 2.3 CP2_READINESS_ROUTER_GATEWAY.md → Vision

**File:** docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY.md  
**Location:** Context / Relation to Beamline Roadmap

- Added a bilingual context paragraph that:
  - States this document is the implementation-level readiness and gap analysis for CP2 (“Transport Online”).
  - Links back to docs/BEAMLINE_VISION_AND_ARCHITECTURE.md for:
    - high-level context,
    - full CP0–CP8 sequence.

### 2.4 ORCHESTRATOR_ROUTER_ABI_BRIDGE.md → Vision

**File:** docs/archive/dev/ORCHESTRATOR_ROUTER_ABI_BRIDGE.md  
**Location:** Intro

- Added bilingual introductory context that:
  - Explains this ABI bridge spec describes how the contract between Orchestrator and Router evolves across Beamline checkpoints.
  - Links to docs/BEAMLINE_VISION_AND_ARCHITECTURE.md for:
    - overall product vision,
    - component map,
    - CP0–CP8 roadmap.

---

## 3. Resulting Navigation Network

- From Vision (docs/BEAMLINE_VISION_AND_ARCHITECTURE.md):
  - Direct links to:
    - CP2 readiness (Router/Gateway),
    - ABI evolution bridge (Orchestrator ↔ Router).

- From CP1/CP2/ABI documents:
  - Direct links back to Vision as the canonical source for:
    - roadmap,
    - high-level architecture,
    - checkpoint sequence CP0–CP8.

- Properties:
  - Bidirectional navigation (Vision ↔ CP1 ↔ CP2 ↔ ABI Bridge).
  - Bilingual (EN/RU) text for all new links.
  - No duplication of spec content – only navigation hints.
  - Formatting consistent with existing documentation style (docs/TYPOGRAPHY_STYLE.md).

---

## 4. Notes / Follow-ups

- No behavioral or code changes were introduced; this task is purely documentation/navigation.
- Future documentation updates for CP3+ should follow the same pattern:
  - Vision ↔ CPn readiness / specs ↔ ABI/API evolution docs.