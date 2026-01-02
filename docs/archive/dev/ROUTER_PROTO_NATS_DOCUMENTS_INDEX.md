# Router Proto/NATS Consistency - Documents Index

**Date**: 2025-01-27  
**Purpose**: Navigation guide for all Proto/NATS consistency documents

## Document Overview

This index provides an overview of all documents related to Router Proto/NATS contracts consistency verification and implementation.

---

## Main Documents

### 1. Consistency Report (Analysis)

**File**: `docs/archive/dev/ROUTER_PROTO_NATS_CONSISTENCY.md`

**Purpose**: Comprehensive analysis of Proto/NATS contracts consistency

**Contents**:
- Source of Truth definitions
- Two-level contract architecture (Proto vs NATS JSON)
- Proto ↔ NATS Subjects verification
- Proto Fields ↔ Routing Policy verification
- DSL Policies ↔ DTO/Proto verification
- Summary and action items

**Audience**: Technical leads, architects, developers

**Status**: ✅ Complete

---

### 2. Action Plan (Prioritized Actions)

**File**: `docs/archive/dev/ROUTER_PROTO_NATS_ACTION_PLAN.md`

**Purpose**: Prioritized action plan with CP1 vs CP2-LC split

**Contents**:
- CP1 actions (documentation only)
- CP2-LC actions (Proto changes)
- Priority levels and dependencies
- Execution order
- Risk assessment

**Audience**: Project managers, technical leads

**Status**: ✅ Ready for Implementation

---

### 3. Summary (Brief Version)

**File**: `docs/archive/dev/ROUTER_PROTO_NATS_CONSISTENCY_SUMMARY.md`

**Purpose**: Brief summary for inclusion in CP1 acceptance reports

**Contents**:
- Executive summary (1-2 paragraphs)
- Key findings (bullet points)
- Action items summary
- CP1 readiness assessment

**Audience**: CP1 acceptance reports, executive summaries

**Status**: ✅ Complete

---

## Detailed Implementation Plans

### 4. CP1 Detailed Plan (Step-by-Step)

**File**: `docs/archive/dev/ROUTER_PROTO_NATS_CP1_DETAILED_PLAN.md`

**Purpose**: Detailed step-by-step instructions for CP1 actions

**Contents**:
- Exact file locations and line numbers
- Specific text to add/modify
- Examples of changes
- Verification steps
- Acceptance criteria

**Scope**: Documentation changes only (no code, no Proto)

**Audience**: Developers implementing CP1 actions

**Status**: ✅ Ready for Implementation

**Actions Covered**:
- CP1.1: Document Required Fields Enforcement
- CP1.2: Clarify JSON Format vs Proto Contract
- CP1.3: Document Policy DSL to Proto Conversion
- CP1.4: Document Missing Proto Files Status
- CP1.5: Document Runtime Validation Rules

---

### 5. CP2+ Detailed Plan (Step-by-Step)

**File**: `docs/archive/dev/ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md`

**Purpose**: Detailed step-by-step instructions for CP2-LC actions

**Contents**:
- Procedures for restoring Proto files
- Safe addition of CP2+ fields with ABI compatibility
- Version gates and breaking change detection
- Migration procedures

**Scope**: Proto file changes, code regeneration, ABI validation

**Audience**: Developers implementing CP2-LC actions

**Status**: ✅ Draft - Ready for CP2-LC

**Actions Covered**:
- CP2.1: Restore Proto Source Files
- CP2.2: Add CP2+ Optional Fields to Message
- CP2.3: Add CP2+ Optional Field to RouteRequest

---

## Document Relationships

```
ROUTER_PROTO_NATS_CONSISTENCY.md (Analysis)
    ↓
ROUTER_PROTO_NATS_ACTION_PLAN.md (Prioritized Actions)
    ↓
    ├── ROUTER_PROTO_NATS_CP1_DETAILED_PLAN.md (CP1 Instructions)
    └── ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md (CP2-LC Instructions)
    ↓
ROUTER_PROTO_NATS_CONSISTENCY_SUMMARY.md (Brief for CP Reports)
```

---

## Quick Navigation

### For Analysis
→ Start with: `ROUTER_PROTO_NATS_CONSISTENCY.md`

### For Implementation Planning
→ Start with: `ROUTER_PROTO_NATS_ACTION_PLAN.md`

### For CP1 Implementation
→ Use: `ROUTER_PROTO_NATS_CP1_DETAILED_PLAN.md`

### For CP2-LC Implementation
→ Use: `ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md`

### For CP Reports
→ Use: `ROUTER_PROTO_NATS_CONSISTENCY_SUMMARY.md`

---

## Document Status

| Document | Status | Version | Last Updated |
|----------|--------|---------|--------------|
| Consistency Report | ✅ Complete | 1.0 | 2025-01-27 |
| Action Plan | ✅ Ready | 1.0 | 2025-01-27 |
| Summary | ✅ Complete | 1.0 | 2025-01-27 |
| CP1 Detailed Plan | ✅ Ready | 1.0 | 2025-01-27 |
| CP2+ Detailed Plan | ✅ Draft | 1.0 | 2025-01-27 |

---

## Key Concepts

### Two-Level Contract Architecture
- **Level 1**: Proto Contract (wire protocol / ABI)
- **Level 2**: NATS JSON Format (logical payload with adapter-layer fields)

**See**: `ROUTER_PROTO_NATS_CONSISTENCY.md` Section 1.5

### CP1 vs CP2-LC Split
- **CP1**: Documentation only (no code, no Proto)
- **CP2-LC**: Proto changes, code regeneration, ABI validation

**See**: `ROUTER_PROTO_NATS_ACTION_PLAN.md` for details

### Version Gates
- **CP1 Actions**: Can start immediately (no gates)
- **CP2-LC Actions**: Require `current_cp >= CP2-LC` in DevState

**See**: `ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md` for version gate requirements

---

## References

- **Consistency Report**: `docs/archive/dev/ROUTER_PROTO_NATS_CONSISTENCY.md`
- **Action Plan**: `docs/archive/dev/ROUTER_PROTO_NATS_ACTION_PLAN.md`
- **CP1 Detailed Plan**: `docs/archive/dev/ROUTER_PROTO_NATS_CP1_DETAILED_PLAN.md`
- **CP2+ Detailed Plan**: `docs/archive/dev/ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md`
- **Summary**: `docs/archive/dev/ROUTER_PROTO_NATS_CONSISTENCY_SUMMARY.md`

