---
version: 1.0
status: accepted
date: 2025-11-05
deciders:
  - Agent 1: Repo/State Bootstrap (CP0-LC)
  - Agent 2: Architecture/Tech Lead
related_adrs:
  - ADR-001: Monorepo structure
supersedes: []
superseded_by: []
---

# ADR-002: State Management via .trae/state.json and .trae/history.json

## Status

accepted

## Context

We need a mechanism to:
- Track project state (current CP, agent status, artifact checksums)
- Maintain audit trail with integrity guarantees
- Support CP transitions with validation
- Enable No-Drift enforcement
- Provide transparency for all stakeholders

Requirements:
- Immutable audit trail
- Integrity verification (HMAC)
- Version control friendly
- Human-readable format
- Machine-validatable

## Decision

Use **JSON files** for state management:

1. **`.trae/state.json`**: Current project state
   - Project metadata (project, version, current_cp)
   - Agent statuses
   - Artifact checksums (SHA256)
   - No-Drift flag
   - Updated timestamp

2. **`.trae/history.json`**: Append-only audit trail
   - Array of entries
   - Each entry: timestamp, actor, action, CP transitions, state checksum
   - HMAC chain for integrity
   - Metadata for context

**Validation**:
- JSON-Schema validation (`docs/STATE.schema.json`)
- Checksum verification
- HMAC chain integrity
- Version consistency checks

**HMAC Calculation**:
```
hmac = HMAC_SHA256(
    secret,
    concat(ts, actor, action, state_checksum, hmac_prev)
)
```

## Consequences

### Positive

- **Transparency**: Human-readable JSON format
- **Integrity**: HMAC chain ensures audit trail integrity
- **Version control**: JSON files work well with Git
- **Validation**: JSON-Schema enables automated validation
- **Audit**: Complete history of all changes

### Negative

- **File size**: History file grows over time
- **Manual editing risk**: JSON can be manually edited (mitigated by validation)
- **Secret management**: HMAC requires secret (handled by Agent 6)

### Neutral

- **Format**: JSON is standard but not the only option
- **Performance**: File-based approach is sufficient for current scale

## Alternatives Considered

### Alternative 1: Database-based State

**Description**: Store state in PostgreSQL

**Pros**:
- ACID guarantees
- Query capabilities
- Better for large history

**Cons**:
- Requires database connection
- Harder to version control
- More complex setup

**Why not chosen**: Need for version control integration and simplicity favor file-based approach

### Alternative 2: YAML Format

**Description**: Use YAML instead of JSON

**Pros**:
- More human-readable
- Better for complex structures

**Cons**:
- Less strict validation
- Whitespace sensitivity
- Less tooling support

**Why not chosen**: JSON has better validation tooling and is more standard

### Alternative 3: Binary Format

**Description**: Use binary format (protobuf, MessagePack)

**Pros**:
- Compact
- Fast parsing

**Cons**:
- Not human-readable
- Harder to debug
- Less tooling

**Why not chosen**: Human-readability and transparency are critical requirements

## Implementation Notes

- State structure defined in CP0-LC
- JSON-Schema created (`docs/STATE.schema.json`)
- HMAC chain initialized
- Validation scripts created
- Integration with manifest.json for versioning

## References

- `docs/STATE.schema.json`: State validation schema
- `docs/CP_TRANSITION_GUIDE.md`: CP transition process
- `.trae/manifest.json`: Schema versions and validation tools
- `.cursor/rules/agents/state-and-history-management.mdc`: Agent rules

## Compliance

- ✅ Aligns with `.trae/manifest.json`
- ✅ Follows compatibility policy
- ✅ Respects security constraints
- ✅ Integrates with STATE/HISTORY


