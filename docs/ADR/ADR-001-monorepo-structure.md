---
version: 1.0
status: accepted
date: 2025-11-05
deciders:
  - Agent 1: Repo/State Bootstrap (CP0-LC)
  - Agent 2: Architecture/Tech Lead
related_adrs: []
supersedes: []
superseded_by: []
---

# ADR-001: Monorepo Structure

## Status

accepted

## Context

BeamLine Constructor is a complex system with multiple components:
- Erlang/OTP applications (router, provider, usage)
- C++ CAF applications (ingress, processor)
- NestJS Gateway (REST/SSE API)
- SvelteKit UI
- Protocol definitions (protobuf)
- Database schemas (PostgreSQL)
- Documentation

We need a clear, maintainable repository structure that:
- Separates concerns by technology stack
- Enables parallel development
- Maintains clear boundaries between modules
- Supports CI/CD pipelines
- Facilitates dependency management

## Decision

Use a **monorepo structure** with the following organization:

```
apps/
├── otp/              # Erlang/OTP applications
│   ├── router/       # Router core
│   ├── provider/     # Provider adapters
│   └── usage/        # Usage metrics
├── caf/              # C++ Actor Framework
│   ├── ingress/      # Ingress gateway
│   └── processor/     # Message processor
├── gateway/          # NestJS Gateway
└── ui/               # SvelteKit UI

proto/                # Protocol buffer definitions
├── beamline/
│   ├── flow/v1/      # flow.proto
│   └── provider/v1/  # provider.proto

sql/                  # Database schemas
└── 000_init.sql      # Initial DDL

docs/                 # Documentation
├── ADR/              # Architecture Decision Records
├── ARCHITECTURE/     # Architecture diagrams and specs
└── [other docs]

.trae/                # State and audit
├── state.json
└── history.json
```

**Key principles**:
- Each technology stack in its own directory (`apps/otp/`, `apps/caf/`, etc.)
- Shared contracts in `proto/` (Source of Truth)
- Database schemas in `sql/` (Source of Truth)
- Documentation centralized in `docs/`
- State management in `.trae/`

## Consequences

### Positive

- **Clear separation**: Each component has its own directory
- **Parallel development**: Teams can work on different components independently
- **Shared contracts**: Proto files are single source of truth
- **Easy navigation**: Structure is intuitive and self-documenting
- **CI/CD friendly**: Each component can have its own build pipeline

### Negative

- **Repository size**: Monorepo can grow large
- **Dependency management**: Need to coordinate shared dependencies
- **Build complexity**: Multiple build systems (rebar3, CMake, npm)

### Neutral

- **Version control**: All components in one repository
- **Cross-cutting changes**: Easier to make changes across components

## Alternatives Considered

### Alternative 1: Multi-repo

**Description**: Separate repository for each component

**Pros**:
- Smaller repositories
- Independent versioning
- Clear ownership

**Cons**:
- Harder to coordinate changes
- Shared contracts require synchronization
- More complex CI/CD

**Why not chosen**: Need for shared contracts (proto, DDL) and coordinated development outweighs benefits

### Alternative 2: Flat Structure

**Description**: All components at root level

**Pros**:
- Simpler structure
- No nesting

**Cons**:
- Harder to navigate
- No clear separation
- Mixing concerns

**Why not chosen**: Lacks clear organization and separation of concerns

## Implementation Notes

- Structure created in CP0-LC
- Each component follows its own build conventions
- Shared contracts (proto, DDL) are Source of Truth
- Documentation structure supports ADR and architecture specs

## References

- `README.md`: Project structure overview
- `docs/CI_CD.md`: Build and deployment pipelines
- `.trae/manifest.json`: Schema versions and validation

## Compliance

- ✅ Aligns with `.trae/manifest.json`
- ✅ Follows compatibility policy
- ✅ Respects security constraints
- ✅ Integrates with STATE/HISTORY


