---
version: 1.0
authors:
  - Agent 2: Architecture/Tech Lead
last_update: 2025-11-05T20:50:00Z
status: approved
---

# Compatibility Rules

## Purpose

Compatibility rules for APIs, schemas, and contracts to ensure No-Drift principle and safe changes.

## Versioning Scheme

**Format**: Semver (MAJOR.MINOR.PATCH)

- **MAJOR**: Breaking changes requiring migration
- **MINOR**: Backward-compatible additions
- **PATCH**: Bug fixes and clarifications

**Reference**: `.trae/manifest.json` compatibility_policy

## API Versioning

### REST API

**Strategy**: URL-based versioning

- **Current**: `/api/v1/*`
- **Future**: `/api/v2/*`

**Rules**:
- Old versions remain available for 90 days after new version release
- Deprecation notice: 90 days before removal
- Breaking changes require new version (v2, v3, etc.)

**Breaking Changes**:
- Removing endpoints
- Removing required fields
- Changing field types
- Changing response structure

**Non-Breaking Changes**:
- Adding optional fields
- Adding new endpoints
- Extending enum values
- Adding response fields

### gRPC API

**Strategy**: Package-based versioning

- **Current**: `beamline.flow.v1`, `beamline.provider.v1`
- **Future**: `beamline.flow.v2`, `beamline.provider.v2`

**Rules**:
- Old packages remain available
- Breaking changes require new package version
- Migration period: 90 days dual support

**Breaking Changes**:
- Removing services or methods
- Removing required fields
- Changing field types
- Changing service signatures

**Non-Breaking Changes**:
- Adding optional fields
- Adding new services or methods
- Extending enum values

### NATS Subjects

**Strategy**: Subject-based versioning

- **Current**: `beamline.router.v1.decide`
- **Future**: `beamline.router.v2.decide`

**Rules**:
- Old subjects remain active during transition
- Gradual migration with dual support
- Breaking changes require new subject version

**Breaking Changes**:
- Removing subjects
- Changing message structure (removing required fields)
- Changing message format

**Non-Breaking Changes**:
- Adding optional fields
- Adding new subjects
- Extending message structure

## Schema Versioning

### State Schema

**Location**: `docs/STATE.schema.json`  
**Version**: Tracked in `.trae/manifest.json`

**Rules**:
- MAJOR version bump: Removing required fields, changing structure
- MINOR version bump: Adding optional fields, extending enums
- PATCH version bump: Bug fixes, clarifications

**Validation**: `scripts/check_schema_changes.sh`

### History Schema

**Location**: `docs/HISTORY.schema.json`  
**Version**: Tracked in `.trae/manifest.json`

**Rules**: Same as State Schema

## Database Schema (DDL)

**Location**: `sql/000_init.sql`  
**Versioning**: Migration-based

**Rules**:
- **Breaking**: Dropping tables, removing columns, changing column types
- **Non-Breaking**: Adding tables, adding nullable columns, adding indexes

**Migration Strategy**:
- Create migration files: `sql/001_*.sql`, `sql/002_*.sql`
- Test migrations on staging
- Apply migrations in order
- Rollback plan required

## Protobuf ABI

**Location**: `proto/beamline/*/v*/*.proto`  
**Versioning**: Package-based (v1, v2, etc.)

**Rules**:
- **Breaking**: Removing fields, changing field types, removing services
- **Non-Breaking**: Adding optional fields, adding services, extending enums

**Validation**: `buf.yaml` breaking checks

**Breaking Change Detection**:
```bash
buf breaking --against '.git#branch=main' proto/
```

## Compatibility Guarantees

### Backward Compatibility

**Guarantee**: All MINOR and PATCH changes are backward compatible

**Examples**:
- ✅ Adding optional fields to requests
- ✅ Adding new endpoints
- ✅ Extending enum values
- ✅ Adding response fields

### Forward Compatibility

**Guarantee**: Clients should ignore unknown fields

**Examples**:
- ✅ Clients ignore unknown response fields
- ✅ Clients ignore unknown enum values
- ✅ Clients handle missing optional fields gracefully

## Migration Strategies

### API Migration

**Strategy**: Gradual migration with dual support

1. **Phase 1**: Deploy new version alongside old
2. **Phase 2**: Migrate clients to new version
3. **Phase 3**: Deprecate old version (90 days notice)
4. **Phase 4**: Remove old version

**Example**:
```
v1 → v2 migration:
- Deploy v2 endpoints
- Keep v1 active
- Migrate clients (90 days)
- Deprecate v1 (90 days notice)
- Remove v1
```

### Schema Migration

**Strategy**: Version bump with migration scripts

1. **Create migration**: Update schema version
2. **Update validation**: Update JSON-Schema
3. **Update state**: Migrate `.trae/state.json` if needed
4. **Update history**: Add migration entry to `.trae/history.json`

### Database Migration

**Strategy**: Sequential migration files

1. **Create migration**: `sql/001_add_column.sql`
2. **Test**: Run on staging
3. **Apply**: Run on production
4. **Verify**: Check data integrity

## Breaking Change Process

### Pre-Change

1. **Identify**: Determine if change is breaking
2. **Document**: Create ADR if significant
3. **Plan**: Create migration plan
4. **Notify**: Notify stakeholders

### During Change

1. **Version bump**: Increment MAJOR version
2. **Dual support**: Keep old version active
3. **Migration tools**: Provide migration scripts/guides
4. **Testing**: Test migration on staging

### Post-Change

1. **Monitor**: Monitor migration progress
2. **Support**: Support clients during migration
3. **Deprecate**: Deprecate old version (90 days)
4. **Remove**: Remove old version after deprecation

## Validation Rules

### Pre-Deployment

1. **Schema validation**: Verify schema changes
2. **Breaking check**: Run `buf breaking` for protobuf
3. **Compatibility check**: Verify backward compatibility
4. **Migration test**: Test migration scripts

### CI/CD Gates

**Required checks**:
- Schema version consistency
- Breaking change detection
- Migration script validation
- Backward compatibility verification

**Scripts**:
- `scripts/check_schema_changes.sh`: Schema version validation
- `buf breaking`: Protobuf breaking change detection
- `scripts/validate_state.sh`: State validation

## Examples

### Example 1: Adding Optional Field (Non-Breaking)

**Change**: Add `metadata` field to `RouteRequest`

**Action**:
- MINOR version bump
- Add field as optional
- Update documentation
- No migration required

### Example 2: Removing Required Field (Breaking)

**Change**: Remove `message_id` from `RouteRequest`

**Action**:
- MAJOR version bump
- Create new API version (v2)
- Keep v1 active
- Migration plan required
- 90 days deprecation notice

### Example 3: Changing Field Type (Breaking)

**Change**: Change `priority` from `int` to `float`

**Action**:
- MAJOR version bump
- Create new API version (v2)
- Migration script to convert data
- Dual support period

## References

- `.trae/manifest.json`: Compatibility policy and versioning scheme
- `docs/SCHEMA_VERSIONING.md`: Schema versioning details
- `proto/buf.yaml`: Protobuf breaking change detection
- `docs/ADR/`: Architecture Decision Records
- `docs/OBSERVABILITY_COMPATIBILITY_RULES.md`: CP1↔CP2 observability compatibility rules


