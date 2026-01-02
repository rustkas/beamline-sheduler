# Agents Contract

Unified contract for all project agents for working with metadata, schemas, and validation.

## Source of Truth

Single manifest: `.trae/manifest.json`

All agents must use the manifest as the single source of truth for:
- Schema versions
- Data formats
- Compatibility rules
- Validation tools
- Security policies

## Mandatory Requirements

### 1. Read the manifest

Each agent must:

1. Read the manifest before starting work:
   ```python
   import json

   with open('.trae/manifest.json', 'r') as f:
       manifest = json.load(f)

   state_version = manifest['schema_versions']['state']['version']
   checksums_format = manifest['artifact_checksums_format']['name']
   ```

2. Check schema versions:
   - Ensure the schemas in use match the versions in the manifest
   - Warn if versions do not match

3. Use formats from the manifest:
   - `artifact_checksums_format.name` for the checksums format name
   - Structure from `artifact_checksums_format.structure`

### 2. Validation

Each agent must use validation tools from the manifest.

Required:
- `validation_tools.state_validation.script`  script to validate `.trae/state.json`
- `validation_tools.history_validation.script`  script to validate `.trae/history.json`

Optional:
- `validation_tools.artifacts_validation.script`  validate referenced artifacts

Invocation example:
```bash
bash "$(jq -r '.validation_tools.state_validation.script' .trae/manifest.json)"
```

### 3. Compatibility Policy

- Agents must remain compatible with the current schema versions from the manifest
- If version mismatches are detected during validation  fail fast
- If breaking changes are planned  update the manifest and validation tools first

### 4. Error Handling

- Any validation failure  return a non-zero exit code
- Output format: structured lines like `[OK]`, `[WARN]`, `[FAIL]`
- Provide actionable messages with file paths and hint links

## Agent Examples

### Agent 1: Provider Adapter (Python)

```python
import json
import os

MANIFEST = '.trae/manifest.json'

with open(MANIFEST, 'r') as f:
    manifest = json.load(f)

state_version = manifest['schema_versions']['state']['version']
checksums_format = manifest['artifact_checksums_format']['name']
validation_script = manifest['validation_tools']['state_validation']['script']

ci_vars = manifest['ci_env_detection']['variables']
is_production = any(os.getenv(var['name']) == 'true' for var in ci_vars)
```

### Agent 2: Router Core (bash)

```bash
#!/bin/bash

MANIFEST=".trae/manifest.json"

STATE_VERSION=$(jq -r '.schema_versions.state.version' "$MANIFEST")
VALIDATION_SCRIPT=$(jq -r '.validation_tools.state_validation.script' "$MANIFEST")

bash "$VALIDATION_SCRIPT"

if [ $? -ne 0 ]; then
    echo "[FAIL] Validation failed"
    exit 1
fi
```

## Manifest Updates

When schemas, formats, or rules change:

1. Update the relevant sections in `.trae/manifest.json`
2. Update `updated_at` in the manifest
3. Bump the manifest version if needed
4. Update documentation if needed
5. Run manifest validation

## Links

- Manifest: `.trae/manifest.json`
- Versioning policy: `docs/SCHEMA_VERSIONING.md`
- CI validation: `docs/CI_VALIDATION.md`
- Secret setup: `docs/archive/dev/CI_SECRETS_SETUP.md`
