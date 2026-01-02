---
version: 1.0
authors:
  - Agent 1: Repo/State Bootstrap (CP0-LC)
last_update: 2025-01-27T12:00:00Z
status: approved
---

# Artifact Checksums Format

## Purpose
This document describes the format for storing artifact checksums in `.trae/state.json` and the policy for its evolution.

## Current Format

### Structure
Checksums are stored in the `artifact_checksums` array within `.trae/state.json`:

```json
{
  "artifact_checksums": [
    {
      "path": "docs/CP1_CHECKLIST.md",
      "hash": "9b90e89cabc590e5ac6d5478eb9c3bc999d15abd591aec824d0cd247bd365d9e",
      "ts": "2025-01-27T12:00:00Z"
    },
    {
      "path": "scripts/validate_state.sh",
      "hash": "b0da632f4779a121b319c768a2277dcf78c9436f9ef41ad610e521a91eec904f",
      "ts": "2025-01-27T12:00:00Z"
    }
  ]
}
```

### Record Fields

- `path` (string, required): Relative path to the artifact from the repository root
- `hash` (string, required): SHA256 file hash in hex format (64 characters)
- `ts` (string, required): ISO-8601 timestamp of the artifacts last update

### Examples

#### Full example from `.trae/state.json`

```json
{
  "artifact_checksums": [
    {
      "path": "README.md",
      "hash": "4e5115fa44400a10d16cfab9ff5a3a7e30921647342ec95ae230e545c87b809e",
      "ts": "2025-01-27T12:00:00Z"
    },
    {
      "path": "docs/CP1_CHECKLIST.md",
      "hash": "9b90e89cabc590e5ac6d5478eb9c3bc999d15abd591aec824d0cd247bd365d9e",
      "ts": "2025-01-27T12:00:00Z"
    }
  ]
}
```

#### Read a checksum

```python
import json

def get_artifact_hash(state_file: str, artifact_path: str) -> str | None:
    """Return the stored checksum for a given artifact path"""
    with open(state_file, 'r') as f:
        state = json.load(f)

    for artifact in state.get('artifact_checksums', []):
        if artifact['path'] == artifact_path:
            return artifact['hash']
    return None

# Usage
hash_value = get_artifact_hash('.trae/state.json', 'docs/CP1_CHECKLIST.md')
if hash_value:
    print(f"Hash: {hash_value}")
```

#### Verify a checksum

```python
import json
import hashlib

def verify_artifact_checksum(state_file: str, artifact_path: str) -> bool:
    """Verify the checksum of an artifact"""
    with open(state_file, 'r') as f:
        state = json.load(f)

    # Find the expected hash
    expected_hash = None
    for artifact in state.get('artifact_checksums', []):
        if artifact['path'] == artifact_path:
            expected_hash = artifact['hash']
            break
    
    if not expected_hash:
        print(f"Artifact {artifact_path} not found in state.json")
        return False

    # Compute the actual hash
    try:
        with open(artifact_path, 'rb') as f:
            actual_hash = hashlib.sha256(f.read()).hexdigest()
    except FileNotFoundError:
        print(f"File {artifact_path} not found")
        return False

    if expected_hash == actual_hash:
        print(f"[OK] {artifact_path}: checksum verified")
        return True
    else:
        print(f"[FAIL] {artifact_path}: checksum mismatch")
        print(f"  Expected: {expected_hash}")
        print(f"  Actual:   {actual_hash}")
        return False

# Usage
verify_artifact_checksum('.trae/state.json', 'docs/CP1_CHECKLIST.md')
```

## Links

- Validation schema: `docs/STATE.schema.json`
- CI validation: `scripts/validate_state.sh`
- Project state: `.trae/state.json`
