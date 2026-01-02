# Schema Changes Test

Demonstrates CI gate behavior when `STATE.schema.json` is modified without bumping the version.

## Steps
1. Modify `STATE.schema.json` by adding a property `test_field_blocking`
2. Run `scripts/check_schema_changes.sh`
3. Expect validation failure with return code `1`

## Expected Output
- The gate detects schema modification
- Fails because version was not updated
- Provides instructions to bump version and update state

## Restore Script (Python)
```python
import json
from pathlib import Path

schema_path = Path('STATE.schema.json')
original = json.loads((schema_path.with_suffix('.backup')).read_text())
schema_path.write_text(json.dumps(original, indent=2))
print('Schema restored')
```

## Conclusion
- The CI gate successfully blocks PRs with unversioned schema changes
- Update the schema version and align `.trae/state.json` accordingly before merging
