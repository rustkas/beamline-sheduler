# Router Contract Snapshot

## Purpose

This document describes the static contract snapshot check for Router, which verifies that documented contracts (NATS subjects, payload schemas, required fields) match Router implementation to catch drift before runtime.

**Script**: `scripts/router_contract_snapshot.sh`

## Goals

- Verify NATS subjects match between documentation and code
- Verify required fields match between documentation and code
- Verify API versions match between documentation and code
- Catch contract drift before runtime errors occur

## What Gets Checked

### 1. NATS Subjects

**DecideRequest Subject**:
- Documentation: `docs/NATS_SUBJECTS.md`, `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`
- Code: `apps/otp/router/src/router_nats_subscriber.erl`
- Expected: `beamline.router.v1.decide`

**ExecResult Subject**:
- Documentation: `docs/NATS_SUBJECTS.md`, `docs/API_CONTRACTS.md`
- Code: `apps/otp/router/src/router_result_consumer.erl`
- Expected: `caf.exec.result.v1`

### 2. Required Fields

**DecideRequest Required Fields**:
- Documentation: `docs/API_CONTRACTS.md` (fields not marked as optional)
- Code: `apps/otp/router/src/router_nats_subscriber.erl` (fields checked for undefined)
- Expected fields: `version`, `tenant_id`, `request_id`, `task`

### 3. API Version

**Version**:
- Documentation: `docs/API_CONTRACTS.md`, `docs/NATS_SUBJECTS.md`
- Code: `apps/otp/router/src/router_nats_subscriber.erl` (version validation)
- Expected: `"1"`

## Usage

### Basic Usage

```bash
# Run contract snapshot check
./scripts/router_contract_snapshot.sh
```

### With Options

```bash
# Show help
./scripts/router_contract_snapshot.sh --help

# Verbose output
./scripts/router_contract_snapshot.sh --verbose

# Output to specific file
./scripts/router_contract_snapshot.sh --output reports/router/contract_snapshot_custom.md
```

## Output

### Console Output

The script outputs:
- ✅ Green checkmarks for matching contracts
- ❌ Red X marks for mismatches
- ⚠️ Yellow warnings for unknown/unable to extract

**Example**:
```
Router Contract Snapshot Check
==============================

Checking contracts...

Checking DecideRequest NATS subject...
✓ DecideRequest subject matches
Checking ExecResult NATS subject...
✓ ExecResult subject matches
Checking DecideRequest required fields...
✓ DecideRequest required fields match
Checking version...
✓ API version matches

Report saved to: reports/router/contract_snapshots/contract_snapshot_20251115_162517.md

✓ All contracts match
```

### Report File

The script generates a Markdown report in `reports/router/contract_snapshots/contract_snapshot_TIMESTAMP.md`.

**Report Structure**:
- **Header**: Generation timestamp, commit, branch
- **Summary**: Overall status (MATCH / MISMATCH)
- **Contract Checks**: Detailed comparison for each check
- **Files Checked**: List of files used for comparison

**Example Report**:
```markdown
# Router Contract Snapshot Report

**Generated**: 2025-11-15 09:25:17 UTC
**Commit**: abc1234
**Branch**: main

## Summary

**Status**: ✅ **ALL CONTRACTS MATCH**

## Contract Checks

### 1. DecideRequest NATS Subject

| Source | Subject |
|--------|---------|
| Documentation | `beamline.router.v1.decide` |
| Code | `beamline.router.v1.decide` |

**Status**: ✅ **MATCH**

### 2. ExecResult NATS Subject

| Source | Subject |
|--------|---------|
| Documentation | `caf.exec.result.v1` |
| Code | `caf.exec.result.v1` |

**Status**: ✅ **MATCH**

### 3. DecideRequest Required Fields

| Source | Required Fields |
|--------|----------------|
| Documentation | `request_id,task,tenant_id,version` |
| Code | `request_id,task,tenant_id,version` |

**Status**: ✅ **MATCH**

### 4. API Version

| Source | Version |
|--------|---------|
| Documentation | `1` |
| Code | `1` |

**Status**: ✅ **MATCH**
```

## Exit Codes

- **0**: All contracts match (OK)
- **1**: Contract mismatches detected
- **2**: Error during check (missing files, etc.)

## Interpreting Results

### All Contracts Match

✅ **Success**: All documented contracts match Router implementation.

**Meaning**:
- NATS subjects are consistent
- Required fields are consistent
- API versions are consistent
- No drift detected

### Contract Mismatches Detected

❌ **Failure**: One or more contracts do not match.

**What to do**:
1. Review the report for specific mismatches
2. Check if documentation needs updating
3. Check if code needs updating
4. Fix the mismatch and re-run the check

**Common Mismatches**:
- **Subject mismatch**: Subject in code doesn't match documentation
  - **Fix**: Update code or documentation to match
- **Required fields mismatch**: Fields marked as required in docs but not validated in code (or vice versa)
  - **Fix**: Update validation logic or documentation
- **Version mismatch**: Version in code doesn't match documentation
  - **Fix**: Update version constant or documentation

## Integration with CI

### Optional CI Stage

The contract snapshot check can be added as an optional CI stage:

```yaml
# .github/workflows/ci.yml
contract-snapshot:
  name: Contract Snapshot Check
  runs-on: ubuntu-latest
  steps:
    - uses: actions/checkout@v4
    - name: Run Contract Snapshot
      run: ./scripts/router_contract_snapshot.sh
    - name: Upload Report
      uses: actions/upload-artifact@v4
      if: always()
      with:
        name: contract-snapshot-report
        path: reports/router/contract_snapshots/*.md
        retention-days: 7
```

**Note**: This is an **optional** check. It should not block PRs, but can be used to detect drift early.

### Pre-Push Hook (Optional)

You can add the check as a pre-push hook:

```bash
# .git/hooks/pre-push
#!/bin/bash
./scripts/router_contract_snapshot.sh
if [ $? -ne 0 ]; then
    echo "Contract snapshot check failed. Please review and fix mismatches."
    exit 1
fi
```

## Limitations

### Static Analysis Only

The script performs **static analysis** only:
- ✅ Extracts subjects from code constants
- ✅ Extracts required fields from validation logic
- ✅ Extracts versions from code checks
- ❌ Does not verify runtime behavior
- ❌ Does not verify actual message parsing

### Parsing Limitations

The script uses simple pattern matching:
- May miss complex validation logic
- May not detect all required fields
- Relies on consistent code patterns

### Documentation Format

The script expects specific documentation formats:
- NATS subjects in markdown code blocks or inline
- Required fields listed in API_CONTRACTS.md
- Versions mentioned in documentation

## Troubleshooting

### Missing Files Error

**Issue**: Script fails with "file not found" errors.

**Solution**:
1. Verify all required files exist:
   - `docs/NATS_SUBJECTS.md`
   - `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`
   - `docs/API_CONTRACTS.md`
   - `apps/otp/router/src/router_nats_subscriber.erl`
   - `apps/otp/router/src/router_result_consumer.erl`
2. Run from project root directory

### False Positives

**Issue**: Script reports mismatches that are not real.

**Solution**:
1. Review the report for specific mismatches
2. Check if parsing logic needs improvement
3. Verify documentation format matches expected patterns
4. Manually verify the actual contract

### False Negatives

**Issue**: Script reports matches but contracts actually differ.

**Solution**:
1. Review code and documentation manually
2. Check if validation logic changed but wasn't detected
3. Improve parsing patterns in script
4. Add more specific checks

## Future Enhancements

- **JSON Schema Validation**: Extract and compare JSON schemas from docs vs code
- **Header Validation**: Check header requirements (trace_id, tenant_id, version)
- **Error Response Format**: Verify ErrorResponse structure matches
- **ExecAssignment Contract**: Check ExecAssignment subject and fields
- **ExecAssignmentAck Contract**: Check ExecAssignmentAck subject and fields
- **Property-Based Contract Tests**: Generate contract tests from extracted schemas

## Related Documentation

- `docs/NATS_SUBJECTS.md` - NATS subjects and headers
- `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` - Protobuf to NATS mapping
- `docs/API_CONTRACTS.md` - API contracts and message formats
- `apps/otp/router/src/router_nats_subscriber.erl` - NATS subscriber implementation
- `apps/otp/router/src/router_result_consumer.erl` - Result consumer implementation
- `docs/archive/dev/GATEWAY_ROUTER_CONTRACT_SMOKE.md` - Runtime contract smoke tests

## Quick Reference

### Run Check

```bash
./scripts/router_contract_snapshot.sh
```

### View Latest Report

```bash
ls -t reports/router/contract_snapshots/*.md | head -1 | xargs cat
```

### Check Exit Code

```bash
./scripts/router_contract_snapshot.sh
echo "Exit code: $?"
# 0 = OK, 1 = Mismatch, 2 = Error
```

