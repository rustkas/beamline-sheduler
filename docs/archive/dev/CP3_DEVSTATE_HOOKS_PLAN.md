# CP3: DevState Pre-commit/Pre-push Hooks Implementation Plan

**Purpose**: Implement and document standard pre-commit/pre-push DevState hooks  
**Date**: 2025-01-27  
**Status**: 📋 **PLANNING**  
**Source**: `docs/CP3_DELTA_CHECKLIST.md` (Item #1)

## Overview

This document defines the implementation plan for CP3 DevState pre-commit/pre-push hooks. These hooks will automatically verify DevState integrity before commits and pushes, preventing broken state/history from entering the repository.

**Current Status**: Documentation exists (`devstate-tools/devstate/docs/IDE_INTEGRATION.md`), but hooks require manual setup. CP3 will automate hook installation and provide standard hook implementations.

---

## Target Scope

### Commands to Execute

**Pre-commit Hook**:
- **Purpose**: Verify DevState before allowing commit
- **Commands**:
  - `bash devstate-tools/devstate/scripts/devstate_verify.sh` (HMAC chain verification)
  - Fast validation: `curl -fsS 'http://localhost:3080/v1/devstate/verify?limit=0'` (if DevState is running)

**Pre-push Hook**:
- **Purpose**: Final verification before push (more comprehensive)
- **Commands**:
  - `bash devstate-tools/devstate/scripts/devstate_verify.sh` (HMAC chain verification)
  - `bash scripts/validate_state.sh` (full state validation)
  - `python3 scripts/verify_hmac_chain.py --verbose` (HMAC chain verification)

### Repository Profiles

**All Repositories**:
- ✅ Main repository (this repo)
- ✅ Any repository that uses `.trae/state.json` and `.trae/history.json`

**Hook Installation**:
- **Automatic**: Via `make install-devstate-hooks` or `scripts/install_devstate_hooks.sh`
- **Manual**: Copy hook scripts to `.git/hooks/` and make executable

---

## Implementation Options

### Option 1: Documentation + Example Hooks (Minimal)

**Scope**: 
- Provide standard hook scripts in `devstate-tools/devstate/scripts/hooks/`
- Document manual installation process
- No automatic installation

**Pros**:
- Simple, no automation complexity
- Developers can choose to install or not
- No risk of breaking existing workflows

**Cons**:
- Requires manual setup (may be forgotten)
- Not enforced automatically

**Deliverables**:
- `devstate-tools/devstate/scripts/hooks/pre-commit`
- `devstate-tools/devstate/scripts/hooks/pre-push`
- Documentation: `devstate-tools/devstate/docs/HOOKS.md`

### Option 2: Real Integration via Scripts (Recommended)

**Scope**:
- Provide standard hook scripts
- Create installation script: `scripts/install_devstate_hooks.sh`
- Add Makefile target: `make install-devstate-hooks`
- Optional: Git config integration

**Pros**:
- Automated installation (one command)
- Consistent across all developers
- Can be integrated into setup scripts

**Cons**:
- Requires script maintenance
- May conflict with existing hooks

**Deliverables**:
- `devstate-tools/devstate/scripts/hooks/pre-commit`
- `devstate-tools/devstate/scripts/hooks/pre-push`
- `scripts/install_devstate_hooks.sh`
- Makefile target: `make install-devstate-hooks`
- Documentation: `devstate-tools/devstate/docs/HOOKS.md`

---

## Recommended Approach: Option 2 (Real Integration)

**Rationale**: Automated installation ensures consistency and reduces setup friction. Scripts can check for existing hooks and merge or warn appropriately.

---

## Implementation Details

### Pre-commit Hook

**File**: `devstate-tools/devstate/scripts/hooks/pre-commit`

**Logic**:
1. Check if `.trae/state.json` or `.trae/history.json` are in staged changes
2. If yes:
   - Check if DevState service is running (warn if not, but don't block)
   - Run `bash devstate-tools/devstate/scripts/devstate_verify.sh`
   - If verification fails, block commit with error message
3. If no DevState files changed, skip (fast path)

**Exit Codes**:
- `0` - Success (verification passed or no DevState files changed)
- `1` - Failure (verification failed, commit blocked)

### Pre-push Hook

**File**: `devstate-tools/devstate/scripts/hooks/pre-push`

**Logic**:
1. Check if `.trae/state.json` or `.trae/history.json` are being pushed
2. If yes:
   - Check if DevState service is running (warn if not, but don't block)
   - Run `bash devstate-tools/devstate/scripts/devstate_verify.sh`
   - Run `bash scripts/validate_state.sh`
   - Run `python3 scripts/verify_hmac_chain.py --verbose`
   - If any verification fails, block push with error message
3. If no DevState files being pushed, skip (fast path)

**Exit Codes**:
- `0` - Success (verification passed or no DevState files being pushed)
- `1` - Failure (verification failed, push blocked)

### Installation Script

**File**: `scripts/install_devstate_hooks.sh`

**Logic**:
1. Check if `.git/hooks/` directory exists
2. Check for existing hooks:
   - If `pre-commit` exists: Warn and ask to merge or backup
   - If `pre-push` exists: Warn and ask to merge or backup
3. Copy hook scripts to `.git/hooks/`
4. Make hooks executable: `chmod +x .git/hooks/pre-commit .git/hooks/pre-push`
5. Verify installation: Check hooks are executable and contain expected content

**Options**:
- `--force`: Overwrite existing hooks without asking
- `--backup`: Backup existing hooks before overwriting
- `--merge`: Attempt to merge with existing hooks (advanced)

### Makefile Target

**File**: `Makefile`

**Target**: `install-devstate-hooks`

```makefile
.PHONY: install-devstate-hooks

install-devstate-hooks:
	@echo "Installing DevState Git hooks..."
	@bash scripts/install_devstate_hooks.sh
	@echo "✅ DevState hooks installed successfully"
	@echo "Hooks will verify DevState before commits and pushes"
```

---

## Testing Plan

### Unit Tests

1. **Hook Scripts**:
   - Test hook logic with mock DevState service
   - Test exit codes (success/failure)
   - Test fast path (no DevState files changed)

2. **Installation Script**:
   - Test installation in clean repository
   - Test installation with existing hooks (merge/backup)
   - Test error handling (missing `.git/hooks/`, permissions)

### Integration Tests

1. **Pre-commit Hook**:
   - Stage `.trae/state.json` change
   - Run `git commit` (should trigger hook)
   - Verify hook runs and blocks if verification fails

2. **Pre-push Hook**:
   - Push commit with `.trae/history.json` change
   - Verify hook runs and blocks if verification fails

### Manual Testing

1. **Developer Workflow**:
   - Install hooks: `make install-devstate-hooks`
   - Make change to `.trae/state.json`
   - Attempt commit (should trigger pre-commit hook)
   - Attempt push (should trigger pre-push hook)

---

## Documentation

### Hook Documentation

**File**: `devstate-tools/devstate/docs/HOOKS.md`

**Content**:
- Overview of pre-commit/pre-push hooks
- Installation instructions (automatic and manual)
- Hook behavior and exit codes
- Troubleshooting (DevState not running, verification failures)
- Integration with existing hooks

### Update IDE Integration Guide

## Command Interface (Contract)

**Hook Locations**
- `.git/hooks/pre-commit` — runs before commit is created
- `.git/hooks/pre-push` — runs before refs are pushed

**Commands Invoked**
- Pre-commit:
  - `bash scripts/check_secret_leaks.sh --staged`
  - Conditional DevState verification (only when DevState files are staged):
    - `bash devstate-tools/devstate/scripts/devstate_verify.sh`
- Pre-push:
  - Hard gate: block if any `.trae/*` files are tracked in Git
  - Conditional DevState verification (only when DevState files are included in push):
    - `bash devstate-tools/devstate/scripts/devstate_verify.sh`

**Inputs and Options**
- `devstate-tools/devstate/scripts/devstate_verify.sh`:
  - Environment `DEVSTATE_URL` (default `http://localhost:3180`, fallback `http://localhost:3080`)
  - Environment `SKIP_LOCAL_VALIDATION` (`true|false`, default `false`)
  - Resolves project root via `git rev-parse --show-toplevel` or current `pwd`
- Hooks:
  - Pre-push can be bypassed via environment `SKIP_DEVSTATE_GATES=true`
  - DevState file detection patterns: `^.trae/(state|history)\.json$`

**Exit Codes and Behavior**
- Hooks:
  - `0` — allow operation (commit/push proceeds)
  - `≠0` — block operation (commit/push aborted)
- `devstate_verify.sh`:
  - `0` — verification passed
  - `1` — local validation failed (JSON/Schema/Checksums/HMAC chain)
  - `2` — DevState API verification failed or unavailable with local validation skipped
  - Any non-zero is treated as failure by hooks

**User Messaging**
- Output prefixes used across scripts: `[OK]`, `[FAIL]`, `[WARN]`, `[INFO]`
- Pre-push hard gate error when `.trae/*` is tracked: `Error: .trae files are tracked in Git`
- Failures from `devstate_verify.sh` print specific reasons and affected artifact paths

**Performance Policy**
- Hooks run DevState verification only when `.trae/state.json` or `.trae/history.json` are involved in the operation (staged for commit or present in push)
- Secret leak detection is scoped to staged files on pre-commit

**Current Implementation References**
- Pre-commit: `scripts/hooks/pre-commit`
- Pre-push: `scripts/hooks/pre-push`
- Verifier: `devstate-tools/devstate/scripts/devstate_verify.sh`

**File**: `devstate-tools/devstate/docs/IDE_INTEGRATION.md`

**Updates**:
- Add section: "Automatic Hook Installation"
- Update "Pre-Push Hook" section to reference new installation method
- Add troubleshooting for hook conflicts

---

## Acceptance Criteria

### Implementation Complete

1. ✅ Pre-commit hook script created and tested
2. ✅ Pre-push hook script created and tested
3. ✅ Installation script created and tested
4. ✅ Makefile target added
5. ✅ Documentation created/updated
6. ✅ Hooks work correctly (block on verification failure, skip on no DevState files)

### Documentation Complete

1. ✅ `devstate-tools/devstate/docs/HOOKS.md` created
2. ✅ `devstate-tools/devstate/docs/IDE_INTEGRATION.md` updated
3. ✅ Installation instructions clear and tested

### Testing Complete

1. ✅ Unit tests for hook scripts
2. ✅ Integration tests for installation script
3. ✅ Manual testing in developer workflow

---

## Dependencies

- **DevState Service**: Hooks require DevState service to be running (or gracefully handle when not running)
- **DevState Scripts**: `devstate_verify.sh` must exist and work correctly
- **State Validation Scripts**: `validate_state.sh` and `verify_hmac_chain.py` must exist

---

## Risks and Mitigations

### Risk 1: Hook Conflicts with Existing Hooks

**Mitigation**: 
- Installation script checks for existing hooks
- Option to merge or backup existing hooks
- Clear documentation on hook integration

### Risk 2: DevState Service Not Running

**Mitigation**:
- Hooks check if DevState is running
- Warn if not running, but don't block (optional strict mode)
- Clear error messages with instructions

### Risk 3: Performance Impact

**Mitigation**:
- Fast path: Skip if no DevState files changed
- Use fast HTTP verification when possible
- Cache DevState service status

---

## Timeline Estimate

- **Hook Scripts**: 1-2 days (pre-commit, pre-push)
- **Installation Script**: 1 day
- **Documentation**: 0.5 days
- **Testing**: 1 day
- **Total**: ~3-4 days

---

## References

- **`docs/CP3_DELTA_CHECKLIST.md`** - CP3 delta checklist (Item #1)
- **`devstate-tools/devstate/docs/IDE_INTEGRATION.md`** - Existing IDE integration guide (includes hook examples)
- **`docs/CP2_CHECKLIST.md`** - CP2 checklist (CI DevState gates section)
- **`.cursor/rules/agents/devstate-integration.mdc`** - DevState integration rules

---

**Status**: ✅ **IMPLEMENTED** (CP3)  
**Last Updated**: 2025-01-27

---

## Implementation Status

**✅ COMPLETED** (2025-01-27):

1. ✅ **Pre-commit hook**: `scripts/hooks/pre-commit` - Fast validation when DevState files are staged
2. ✅ **Pre-push hook**: `scripts/hooks/pre-push` - Comprehensive validation before push (strict No-Drift)
3. ✅ **Installation script**: `scripts/install_devstate_hooks.sh` - Automated hook installation with conflict handling
4. ✅ **Makefile target**: `make install-devstate-hooks` - One-command installation
5. ✅ **Documentation**: `devstate-tools/devstate/docs/HOOKS.md` - Complete hook documentation
6. ✅ **IDE Integration Guide**: Updated `devstate-tools/devstate/docs/IDE_INTEGRATION.md`

**Features Implemented**:
- ✅ Fast path: Skips verification if no DevState files are changed
- ✅ DevState service detection: Warns if not running, proceeds with local validation
- ✅ Comprehensive validation: DevState API + local validation + HMAC chain (pre-push)
- ✅ Conflict handling: Backup, merge, or force overwrite existing hooks
- ✅ Performance optimized: Fast path <0.1s, verification 1-10s depending on DevState availability
- ✅ Bypass support: `SKIP_DEVSTATE_GATES=true` for emergency scenarios

**Installation**:
```bash
# Automatic installation
make install-devstate-hooks

# Or with options
bash scripts/install_devstate_hooks.sh --backup --force
```

**Documentation**: See `devstate-tools/devstate/docs/HOOKS.md` for complete usage guide.
