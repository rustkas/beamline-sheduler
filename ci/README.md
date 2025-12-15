# CI/CD Configuration Guide

Instructions for configuring and enabling CI/CD checks for the project.

## Overview

The project uses automated checks to ensure state, schema, and documentation integrity:

- **Schema Changes Gate**: Checks for schema and version changes
- **State Validation**: Validates `.trae/state.json` and `.trae/history.json`
- **HMAC Masking Check**: Checks HMAC masking in documentation
- **Secret Compatibility**: Checks secret compatibility with HMAC chain

## Supported CI/CD Systems

- **GitHub Actions**: `.github/workflows/validate.yml`
- **GitLab CI**: See `docs/CI_VALIDATION.md` for configuration example
- **Drone CI**: See `docs/CI_VALIDATION.md` for configuration example

## Enabling CI on GitHub

### Step 1: Local Validation

**Before enabling CI, run local checks:**

```bash
# 1. Schema validation
bash scripts/check_schema_changes.sh

# 2. State validation
bash scripts/validate_state.sh

# 3. HMAC masking check
bash scripts/check_hmac_masking.sh docs/CI_VALIDATION.md docs/DRY_RUN_LOGS.md

# 4. Dry-run for GitHub Actions
bash scripts/dry_run_ci.sh github
```

**Expected result**: All checks should pass successfully.

### Step 2: Secret Compatibility Check

**Important**: Check the compatibility of the planned secret with the current HMAC chain:

```bash
# Check compatibility
python3 scripts/check_secret_compatibility.py --secret "your-planned-secret" --verbose
```

**If secret is compatible**:
- Can be used in CI without migration
- Proceed to Step 3

**If secret is incompatible**:
- HMAC chain migration is required
- See "HMAC Migration" section below

### Step 3: Adding Secret to GitHub

1. Go to **Settings → Secrets and variables → Actions**
2. Click **"New repository secret"**
3. Name: `BEAMLINE_HMAC_SECRET`
4. Value: your secret key for HMAC
5. Save

**Important**: Do not use test secrets in production. The secret should be sufficiently long (recommended 64+ characters).

### Step 4: Enabling Workflow

**After adding the secret:**

1. Workflow will automatically run on the next push or PR
2. Check the logs of the first run:
   - All checks should pass successfully
   - `[OK] HMAC chain integrity verified`
   - `[OK] Secret is compatible with current HMAC chain`

3. If there are errors:
   - See "Troubleshooting" section in `docs/CI_VALIDATION.md`
   - Check secret compatibility locally

### Step 5: Branch Protection (Optional)

To protect main branches (`main`, `develop`):

1. Go to **Settings → Branches**
2. Add a rule for `main`:
   - Require status checks to pass before merging
   - Select: `schema-gate`, `validate-state`, `docs-hmac-mask`
3. Repeat for `develop`

## HMAC Migration

If the planned secret is incompatible with the current HMAC chain:

### Step 1: Create Backup

```bash
cp .trae/history.json .trae/history.json.backup
cp .trae/state.json .trae/state.json.backup
```

### Step 2: Recalculate HMAC Chain

```bash
python3 scripts/recalculate_hmac_chain.py \
  --secret "$NEW_SECRET" \
  --rebuild \
  --backup
```

### Step 3: Integrity Check

```bash
# Local validation
BEAMLINE_HMAC_SECRET="$NEW_SECRET" bash scripts/validate_state.sh

# Detailed HMAC chain validation
BEAMLINE_HMAC_SECRET="$NEW_SECRET" python3 scripts/verify_hmac_chain.py --verbose
```

### Step 4: Commit Changes

```bash
git add .trae/history.json .trae/state.json
git commit -m "chore: migrate HMAC chain to new secret"
```

### Step 5: Add Secret to CI

After successful local validation, add the new secret to the CI/CD system (see Step 3 above).

## Configuration for Other CI/CD Systems

### GitLab CI

See example in `docs/CI_VALIDATION.md`:

```yaml
validate:
  script:
    - export BEAMLINE_HMAC_SECRET=$BEAMLINE_HMAC_SECRET
    - export GITLAB_CI=true
    - bash scripts/validate_state.sh
```

### Drone CI

See example in `docs/CI_VALIDATION.md`:

```yaml
steps:
  - name: validate
    environment:
      BEAMLINE_HMAC_SECRET:
        from_secret: beamline_hmac_secret
      DRONE: true
    commands:
      - bash scripts/validate_state.sh
```

## Local CI Reproduction

For local testing of all checks:

```bash
# GitHub Actions mode
GITHUB_ACTIONS=true BEAMLINE_HMAC_SECRET='test-secret' bash scripts/validate_state.sh

# GitLab CI mode
CI=true BEAMLINE_HMAC_SECRET='test-secret' bash scripts/validate_state.sh

# Drone CI mode
DRONE=true BEAMLINE_HMAC_SECRET='test-secret' bash scripts/validate_state.sh

# Production mode
PRODUCTION=true BEAMLINE_HMAC_SECRET='test-secret' bash scripts/validate_state.sh

# Development mode (without secret)
bash scripts/validate_state.sh
```

**See also**: `scripts/dry_run_ci.sh` for automated scenarios.

## Troubleshooting

### Error: "BEAMLINE_HMAC_SECRET environment variable is not set"

**Cause**: Secret is not set in the CI/CD system.

**Solution**:
1. Add secret in Settings → Secrets (GitHub Actions)
2. Ensure the secret is available in the workflow

### Error: "HMAC mismatch"

**Cause**: Secret is incompatible with the current HMAC chain.

**Solution**:
1. Check compatibility: `python3 scripts/check_secret_compatibility.py --secret "$SECRET"`
2. If incompatible, perform migration (see "HMAC Migration" section)

### Error: "Schema version mismatch"

**Cause**: Schema version does not match the manifest.

**Solution**:
1. Update version in schema or manifest
2. See `docs/SCHEMA_VERSIONING.md` for versioning rules

### Error: "Checksum mismatch"

**Cause**: Artifact checksum does not match the one recorded in state.json.

**Solution**:
1. Update checksum in `.trae/state.json`
2. Recalculate state_checksum and update history.json

## References

- **Manifest**: `.trae/manifest.json` - single source of truth
- **Validation**: `docs/CI_VALIDATION.md` - validation process
- **Secrets**: `docs/CI_SECRETS_SETUP.md` - secrets setup
- **Versioning**: `docs/SCHEMA_VERSIONING.md` - versioning policy
- **WORKERs Contract**: `docs/WORKERS_CONTRACT.md` - rules for WORKERs
- **PR Checklist**: `docs/PR_CHECKLIST.md` - PR checklist

## CI Readiness Checklist

- [ ] All local checks pass successfully
- [ ] Secret is checked for compatibility (or migration performed)
- [ ] Secret is added to CI/CD system
- [ ] Workflow file is created and verified
- [ ] Documentation is up to date
- [ ] Dry-run logs match expected behavior
- [ ] Branch protection is configured (optional)

**After completing all items**: CI is ready to enable. The first run will occur automatically on the next push or PR.
