# Dry-Run Logs for Quick PR Verification

Minimal yet representative logs for each CI/CD validation mode to enable fast checks in pull requests.

## Purpose

This document provides baseline logs for CI/CD validation scenarios to enable quick verification in PRs. All scenarios are reproducible locally using `scripts/dry_run_ci.sh`.

## Local Reproduction

All scenarios can be reproduced locally:

```bash
# Run all scenarios
bash scripts/dry_run_ci.sh all

# Run specific scenario
bash scripts/dry_run_ci.sh github
bash scripts/dry_run_ci.sh drone
bash scripts/dry_run_ci.sh gitlab
bash scripts/dry_run_ci.sh production
bash scripts/dry_run_ci.sh development
```

Logs are automatically saved to `reports/dry-run-logs/` with timestamps.

## Validation Steps

Each scenario runs six validation steps:

1. **Schema Changes Check**: Validates schema version changes and manifest consistency
2. **State Validation**: Validates `.trae/state.json` and `.trae/history.json` against schema
3. **HMAC Masking Validation**: Ensures secrets are masked and the HMAC chain is valid
4. **Secrets Scan**: Checks that secrets are never embedded in code or artifacts
5. **Frontend Install/Build**: Installs and builds UI to catch regressions
6. **Backend Build**: Builds OTP apps and Gateway to ensure integration health

---

## Example: GitHub Actions (development)

### Dry-Run Summary

**Date**: 2025-11-06
**Time**: 19:50:08
**Timestamp**: 2025-11-06T12:50:08Z

### Manifest Versions

- STATE: 1.0.0
- HISTORY: 1.0.0
- artifact_checksums_format: array_object_v1
- hmac_masking_policy: 16+

### Environment

- Type: [INFO] Detected environment: development

### Step Results

| Step | Status | Duration | Log File |
|------|--------|----------|----------|
| compliance_pii | PASSED | 1s | [compliance_pii.log](compliance_pii.log) |
| frontend_build | PASSED | 1s | [frontend_build.log](frontend_build.log) |
| schema_check | PASSED | 1s | [schema_check.log](schema_check.log) |
| backend_otp_compile | PASSED | 1s | [backend_otp_compile.log](backend_otp_compile.log) |
| backend_gateway_install | PASSED | 2s | [backend_gateway_install.log](backend_gateway_install.log) |
| state_validation | PASSED | 2s | [state_validation.log](state_validation.log) |
| hmac_masking | PASSED | 0s | [hmac_masking.log](hmac_masking.log) |
| backend_gateway_build | PASSED | 9s | [backend_gateway_build.log](backend_gateway_build.log) |
| hmac_chain | PASSED | 0s | [hmac_chain.log](hmac_chain.log) |
| frontend_install | PASSED | 0s | [frontend_install.log](frontend_install.log) |
| backend_caf_build | PASSED | 0s | [backend_caf_build.log](backend_caf_build.log) |
| backend_caf_configure | PASSED | 0s | [backend_caf_configure.log](backend_caf_configure.log) |
| secrets_scan | PASSED | 0s | [secrets_scan.log](secrets_scan.log) |
| backend_otp_deps | PASSED | 0s | [backend_otp_deps.log](backend_otp_deps.log) |

### Total Duration

**17s**

### Logs Location

All logs saved to: `reports/dry-run-logs/`

---

## Interpreting Results

- PASSED: Step completed successfully
- FAILED: Step failed; see the corresponding log file for details
- SKIPPED: Step was intentionally skipped in this scenario

## Troubleshooting

Common failures and fixes:

1. Schema check fails
   - Cause: Manual changes to `STATE.schema.json` without a version bump
   - Fix: Use `scripts/check_schema_changes.sh` and create a proper migration

2. HMAC mismatch
   - Cause: `BEAMLINE_HMAC_SECRET` not set or changed
   - Fix: Set the secret in CI and recompute the HMAC chain if changed

3. Secrets scan fails
   - Cause: Accidental secret in code or artifacts
   - Fix: Remove the secret, ensure masking, and re-run the scan

## Policy

- Do not commit logs to the repository
- Attach relevant logs to PR as artifacts
- Keep logs clean of PII and secrets

## References

- `scripts/dry_run_ci.sh`
- `docs/SECURITY_GUIDE.md`
- `docs/archive/dev/PR_CHECKLIST.md`
- `docs/archive/dev/PR_CHECKLIST_DEVEX.md`
