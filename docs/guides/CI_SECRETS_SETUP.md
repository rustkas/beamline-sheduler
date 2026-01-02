# Security Policy: Secrets and Masking

Comprehensive security policy for secrets management, masking, and compatibility checks.

## Secret Registry

### Registered Secrets

**All secrets used in the project must be registered here:**

| Secret Name | Environment Variable | Purpose | Min Length | Recommended Length | Storage Location |
|------------|---------------------|---------|------------|-------------------|-----------------|
| HMAC Secret | `BEAMLINE_HMAC_SECRET` | HMAC signing for history entries | 16 | 64 | CI/CD secrets/variables only |

### Secret Storage Policy

**CRITICAL**: Secrets MUST be stored only in:

-  CI/CD secrets/variables (GitHub Secrets, GitLab Variables, Drone Secrets, etc.)
-  Secure key management systems (KMS, Vault, etc.)

**FORBIDDEN locations**:

-  Repository code
-  Public documentation
-  Environment variables on servers (unless in secure KMS)
-  Version control (Git history)
-  Configuration files committed to repo

## CI Integration

Set the `BEAMLINE_HMAC_SECRET` in your CI provider and pass it to jobs that run validators and the HMAC chain check.

### GitHub Actions

```yaml
name: CI

on: [push, pull_request]

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Set up environment
        env:
          BEAMLINE_HMAC_SECRET: ${{ secrets.BEAMLINE_HMAC_SECRET }}
        run: |
          echo "Secret is set"
      - name: Run validators
        env:
          BEAMLINE_HMAC_SECRET: ${{ secrets.BEAMLINE_HMAC_SECRET }}
        run: |
          bash scripts/validate_state.sh
          bash scripts/check_hmac_chain.sh
```

### GitLab CI

```yaml
stages: [validate]

validate:
  stage: validate
  image: alpine:latest
  variables:
    BEAMLINE_HMAC_SECRET: "$BEAMLINE_HMAC_SECRET"
  script:
    - bash scripts/validate_state.sh
    - bash scripts/check_hmac_chain.sh
```

### Drone CI

```yaml
kind: pipeline
type: docker
name: default

steps:
  - name: validate
    image: alpine:latest
    environment:
      BEAMLINE_HMAC_SECRET:
        from_secret: BEAMLINE_HMAC_SECRET
    commands:
      - bash scripts/validate_state.sh
      - bash scripts/check_hmac_chain.sh
```

## Local Development

Export the secret only for the validator processes (do not persist):

```bash
export BEAMLINE_HMAC_SECRET="..."
bash scripts/validate_state.sh
bash scripts/check_hmac_chain.sh
```

Or pass inline without exporting:

```bash
BEAMLINE_HMAC_SECRET="..." bash scripts/validate_state.sh
BEAMLINE_HMAC_SECRET="..." bash scripts/check_hmac_chain.sh
```

## Troubleshooting

### Secret not set

**Symptoms**:
```
[FAIL] BEAMLINE_HMAC_SECRET environment variable is not set
```

**Fix**:
1. Verify the secret is added in your CI/CD system
2. Verify the variable name (must be `BEAMLINE_HMAC_SECRET`)
3. Verify the secret is exported to the workflow/pipeline scope

### Secret not passed to the script

**Symptoms**:
- Secret is configured in CI/CD, but the script does not see it

**Fix**:
1. Check the export syntax in your CI configuration
2. Ensure the secret is available in the required environment scope
3. Verify access permissions to the secret

### HMAC mismatch after setting the secret

**Symptoms**:
```
[FAIL] Entry 0: HMAC mismatch
```

**Fix**:
1. Ensure the correct secret is used (the same as when entries were created)
2. If the secret was changed, recompute the HMAC for all entries
3. Verify the secret was not accidentally altered

## References

- [GitHub Actions Secrets](https://docs.github.com/en/actions/security-guides/encrypted-secrets)
- [Drone CI Secrets](https://docs.drone.io/secret/)
- [GitLab CI Variables](https://docs.gitlab.com/ee/ci/variables/)
- [Jenkins Credentials](https://www.jenkins.io/doc/book/using/using-credentials/)
