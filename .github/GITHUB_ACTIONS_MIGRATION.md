# Migration from GitLab CI to GitHub Actions

This document explains the migration from GitLab CI (`.gitlab-ci.yml`) to GitHub Actions (`.github/workflows/*.yml`).

## Overview

We maintain **parallel CI/CD configurations** for both GitLab and GitHub:
- **GitLab CI**: `.gitlab-ci.yml` (original configuration)
- **GitHub Actions**: `.github/workflows/*.yml` (new configuration)

Both configurations provide the same testing coverage and quality gates.

## Key Differences

### 1. Configuration Structure

**GitLab CI** uses a single file with stages:
```yaml
stages:
  - build
  - test
  - coverage

job_name:
  stage: test
  script:
    - make test
```

**GitHub Actions** uses multiple workflow files:
```yaml
# .github/workflows/tests.yml
jobs:
  job_name:
    runs-on: ubuntu-latest
    steps:
      - name: Run tests
        run: make test
```

### 2. Workflow Organization

| Feature | GitLab CI | GitHub Actions |
|---------|-----------|----------------|
| **File Structure** | Single `.gitlab-ci.yml` | Multiple `.github/workflows/*.yml` |
| **Job Dependencies** | `dependencies:` | `needs:` |
| **Conditional Execution** | `only:`, `except:` | `if:`, path filters |
| **Manual Jobs** | `when: manual` | `workflow_dispatch` event |
| **Scheduled Jobs** | `only: schedules` | `schedule:` trigger |
| **Artifacts** | `artifacts:` section | `actions/upload-artifact@v4` |
| **Caching** | `cache:` section | `actions/cache@v4` |

### 3. Triggers Comparison

| GitLab CI | GitHub Actions |
|-----------|----------------|
| `only: changes:` | `on.push.paths:` |
| `only: schedules` | `on.schedule:` |
| `only: triggers` | `on.workflow_dispatch:` |
| `only: main` | `on.push.branches: [main]` |
| `except: schedules` | `if: github.event_name != 'schedule'` |

### 4. Feature Mapping

#### Path-Based Triggers
**GitLab**:
```yaml
only:
  changes:
    - apps/c-gateway/**/*
```

**GitHub Actions**:
```yaml
on:
  push:
    paths:
      - 'apps/c-gateway/**'
```

#### Manual Triggers
**GitLab**:
```yaml
when: manual
```

**GitHub Actions**:
```yaml
on:
  workflow_dispatch:
```

#### Scheduled Jobs
**GitLab**:
```yaml
only:
  - schedules
```

**GitHub Actions**:
```yaml
on:
  schedule:
    - cron: '0 2 * * *'
```

## Complete Job Mapping

### Gateway Jobs

| GitLab CI Job | GitHub Actions Workflow | GitHub Actions Job |
|---------------|------------------------|-------------------|
| `gateway-observability-tests` | `gateway-tests.yml` | `gateway-observability-tests` |
| `gateway-coverage` | `gateway-tests.yml` | `gateway-coverage` |

**Triggers**:
- GitLab: `only.changes: apps/c-gateway/**/*`
- GitHub: `on.push.paths: apps/c-gateway/**`

### Router Jobs

| GitLab CI Job | GitHub Actions Workflow | GitHub Actions Job |
|---------------|------------------------|-------------------|
| `router-ct-fast` | `router-tests.yml` | `router-ct-fast` |
| `router-ct-full` | `router-tests.yml` | `router-ct-full` |
| `router-ct-heavy` | `nightly-tests.yml` | `router-heavy-tests` |
| `router-r10-unit` | `router-tests.yml` | `router-r10-unit` |
| `router-r10-e2e-ci` | `router-tests.yml` | `router-r10-e2e-ci` |
| `router-r10-e2e-heavy` | `nightly-tests.yml` | `router-r10-e2e-heavy` |
| `router-r10-property` | `nightly-tests.yml` | `router-r10-property` |
| `router-r10-protective-rails` | `router-tests.yml` | `router-r10-protective-rails` |
| `router-nats-performance-tests` | `nightly-tests.yml` | `router-nats-performance` |

**Trigger Differences**:

| Test Type | GitLab | GitHub |
|-----------|--------|--------|
| **Fast Tier** | `only.changes: router/**` | `on.push.paths: router/**` |
| **Full Tier** | `only: main, master, merge_requests` | `if: (main/master) or pull_request` |
| **Heavy Tier** | `only: schedules, triggers, main` + `when: manual` | `if: schedule or workflow_dispatch` |

### Worker Jobs

| GitLab CI Job | GitHub Actions Workflow | GitHub Actions Job |
|---------------|------------------------|-------------------|
| `worker-observability-tests` | `worker-tests.yml` | `worker-observability-tests` |
| `worker-coverage` | `worker-tests.yml` | `worker-coverage` |

**Triggers**:
- GitLab: `only.changes: apps/caf/processor/**/*`
- GitHub: `on.push.paths: apps/caf/processor/**`

## Environment Variables

### GitLab CI
```yaml
variables:
  GATEWAY_DIR: apps/c-gateway
  WORKER_DIR: apps/caf/processor
```

### GitHub Actions
```yaml
env:
  GATEWAY_DIR: apps/c-gateway
  WORKER_DIR: apps/caf/processor
```

## Artifacts

### GitLab CI
```yaml
artifacts:
  when: always
  paths:
    - ${GATEWAY_DIR}/build/
  expire_in: 1 week
  reports:
    junit: ${GATEWAY_DIR}/build/test-results.xml
```

### GitHub Actions
```yaml
- name: Upload test results
  uses: actions/upload-artifact@v4
  if: always()
  with:
    name: gateway-test-results
    path: ${{ env.GATEWAY_DIR }}/build/
    retention-days: 7
```

## Coverage Reporting

### GitLab CI
```yaml
coverage: '/lines\\.\\.\\.: \\d+\\.\\d+%/'
artifacts:
  reports:
    coverage_report:
      coverage_format: cobertura
      path: ${GATEWAY_DIR}/build/coverage.xml
```

### GitHub Actions
```yaml
# Coverage is extracted from lcov summary
# No built-in coverage reporting like GitLab
# Upload coverage.xml as artifact for external tools
```

## Migration Checklist

- [x] Create main CI workflow (`.github/workflows/ci.yml`)
- [x] Create Gateway tests workflow
- [x] Create Router tests workflow
- [x] Create Worker tests workflow
- [x] Create Nightly tests workflow
- [x] Set up artifact uploads
- [x] Configure caching (rebar3, etc.)
- [x] Document workflow structure
- [ ] Configure GitHub secrets (if needed)
- [ ] Test all workflows in GitHub
- [ ] Set up branch protection rules
- [ ] Configure required status checks

## Running Both Systems

For now, we run **both** GitLab CI and GitHub Actions in parallel:

1. **GitLab CI** remains authoritative
2. **GitHub Actions** provides additional visibility for GitHub users
3. Both should pass before merging

Eventually, we may consolidate to one platform.

## Testing Workflows

### Local Testing (GitHub Actions)
```bash
# Install act (GitHub Actions runner)
curl https://raw.githubusercontent.com/nektos/act/master/install.sh | sudo bash

# Test CI workflow
act push

# Test specific job
act -j gateway-tests

# Test PR workflow
act pull_request
```

### Local Testing (GitLab CI)
```bash
# Use gitlab-runner locally
gitlab-runner exec docker gateway-observability-tests
```

## Benefits of GitHub Actions

1. **Better UI**: GitHub Actions has a more modern interface
2. **Workflow Visualization**: See job dependencies in a graph
3. **Reusable Workflows**: Share workflows across repositories
4. **Marketplace**: Access to thousands of pre-built actions
5. **Native Integration**: Better integration with GitHub features (PR comments, status checks)

## Benefits of GitLab CI

1. **Simplicity**: Single file configuration
2. **Mature**: Well-established with years of refinement
3. **Coverage Reporting**: Built-in coverage tracking
4. **Pipeline Visualization**: Excellent pipeline DAG visualization

## Recommendations

1. **Keep GitLab CI as primary** until GitHub Actions is fully tested
2. **Use GitHub Actions for GitHub-specific features** (e.g., PR automation)
3. **Monitor both systems** to ensure they stay in sync
4. **Document changes** to both configurations when updating tests

## Support

For issues with:
- **GitLab CI**: Check `.gitlab-ci.yml` and GitLab pipeline logs
- **GitHub Actions**: Check `.github/workflows/*.yml` and GitHub Actions tab
