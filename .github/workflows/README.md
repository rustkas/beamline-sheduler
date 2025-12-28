# GitHub Actions CI/CD Workflows

This directory contains GitHub Actions workflows for continuous integration and deployment.

## Overview

Our CI/CD pipeline is organized into several workflows that run different types of tests:

### Main Workflows

#### 1. **CI Workflow** (`.github/workflows/ci.yml`)
- **Trigger**: Push to `main`/`master`, Pull Requests
- **Purpose**: Main CI pipeline that runs on every commit
- **Components**:
  - Gateway tests (C Gateway)
  - Router tests (Erlang/OTP)
  - Worker tests (C++/CAF)
  - Rust Worker tests

#### 2. **Gateway Tests** (`.github/workflows/gateway-tests.yml`)
- **Trigger**: Changes to `apps/c-gateway/**`
- **Tests**:
  - Observability tests
  - Integration tests
  - Performance tests
  - E2E tests
- **Coverage**: Full code coverage reporting with lcov

#### 3. **Router Tests** (`.github/workflows/router-tests.yml`)
- **Trigger**: Changes to `apps/otp/router/**`
- **Test Tiers**:
  - **Fast Tier**: Unit tests + critical integration (runs on every commit)
  - **Full Tier**: All tests except heavy (runs on main/PR)
  - **Heavy Tier**: Chaos, soak, performance (scheduled/manual)
- **R10 Features**:
  - Circuit Breaker Unit Tests
  - E2E Tests (CI profile: 10 clients × 20 requests)
  - E2E Tests (Heavy profile: 50 clients × 100 requests)
  - Property-Based Tests
  - Protective Rails Validation
- **Performance**:
  - NATS performance tests (scheduled/manual)

#### 4. **Worker Tests** (`.github/workflows/worker-tests.yml`)
- **Trigger**: Changes to `apps/caf/processor/**`
- **Tests**:
  - Observability tests
  - Health endpoint tests
  - Performance tests
  - E2E tests
- **Coverage**: Full code coverage reporting with lcov

#### 5. **Nightly Tests** (`.github/workflows/nightly-tests.yml`)
- **Trigger**: Scheduled (2:00 AM UTC daily) or manual dispatch
- **Purpose**: Run heavy, long-running tests overnight
- **Includes**:
  - Router Heavy Tier Tests
  - R10 E2E Heavy Profile
  - R10 Property-Based Tests
  - Router NATS Performance Tests

## Workflow Structure

### Triggers

- **On Push**: Runs when code is pushed to main/master branches
- **On Pull Request**: Runs when a PR is opened or updated
- **On Schedule**: Runs nightly at 2:00 AM UTC
- **On Workflow Dispatch**: Manual trigger from GitHub UI

### Path Filters

Most workflows use path filters to run only when relevant files change:

```yaml
on:
  push:
    paths:
      - 'apps/c-gateway/**'
      - '.github/workflows/gateway-tests.yml'
```

### Concurrency Control

All workflows use concurrency groups to cancel in-progress runs:

```yaml
concurrency:
  group: gateway-${{ github.ref }}
  cancel-in-progress: true
```

## Artifacts

### Test Results
- **Retention**: 7 days (14 days for heavy tests)
- **Contents**: Test logs, reports, metrics

### Coverage Reports
- **Retention**: 7 days
- **Formats**: HTML reports, lcov info files, Cobertura XML
- **Components**: Gateway, Router, Worker

## Comparison with GitLab CI

This GitHub Actions setup mirrors our GitLab CI configuration (`.gitlab-ci.yml`) with the following equivalences:

| GitLab CI Job | GitHub Actions Workflow |
|---------------|------------------------|
| `gateway-observability-tests` | `gateway-tests.yml` → `gateway-observability-tests` |
| `gateway-coverage` | `gateway-tests.yml` → `gateway-coverage` |
| `router-ct-fast` | `router-tests.yml` → `router-ct-fast` |
| `router-ct-full` | `router-tests.yml` → `router-ct-full` |
| `router-ct-heavy` | `nightly-tests.yml` → `router-heavy-tests` |
| `router-r10-unit` | `router-tests.yml` → `router-r10-unit` |
| `router-r10-e2e-ci` | `router-tests.yml` → `router-r10-e2e-ci` |
| `router-r10-e2e-heavy` | `nightly-tests.yml` → `router-r10-e2e-heavy` |
| `router-r10-property` | `nightly-tests.yml` → `router-r10-property` |
| `router-r10-protective-rails` | `router-tests.yml` → `router-r10-protective-rails` |
| `router-nats-performance-tests` | `nightly-tests.yml` → `router-nats-performance` |
| `worker-observability-tests` | `worker-tests.yml` → `worker-observability-tests` |
| `worker-coverage` | `worker-tests.yml` → `worker-coverage` |

## Key Differences from GitLab CI

1. **Workflow Reusability**: GitHub Actions uses reusable workflows (`.github/workflows/*.yml`) called from main `ci.yml`
2. **Matrix Builds**: GitLab uses `only:` for conditions, GitHub uses `if:` and `strategy.matrix`
3. **Artifacts**: GitHub Actions has built-in artifact upload/download actions
4. **Caching**: GitHub Actions has dedicated cache actions for dependencies

## Running Workflows Locally

To test workflows locally, you can use [act](https://github.com/nektos/act):

```bash
# Install act
curl https://raw.githubusercontent.com/nektos/act/master/install.sh | sudo bash

# Run all CI jobs
act push

# Run specific job
act -j gateway-tests

# Run with specific event
act pull_request
```

## Monitoring

### GitHub Actions Dashboard
- Navigate to **Actions** tab in GitHub repository
- View running, queued, and completed workflows
- Download artifacts from completed runs

### Workflow Status Badges
Add to README.md:

```markdown
![CI](https://github.com/YOUR_ORG/YOUR_REPO/workflows/CI/badge.svg)
![Nightly Tests](https://github.com/YOUR_ORG/YOUR_REPO/workflows/Nightly%20Tests/badge.svg)
```

## Best Practices

1. **Fast Feedback**: Fast tests run on every commit, heavy tests are scheduled
2. **Path Filters**: Use path filters to run only relevant tests
3. **Concurrency**: Cancel in-progress runs to save resources
4. **Caching**: Cache dependencies (rebar3, npm, etc.) to speed up builds
5. **Artifacts**: Upload test results and coverage reports for debugging
6. **Matrix Builds**: Test against multiple versions (e.g., Erlang 25.3, 26.0)

## Secrets Required

The following secrets should be configured in GitHub repository settings:

- `BEAMLINE_HMAC_SECRET`: For DevState validation (if applicable)
- Other project-specific secrets as needed

## Maintenance

### Adding New Tests

1. Create or modify workflow file in `.github/workflows/`
2. Use appropriate triggers and path filters
3. Add job to appropriate workflow (CI or Nightly)
4. Update this README with new workflow details

### Debugging Failed Workflows

1. Go to **Actions** tab in GitHub
2. Click on failed workflow run
3. Expand failed job/step
4. Download artifacts for detailed logs
5. Re-run failed jobs if needed

## Additional Workflows

For other specialized workflows, see the `_github/workflows/` directory which contains:
- Integration tests
- Security scanning
- Release automation
- And more...

These can be moved to `.github/workflows/` as needed.
