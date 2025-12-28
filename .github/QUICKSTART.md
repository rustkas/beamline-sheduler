# GitHub Actions Quick Start 🚀

## What We Have

✅ **Complete GitHub Actions CI/CD** equivalent to GitLab CI  
✅ **9 Workflow Files** ready to use  
✅ **Full Test Coverage** for Gateway, Router, and Worker

## Active Workflows

### 1. 🔄 **Main CI** (`ci.yml`)
**Runs on**: Every push & PR to `main`/`master`  
**Tests**: Gateway, Router, Worker, Rust Worker  
**Time**: ~10-15 minutes

### 2. 🌙 **Nightly Tests** (`nightly-tests.yml`)
**Runs on**: 2:00 AM UTC daily (or manual trigger)  
**Tests**: Heavy tests, E2E stress, property-based tests  
**Time**: ~30-60 minutes

### 3. 🛠️ **Component-Specific Workflows**
- `gateway-tests.yml` - C Gateway tests + coverage
- `router-tests.yml` - Erlang Router (fast/full/heavy tiers)
- `worker-tests.yml` - C++ Worker tests + coverage

### 4. ✅ **Validation Workflows**
- `validate-cp2.yml` - CP2 contract validation
- `ci-validate.yml` - CI validation checks
- `devstate-hooks-tests.yml` - DevState hooks testing

## Quick Commands

```bash
# View all workflows
ls -la .github/workflows/

# Check workflow status on GitHub
# Go to: https://github.com/YOUR_ORG/YOUR_REPO/actions

# Test workflow locally (requires 'act')
act push                    # Test on push event
act pull_request           # Test on PR event
act -j gateway-tests       # Test specific job
```

## What Runs When?

| Event | Workflows Triggered |
|-------|-------------------|
| **Push to main** | Main CI (all components) |
| **Pull Request** | Main CI (all components) |
| **Change in `apps/c-gateway/**`** | Gateway tests only |
| **Change in `apps/otp/router/**`** | Router tests only |
| **Change in `apps/caf/processor/**`** | Worker tests only |
| **Every night at 2 AM UTC** | Nightly tests (heavy) |
| **Manual trigger** | Any workflow via GitHub UI |

## Viewing Results

1. Go to **GitHub Actions** tab in your repository
2. Click on a workflow run
3. Expand jobs to see details
4. Download artifacts for test results/coverage

## Comparison with GitLab CI

| Feature | GitLab CI | GitHub Actions |
|---------|-----------|----------------|
| **Config file** | `.gitlab-ci.yml` | `.github/workflows/*.yml` |
| **All tests** | ✅ | ✅ |
| **Coverage** | ✅ Built-in | ✅ Via artifacts |
| **Nightly tests** | ✅ | ✅ |
| **Path triggers** | ✅ | ✅ |
| **Status** | 🟢 Primary | 🟢 Active |

## Enable Required Checks

To require GitHub Actions to pass before merging:

1. Go to **Settings** → **Branches**
2. Edit branch protection for `main`
3. Enable **Require status checks to pass**
4. Select workflows:
   - ✅ CI / gateway-tests
   - ✅ CI / router-tests
   - ✅ CI / worker-tests

## Troubleshooting

### ❌ Workflow not running?
- Check `.github/workflows/` (not `.github/workflow/`)
- Verify file has `.yml` extension
- Check path filters match your changes

### ❌ Tests failing on GitHub but passing locally?
- Check environment differences (OS, dependencies)
- Review workflow logs for specific errors
- Download artifacts for detailed test output

### ❌ Need to re-run failed job?
- Click on failed workflow run
- Click **Re-run failed jobs** button

## Next Steps

1. ✅ Push code to see workflows in action
2. ✅ Check GitHub Actions tab for results
3. ✅ Set up branch protection rules
4. 📚 Read full docs: [GITHUB_ACTIONS_MIGRATION.md](../GITHUB_ACTIONS_MIGRATION.md)

## Need Help?

- **Full documentation**: `.github/workflows/README.md`
- **Migration guide**: `.github/GITHUB_ACTIONS_MIGRATION.md`
- **GitLab CI comparison**: `.gitlab-ci.yml`

---

**Status**: ✅ GitHub Actions fully configured and ready to use!
