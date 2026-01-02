````md
# Development Git Workflow (Umbrella Repo + Submodules)

This repository is an **umbrella/monorepo** that includes multiple independent projects as **Git submodules** under `apps/*`.
The umbrella repo stores **pointers to exact submodule commits** (not the full submodule history/content).

---

## Quick start

### Clone (first time)

Clone the umbrella repo **with all submodules**:

```bash
git clone --recurse-submodules git@github.com:rustkas/beamline-scheduler.git
cd beamline-scheduler
````

If you already cloned without submodules:

```bash
git submodule update --init --recursive
```

### After every `git pull` in umbrella

Always keep submodules in sync with the commits recorded by umbrella:

```bash
git pull --rebase
git submodule update --init --recursive
```

---

## Day-to-day workflows

## A) Change only umbrella files (root/config/scripts/docs/etc.)

Typical flow:

```bash
git checkout main
git pull --rebase

# edit files...

bash scripts/check_secret_leaks.sh --staged
git add -A
git commit -m "docs: update dev readme"
git push
```

---

## B) Change a specific submodule (example: `apps/otp/router`)

**Important:** commit changes inside the submodule first, then update the umbrella pointer.

### 1) Work and commit inside the submodule

```bash
cd apps/otp/router
git checkout master   # or main — use the branch used by that submodule
git pull --rebase

# edit files...

git status
git add -A
git commit -m "fix: ..."
git push
```

### 2) Record the new submodule commit in umbrella

```bash
cd ../../..

git status
# you will see: modified: apps/otp/router (new commits)

git add apps/otp/router
bash scripts/check_secret_leaks.sh --staged
git commit -m "chore: bump router submodule"
git push
```

---

## Updating submodules

## Update submodules to the exact commits pinned by umbrella (safe)

Use this after switching branches or pulling in umbrella:

```bash
git submodule update --init --recursive
```
