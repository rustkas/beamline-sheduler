#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(pwd -P)"
TMP_DIR="$(mktemp -d)"
trap 'rm -rf "$TMP_DIR"' EXIT

cd "$TMP_DIR"
git init -q

# Create structure
mkdir -p scripts/hooks devstate-tools/devstate/scripts .trae

# Copy hook sources and verifier
cp "$REPO_ROOT/scripts/hooks/pre-commit" scripts/hooks/pre-commit
cp "$REPO_ROOT/scripts/hooks/pre-push" scripts/hooks/pre-push
cp "$REPO_ROOT/devstate-tools/devstate/scripts/devstate_verify.sh" devstate-tools/devstate/scripts/devstate_verify.sh

# Stub secret leaks checker
cat > scripts/check_secret_leaks.sh <<'EOF'
#!/usr/bin/env bash
exit 0
EOF
chmod +x scripts/check_secret_leaks.sh

# Minimal DevState files
echo '{}' > .trae/state.json
echo '[]' > .trae/history.json

# Install hooks
bash "$REPO_ROOT/scripts/install_devstate_hooks.sh" --backup --link

# Stage DevState change to trigger verification
git add .trae/state.json .trae/history.json

.git/hooks/pre-commit
echo "[OK] pre_commit test passed"
