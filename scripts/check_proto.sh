#!/bin/bash
# Protobuf file validation: lint and build

set -euo pipefail

PROTO_DIR="proto"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
LOCAL_BIN="${PROJECT_ROOT}/tools/bin"
PATH="${LOCAL_BIN}:$PATH"

echo "=========================================="
echo "Protobuf Validation"
echo "=========================================="

cd "$PROTO_DIR"

if ! command -v buf &> /dev/null; then
    echo "[INFO] buf not found in PATH. Attempting local install to tools/bin..."
    bash "${SCRIPT_DIR}/install_buf.sh" || {
        echo "[ERROR] Failed to auto-install buf. Install manually: https://docs.buf.build/installation" ;
        exit 1 ;
    }
fi

# Lint
echo "[1/2] Running buf lint..."
if buf lint; then
    echo "[OK] buf lint passed"
else
    echo "[FAIL] buf lint failed"
    exit 1
fi

# Build
echo "[2/2] Running buf build..."
if buf build; then
    echo "[OK] buf build passed"
else
    echo "[FAIL] buf build failed"
    exit 1
fi

echo "=========================================="
echo "[OK] All protobuf checks passed"
