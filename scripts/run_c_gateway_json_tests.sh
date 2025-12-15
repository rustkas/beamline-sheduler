#!/usr/bin/env bash
set -euo pipefail

# Run C-Gateway JSON unit tests
cd "$(dirname "$0")/../apps/c-gateway"

mkdir -p build
cd build
cmake .. >/dev/null
make -j"$(nproc)" c-gateway-json-test

./c-gateway-json-test
