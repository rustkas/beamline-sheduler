#\!/usr/bin/env bash
set -euo pipefail
cd apps/c-gateway
mkdir -p build
cd build
cmake ..
make c-gateway
