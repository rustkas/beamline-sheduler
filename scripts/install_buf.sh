#!/bin/bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
BIN_DIR="$PROJECT_ROOT/tools/bin"
mkdir -p "$BIN_DIR"

OS="$(uname -s)"
ARCH_RAW="$(uname -m)"
case "$ARCH_RAW" in
  aarch64) ARCH="arm64" ;;
  x86_64) ARCH="x86_64" ;;
  arm64) ARCH="arm64" ;;
  *) ARCH="$ARCH_RAW" ;;
esac

URL="https://github.com/bufbuild/buf/releases/latest/download/buf-${OS}-${ARCH}"
OUT="$BIN_DIR/buf"

curl -fsSL "$URL" -o "$OUT"
chmod +x "$OUT"
"$OUT" --version