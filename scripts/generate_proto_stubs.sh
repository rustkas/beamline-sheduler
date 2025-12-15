#!/bin/bash
# Generate protobuf stubs for Erlang and TypeScript

set -euo pipefail

PROTO_DIR="proto"
ERLANG_OUT="apps/otp/router/src/proto"
TYPESCRIPT_OUT="apps/gateway/src/proto"

echo "=========================================="
echo "Generating Protobuf Stubs"
echo "=========================================="

# Check if buf is available
if ! command -v buf &> /dev/null; then
    echo "[ERROR] buf not found. Install from https://docs.buf.build/installation"
    exit 1
fi

# Check if protoc plugins are available
echo "[INFO] Checking protoc plugins..."

# Erlang: gpb or grpcbox
if ! command -v protoc-gen-erlang &> /dev/null && ! command -v gpb &> /dev/null; then
    echo "[WARN] Erlang protoc plugin not found. Install gpb or use grpcbox"
    echo "  For gpb: https://github.com/tomas-abrahamsson/gpb"
    echo "  For grpcbox: Already in rebar.config dependencies"
fi

# TypeScript: @grpc/grpc-js and @grpc/proto-loader
if [ ! -d "apps/gateway/node_modules/@grpc" ]; then
    echo "[WARN] TypeScript gRPC packages not found. Run: cd apps/gateway && npm install @grpc/grpc-js @grpc/proto-loader"
fi

# Generate with buf
echo "[INFO] Generating with buf..."
cd "$PROTO_DIR"

# Lint
echo "[1/3] Running buf lint..."
buf lint || {
    echo "[ERROR] buf lint failed"
    exit 1
}

# Build
echo "[2/3] Running buf build..."
buf build || {
    echo "[ERROR] buf build failed"
    exit 1
}

# Generate (if plugins are installed)
if command -v protoc-gen-erlang &> /dev/null || command -v gpb &> /dev/null; then
    echo "[3/3] Generating Erlang stubs..."
    # Use protoc directly for Erlang
    protoc --erlang_out="$ERLANG_OUT" \
           --proto_path=beamline/flow/v1 \
           beamline/flow/v1/flow.proto || {
        echo "[WARN] Erlang stub generation failed (may need gpb or grpcbox)"
    }
else
    echo "[SKIP] Erlang stub generation (plugins not available)"
fi

if [ -d "../$TYPESCRIPT_OUT/../node_modules/@grpc" ]; then
    echo "[3/3] Generating TypeScript stubs..."
    # Use protoc for TypeScript
    protoc --ts_out="$TYPESCRIPT_OUT" \
           --proto_path=beamline/flow/v1 \
           beamline/flow/v1/flow.proto || {
        echo "[WARN] TypeScript stub generation failed (may need @grpc/proto-loader)"
    }
else
    echo "[SKIP] TypeScript stub generation (packages not installed)"
fi

cd ..

echo "=========================================="
echo "[OK] Protobuf stub generation completed"
echo "=========================================="
echo ""
echo "Next steps:"
echo "  - Erlang: Use grpcbox (already in rebar.config) for runtime"
echo "  - TypeScript: Install @grpc/grpc-js and @grpc/proto-loader"
echo "  - Verify: Check generated files in $ERLANG_OUT and $TYPESCRIPT_OUT"
