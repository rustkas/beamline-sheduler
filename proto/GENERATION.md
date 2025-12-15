# Protobuf Code Generation Guide

Instructions for generating client stubs from protobuf definitions for Gateway (TypeScript) and Router (Erlang/OTP).

## Proto Files Structure

```
proto/
├── beamline/
│   ├── flow/v1/
│   │   └── flow.proto          # Router.Decide service
│   ├── provider/v1/
│   │   └── provider.proto      # Provider.Invoke service
│   └── ingress/v1/
│       └── ingress.proto        # Ingress.Send service
├── buf.yaml                     # Buf configuration
├── buf.gen.yaml                 # Code generation configuration
└── GENERATION.md                 # This file
```

## Prerequisites

### General Requirements

- **buf**: Tool for working with protobuf
  ```bash
  # Install buf
  curl -sSL "https://github.com/bufbuild/buf/releases/latest/download/buf-$(uname -s)-$(uname -m)" -o "/usr/local/bin/buf"
  chmod +x /usr/local/bin/buf
  ```

- **protoc**: Protocol Buffers compiler
  ```bash
  # macOS
  brew install protobuf
  
  # Linux
  sudo apt-get install protobuf-compiler
  ```

### TypeScript (Gateway)

**Dependencies**:
```json
{
  "dependencies": {
    "@grpc/grpc-js": "^1.9.0",
    "@grpc/proto-loader": "^0.7.10"
  },
  "devDependencies": {
    "grpc-tools": "^1.12.0",
    "grpc-tools-node-proto-loader": "^1.12.0"
  }
}
```

**Installation**:
```bash
cd apps/gateway
npm install @grpc/grpc-js @grpc/proto-loader
npm install --save-dev grpc-tools grpc-tools-node-proto-loader
```

### Erlang/OTP (Router)

**Dependencies** (already in `rebar.config`):
- `grpcbox`: gRPC library for Erlang
- `gpb`: Protocol Buffers compiler for Erlang (optional)

**Installation**:
```bash
cd apps/otp/router
rebar3 get-deps
```

## Code Generation

### TypeScript (Gateway)

#### Method 1: Using buf (recommended)

```bash
cd proto
buf generate --template buf.gen.yaml
```

Generated files will be in `apps/gateway/src/proto/`.

#### Method 2: Using protoc directly

```bash
cd proto

# Generate TypeScript stubs
protoc \
  --plugin=protoc-gen-ts=./node_modules/.bin/protoc-gen-ts \
  --ts_out=grpc_js:../apps/gateway/src/proto \
  --proto_path=beamline/flow/v1 \
  --proto_path=beamline/provider/v1 \
  --proto_path=beamline/ingress/v1 \
  beamline/flow/v1/flow.proto \
  beamline/provider/v1/provider.proto \
  beamline/ingress/v1/ingress.proto
```

#### Method 3: npm script in package.json

Add to `apps/gateway/package.json`:
```json
{
  "scripts": {
    "proto:generate": "cd ../../proto && buf generate --template buf.gen.yaml",
    "proto:lint": "cd ../../proto && buf lint",
    "proto:build": "cd ../../proto && buf build"
  }
}
```

Usage:
```bash
cd apps/gateway
npm run proto:generate
```

### Erlang/OTP (Router)

#### Method 1: Using grpcbox (recommended)

`grpcbox` automatically generates code from proto files during compilation.

**Configuration in `rebar.config`**:
```erlang
{grpc, [
    {protos, ["proto/beamline/flow/v1/flow.proto",
              "proto/beamline/provider/v1/provider.proto",
              "proto/beamline/ingress/v1/ingress.proto"]},
    {gpb_opts, [
        {module_name_suffix, "_pb"},
        {o_include, ["apps/otp/router/src/proto"]}
    ]}
]}.
```

**Generation**:
```bash
cd apps/otp/router
rebar3 grpc gen
```

#### Method 2: Using gpb directly

```bash
cd proto

# Generate Erlang stubs
gpb -I beamline/flow/v1 \
    -I beamline/provider/v1 \
    -I beamline/ingress/v1 \
    -o-erl apps/otp/router/src/proto \
    -o-hrl apps/otp/router/src/proto \
    beamline/flow/v1/flow.proto \
    beamline/provider/v1/provider.proto \
    beamline/ingress/v1/ingress.proto
```

#### Method 3: rebar3 script

Create `apps/otp/router/scripts/generate_proto.sh`:
```bash
#!/bin/bash
cd "$(dirname "$0")/../../.."
cd proto
buf generate --template buf.gen.yaml
```

Usage:
```bash
cd apps/otp/router
./scripts/generate_proto.sh
```

## Validation

### Proto File Validation

```bash
cd proto

# Lint
buf lint

# Build
buf build

# Breaking changes check
buf breaking --against '.git#branch=main'
```

### Generated Code Validation

**TypeScript**:
```bash
cd apps/gateway
npm run build
npm run test
```

**Erlang**:
```bash
cd apps/otp/router
rebar3 compile
rebar3 dialyzer
```

## CI/CD Integration

### GitHub Actions

```yaml
- name: Validate Protobuf
  run: |
    cd proto
    buf lint
    buf build

- name: Generate TypeScript Stubs
  run: |
    cd proto
    buf generate --template buf.gen.yaml

- name: Generate Erlang Stubs
  run: |
    cd apps/otp/router
    rebar3 grpc gen
```

## Troubleshooting

### TypeScript: "Cannot find module '@grpc/grpc-js'"

**Solution**: Install dependencies:
```bash
cd apps/gateway
npm install @grpc/grpc-js @grpc/proto-loader
```

### Erlang: "undefined function grpcbox:..."

**Solution**: Ensure `grpcbox` is in dependencies:
```bash
cd apps/otp/router
rebar3 get-deps
rebar3 compile
```

### buf: "command not found"

**Solution**: Install buf:
```bash
curl -sSL "https://github.com/bufbuild/buf/releases/latest/download/buf-$(uname -s)-$(uname -m)" -o "/usr/local/bin/buf"
chmod +x /usr/local/bin/buf
```

## References

- [Buf Documentation](https://docs.buf.build)
- [Protocol Buffers Guide](https://developers.google.com/protocol-buffers)
- [gRPC TypeScript Guide](https://grpc.io/docs/languages/node/)
- [grpcbox Documentation](https://github.com/tsloughter/grpcbox)
