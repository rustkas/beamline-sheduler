# Mock Servers

Mock servers for testing without external dependencies.

## NATS Mock Server

Mock NATS server for testing NATS subject publishing/subscribing.

**Status**: Planned

**Implementation**: Use `nats-server` in test mode or mock NATS client

## gRPC Mock Server

Mock gRPC server for testing protobuf contracts.

**Status**: Planned

**Implementation**: Use `grpc-mock` or similar tool

## Usage

```bash
# Start mock servers
npm run test:mocks:start

# Run tests
npm run test:integration

# Stop mock servers
npm run test:mocks:stop
```

