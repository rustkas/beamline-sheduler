# Integration Tests Documentation

## Overview

This document describes the comprehensive integration test suite for the BeamLine Constructor project, specifically designed to validate **CP4-LC** (End-to-End Integration) checkpoint requirements.

## Test Architecture

The integration test suite validates the complete workflow: **Gateway → Router → CAF → Router → Gateway**, ensuring all components work together seamlessly.

### Test Infrastructure

The test suite consists of several helper classes that simulate different components of the system:

#### 1. **NATSConnection Helper** (`tests/helpers/nats-helper.ts`)
- **Purpose**: Simulates NATS messaging infrastructure
- **Key Features**:
  - Message publishing and subscription
  - Connection/disconnection simulation
  - Message monitoring and validation
  - Network failure simulation
- **Usage**: Tests communication between Router and CAF components

#### 2. **GatewayClient Helper** (`tests/helpers/gateway-client.ts`)
- **Purpose**: Simulates HTTP client for Gateway API
- **Key Features**:
  - OpenAI-compatible API endpoints
  - SSE streaming support
  - Result polling mechanisms
  - Error handling and retry logic
- **Usage**: Tests Gateway API integration and response handling

#### 3. **CAFWorkerSimulator Helper** (`tests/helpers/caf-worker-simulator.ts`)
- **Purpose**: Simulates CAF (C++ Actor Framework) worker behavior
- **Key Features**:
  - Task processing simulation
  - Multiple task types (text completion, embeddings, image generation, code execution)
  - Worker metrics and health monitoring
  - Failure simulation and recovery
- **Usage**: Tests worker task execution and Router-CAF communication

#### 4. **DevStateHelper Helper** (`tests/helpers/devstate-helper.ts`)
- **Purpose**: Manages development state and checkpoint validation
- **Key Features**:
  - State initialization and persistence
  - Checkpoint advancement tracking
  - HMAC-based audit trail
  - Agent status management
- **Usage**: Validates checkpoint transitions and system readiness

## Test Suites

### 1. **Basic Message Flow Tests**

Validates the fundamental message flow through the system:

```typescript
describe('Basic Message Flow', () => {
  it('should complete full chat completion workflow', async () => {
    // 1. Send request through Gateway
    // 2. Route to appropriate provider
    // 3. Process through CAF worker
    // 4. Return response through Gateway
  });

  it('should handle provider fallback on timeout', async () => {
    // 1. Primary provider times out
    // 2. System falls back to secondary provider
    // 3. Request completes successfully
  });
});
```

### 2. **Extension System Integration Tests**

Tests the extension system integration:

```typescript
describe('Extension System Integration', () => {
  it('should process pre-processor extensions', async () => {
    // 1. Request passes through pre-processor
    // 2. Extension modifies request
    // 3. Modified request processed successfully
  });

  it('should validate with validator extensions', async () => {
    // 1. Request validated by extension
    // 2. Invalid requests rejected
    // 3. Valid requests processed
  });
});
```

### 3. **Error Handling and Resilience Tests**

Validates system resilience under failure conditions:

```typescript
describe('Error Handling and Resilience', () => {
  it('should handle NATS connection failures gracefully', async () => {
    // 1. Simulate NATS disconnection
    // 2. System handles gracefully
    // 3. Recovers when connection restored
  });

  it('should handle provider failures with retry', async () => {
    // 1. Provider fails with retryable error
    // 2. System retries with backoff
    // 3. Falls back if retries exhausted
  });
});
```

### 4. **Performance and Throughput Tests**

Validates performance requirements:

```typescript
describe('Performance and Throughput', () => {
  it('should handle concurrent requests efficiently', async () => {
    // 1. Send multiple concurrent requests
    // 2. Measure throughput and latency
    // 3. Verify no request loss
  });

  it('should meet latency SLA requirements', async () => {
    // 1. Measure end-to-end latency
    // 2. Verify p95 latency < 500ms
    // 3. Check p99 latency < 1000ms
  });
});
```

### 5. **Multi-tenancy and Isolation Tests**

Validates tenant isolation:

```typescript
describe('Multi-tenancy and Isolation', () => {
  it('should isolate tenant data and resources', async () => {
    // 1. Send requests from different tenants
    // 2. Verify data isolation
    // 3. Check resource quotas enforced
  });
});
```

### 6. **Observability and Monitoring Tests**

Validates observability features:

```typescript
describe('Observability and Monitoring', () => {
  it('should generate proper trace spans across components', async () => {
    // 1. Generate distributed trace
    // 2. Verify span creation
    // 3. Check trace propagation
  });

  it('should emit correct metrics for monitoring', async () => {
    // 1. Process requests
    // 2. Collect metrics
    // 3. Verify metric accuracy
  });
});
```

## Running the Tests

### Prerequisites

1. **Install Dependencies**:
   ```bash
   cd tests
   pnpm install
   ```

2. **Environment Variables** (optional):
   ```bash
   export NATS_URL=nats://localhost:4222
   export GATEWAY_URL=http://localhost:3000
   export TEST_API_KEY=test-key
   ```

### Test Execution

Run all integration tests:
```bash
cd tests
pnpm test:integration
```

Run specific test suite:
```bash
npx vitest run integration/router-gateway-caf.e2e.test.ts
```

Run with coverage:
```bash
pnpm test:coverage
```

### Test Configuration

The test suite uses Vitest configuration (`tests/vitest.config.ts`):
- **Timeout**: 30 seconds per test
- **Environment**: Node.js
- **Coverage**: V8 provider with text, JSON, and HTML reports
- **Globals**: Enabled for cleaner test syntax

## CP4-LC Validation Criteria

The integration tests validate the following CP4-LC requirements:

### ✅ **Component Integration**
- Gateway ↔ Router communication
- Router ↔ CAF worker coordination
- Message routing and transformation
- Response aggregation

### ✅ **Extension System**
- Pre-processor extension execution
- Post-processor extension execution
- Validator extension enforcement
- Extension lifecycle management

### ✅ **Error Handling**
- Graceful degradation on failures
- Retry mechanisms with backoff
- Circuit breaker patterns
- Fallback provider selection

### ✅ **Performance Requirements**
- Throughput: ≥ 500 tasks/second
- Latency p95: < 500ms
- Latency p99: < 1000ms
- Concurrent request handling

### ✅ **Multi-tenancy**
- Tenant data isolation
- Resource quota enforcement
- Per-tenant configuration
- Security boundary validation

### ✅ **Observability**
- Distributed tracing
- Metrics collection
- Health check endpoints
- Error reporting

## Test Data and Mocking

The test suite uses realistic mock data:

- **Chat Completions**: Simulated AI responses with tokens and metadata
- **Embeddings**: Vector representations with configurable dimensions
- **Images**: Generated image URLs with metadata
- **Code Execution**: Simulated execution results

## Continuous Integration

The integration tests are designed to run in CI/CD pipelines:

1. **Pre-merge Validation**: All tests must pass before merging
2. **Performance Regression**: Detect performance degradation
3. **Integration Gates**: Block deployment on test failures
4. **Coverage Requirements**: Maintain ≥ 80% test coverage

## Troubleshooting

### Common Issues

1. **Connection Refused**: Ensure Gateway service is running
2. **NATS Connection**: Verify NATS server is accessible
3. **Timeout Errors**: Increase test timeout for slow environments
4. **State Validation**: Check `.trae/state.json` integrity

### Debug Mode

Enable debug logging:
```bash
DEBUG=beamline:* pnpm test:integration
```

### Test Isolation

Tests are designed to be isolated and can run independently. Each test:
- Creates its own test data
- Cleans up resources after completion
- Uses unique identifiers to avoid conflicts

## Future Enhancements

Planned improvements to the integration test suite:

1. **Load Testing**: Automated load testing scripts
2. **Chaos Engineering**: Randomized failure injection
3. **Performance Profiling**: Detailed performance analysis
4. **Security Testing**: Security vulnerability validation
5. **Compliance Testing**: Regulatory requirement validation

## Conclusion

The integration test suite provides comprehensive validation of the BeamLine Constructor platform's CP4-LC requirements. It ensures that all components work together correctly, performance requirements are met, and the system is resilient to failures.

The test infrastructure is designed to be maintainable, extensible, and suitable for continuous integration environments.