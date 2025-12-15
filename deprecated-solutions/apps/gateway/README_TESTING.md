# Gateway Testing Guide

## Overview

This guide explains how to run tests for the Gateway application.

## Test Structure

- **Unit Tests**: `*.spec.ts` files in `src/` directory
  - DTO validation tests
  - Service tests
  - Error handling tests

- **Integration Tests**: `*.e2e-spec.ts` files in `test/` directory
  - API endpoint tests
  - End-to-end flow tests

## Running Tests

### All Tests
```bash
npm test
```

### Unit Tests Only
```bash
npm run test:unit
```

### Integration Tests Only
```bash
npm run test:integration
```

### Watch Mode
```bash
npm run test:watch
```

### Coverage Report
```bash
npm run test:cov
```

### E2E Tests
```bash
npm run test:e2e
```

## Test Fixtures

Test fixtures are located in `test/fixtures/messages.fixture.ts`:
- `validMessage`: Basic valid message
- `messageWithOptionalFields`: Message with all optional fields
- `validRouteRequest`: Basic route request
- `routeRequestWithSticky`: Route request with sticky session
- `routeRequestWithWeighted`: Route request with weighted policy
- `validRouteDecision`: Basic route decision
- `stickyRouteDecision`: Sticky session decision
- `fallbackRouteDecision`: Fallback decision

## Mock Clients

Mock Router client is available in `src/routes/mocks/router-client.mock.ts`:
- `MockRouterClient`: Simulates Router/NATS adapter behavior
- Supports delay simulation
- Supports error injection
- Supports pre-configured decisions

## Test Coverage

Target coverage: **≥ 80%** for unit tests.

Current coverage can be viewed by running:
```bash
npm run test:cov
```

Coverage reports are generated in `coverage/` directory.

## Stability

Tests are designed to be stable and non-flaky:
- No external dependencies (mocked Router/NATS)
- Deterministic test data (fixtures)
- Isolated test cases
- Proper cleanup in `afterEach` hooks

## Troubleshooting

### Tests failing with timeout
- Increase timeout in `jest-e2e.json` if needed
- Check for hanging async operations

### Validation errors
- Ensure test data matches DTO validation rules
- Check required vs optional fields

### Mock client not working
- Verify mock client is properly injected
- Check mock configuration (delay, errors)

