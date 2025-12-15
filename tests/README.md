---
version: 1.0
authors:
  - WORKER wrk-7: QA/Test Automation
last_update: 2025-01-27T12:00:00Z
status: approved
rule_version: v10
message_protocol: v1
---

# Test Infrastructure

## Test Pyramid

```
        /\
       /E2E\          ← Few, critical user flows
      /------\
     /Integration\    ← API contracts, NATS, gRPC
    /------------\
   /    Unit      \   ← Many, isolated components
  /----------------\
```

## Structure

```
tests/
├── unit/              # Unit tests (isolated components)
│   ├── state/         # State validation tests
│   ├── history/       # History/HMAC tests
│   └── checksums/     # Checksum validation tests
├── integration/       # Integration tests (API contracts)
│   ├── protobuf/      # gRPC/protobuf contract tests
│   ├── nats/          # NATS subject tests
│   ├── gateway/       # Gateway API tests
│   └── dry-run/       # Dry-run validation tests
├── e2e/               # End-to-end tests (user flows)
│   ├── flows/         # Key user flows
│   └── scenarios/     # Regression scenarios
├── fixtures/          # Test fixtures and mocks
│   ├── state/         # STATE.json fixtures
│   ├── history/       # HISTORY.json fixtures
│   └── protobuf/      # Protobuf message fixtures
├── mocks/             # Mock servers
│   ├── nats/          # NATS mock server
│   └── grpc/          # gRPC mock server
├── utils/             # Test utilities
│   ├── validators/    # Validation helpers
│   └── generators/    # Test data generators
└── scripts/           # Test scripts
    ├── run_tests_with_gates.sh    # Run tests with validation gates
    └── generate_test_report.sh    # Generate test report
```

## Test Types

### Unit Tests

**Purpose**: Test isolated components without external dependencies

**Examples**:
- State validation against JSON-Schema
- HMAC calculation and verification
- Checksum validation
- Schema validation

**Location**: `tests/unit/`

**Tools**: Vitest (TypeScript/JavaScript)

**Coverage**: ≥ 80%

### Integration Tests

**Purpose**: Test API contracts and inter-component communication

**Examples**:
- Protobuf message serialization/deserialization
- NATS subject publishing/subscribing
- Gateway REST/SSE endpoints
- Dry-run validation flows

**Location**: `tests/integration/`

**Tools**: Mock servers, contract testing tools

### E2E Tests

**Purpose**: Test complete user flows end-to-end

**Examples**:
- Message routing flow (Ingress → Router → Provider)
- CP transition flow
- Error handling and recovery
- Regression scenarios

**Location**: `tests/e2e/`

**Tools**: Playwright, k6 (for load testing)

## Running Tests

### Quick Start

```bash
cd tests
npm install
npm run test
```

### Unit Tests

```bash
npm run test:unit
```

### Integration Tests

```bash
npm run test:integration
```

### E2E Tests

```bash
npm run test:e2e
```

### All Tests

```bash
npm run test:all
```

### Tests with Coverage

```bash
npm run test:coverage
```

### Tests with Validation Gates

**CRITICAL**: Run tests with validation gates to ensure compliance:

```bash
npm run test:gates
# or
bash tests/scripts/run_tests_with_gates.sh [unit|integration|e2e|coverage|all]
```

This script:
1. Reads `.trae/manifest.json`
2. Validates `.trae/state.json`
3. Verifies HMAC chain
4. Checks schema changes
5. Checks HMAC masking
6. Installs dependencies
7. Runs tests

### Generate Test Report

```bash
npm run test:report
# or
bash tests/scripts/generate_test_report.sh
```

Report includes:
- Test list
- Commands for running tests
- Test output
- Coverage information (if available)

## Test List

### Unit Tests

- `state_validation.test.ts` - State validation against JSON-Schema
- `history_validation.test.ts` - History validation and HMAC chain
- `checksum_validation.test.ts` - Checksum validation and format

### Integration Tests

- `dry_run_validation.test.ts` - Dry-run validation flows
- `protobuf_contracts.test.ts` - Protobuf contract validation

### E2E Tests

- `message_routing_flow.test.ts` - Message routing flow (Ingress → Router → Provider)
- `cp_transition.test.ts` - CP transition flow with validation

## Coverage Requirements

- **Unit tests**: ≥ 80% coverage
- **Integration tests**: All API contracts covered
- **E2E tests**: All critical user flows covered

## Test Data

- **Fixtures**: Use fixtures from `tests/fixtures/` for consistent test data
- **Mocks**: Use mock servers from `tests/mocks/` to avoid external dependencies
- **Generators**: Use generators from `tests/utils/generators/` for dynamic test data

## Continuous Integration

Tests run automatically in CI/CD pipelines:
- On every commit (unit + integration)
- On pull requests (all tests)
- Before CP transitions (dry-run + all tests)

## Integration with Validation Gates

All tests are integrated with local validation gates:

1. **State Validation**: Validates `.trae/state.json` against schema
2. **HMAC Chain**: Verifies HMAC chain integrity
3. **Schema Changes**: Checks for schema version changes
4. **HMAC Masking**: Verifies HMAC masking in documentation

See `tests/scripts/run_tests_with_gates.sh` for details.
