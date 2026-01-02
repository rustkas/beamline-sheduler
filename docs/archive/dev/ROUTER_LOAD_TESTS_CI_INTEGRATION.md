# Router Load Tests CI/CD Integration

**Date**: 2025-01-27  
**Status**: ✅ **Integrated**

## Overview

Router Intake Load Tests are integrated into CI/CD pipeline as part of acceptance criteria for CP2+.

## CI/CD Integration

### GitHub Actions

**Workflow**: `.github/workflows/router-load-tests.yml`

**Triggers**:
- Push to `main` or `develop` branches (when Router code changes)
- Pull requests to `main` or `develop` branches
- Manual workflow dispatch (with configurable parameters)

**Configuration**:
- **Message Count**: Configurable via `LOAD_TEST_MESSAGE_COUNT` (default: 2000)
- **Parallel Workers**: Configurable via `LOAD_TEST_PARALLEL_WORKERS` (default: 1)

**Steps**:
1. Checkout code
2. Set up Erlang/OTP 26.0
3. Install dependencies
4. Compile Router
5. Run load tests
6. Upload test results

### Local Execution

**Script**: `scripts/run_router_load_tests.sh`

**Usage**:
```bash
# Default: 2000 messages, sequential
./scripts/run_router_load_tests.sh

# Custom message count
./scripts/run_router_load_tests.sh 10000

# Custom message count + parallel workers
./scripts/run_router_load_tests.sh 10000 5
```

**Environment Variables**:
- `LOAD_TEST_MESSAGE_COUNT`: Number of messages per test (default: 2000)
- `LOAD_TEST_PARALLEL_WORKERS`: Number of parallel workers (default: 1, max: 10)

### Test Configuration

**Scalability**:
- **Default**: 2000 messages, sequential execution
- **Stress Testing**: Up to 10000 messages via environment variable
- **Parallel Execution**: Up to 10 parallel workers via environment variable

**Test Timeout**: 180 seconds per test (configurable via `ct` timeout)

## Acceptance Criteria

**Load tests must pass** for CP2+ acceptance:

1. ✅ **High-Volume Success Flood**: 2000+ valid messages processed correctly
2. ✅ **High-Volume Error Flood**: 2000+ invalid messages handled correctly
3. ✅ **Mixed Stream**: 2000+ mixed messages (70% valid, 30% invalid) processed correctly
4. ✅ **Idempotency Stress**: 1000+ messages with repeating keys handled correctly

**Success Criteria**:
- All messages ACKed
- DLQ count matches expected
- Metrics match message counts
- Process stability (growth < 10%)
- Router remains alive

## CI/CD Status

**Current Status**: ✅ **Integrated**

**Workflow File**: `.github/workflows/router-load-tests.yml`

**Test Script**: `scripts/run_router_load_tests.sh`

**Documentation**: `docs/archive/dev/ROUTER_INTAKE_LOAD_TESTS_SPEC.md`

## Future Enhancements

1. **Performance Benchmarking**: Track latency percentiles, throughput
2. **Parallel Execution**: Enable parallel workers by default in CI
3. **Stress Testing**: Run with 10000 messages in nightly builds
4. **Metrics Collection**: Collect and report test metrics (duration, throughput)

## References

- `apps/otp/router/test/router_intake_e2e_SUITE.erl` - Test implementation
- `docs/archive/dev/ROUTER_INTAKE_LOAD_TESTS_SPEC.md` - Test specification
- `.github/workflows/router-load-tests.yml` - CI/CD workflow

