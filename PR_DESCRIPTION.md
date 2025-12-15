# Synchronize JETSTREAM_FAULT_INJECTION_TESTS.md with Actual Test Implementations

## Summary

This PR synchronizes the fault injection test specification document (`apps/otp/router/docs/dev/JETSTREAM_FAULT_INJECTION_TESTS.md`) with the actual test implementations in the codebase. The document now serves as the **single source of truth** for fault injection test coverage.

## Changes

### Document Updates

1. **Added explicit scenario-to-test mapping table** - Quick reference table mapping scenarios S1, S2, S3 to their test implementations with file paths and line numbers
2. **Updated all test references** - All test names now match actual implementation (legacy names removed/updated)
3. **Added status indicators** - Each scenario clearly marked as ✅ Implemented, ⚠️ Partial, or ❌ Planned
4. **Expanded test coverage matrix** - Comprehensive table including all relevant test suites
5. **Enhanced Future Enhancements section** - Detailed planned scenarios with implementation priorities

### Test Name Corrections

The following legacy test names from the original document have been corrected to match actual implementations:

- `test_intermittent_ack_nak_errors/1` → `test_intermittent_ack_failure_recovery/1` (router_jetstream_e2e_SUITE.erl)
- `test_processing_delays_redelivery_growth/1` → `test_processing_delays_redelivery_with_delivery_count/1` (multiple suites)
- `test_maxdeliver_exhaustion_partial_messages/1` → `test_maxdeliver_exhaustion_partial_messages_e2e/1` (router_jetstream_e2e_SUITE.erl)

All test names in the document now match the codebase.

## Test Coverage

### Scenario Coverage Status

- ✅ **S1: Intermittent ACK/NAK Errors** - Fully implemented (2 tests)
- ✅ **S2: Processing Delays → Redelivery Growth** - Fully implemented (4 tests)
- ✅ **S3: MaxDeliver Exhaustion for Partial Messages** - Fully implemented (5 tests)

All scenarios are covered by multiple test implementations across different test suites (E2E, unit, integration).

## Documentation Reference

**Single Source of Truth**: `apps/otp/router/docs/dev/JETSTREAM_FAULT_INJECTION_TESTS.md`

This document now serves as the **single source of truth** for fault injection test coverage. It provides:

- **Explicit scenario-to-test mapping table** (S1/S2/S3 → test implementations) - Quick reference at the top of the document
- **Complete test coverage matrix** - Comprehensive table with file paths and line numbers
- **Status indicators** - Each scenario clearly marked (✅ Implemented, ⚠️ Partial, ❌ Planned)
- **Detailed Future Enhancements section** - Planned scenarios with implementation priorities
- **Legacy test name corrections** - All test names now match actual implementation (old names documented as legacy)

## Verification

All test references have been verified against the actual codebase:
- ✅ All test names exist in the codebase
- ✅ All file paths are correct
- ✅ All line numbers are accurate
- ✅ All scenarios have at least one test implementation

## Related Files

- `apps/otp/router/docs/dev/JETSTREAM_FAULT_INJECTION_TESTS.md` - Main specification document (updated)
- `apps/otp/router/test/router_jetstream_e2e_SUITE.erl` - E2E test suite
- `apps/otp/router/test/router_jetstream_fault_injection_SUITE.erl` - Fault injection test suite
- `apps/otp/router/test/router_delivery_count_tracking_SUITE.erl` - Delivery count tracking suite
- `apps/otp/router/test/router_concurrent_faults_SUITE.erl` - Concurrent faults suite

## Impact

- **No code changes** - Documentation only
- **No test changes** - Only documentation synchronization
- **Improved maintainability** - Document now accurately reflects test implementation
- **Better discoverability** - Explicit mapping table makes it easy to find tests for each scenario

