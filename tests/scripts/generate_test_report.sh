#!/bin/bash
# Generate test report with test list, commands, output, and coverage

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
TESTS_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
REPORT_DIR="$PROJECT_ROOT/reports/tests"

mkdir -p "$REPORT_DIR"

TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
REPORT_FILE="$REPORT_DIR/test_report_${TIMESTAMP}.md"

cd "$TESTS_DIR"

echo "Generating test report..."
echo "Report file: $REPORT_FILE"

cat > "$REPORT_FILE" << 'EOF'
---
version: 1.0
authors:
  - WORKER wrk-7: QA/Test Automation
generated_at: TIMESTAMP_PLACEHOLDER
rule_version: v10
message_protocol: v1
---

# Test Report

## Test Execution Summary

EOF

echo "generated_at: $(date -u +"%Y-%m-%dT%H:%M:%SZ")" >> "$REPORT_FILE"
echo "" >> "$REPORT_FILE"

# Test List
echo "## Test List" >> "$REPORT_FILE"
echo "" >> "$REPORT_FILE"

echo "### Unit Tests" >> "$REPORT_FILE"
echo "" >> "$REPORT_FILE"
find tests/unit -name "*.test.ts" -type f | while read test_file; do
    test_name=$(basename "$test_file" .test.ts)
    echo "- \`$test_name\` - \`$test_file\`" >> "$REPORT_FILE"
done
echo "" >> "$REPORT_FILE"

echo "### Integration Tests" >> "$REPORT_FILE"
echo "" >> "$REPORT_FILE"
find tests/integration -name "*.test.ts" -type f | while read test_file; do
    test_name=$(basename "$test_file" .test.ts)
    echo "- \`$test_name\` - \`$test_file\`" >> "$REPORT_FILE"
done
echo "" >> "$REPORT_FILE"

echo "### E2E Tests" >> "$REPORT_FILE"
echo "" >> "$REPORT_FILE"
find tests/e2e -name "*.test.ts" -type f | while read test_file; do
    test_name=$(basename "$test_file" .test.ts)
    echo "- \`$test_name\` - \`$test_file\`" >> "$REPORT_FILE"
done
echo "" >> "$REPORT_FILE"

# Commands
cat >> "$REPORT_FILE" << 'EOF'
## Commands

### Run All Tests

```bash
cd tests
npm run test:all
```

### Run Unit Tests

```bash
cd tests
npm run test:unit
```

### Run Integration Tests

```bash
cd tests
npm run test:integration
```

### Run E2E Tests

```bash
cd tests
npm run test:e2e
```

### Run Tests with Coverage

```bash
cd tests
npm run test:coverage
```

### Run Tests with Validation Gates

```bash
bash tests/scripts/run_tests_with_gates.sh [unit|integration|e2e|coverage|all]
```

## Test Output

EOF

# Run tests and capture output
echo "Running tests to capture output..."

if [ -f "package.json" ] && command -v npm &> /dev/null; then
    echo "### Unit Tests Output" >> "$REPORT_FILE"
    echo "" >> "$REPORT_FILE"
    echo "\`\`\`" >> "$REPORT_FILE"
    npm run test:unit 2>&1 | head -100 >> "$REPORT_FILE" || true
    echo "\`\`\`" >> "$REPORT_FILE"
    echo "" >> "$REPORT_FILE"
    
    echo "### Integration Tests Output" >> "$REPORT_FILE"
    echo "" >> "$REPORT_FILE"
    echo "\`\`\`" >> "$REPORT_FILE"
    npm run test:integration 2>&1 | head -100 >> "$REPORT_FILE" || true
    echo "\`\`\`" >> "$REPORT_FILE"
    echo "" >> "$REPORT_FILE"
    
    # Coverage
    echo "### Coverage Report" >> "$REPORT_FILE"
    echo "" >> "$REPORT_FILE"
    if npm run test:coverage 2>&1 | tee /tmp/coverage_output.log; then
        echo "\`\`\`" >> "$REPORT_FILE"
        cat /tmp/coverage_output.log | head -50 >> "$REPORT_FILE" || true
        echo "\`\`\`" >> "$REPORT_FILE"
        echo "" >> "$REPORT_FILE"
        
        # Check if coverage files exist
        if [ -d "coverage" ]; then
            echo "Coverage files generated in \`tests/coverage/\`" >> "$REPORT_FILE"
            echo "" >> "$REPORT_FILE"
        fi
    else
        echo "Coverage report generation failed or not available" >> "$REPORT_FILE"
        echo "" >> "$REPORT_FILE"
    fi
else
    echo "npm not available or package.json not found" >> "$REPORT_FILE"
    echo "Skipping test execution output" >> "$REPORT_FILE"
    echo "" >> "$REPORT_FILE"
fi

# Test Infrastructure
cat >> "$REPORT_FILE" << 'EOF'
## Test Infrastructure

### Fixtures

- `tests/fixtures/state/` - STATE.json fixtures
- `tests/fixtures/history/` - HISTORY.json fixtures
- `tests/fixtures/protobuf/` - Protobuf message fixtures

### Utilities

- `tests/utils/validators/` - Validation utilities
- `tests/utils/generators/` - Test data generators

### Mocks

- `tests/mocks/` - Mock servers (NATS, gRPC)

## Coverage Requirements

- **Unit tests**: ≥ 80% coverage
- **Integration tests**: All API contracts covered
- **E2E tests**: All critical user flows covered

## Next Steps

1. Implement mock servers for NATS and gRPC
2. Add more unit tests for edge cases
3. Expand integration tests for all API contracts
4. Implement E2E tests for complete user flows
5. Set up CI/CD integration for automated test execution

EOF

echo "Test report generated: $REPORT_FILE"
echo ""
echo "Report contents:"
cat "$REPORT_FILE"

