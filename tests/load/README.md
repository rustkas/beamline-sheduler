# Beamline Constructor - CP4-LC Load Testing Suite

This directory contains comprehensive load testing scripts for validating CP4-LC (End-to-End Integration) requirements in the Beamline Constructor project.

## Overview

The load testing suite validates that the integrated Router-Gateway-CAF workflow can handle production-level throughput and latency requirements as specified in CP4-LC.

### CP4-LC Requirements

- **Minimum Throughput**: 100 RPS (Requests Per Second)
- **Maximum P95 Latency**: 1000ms
- **Success Rate**: ≥ 95%
- **Test Coverage**: Chat, Embedding, Image Generation, Code Execution

## Test Scripts

### 1. Main Load Test (`load-test.ts`)

Comprehensive load testing with configurable parameters:

```bash
# Run all load tests
npm run load-test

# Set custom gateway URL
export GATEWAY_URL=http://localhost:3000
npm run load-test
```

**Features:**
- Multi-request type support (chat, embedding, image, code)
- Configurable concurrency and request counts
- Ramp-up periods for gradual load increase
- Detailed latency statistics (P50, P95, P99)
- Error analysis and reporting
- JSON result export

### 2. Throughput Validation (`throughput-test.ts`)

Specialized throughput testing with sustained load:

```bash
# Run throughput validation
npm run throughput-test
```

**Features:**
- Target RPS validation
- Sustained load over time
- Performance ratio analysis
- CP4-LC requirement validation

### 3. Stress Test (Planned)

High-load stress testing for system limits discovery.

## Test Architecture

### LoadTester Class

Core load testing engine with:
- **Concurrent Request Processing**: Configurable worker pool
- **Request Queue Management**: Efficient request distribution
- **Metrics Collection**: Detailed performance tracking
- **Error Handling**: Comprehensive failure analysis
- **Progress Reporting**: Real-time test status

### Request Types

1. **Chat Requests**
   - Model: `gpt-3.5-turbo`
   - Max tokens: 150
   - Temperature: 0.7
   - Timeout: 30s

2. **Embedding Requests**
   - Model: `text-embedding-ada-002`
   - Input text generation
   - Timeout: 15s

3. **Image Generation**
   - Model: `dall-e-3`
   - Size: 256x256
   - Single image per request
   - Timeout: 45s

4. **Code Execution**
   - Python function generation
   - Max tokens: 200
   - Temperature: 0.1
   - Timeout: 45s

## Configuration

### Environment Variables

```bash
# Gateway URL (default: http://localhost:3000)
export GATEWAY_URL=http://your-gateway:3000

# Custom test parameters (optional)
export CONCURRENT_REQUESTS=20
export TOTAL_REQUESTS=1000
export RAMP_UP_SECONDS=10
```

### Test Configurations

Default test configurations in `load-test.ts`:

```typescript
const testConfigs: LoadTestConfig[] = [
  {
    concurrentRequests: 10,
    totalRequests: 100,
    requestType: 'chat',
    timeoutMs: 30000,
    rampUpSeconds: 5
  },
  {
    concurrentRequests: 20,
    totalRequests: 200,
    requestType: 'embedding',
    timeoutMs: 15000,
    rampUpSeconds: 10
  },
  {
    concurrentRequests: 5,
    totalRequests: 50,
    requestType: 'code',
    timeoutMs: 45000,
    rampUpSeconds: 3
  }
];
```

## Running Tests

### Prerequisites

```bash
# Install dependencies
npm install

# Build TypeScript
npm run build
```

### Basic Usage

```bash
# Run all load tests
npm test

# Run specific test type
npm run load-test
npm run throughput-test

# Clean generated files
npm run clean
```

### Advanced Usage

```bash
# Custom gateway with specific tests
GATEWAY_URL=http://gateway.example.com npm run load-test

# Run with custom parameters
CONCURRENT_REQUESTS=50 TOTAL_REQUESTS=500 npm run load-test

# Save results to specific directory
npm run load-test && mv load-test-*.json /path/to/results/
```

## Results Analysis

### Output Files

Tests generate JSON files with naming pattern:
```
load-test-{request-type}-{timestamp}.json
```

### Result Structure

```json
{
  "timestamp": "2024-01-15T10:30:00.000Z",
  "results": {
    "totalRequests": 100,
    "successfulRequests": 98,
    "failedRequests": 2,
    "averageLatency": 245.67,
    "p50Latency": 198.45,
    "p95Latency": 456.78,
    "p99Latency": 789.01,
    "throughputRPS": 45.23,
    "durationSeconds": 2.21,
    "errors": []
  },
  "config": {
    "cp4_lc_requirements": {
      "min_throughput_rps": 100,
      "max_p95_latency_ms": 1000,
      "success_rate_threshold": 0.95
    }
  }
}
```

### Validation Criteria

Tests automatically validate against CP4-LC requirements:

1. **Throughput**: ≥ 100 RPS
2. **Latency**: P95 ≤ 1000ms
3. **Success Rate**: ≥ 95%

### Performance Indicators

- ✅ **EXCELLENT**: ≥ 95% of target performance
- ✅ **GOOD**: 85-94% of target performance
- ⚠️ **FAIR**: 70-84% of target performance
- ❌ **POOR**: < 70% of target performance

## Troubleshooting

### Common Issues

1. **Connection Refused**
   ```bash
   # Verify gateway is running
   curl http://localhost:3000/health
   
   # Check gateway logs
   docker logs beamline-gateway
   ```

2. **High Error Rate**
   - Reduce concurrent requests
   - Increase timeout values
   - Check gateway capacity

3. **Low Throughput**
   - Verify network connectivity
   - Check gateway worker configuration
   - Monitor system resources

### Debug Mode

Enable verbose logging:
```bash
DEBUG=load-test npm run load-test
```

### Health Checks

Before running tests, verify system health:
```bash
# Gateway health
curl http://localhost:3000/health

# Router health
curl http://localhost:8080/health

# CAF worker health
curl http://localhost:8081/health
```

## Integration with CI/CD

### GitHub Actions Example

```yaml
- name: Run Load Tests
  run: |
    cd tests/load
    npm install
    npm run load-test
    
- name: Validate Results
  run: |
    # Check if all tests passed CP4-LC requirements
    node validate-results.js
```

### Result Validation Script

```javascript
const fs = require('fs');
const results = JSON.parse(fs.readFileSync('load-test-chat-*.json'));

const passed = results.results.throughputRPS >= 100 && 
               results.results.p95Latency <= 1000 &&
               results.results.successfulRequests / results.results.totalRequests >= 0.95;

if (!passed) {
  console.error('CP4-LC requirements not met');
  process.exit(1);
}
```

## Next Steps

After successful CP4-LC validation:

1. **Advance to CP5-LC**: Governance and Policies
2. **Performance Optimization**: Address any bottlenecks
3. **Production Deployment**: Prepare for live traffic
4. **Monitoring Setup**: Implement production observability

## Support

For issues or questions:
- Check existing test logs
- Review gateway/router/worker logs
- Verify network connectivity
- Consult integration test documentation

## Contributing

To add new test scenarios:

1. Extend `LoadTestConfig` interface
2. Add new request types in `generateRequestData()`
3. Update validation criteria if needed
4. Add documentation and examples