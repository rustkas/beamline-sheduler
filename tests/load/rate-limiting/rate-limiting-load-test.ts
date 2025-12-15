/**
 * Rate Limiting Load Test
 * 
 * Purpose: Load testing for Router and Gateway rate limiting functionality
 * 
 * Test Scenarios:
 * 1. Single tenant, under limit
 * 2. Single tenant, at limit
 * 3. Single tenant, over limit (constant exceed)
 * 4. Multiple tenants, independent limits
 * 5. Traffic spikes (burst handling)
 * 6. Long-running exceed (sustained over-limit)
 * 7. Gateway + Router combined (E2E)
 * 
 * Reference: docs/dev/RATE_LIMITING_LOAD_TEST_PLAN.md
 */

import axios, { AxiosError } from 'axios';
import { performance } from 'perf_hooks';
import * as fs from 'fs';
import * as path from 'path';

interface LoadTestConfig {
  scenario: string;
  gatewayUrl: string;
  routerMetricsUrl?: string;
  gatewayMetricsUrl?: string;
  tenantId: string;
  policyId: string;
  requestRate: number; // requests per second
  duration: number; // seconds
  rateLimitRPS?: number; // Router rate limit (requests per second)
  rateLimitBurst?: number; // Router rate limit (burst capacity)
  gatewayRateLimit?: number; // Gateway rate limit (requests per minute)
}

interface RequestMetrics {
  startTime: number;
  endTime: number;
  duration: number;
  success: boolean;
  statusCode?: number;
  error?: string;
  rateLimited?: boolean;
}

interface LoadTestResult {
  scenario: string;
  config: LoadTestConfig;
  totalRequests: number;
  allowedRequests: number;
  rejectedRequests: number;
  successRate: number;
  latency: {
    p50: number;
    p95: number;
    p99: number;
  };
  metrics: {
    routerAllowed?: number;
    routerExceeded?: number;
    gatewayAllowed?: number;
    gatewayExceeded?: number;
  };
  system?: {
    cpuUsage?: number;
    memoryUsage?: number;
    etsTableSize?: number;
  };
}

class RateLimitingLoadTester {
  private config: LoadTestConfig;
  private metrics: RequestMetrics[] = [];
  private startTime = 0;
  private endTime = 0;
  private metricsBefore?: any;
  private metricsAfter?: any;

  constructor(config: LoadTestConfig) {
    this.config = config;
  }

  async runLoadTest(): Promise<LoadTestResult> {
    console.log(`🚀 Starting rate limiting load test: ${this.config.scenario}`);
    console.log(`📊 Config: ${JSON.stringify(this.config, null, 2)}`);

    // Collect metrics before test
    this.metricsBefore = await this.collectMetrics();

    this.startTime = performance.now();
    this.metrics = [];

    // Generate requests at specified rate
    const requestInterval = 1000 / this.config.requestRate; // milliseconds between requests
    const totalRequests = Math.floor(this.config.requestRate * this.config.duration);
    
    console.log(`📈 Generating ${totalRequests} requests at ${this.config.requestRate} req/s`);

    const requests: Promise<void>[] = [];
    for (let i = 0; i < totalRequests; i++) {
      const delay = i * requestInterval;
      requests.push(this.executeRequestWithDelay(i, delay));
    }

    await Promise.all(requests);

    this.endTime = performance.now();

    // Collect metrics after test
    this.metricsAfter = await this.collectMetrics();

    return this.generateResults();
  }

  private async executeRequestWithDelay(requestIndex: number, delayMs: number): Promise<void> {
    // Wait for delay to maintain request rate
    if (delayMs > 0) {
      await this.sleep(delayMs);
    }

    const startTime = performance.now();
    const requestId = `req-${Date.now()}-${requestIndex}`;

    try {
      const response = await axios.post(
        `${this.config.gatewayUrl}/api/v1/routes/decide`,
        {
          version: '1',
          tenant_id: this.config.tenantId,
          request_id: requestId,
          task: {
            type: 'text.generate',
            payload: {}
          }
        },
        {
          headers: {
            'Content-Type': 'application/json',
            'X-Tenant-ID': this.config.tenantId
          },
          validateStatus: () => true // Accept all status codes
        }
      );

      const endTime = performance.now();
      const rateLimited = response.status === 429;

      this.metrics.push({
        startTime,
        endTime,
        duration: endTime - startTime,
        success: response.status < 400,
        statusCode: response.status,
        rateLimited
      });

    } catch (error) {
      const endTime = performance.now();
      const rateLimited = error instanceof AxiosError && error.response?.status === 429;

      this.metrics.push({
        startTime,
        endTime,
        duration: endTime - startTime,
        success: false,
        statusCode: error instanceof AxiosError ? error.response?.status : undefined,
        error: error instanceof Error ? error.message : 'Unknown error',
        rateLimited
      });
    }
  }

  private async collectMetrics(): Promise<any> {
    const metrics: any = {};

    // Collect Router metrics
    if (this.config.routerMetricsUrl) {
      try {
        const response = await axios.get(this.config.routerMetricsUrl);
        metrics.router = this.parsePrometheusMetrics(response.data);
      } catch (error) {
        console.warn('Failed to collect Router metrics:', error);
      }
    }

    // Collect Gateway metrics
    if (this.config.gatewayMetricsUrl) {
      try {
        const response = await axios.get(this.config.gatewayMetricsUrl);
        metrics.gateway = this.parsePrometheusMetrics(response.data);
      } catch (error) {
        console.warn('Failed to collect Gateway metrics:', error);
      }
    }

    return metrics;
  }

  private parsePrometheusMetrics(metricsText: string): any {
    // Simple Prometheus metrics parser (extract counter values)
    const metrics: any = {};
    const lines = metricsText.split('\n');
    
    for (const line of lines) {
      // Match Prometheus metric format: metric_name{labels} value
      const match = line.match(/^([a-z_]+)\{([^}]*)\}\s+(\d+)/);
      if (match) {
        const [, name, labels, value] = match;
        metrics[name] = metrics[name] || {};
        metrics[name][labels] = parseInt(value, 10);
      }
    }

    return metrics;
  }

  private generateResults(): LoadTestResult {
    const totalRequests = this.metrics.length;
    const allowedRequests = this.metrics.filter(m => m.success && !m.rateLimited).length;
    const rejectedRequests = this.metrics.filter(m => m.rateLimited).length;
    const successRate = totalRequests > 0 ? allowedRequests / totalRequests : 0;

    // Calculate latency percentiles
    const latencies = this.metrics
      .map(m => m.duration)
      .sort((a, b) => a - b);

    const p50 = latencies[Math.floor(latencies.length * 0.5)] || 0;
    const p95 = latencies[Math.floor(latencies.length * 0.95)] || 0;
    const p99 = latencies[Math.floor(latencies.length * 0.99)] || 0;

    // Calculate metrics differences
    const metrics: any = {};
    if (this.metricsBefore?.router && this.metricsAfter?.router) {
      metrics.routerAllowed = this.calculateMetricDiff(
        this.metricsBefore.router,
        this.metricsAfter.router,
        'router_rate_limit_allowed_total'
      );
      metrics.routerExceeded = this.calculateMetricDiff(
        this.metricsBefore.router,
        this.metricsAfter.router,
        'router_rate_limit_exceeded_total'
      );
    }

    if (this.metricsBefore?.gateway && this.metricsAfter?.gateway) {
      metrics.gatewayAllowed = this.calculateMetricDiff(
        this.metricsBefore.gateway,
        this.metricsAfter.gateway,
        'gateway_rate_limit_allowed_total'
      );
      metrics.gatewayExceeded = this.calculateMetricDiff(
        this.metricsBefore.gateway,
        this.metricsAfter.gateway,
        'gateway_rate_limit_exceeded_total'
      );
    }

    return {
      scenario: this.config.scenario,
      config: this.config,
      totalRequests,
      allowedRequests,
      rejectedRequests,
      successRate,
      latency: { p50, p95, p99 },
      metrics
    };
  }

  private calculateMetricDiff(before: any, after: any, metricName: string): number {
    // Sum all label combinations
    let beforeSum = 0;
    let afterSum = 0;

    if (before[metricName]) {
      beforeSum = Object.values(before[metricName]).reduce((sum: number, val: any) => sum + val, 0);
    }
    if (after[metricName]) {
      afterSum = Object.values(after[metricName]).reduce((sum: number, val: any) => sum + val, 0);
    }

    return afterSum - beforeSum;
  }

  private sleep(ms: number): Promise<void> {
    return new Promise(resolve => setTimeout(resolve, ms));
  }
}

// Scenario implementations

async function scenario1_SingleTenantUnderLimit(): Promise<LoadTestResult> {
  const tester = new RateLimitingLoadTester({
    scenario: 'scenario1_single_tenant_under_limit',
    gatewayUrl: process.env.GATEWAY_URL || 'http://localhost:8080',
    routerMetricsUrl: process.env.ROUTER_METRICS_URL || 'http://localhost:9001/metrics',
    gatewayMetricsUrl: process.env.GATEWAY_METRICS_URL || 'http://localhost:8080/_metrics',
    tenantId: `test-tenant-${Date.now()}`,
    policyId: 'default',
    requestRate: 50, // Under limit (limit: 100 req/s)
    duration: 60,
    rateLimitRPS: 100,
    rateLimitBurst: 50
  });

  return await tester.runLoadTest();
}

async function scenario2_SingleTenantAtLimit(): Promise<LoadTestResult> {
  const tester = new RateLimitingLoadTester({
    scenario: 'scenario2_single_tenant_at_limit',
    gatewayUrl: process.env.GATEWAY_URL || 'http://localhost:8080',
    routerMetricsUrl: process.env.ROUTER_METRICS_URL || 'http://localhost:9001/metrics',
    gatewayMetricsUrl: process.env.GATEWAY_METRICS_URL || 'http://localhost:8080/_metrics',
    tenantId: `test-tenant-${Date.now()}`,
    policyId: 'default',
    requestRate: 100, // At limit (limit: 100 req/s)
    duration: 60,
    rateLimitRPS: 100,
    rateLimitBurst: 50
  });

  return await tester.runLoadTest();
}

async function scenario3_SingleTenantOverLimit(): Promise<LoadTestResult> {
  const tester = new RateLimitingLoadTester({
    scenario: 'scenario3_single_tenant_over_limit',
    gatewayUrl: process.env.GATEWAY_URL || 'http://localhost:8080',
    routerMetricsUrl: process.env.ROUTER_METRICS_URL || 'http://localhost:9001/metrics',
    gatewayMetricsUrl: process.env.GATEWAY_METRICS_URL || 'http://localhost:8080/_metrics',
    tenantId: `test-tenant-${Date.now()}`,
    policyId: 'default',
    requestRate: 200, // Over limit (2x limit: 100 req/s)
    duration: 60,
    rateLimitRPS: 100,
    rateLimitBurst: 50
  });

  return await tester.runLoadTest();
}

async function scenario4_MultipleTenants(): Promise<LoadTestResult> {
  // Note: This scenario requires multiple tenants
  // For simplicity, we test with one tenant but document the multi-tenant requirement
  const tester = new RateLimitingLoadTester({
    scenario: 'scenario4_multiple_tenants',
    gatewayUrl: process.env.GATEWAY_URL || 'http://localhost:8080',
    routerMetricsUrl: process.env.ROUTER_METRICS_URL || 'http://localhost:9001/metrics',
    gatewayMetricsUrl: process.env.GATEWAY_METRICS_URL || 'http://localhost:8080/_metrics',
    tenantId: `test-tenant-${Date.now()}`,
    policyId: 'default',
    requestRate: 50, // Per tenant (10 tenants × 50 req/s = 500 req/s total)
    duration: 60,
    rateLimitRPS: 100,
    rateLimitBurst: 50
  });

  return await tester.runLoadTest();
}

async function scenario5_TrafficSpikes(): Promise<LoadTestResult> {
  // Note: This scenario requires variable request rate
  // For skeleton, we use constant rate but document the spike requirement
  const tester = new RateLimitingLoadTester({
    scenario: 'scenario5_traffic_spikes',
    gatewayUrl: process.env.GATEWAY_URL || 'http://localhost:8080',
    routerMetricsUrl: process.env.ROUTER_METRICS_URL || 'http://localhost:9001/metrics',
    gatewayMetricsUrl: process.env.GATEWAY_METRICS_URL || 'http://localhost:8080/_metrics',
    tenantId: `test-tenant-${Date.now()}`,
    policyId: 'default',
    requestRate: 200, // Spike rate (4x baseline: 50 req/s)
    duration: 5, // Short duration for spike
    rateLimitRPS: 100,
    rateLimitBurst: 50
  });

  return await tester.runLoadTest();
}

async function scenario6_LongRunningExceed(): Promise<LoadTestResult> {
  const tester = new RateLimitingLoadTester({
    scenario: 'scenario6_long_running_exceed',
    gatewayUrl: process.env.GATEWAY_URL || 'http://localhost:8080',
    routerMetricsUrl: process.env.ROUTER_METRICS_URL || 'http://localhost:9001/metrics',
    gatewayMetricsUrl: process.env.GATEWAY_METRICS_URL || 'http://localhost:8080/_metrics',
    tenantId: `test-tenant-${Date.now()}`,
    policyId: 'default',
    requestRate: 200, // Over limit (2x limit: 100 req/s)
    duration: 300, // 5 minutes
    rateLimitRPS: 100,
    rateLimitBurst: 50
  });

  return await tester.runLoadTest();
}

async function scenario7_GatewayRouterCombined(): Promise<LoadTestResult> {
  const tester = new RateLimitingLoadTester({
    scenario: 'scenario7_gateway_router_combined',
    gatewayUrl: process.env.GATEWAY_URL || 'http://localhost:8080',
    routerMetricsUrl: process.env.ROUTER_METRICS_URL || 'http://localhost:9001/metrics',
    gatewayMetricsUrl: process.env.GATEWAY_METRICS_URL || 'http://localhost:8080/_metrics',
    tenantId: `test-tenant-${Date.now()}`,
    policyId: 'default',
    requestRate: 150, // Over Router limit (100 req/s), under Gateway limit (1000 req/min)
    duration: 60,
    rateLimitRPS: 100,
    rateLimitBurst: 50,
    gatewayRateLimit: 1000 // requests per minute
  });

  return await tester.runLoadTest();
}

// Main execution

async function main() {
  const scenario = process.argv[2] || 'all';

  console.log('📋 Rate Limiting Load Test');
  console.log(`📊 Scenario: ${scenario}`);
  console.log('');

  const results: LoadTestResult[] = [];

  try {
    if (scenario === 'all' || scenario === 'scenario1') {
      console.log('Running Scenario 1: Single Tenant, Under Limit');
      results.push(await scenario1_SingleTenantUnderLimit());
    }

    if (scenario === 'all' || scenario === 'scenario2') {
      console.log('Running Scenario 2: Single Tenant, At Limit');
      results.push(await scenario2_SingleTenantAtLimit());
    }

    if (scenario === 'all' || scenario === 'scenario3') {
      console.log('Running Scenario 3: Single Tenant, Over Limit');
      results.push(await scenario3_SingleTenantOverLimit());
    }

    if (scenario === 'all' || scenario === 'scenario4') {
      console.log('Running Scenario 4: Multiple Tenants');
      results.push(await scenario4_MultipleTenants());
    }

    if (scenario === 'all' || scenario === 'scenario5') {
      console.log('Running Scenario 5: Traffic Spikes');
      results.push(await scenario5_TrafficSpikes());
    }

    if (scenario === 'all' || scenario === 'scenario6') {
      console.log('Running Scenario 6: Long-Running Exceed');
      results.push(await scenario6_LongRunningExceed());
    }

    if (scenario === 'all' || scenario === 'scenario7') {
      console.log('Running Scenario 7: Gateway + Router Combined');
      results.push(await scenario7_GatewayRouterCombined());
    }

    // Save results
    const resultsDir = path.join(process.cwd(), 'reports', 'load-tests', 'rate-limiting');
    if (!fs.existsSync(resultsDir)) {
      fs.mkdirSync(resultsDir, { recursive: true });
    }

    const resultsFile = path.join(resultsDir, `rate-limiting-load-test-results-${Date.now()}.json`);
    fs.writeFileSync(resultsFile, JSON.stringify(results, null, 2));

    console.log('');
    console.log('✅ Load test completed');
    console.log(`📄 Results saved to: ${resultsFile}`);

    // Print summary
    console.log('');
    console.log('📊 Summary:');
    results.forEach(result => {
      console.log(`  ${result.scenario}:`);
      console.log(`    Total Requests: ${result.totalRequests}`);
      console.log(`    Allowed: ${result.allowedRequests}, Rejected: ${result.rejectedRequests}`);
      console.log(`    Success Rate: ${(result.successRate * 100).toFixed(2)}%`);
      console.log(`    Latency: P50=${result.latency.p50.toFixed(2)}ms, P95=${result.latency.p95.toFixed(2)}ms, P99=${result.latency.p99.toFixed(2)}ms`);
    });

  } catch (error) {
    console.error('❌ Load test failed:', error);
    process.exit(1);
  }
}

// Run if executed directly
if (require.main === module) {
  main().catch(console.error);
}

export {
  RateLimitingLoadTester,
  scenario1_SingleTenantUnderLimit,
  scenario2_SingleTenantAtLimit,
  scenario3_SingleTenantOverLimit,
  scenario4_MultipleTenants,
  scenario5_TrafficSpikes,
  scenario6_LongRunningExceed,
  scenario7_GatewayRouterCombined
};

