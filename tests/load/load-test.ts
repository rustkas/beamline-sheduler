import axios from 'axios';
import { v4 as uuidv4 } from 'uuid';
import { performance } from 'perf_hooks';

interface LoadTestConfig {
  baseUrl: string;
  concurrentRequests: number;
  totalRequests: number;
  requestType: 'chat' | 'embedding' | 'image' | 'code';
  timeoutMs: number;
  rampUpSeconds: number;
}

interface LoadTestResult {
  totalRequests: number;
  successfulRequests: number;
  failedRequests: number;
  averageLatency: number;
  p50Latency: number;
  p95Latency: number;
  p99Latency: number;
  throughputRPS: number;
  durationSeconds: number;
  errors: string[];
}

interface RequestMetrics {
  startTime: number;
  endTime: number;
  duration: number;
  success: boolean;
  error?: string;
  statusCode?: number;
}

class LoadTester {
  private config: LoadTestConfig;
  private metrics: RequestMetrics[] = [];
  private activeRequests = 0;
  private startTime = 0;
  private endTime = 0;

  constructor(config: LoadTestConfig) {
    this.config = config;
  }

  async runLoadTest(): Promise<LoadTestResult> {
    console.log(`🚀 Starting load test with ${this.config.totalRequests} requests, ${this.config.concurrentRequests} concurrent`);
    console.log(`📊 Request type: ${this.config.requestType}`);
    console.log(`⏱️  Timeout: ${this.config.timeoutMs}ms, Ramp-up: ${this.config.rampUpSeconds}s`);

    this.startTime = performance.now();
    this.metrics = [];
    this.activeRequests = 0;

    // Create request queue
    const requestQueue = Array.from({ length: this.config.totalRequests }, (_, i) => i);
    
    // Process requests with concurrency control
    const workers = Array.from({ length: this.config.concurrentRequests }, () => 
      this.processWorker(requestQueue)
    );

    await Promise.all(workers);
    
    this.endTime = performance.now();
    
    return this.generateResults();
  }

  private async processWorker(queue: number[]): Promise<void> {
    while (queue.length > 0) {
      const requestIndex = queue.shift();
      if (requestIndex === undefined) break;

      // Ramp-up delay
      if (this.config.rampUpSeconds > 0) {
        const delay = (requestIndex / this.config.totalRequests) * this.config.rampUpSeconds * 1000;
        await this.sleep(delay);
      }

      await this.executeRequest(requestIndex);
    }
  }

  private async executeRequest(requestIndex: number): Promise<void> {
    const startTime = performance.now();
    const requestId = uuidv4();
    
    try {
      this.activeRequests++;
      
      const response = await this.sendRequest(requestId);
      const endTime = performance.now();
      
      this.metrics.push({
        startTime,
        endTime,
        duration: endTime - startTime,
        success: true,
        statusCode: response.status
      });

      console.log(`✅ Request ${requestIndex + 1}/${this.config.totalRequests} completed in ${(endTime - startTime).toFixed(2)}ms`);
      
    } catch (error) {
      const endTime = performance.now();
      const errorMessage = error instanceof Error ? error.message : 'Unknown error';
      
      this.metrics.push({
        startTime,
        endTime,
        duration: endTime - startTime,
        success: false,
        error: errorMessage
      });

      console.log(`❌ Request ${requestIndex + 1}/${this.config.totalRequests} failed: ${errorMessage}`);
      
    } finally {
      this.activeRequests--;
    }
  }

  private async sendRequest(requestId: string): Promise<any> {
    const url = `${this.config.baseUrl}/v1/chat/completions`;
    
    const requestData = this.generateRequestData(requestId);
    
    return await axios.post(url, requestData, {
      timeout: this.config.timeoutMs,
      headers: {
        'Content-Type': 'application/json',
        'X-Request-ID': requestId,
        'X-Load-Test': 'true'
      }
    });
  }

  private generateRequestData(requestId: string): any {
    const baseData = {
      model: 'gpt-3.5-turbo',
      stream: false,
      metadata: {
        requestId,
        testType: 'load',
        timestamp: new Date().toISOString()
      }
    };

    switch (this.config.requestType) {
      case 'chat':
        return {
          ...baseData,
          messages: [
            {
              role: 'user',
              content: `Load test request ${requestId}. Generate a short response about AI orchestration.`
            }
          ],
          max_tokens: 150,
          temperature: 0.7
        };

      case 'embedding':
        return {
          ...baseData,
          input: `Load test embedding text ${requestId}`,
          model: 'text-embedding-ada-002'
        };

      case 'image':
        return {
          ...baseData,
          prompt: `Load test image generation ${requestId}`,
          model: 'dall-e-3',
          size: '256x256',
          n: 1
        };

      case 'code':
        return {
          ...baseData,
          messages: [
            {
              role: 'user',
              content: `Write a simple function in Python that returns the factorial of a number. Request ID: ${requestId}`
            }
          ],
          max_tokens: 200,
          temperature: 0.1
        };

      default:
        throw new Error(`Unsupported request type: ${this.config.requestType}`);
    }
  }

  private generateResults(): LoadTestResult {
    const successfulMetrics = this.metrics.filter(m => m.success);
    const failedMetrics = this.metrics.filter(m => !m.success);
    
    const latencies = successfulMetrics.map(m => m.duration);
    latencies.sort((a, b) => a - b);
    
    const totalDuration = (this.endTime - this.startTime) / 1000; // seconds
    const throughput = this.config.totalRequests / totalDuration;
    
    return {
      totalRequests: this.config.totalRequests,
      successfulRequests: successfulMetrics.length,
      failedRequests: failedMetrics.length,
      averageLatency: latencies.length > 0 ? latencies.reduce((a, b) => a + b, 0) / latencies.length : 0,
      p50Latency: this.percentile(latencies, 0.5),
      p95Latency: this.percentile(latencies, 0.95),
      p99Latency: this.percentile(latencies, 0.99),
      throughputRPS: throughput,
      durationSeconds: totalDuration,
      errors: failedMetrics.map(m => m.error || 'Unknown error')
    };
  }

  private percentile(sortedArray: number[], p: number): number {
    if (sortedArray.length === 0) return 0;
    const index = Math.ceil(sortedArray.length * p) - 1;
    return sortedArray[Math.max(0, index)];
  }

  private sleep(ms: number): Promise<void> {
    return new Promise(resolve => setTimeout(resolve, ms));
  }
}

class LoadTestReporter {
  static printResults(results: LoadTestResult): void {
    console.log('\n📊 LOAD TEST RESULTS');
    console.log('═'.repeat(50));
    console.log(`Total Requests:     ${results.totalRequests}`);
    console.log(`Successful:       ${results.successfulRequests} (${((results.successfulRequests / results.totalRequests) * 100).toFixed(1)}%)`);
    console.log(`Failed:           ${results.failedRequests} (${((results.failedRequests / results.totalRequests) * 100).toFixed(1)}%)`);
    console.log(`Duration:         ${results.durationSeconds.toFixed(2)}s`);
    console.log(`Throughput:       ${results.throughputRPS.toFixed(2)} RPS`);
    console.log('');
    console.log('Latency Statistics:');
    console.log(`Average:          ${results.averageLatency.toFixed(2)}ms`);
    console.log(`P50:              ${results.p50Latency.toFixed(2)}ms`);
    console.log(`P95:              ${results.p95Latency.toFixed(2)}ms`);
    console.log(`P99:              ${results.p99Latency.toFixed(2)}ms`);
    
    if (results.errors.length > 0) {
      console.log('\n❌ Errors:');
      const errorCounts = results.errors.reduce((acc, error) => {
        acc[error] = (acc[error] || 0) + 1;
        return acc;
      }, {} as Record<string, number>);
      
      Object.entries(errorCounts).forEach(([error, count]) => {
        console.log(`  ${error}: ${count}`);
      });
    }
    
    console.log('═'.repeat(50));
  }

  static saveResults(results: LoadTestResult, filename: string): void {
    const fs = require('fs');
    const data = {
      timestamp: new Date().toISOString(),
      results,
      config: {
        cp4_lc_requirements: {
          min_throughput_rps: 100,
          max_p95_latency_ms: 1000,
          success_rate_threshold: 0.95
        }
      }
    };
    
    fs.writeFileSync(filename, JSON.stringify(data, null, 2));
    console.log(`\n💾 Results saved to ${filename}`);
  }
}

async function runCP4LoadTests(): Promise<void> {
  console.log('🎯 Starting CP4-LC Load Testing Suite\n');
  
  const baseUrl = process.env.GATEWAY_URL || 'http://localhost:3000';
  
  // Test configurations for CP4-LC validation
  const testConfigs: LoadTestConfig[] = [
    {
      baseUrl,
      concurrentRequests: 10,
      totalRequests: 100,
      requestType: 'chat',
      timeoutMs: 30000,
      rampUpSeconds: 5
    },
    {
      baseUrl,
      concurrentRequests: 20,
      totalRequests: 200,
      requestType: 'embedding',
      timeoutMs: 15000,
      rampUpSeconds: 10
    },
    {
      baseUrl,
      concurrentRequests: 5,
      totalRequests: 50,
      requestType: 'code',
      timeoutMs: 45000,
      rampUpSeconds: 3
    }
  ];
  
  const allResults: LoadTestResult[] = [];
  
  for (let i = 0; i < testConfigs.length; i++) {
    const config = testConfigs[i];
    console.log(`\n🔥 Test ${i + 1}/${testConfigs.length}: ${config.requestType.toUpperCase()}`);
    console.log('─'.repeat(40));
    
    const tester = new LoadTester(config);
    const results = await tester.runLoadTest();
    
    LoadTestReporter.printResults(results);
    allResults.push(results);
    
    // Validate CP4-LC requirements
    const cp4Validation = validateCP4Requirements(results);
    console.log(`\n✅ CP4-LC Validation: ${cp4Validation.passed ? 'PASSED' : 'FAILED'}`);
    if (!cp4Validation.passed) {
      console.log('Issues found:');
      cp4Validation.issues.forEach(issue => console.log(`  - ${issue}`));
    }
    
    // Save results
    const timestamp = new Date().toISOString().replace(/[:.]/g, '-');
    const filename = `load-test-${config.requestType}-${timestamp}.json`;
    LoadTestReporter.saveResults(results, filename);
    
    // Brief pause between tests
    if (i < testConfigs.length - 1) {
      console.log('\n⏳ Pausing 10 seconds before next test...\n');
      await new Promise(resolve => setTimeout(resolve, 10000));
    }
  }
  
  // Summary report
  generateSummaryReport(allResults);
}

function validateCP4Requirements(results: LoadTestResult): { passed: boolean; issues: string[] } {
  const issues: string[] = [];
  
  // CP4-LC requirements
  const minThroughput = 100; // RPS
  const maxP95Latency = 1000; // ms
  const minSuccessRate = 0.95; // 95%
  
  if (results.throughputRPS < minThroughput) {
    issues.push(`Throughput ${results.throughputRPS.toFixed(2)} RPS below minimum ${minThroughput} RPS`);
  }
  
  if (results.p95Latency > maxP95Latency) {
    issues.push(`P95 latency ${results.p95Latency.toFixed(2)}ms exceeds maximum ${maxP95Latency}ms`);
  }
  
  const successRate = results.successfulRequests / results.totalRequests;
  if (successRate < minSuccessRate) {
    issues.push(`Success rate ${(successRate * 100).toFixed(1)}% below minimum ${(minSuccessRate * 100).toFixed(0)}%`);
  }
  
  return {
    passed: issues.length === 0,
    issues
  };
}

function generateSummaryReport(results: LoadTestResult[]): void {
  console.log('\n📋 CP4-LC LOAD TEST SUMMARY REPORT');
  console.log('═'.repeat(60));
  
  const overallThroughput = results.reduce((sum, r) => sum + r.throughputRPS, 0) / results.length;
  const overallSuccessRate = results.reduce((sum, r) => sum + (r.successfulRequests / r.totalRequests), 0) / results.length;
  const avgP95Latency = results.reduce((sum, r) => sum + r.p95Latency, 0) / results.length;
  
  console.log(`Overall Average Throughput: ${overallThroughput.toFixed(2)} RPS`);
  console.log(`Overall Success Rate:       ${(overallSuccessRate * 100).toFixed(1)}%`);
  console.log(`Average P95 Latency:        ${avgP95Latency.toFixed(2)}ms`);
  
  const allTestsPassed = results.every((result, index) => {
    const validation = validateCP4Requirements(result);
    return validation.passed;
  });
  
  console.log(`\n🎯 CP4-LC Validation: ${allTestsPassed ? '✅ ALL TESTS PASSED' : '❌ SOME TESTS FAILED'}`);
  
  if (allTestsPassed) {
    console.log('\n✨ The system meets CP4-LC throughput and latency requirements!');
    console.log('Ready to advance to CP5-LC: Governance and Policies.');
  } else {
    console.log('\n⚠️  Some tests failed CP4-LC requirements. Review results above.');
  }
  
  console.log('═'.repeat(60));
}

// CLI execution
if (require.main === module) {
  runCP4LoadTests().catch(error => {
    console.error('Load test failed:', error);
    process.exit(1);
  });
}

export { LoadTester, LoadTestConfig, LoadTestResult, runCP4LoadTests };