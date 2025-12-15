import { LoadTester, LoadTestConfig, runCP4LoadTests } from './load-test';
import { performance } from 'perf_hooks';

interface ThroughputTestConfig {
  baseUrl: string;
  targetRPS: number;
  durationSeconds: number;
  requestType: 'chat' | 'embedding' | 'image' | 'code';
}

interface ThroughputResult {
  achievedRPS: number;
  targetRPS: number;
  successRate: number;
  averageLatency: number;
  p95Latency: number;
  totalRequests: number;
  successfulRequests: number;
  failedRequests: number;
  durationSeconds: number;
}

class ThroughputTester {
  private config: ThroughputTestConfig;

  constructor(config: ThroughputTestConfig) {
    this.config = config;
  }

  async runThroughputTest(): Promise<ThroughputResult> {
    console.log(`🚀 Starting throughput test: ${this.config.targetRPS} RPS for ${this.config.durationSeconds}s`);
    console.log(`📊 Request type: ${this.config.requestType}`);
    console.log(`🎯 Target: ${this.config.targetRPS} RPS | Duration: ${this.config.durationSeconds}s`);
    
    const startTime = performance.now();
    const totalRequests = this.config.targetRPS * this.config.durationSeconds;
    
    // Create load test config for sustained throughput
    const loadTestConfig: LoadTestConfig = {
      baseUrl: this.config.baseUrl,
      concurrentRequests: Math.ceil(this.config.targetRPS / 10), // Adjust concurrency based on target
      totalRequests,
      requestType: this.config.requestType,
      timeoutMs: 30000,
      rampUpSeconds: Math.min(30, this.config.durationSeconds / 4) // Ramp up to 25% of duration
    };

    const tester = new LoadTester(loadTestConfig);
    const results = await tester.runLoadTest();
    
    const endTime = performance.now();
    const actualDuration = (endTime - startTime) / 1000;
    
    return {
      achievedRPS: results.throughputRPS,
      targetRPS: this.config.targetRPS,
      successRate: results.successfulRequests / results.totalRequests,
      averageLatency: results.averageLatency,
      p95Latency: results.p95Latency,
      totalRequests: results.totalRequests,
      successfulRequests: results.successfulRequests,
      failedRequests: results.failedRequests,
      durationSeconds: actualDuration
    };
  }
}

function printThroughputResults(result: ThroughputResult): void {
  console.log('\n📈 THROUGHPUT TEST RESULTS');
  console.log('═'.repeat(50));
  console.log(`Target RPS:        ${result.targetRPS}`);
  console.log(`Achieved RPS:      ${result.achievedRPS.toFixed(2)}`);
  console.log(`Success Rate:      ${(result.successRate * 100).toFixed(1)}%`);
  console.log(`Average Latency:   ${result.averageLatency.toFixed(2)}ms`);
  console.log(`P95 Latency:       ${result.p95Latency.toFixed(2)}ms`);
  console.log(`Total Requests:    ${result.totalRequests}`);
  console.log(`Successful:        ${result.successfulRequests}`);
  console.log(`Failed:            ${result.failedRequests}`);
  console.log(`Duration:          ${result.durationSeconds.toFixed(2)}s`);
  
  const performanceRatio = result.achievedRPS / result.targetRPS;
  const performanceStatus = performanceRatio >= 0.95 ? '✅ EXCELLENT' : 
                           performanceRatio >= 0.85 ? '✅ GOOD' :
                           performanceRatio >= 0.70 ? '⚠️  FAIR' : '❌ POOR';
  
  console.log(`\nPerformance: ${performanceStatus} (${(performanceRatio * 100).toFixed(1)}% of target)`);
  
  if (result.successRate < 0.95) {
    console.log(`⚠️  Warning: Success rate below 95% threshold`);
  }
  
  if (result.p95Latency > 1000) {
    console.log(`⚠️  Warning: P95 latency exceeds 1000ms threshold`);
  }
  
  console.log('═'.repeat(50));
}

async function runThroughputValidation(): Promise<void> {
  console.log('🎯 Starting CP4-LC Throughput Validation Suite\n');
  
  const baseUrl = process.env.GATEWAY_URL || 'http://localhost:3000';
  
  // CP4-LC throughput requirements validation
  const testConfigs: ThroughputTestConfig[] = [
    {
      baseUrl,
      targetRPS: 50,
      durationSeconds: 60,
      requestType: 'chat'
    },
    {
      baseUrl,
      targetRPS: 100,
      durationSeconds: 120,
      requestType: 'chat'
    },
    {
      baseUrl,
      targetRPS: 150,
      durationSeconds: 90,
      requestType: 'embedding'
    }
  ];
  
  const results: ThroughputResult[] = [];
  
  for (const config of testConfigs) {
    console.log(`\n🔥 Running throughput test: ${config.targetRPS} RPS`);
    console.log('─'.repeat(40));
    
    const tester = new ThroughputTester(config);
    const result = await tester.runThroughputTest();
    
    printThroughputResults(result);
    results.push(result);
    
    // Brief pause between tests
    console.log('\n⏳ Pausing 15 seconds before next test...\n');
    await new Promise(resolve => setTimeout(resolve, 15000));
  }
  
  // Generate summary
  generateThroughputSummary(results);
}

function generateThroughputSummary(results: ThroughputResult[]): void {
  console.log('\n📊 THROUGHPUT VALIDATION SUMMARY');
  console.log('═'.repeat(60));
  
  const totalTests = results.length;
  const passedTests = results.filter(r => 
    r.achievedRPS >= r.targetRPS * 0.95 && 
    r.successRate >= 0.95 && 
    r.p95Latency <= 1000
  ).length;
  
  console.log(`Total Tests: ${totalTests}`);
  console.log(`Passed Tests: ${passedTests}`);
  console.log(`Success Rate: ${(passedTests / totalTests * 100).toFixed(1)}%`);
  
  const avgThroughput = results.reduce((sum, r) => sum + r.achievedRPS, 0) / results.length;
  const maxThroughput = Math.max(...results.map(r => r.achievedRPS));
  const avgLatency = results.reduce((sum, r) => sum + r.p95Latency, 0) / results.length;
  
  console.log(`\nAverage Throughput: ${avgThroughput.toFixed(2)} RPS`);
  console.log(`Maximum Throughput: ${maxThroughput.toFixed(2)} RPS`);
  console.log(`Average P95 Latency: ${avgLatency.toFixed(2)}ms`);
  
  console.log(`\n🎯 CP4-LC Throughput Validation: ${passedTests === totalTests ? '✅ PASSED' : '❌ FAILED'}`);
  
  if (passedTests === totalTests) {
    console.log('\n✨ All throughput tests passed CP4-LC requirements!');
    console.log('System is ready for high-load production scenarios.');
  } else {
    console.log('\n⚠️  Some throughput tests failed CP4-LC requirements.');
    console.log('Review performance bottlenecks before advancing to CP5-LC.');
  }
  
  console.log('═'.repeat(60));
}

// CLI execution
if (require.main === module) {
  runThroughputValidation().catch(error => {
    console.error('Throughput test failed:', error);
    process.exit(1);
  });
}

export { ThroughputTester, ThroughputTestConfig, ThroughputResult, runThroughputValidation };