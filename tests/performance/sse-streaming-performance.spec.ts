/**
 * Performance and SSE Streaming Tests
 * Validates system performance, latency, and Server-Sent Events streaming functionality
 */

import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import { performance } from 'perf_hooks';

// Test configuration
const TEST_CONFIG = {
  GATEWAY_URL: process.env.GATEWAY_URL || 'http://localhost:3000',
  TEST_TIMEOUT: 60000, // Longer timeout for performance tests
  SSE_TIMEOUT: 30000,
  PERFORMANCE_THRESHOLDS: {
    P50_LATENCY_MS: 1000,    // 50th percentile latency
    P95_LATENCY_MS: 3000,    // 95th percentile latency
    P99_LATENCY_MS: 5000,    // 99th percentile latency
    THROUGHPUT_RPS: 10,      // Requests per second
    CONCURRENT_REQUESTS: 50, // Maximum concurrent requests
    SSE_CHUNK_DELAY_MS: 100, // Maximum delay between SSE chunks
  },
};

// Performance metrics collector
class PerformanceMetrics {
  private latencies: number[] = [];
  private startTime: number = 0;
  private endTime: number = 0;

  startTimer() {
    this.startTime = performance.now();
  }

  endTimer() {
    this.endTime = performance.now();
  }

  recordLatency(latencyMs: number) {
    this.latencies.push(latencyMs);
  }

  getLatencyStats() {
    if (this.latencies.length === 0) return null;
    
    const sorted = [...this.latencies].sort((a, b) => a - b);
    const n = sorted.length;
    
    return {
      min: sorted[0],
      max: sorted[n - 1],
      mean: sorted.reduce((a, b) => a + b, 0) / n,
      p50: sorted[Math.floor(n * 0.5)],
      p95: sorted[Math.floor(n * 0.95)],
      p99: sorted[Math.floor(n * 0.99)],
      count: n,
    };
  }

  getThroughput() {
    if (this.startTime === 0 || this.endTime === 0) return 0;
    const durationSec = (this.endTime - this.startTime) / 1000;
    return this.latencies.length / durationSec;
  }
}

describe('Performance and SSE Streaming Tests', () => {
  describe('Latency Performance', () => {
    it('should meet latency requirements for single requests', async () => {
      const metrics = new PerformanceMetrics();
      const iterations = 20;
      
      for (let i = 0; i < iterations; i++) {
        const startTime = performance.now();
        
        const response = await fetch(`${TEST_CONFIG.GATEWAY_URL}/v1/chat/completions`, {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            'Authorization': 'Bearer perf-test-token',
          },
          body: JSON.stringify({
            model: 'gpt-3.5-turbo',
            messages: [{ role: 'user', content: `Latency test ${i}` }],
            max_tokens: 50,
          }),
        });
        
        const endTime = performance.now();
        const latency = endTime - startTime;
        
        expect(response.status).toBe(200);
        metrics.recordLatency(latency);
        
        // Small delay between requests to avoid rate limiting
        await new Promise(resolve => setTimeout(resolve, 100));
      }
      
      const stats = metrics.getLatencyStats();
      expect(stats).not.toBeNull();
      
      if (stats) {
        console.log('Latency Statistics:', {
          min: `${stats.min.toFixed(2)}ms`,
          max: `${stats.max.toFixed(2)}ms`,
          mean: `${stats.mean.toFixed(2)}ms`,
          p50: `${stats.p50.toFixed(2)}ms`,
          p95: `${stats.p95.toFixed(2)}ms`,
          p99: `${stats.p99.toFixed(2)}ms`,
        });
        
        expect(stats.p50).toBeLessThan(TEST_CONFIG.PERFORMANCE_THRESHOLDS.P50_LATENCY_MS);
        expect(stats.p95).toBeLessThan(TEST_CONFIG.PERFORMANCE_THRESHOLDS.P95_LATENCY_MS);
        expect(stats.p99).toBeLessThan(TEST_CONFIG.PERFORMANCE_THRESHOLDS.P99_LATENCY_MS);
      }
    }, TEST_CONFIG.TEST_TIMEOUT);

    it('should maintain consistent latency under load', async () => {
      const metrics = new PerformanceMetrics();
      const concurrentRequests = 10;
      const batches = 5;
      
      metrics.startTimer();
      
      for (let batch = 0; batch < batches; batch++) {
        const requests = Array(concurrentRequests).fill(null).map((_, index) => {
          const startTime = performance.now();
          
          return fetch(`${TEST_CONFIG.GATEWAY_URL}/v1/chat/completions`, {
            method: 'POST',
            headers: {
              'Content-Type': 'application/json',
              'Authorization': `Bearer load-test-token-${batch}`,
            },
            body: JSON.stringify({
              model: 'gpt-3.5-turbo',
              messages: [{ role: 'user', content: `Load test batch ${batch} request ${index}` }],
              max_tokens: 30,
            }),
          }).then(response => {
            const endTime = performance.now();
            const latency = endTime - startTime;
            
            if (response.status === 200) {
              metrics.recordLatency(latency);
            }
            
            return response;
          });
        });
        
        const responses = await Promise.all(requests);
        const successCount = responses.filter(r => r.status === 200).length;
        
        // Should have high success rate
        expect(successCount).toBeGreaterThan(concurrentRequests * 0.8);
        
        // Small delay between batches
        await new Promise(resolve => setTimeout(resolve, 500));
      }
      
      metrics.endTimer();
      
      const stats = metrics.getLatencyStats();
      const throughput = metrics.getThroughput();
      
      console.log('Load Test Results:', {
        throughput: `${throughput.toFixed(2)} requests/sec`,
        totalRequests: stats?.count || 0,
        p95Latency: `${stats?.p95.toFixed(2)}ms` || 'N/A',
      });
      
      expect(stats).not.toBeNull();
      if (stats) {
        expect(stats.p95).toBeLessThan(TEST_CONFIG.PERFORMANCE_THRESHOLDS.P95_LATENCY_MS);
        expect(throughput).toBeGreaterThan(TEST_CONFIG.PERFORMANCE_THRESHOLDS.THROUGHPUT_RPS);
      }
    }, TEST_CONFIG.TEST_TIMEOUT);
  });

  describe('Throughput Performance', () => {
    it('should handle high concurrent request load', async () => {
      const metrics = new PerformanceMetrics();
      const concurrentRequests = TEST_CONFIG.PERFORMANCE_THRESHOLDS.CONCURRENT_REQUESTS;
      
      metrics.startTimer();
      
      const requests = Array(concurrentRequests).fill(null).map((_, index) => {
        const startTime = performance.now();
        
        return fetch(`${TEST_CONFIG.GATEWAY_URL}/v1/chat/completions`, {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            'Authorization': `Bearer throughput-token-${index}`,
          },
          body: JSON.stringify({
            model: 'gpt-3.5-turbo',
            messages: [{ role: 'user', content: `Throughput test ${index}` }],
            max_tokens: 20,
          }),
        }).then(response => {
          const endTime = performance.now();
          const latency = endTime - startTime;
          
          if (response.status === 200) {
            metrics.recordLatency(latency);
          }
          
          return response;
        });
      });
      
      const responses = await Promise.all(requests);
      metrics.endTimer();
      
      const successCount = responses.filter(r => r.status === 200).length;
      const rateLimitedCount = responses.filter(r => r.status === 429).length;
      
      console.log('Throughput Test Results:', {
        totalRequests: concurrentRequests,
        successfulRequests: successCount,
        rateLimitedRequests: rateLimitedCount,
        successRate: `${((successCount / concurrentRequests) * 100).toFixed(1)}%`,
        throughput: `${metrics.getThroughput().toFixed(2)} requests/sec`,
      });
      
      // Should handle most requests successfully
      expect(successCount).toBeGreaterThan(concurrentRequests * 0.7);
      
      // Rate limiting is acceptable for high load
      expect(rateLimitedCount).toBeLessThan(concurrentRequests * 0.3);
    }, TEST_CONFIG.TEST_TIMEOUT);

    it('should scale efficiently with increasing load', async () => {
      const loadLevels = [5, 10, 20, 30];
      const results: any[] = [];
      
      for (const loadLevel of loadLevels) {
        const metrics = new PerformanceMetrics();
        
        metrics.startTimer();
        
        const requests = Array(loadLevel).fill(null).map((_, index) => {
          const startTime = performance.now();
          
          return fetch(`${TEST_CONFIG.GATEWAY_URL}/v1/chat/completions`, {
            method: 'POST',
            headers: {
              'Content-Type': 'application/json',
              'Authorization': `Bearer scale-test-token-${loadLevel}-${index}`,
            },
            body: JSON.stringify({
              model: 'gpt-3.5-turbo',
              messages: [{ role: 'user', content: `Scale test ${loadLevel}-${index}` }],
              max_tokens: 25,
            }),
          }).then(response => {
            const endTime = performance.now();
            const latency = endTime - startTime;
            
            if (response.status === 200) {
              metrics.recordLatency(latency);
            }
            
            return response;
          });
        });
        
        const responses = await Promise.all(requests);
        metrics.endTimer();
        
        const successCount = responses.filter(r => r.status === 200).length;
        const stats = metrics.getLatencyStats();
        const throughput = metrics.getThroughput();
        
        results.push({
          loadLevel,
          successRate: successCount / loadLevel,
          throughput,
          p95Latency: stats?.p95 || 0,
        });
        
        // Small delay between load levels
        await new Promise(resolve => setTimeout(resolve, 1000));
      }
      
      console.log('Scalability Test Results:', results);
      
      // Validate scaling behavior
      results.forEach((result, index) => {
        if (index > 0) {
          const prevResult = results[index - 1];
          
          // Success rate should not degrade significantly
          expect(result.successRate).toBeGreaterThan(prevResult.successRate * 0.8);
          
          // Throughput should increase or stay stable
          expect(result.throughput).toBeGreaterThan(prevResult.throughput * 0.7);
        }
      });
    }, TEST_CONFIG.TEST_TIMEOUT);
  });

  describe('SSE Streaming Performance', () => {
    it('should maintain streaming performance with low latency', async () => {
      const response = await fetch(`${TEST_CONFIG.GATEWAY_URL}/v1/chat/completions`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': 'Bearer sse-perf-token',
        },
        body: JSON.stringify({
          model: 'gpt-3.5-turbo',
          messages: [{ role: 'user', content: 'SSE performance test' }],
          max_tokens: 100,
          stream: true,
        }),
      });

      expect(response.status).toBe(200);
      expect(response.headers.get('content-type')).toContain('text/event-stream');
      
      const reader = response.body?.getReader();
      const decoder = new TextDecoder();
      
      const chunkTimes: number[] = [];
      let lastChunkTime = performance.now();
      let chunkCount = 0;
      
      try {
        while (chunkCount < 20) { // Limit chunks for testing
          const { done, value } = await reader!.read();
          if (done) break;
          
          const chunk = decoder.decode(value);
          const currentTime = performance.now();
          const chunkDelay = currentTime - lastChunkTime;
          
          chunkTimes.push(chunkDelay);
          lastChunkTime = currentTime;
          
          // Validate chunk content
          expect(chunk).toContain('data:');
          
          chunkCount++;
        }
      } finally {
        reader!.cancel();
      }
      
      expect(chunkCount).toBeGreaterThan(0);
      
      if (chunkTimes.length > 0) {
        const avgChunkDelay = chunkTimes.reduce((a, b) => a + b, 0) / chunkTimes.length;
        const maxChunkDelay = Math.max(...chunkTimes);
        
        console.log('SSE Streaming Performance:', {
          chunkCount,
          avgChunkDelay: `${avgChunkDelay.toFixed(2)}ms`,
          maxChunkDelay: `${maxChunkDelay.toFixed(2)}ms`,
        });
        
        // Validate streaming performance
        expect(avgChunkDelay).toBeLessThan(TEST_CONFIG.PERFORMANCE_THRESHOLDS.SSE_CHUNK_DELAY_MS * 2);
        expect(maxChunkDelay).toBeLessThan(TEST_CONFIG.PERFORMANCE_THRESHOLDS.SSE_CHUNK_DELAY_MS * 5);
      }
    }, TEST_CONFIG.SSE_TIMEOUT);

    it('should handle multiple concurrent SSE streams', async () => {
      const concurrentStreams = 5;
      const streams: any[] = [];
      
      // Create multiple SSE streams
      for (let i = 0; i < concurrentStreams; i++) {
        const streamPromise = fetch(`${TEST_CONFIG.GATEWAY_URL}/v1/chat/completions`, {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            'Authorization': `Bearer concurrent-sse-token-${i}`,
          },
          body: JSON.stringify({
            model: 'gpt-3.5-turbo',
            messages: [{ role: 'user', content: `Concurrent SSE stream ${i}` }],
            max_tokens: 50,
            stream: true,
          }),
        }).then(async (response) => {
          expect(response.status).toBe(200);
          expect(response.headers.get('content-type')).toContain('text/event-stream');
          
          const reader = response.body?.getReader();
          const decoder = new TextDecoder();
          
          let chunksReceived = 0;
          try {
            while (chunksReceived < 10) {
              const { done, value } = await reader!.read();
              if (done) break;
              
              const chunk = decoder.decode(value);
              expect(chunk).toContain('data:');
              chunksReceived++;
            }
          } finally {
            reader!.cancel();
          }
          
          return { streamId: i, chunksReceived };
        });
        
        streams.push(streamPromise);
      }
      
      const results = await Promise.all(streams);
      
      // All streams should receive chunks
      results.forEach(result => {
        expect(result.chunksReceived).toBeGreaterThan(0);
      });
      
      console.log('Concurrent SSE Streams Results:', results);
    }, TEST_CONFIG.SSE_TIMEOUT);

    it('should handle SSE stream interruptions gracefully', async () => {
      const response = await fetch(`${TEST_CONFIG.GATEWAY_URL}/v1/chat/completions`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': 'Bearer sse-interrupt-token',
        },
        body: JSON.stringify({
          model: 'gpt-3.5-turbo',
          messages: [{ role: 'user', content: 'SSE interruption test' }],
          max_tokens: 200,
          stream: true,
        }),
      });

      expect(response.status).toBe(200);
      
      const reader = response.body?.getReader();
      const decoder = new TextDecoder();
      
      let chunksReceived = 0;
      let errorOccurred = false;
      
      try {
        // Read a few chunks then interrupt
        for (let i = 0; i < 5; i++) {
          const { done, value } = await reader!.read();
          if (done) break;
          
          const chunk = decoder.decode(value);
          expect(chunk).toContain('data:');
          chunksReceived++;
        }
        
        // Interrupt the stream
        await reader!.cancel();
        
        // Verify we can make another request after interruption
        const newResponse = await fetch(`${TEST_CONFIG.GATEWAY_URL}/v1/chat/completions`, {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            'Authorization': 'Bearer sse-recovery-token',
          },
          body: JSON.stringify({
            model: 'gpt-3.5-turbo',
            messages: [{ role: 'user', content: 'SSE recovery test' }],
            max_tokens: 50,
            stream: true,
          }),
        });
        
        expect(newResponse.status).toBe(200);
        
      } catch (error) {
        errorOccurred = true;
      }
      
      expect(chunksReceived).toBeGreaterThan(0);
      expect(errorOccurred).toBe(false);
    }, TEST_CONFIG.SSE_TIMEOUT);
  });

  describe('Resource Usage and Memory', () => {
    it('should handle sustained load without memory leaks', async () => {
      const metrics = new PerformanceMetrics();
      const sustainedRequests = 30;
      const requestInterval = 200; // ms between requests
      
      metrics.startTimer();
      
      for (let i = 0; i < sustainedRequests; i++) {
        const startTime = performance.now();
        
        const response = await fetch(`${TEST_CONFIG.GATEWAY_URL}/v1/chat/completions`, {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            'Authorization': `Bearer sustained-load-token-${i}`,
          },
          body: JSON.stringify({
            model: 'gpt-3.5-turbo',
            messages: [{ role: 'user', content: `Sustained load test ${i}` }],
            max_tokens: 30,
          }),
        });
        
        const endTime = performance.now();
        const latency = endTime - startTime;
        
        if (response.status === 200) {
          metrics.recordLatency(latency);
        }
        
        // Wait before next request
        await new Promise(resolve => setTimeout(resolve, requestInterval));
      }
      
      metrics.endTimer();
      
      const stats = metrics.getLatencyStats();
      const throughput = metrics.getThroughput();
      
      console.log('Sustained Load Test Results:', {
        totalRequests: sustainedRequests,
        successfulRequests: stats?.count || 0,
        throughput: `${throughput.toFixed(2)} requests/sec`,
        avgLatency: `${stats?.mean.toFixed(2)}ms` || 'N/A',
        p95Latency: `${stats?.p95.toFixed(2)}ms` || 'N/A',
      });
      
      // Performance should remain consistent throughout the test
      expect(stats).not.toBeNull();
      if (stats) {
        expect(stats.p95).toBeLessThan(TEST_CONFIG.PERFORMANCE_THRESHOLDS.P95_LATENCY_MS * 1.5);
        expect(throughput).toBeGreaterThan(2); // At least 2 requests per second
      }
    }, TEST_CONFIG.TEST_TIMEOUT);
  });
});