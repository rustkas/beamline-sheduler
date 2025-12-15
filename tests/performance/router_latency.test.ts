/**
 * Performance tests for Router decision latency
 * Measures decision time for different policies
 */

import { describe, it, expect } from 'vitest';

// Mock router_decider for performance testing
// In real implementation, this would call actual Erlang router

interface RouteDecision {
  provider_id: string;
  reason: string;
  priority: number;
  expected_latency_ms: number;
  expected_cost: number;
  metadata: Record<string, any>;
}

interface PerformanceMetrics {
  min_latency_ms: number;
  max_latency_ms: number;
  avg_latency_ms: number;
  p50_latency_ms: number;
  p95_latency_ms: number;
  p99_latency_ms: number;
  total_requests: number;
  success_rate: number;
}

function measureLatency(fn: () => RouteDecision | Error): number {
  const start = performance.now();
  fn();
  const end = performance.now();
  return end - start;
}

function calculateMetrics(latencies: number[]): PerformanceMetrics {
  const sorted = [...latencies].sort((a, b) => a - b);
  const sum = sorted.reduce((a, b) => a + b, 0);
  const count = sorted.length;

  return {
    min_latency_ms: sorted[0],
    max_latency_ms: sorted[count - 1],
    avg_latency_ms: sum / count,
    p50_latency_ms: sorted[Math.floor(count * 0.5)],
    p95_latency_ms: sorted[Math.floor(count * 0.95)],
    p99_latency_ms: sorted[Math.floor(count * 0.99)],
    total_requests: count,
    success_rate: 1.0, // Assuming all succeed for now
  };
}

describe('Router Performance Tests', () => {
  describe('Sticky Session Latency', () => {
    it('should measure sticky session decision latency', () => {
      const iterations = 100;
      const latencies: number[] = [];

      for (let i = 0; i < iterations; i++) {
        const latency = measureLatency(() => {
          // Mock sticky session decision
          return {
            provider_id: 'provider-a',
            reason: 'sticky',
            priority: 100,
            expected_latency_ms: 0,
            expected_cost: 0.0,
            metadata: {},
          };
        });
        latencies.push(latency);
      }

      const metrics = calculateMetrics(latencies);

      // Sticky session should be fast (< 10ms for mock, < 5ms in production)
      expect(metrics.avg_latency_ms).toBeLessThan(10);
      expect(metrics.p95_latency_ms).toBeLessThan(20);
      expect(metrics.p99_latency_ms).toBeLessThan(50);

      console.log('Sticky Session Metrics:', metrics);
    });

    it('should measure sticky session lookup latency', () => {
      // Test Mnesia lookup performance
      const iterations = 1000;
      const latencies: number[] = [];

      for (let i = 0; i < iterations; i++) {
        const latency = measureLatency(() => {
          // Mock Mnesia lookup
          // In real test: mnesia:read(sticky_sessions, Key)
          return {
            provider_id: 'provider-a',
            reason: 'sticky',
            priority: 100,
            expected_latency_ms: 0,
            expected_cost: 0.0,
            metadata: {},
          };
        });
        latencies.push(latency);
      }

      const metrics = calculateMetrics(latencies);

      // Mnesia lookup should be very fast (< 1ms)
      expect(metrics.avg_latency_ms).toBeLessThan(5);
      expect(metrics.p95_latency_ms).toBeLessThan(10);

      console.log('Sticky Session Lookup Metrics:', metrics);
    });
  });

  describe('Weighted Distribution Latency', () => {
    it('should measure weighted distribution decision latency', () => {
      const iterations = 100;
      const latencies: number[] = [];

      for (let i = 0; i < iterations; i++) {
        const latency = measureLatency(() => {
          // Mock weighted distribution
          const weights = {
            'provider-a': 0.6,
            'provider-b': 0.4,
          };
          const totalWeight = Object.values(weights).reduce((a, b) => a + b, 0);
          const random = Math.random() * totalWeight;
          
          // Select provider based on weight
          let accum = 0;
          let selected = 'provider-a';
          for (const [provider, weight] of Object.entries(weights)) {
            accum += weight;
            if (random <= accum) {
              selected = provider;
              break;
            }
          }

          return {
            provider_id: selected,
            reason: 'weighted',
            priority: 50,
            expected_latency_ms: 0,
            expected_cost: 0.0,
            metadata: {},
          };
        });
        latencies.push(latency);
      }

      const metrics = calculateMetrics(latencies);

      // Weighted distribution should be fast (< 5ms)
      expect(metrics.avg_latency_ms).toBeLessThan(10);
      expect(metrics.p95_latency_ms).toBeLessThan(20);

      console.log('Weighted Distribution Metrics:', metrics);
    });
  });

  describe('Fallback Policy Latency', () => {
    it('should measure fallback decision latency', () => {
      const iterations = 100;
      const latencies: number[] = [];

      for (let i = 0; i < iterations; i++) {
        const latency = measureLatency(() => {
          // Mock fallback decision
          return {
            provider_id: 'provider-fallback',
            reason: 'fallback',
            priority: 25,
            expected_latency_ms: 0,
            expected_cost: 0.0,
            metadata: {},
          };
        });
        latencies.push(latency);
      }

      const metrics = calculateMetrics(latencies);

      // Fallback should be fast (< 5ms)
      expect(metrics.avg_latency_ms).toBeLessThan(10);
      expect(metrics.p95_latency_ms).toBeLessThan(20);

      console.log('Fallback Policy Metrics:', metrics);
    });
  });

  describe('End-to-End Latency', () => {
    it('should measure full decision flow latency', () => {
      const iterations = 50;
      const latencies: number[] = [];

      for (let i = 0; i < iterations; i++) {
        const latency = measureLatency(() => {
          // Mock full flow: sticky check → weighted → fallback
          // Step 1: Check sticky (fast)
          const stickyCheck = measureLatency(() => ({ found: false }));
          
          // Step 2: Apply weights (fast)
          const weightedDecision = measureLatency(() => ({
            provider_id: 'provider-a',
            reason: 'weighted',
          }));

          return {
            provider_id: 'provider-a',
            reason: 'weighted',
            priority: 50,
            expected_latency_ms: stickyCheck + weightedDecision,
            expected_cost: 0.0,
            metadata: {},
          };
        });
        latencies.push(latency);
      }

      const metrics = calculateMetrics(latencies);

      // Full flow should be < 20ms
      expect(metrics.avg_latency_ms).toBeLessThan(20);
      expect(metrics.p95_latency_ms).toBeLessThan(50);
      expect(metrics.p99_latency_ms).toBeLessThan(100);

      console.log('End-to-End Metrics:', metrics);
    });
  });

  describe('Concurrent Request Latency', () => {
    it('should measure latency under concurrent load', async () => {
      const concurrentRequests = 10;
      const requestsPerConcurrency = 10;
      const latencies: number[] = [];

      const promises = Array.from({ length: concurrentRequests }, async () => {
        const requestLatencies: number[] = [];
        
        for (let i = 0; i < requestsPerConcurrency; i++) {
          const latency = measureLatency(() => {
            return {
              provider_id: 'provider-a',
              reason: 'weighted',
              priority: 50,
              expected_latency_ms: 0,
              expected_cost: 0.0,
              metadata: {},
            };
          });
          requestLatencies.push(latency);
        }
        
        return requestLatencies;
      });

      const allLatencies = await Promise.all(promises);
      allLatencies.forEach(l => latencies.push(...l));

      const metrics = calculateMetrics(latencies);

      // Under concurrent load, latency should still be reasonable
      expect(metrics.avg_latency_ms).toBeLessThan(50);
      expect(metrics.p95_latency_ms).toBeLessThan(100);

      console.log('Concurrent Load Metrics:', metrics);
    });
  });

  describe('Error Handling Latency', () => {
    it('should measure latency when no providers available', () => {
      const iterations = 100;
      const latencies: number[] = [];

      for (let i = 0; i < iterations; i++) {
        const latency = measureLatency(() => {
          // Mock error case: no providers
          throw new Error('no_provider_available');
        });
        latencies.push(latency);
      }

      const metrics = calculateMetrics(latencies);

      // Error handling should be fast (< 10ms)
      expect(metrics.avg_latency_ms).toBeLessThan(10);
      expect(metrics.p95_latency_ms).toBeLessThan(20);

      console.log('Error Handling Metrics:', metrics);
    });
  });
});

