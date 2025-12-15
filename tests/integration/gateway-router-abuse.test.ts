/**
 * Gateway + Router Abuse Scenarios Integration Tests
 * 
 * Tests abuse detection and protection mechanisms:
 * - Empty payload flood (valid but empty requests)
 * - Targeted tenant attack (high volume on single tenant)
 * - Rate limit evasion (multiple API keys, IP rotation)
 * - Heavy payload attacks (large payload_ref / blobs)
 * - Multi-tenant flood (distributed attack)
 */

import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import axios, { AxiosInstance, AxiosError } from 'axios';

const BASE_URL = process.env.C_GATEWAY_URL || 'http://localhost:8080';
const ROUTER_URL = process.env.ROUTER_URL || 'http://localhost:9000';

function gateway(): AxiosInstance {
  return axios.create({
    baseURL: BASE_URL,
    timeout: 10000,
    validateStatus: () => true, // Don't throw on any status code
  });
}

function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

describe('Abuse Scenario 1: Empty Payload Flood', () => {
  const client = gateway();
  const tenantId = `tenant-abuse-empty-${Date.now()}`;

  it('should detect and reject empty payload requests', async () => {
    const requests = [];
    
    // Send 10 requests with empty payloads
    for (let i = 0; i < 10; i++) {
      requests.push(
        client.post(
          '/api/v1/routes/decide',
          {
            version: '1',
            tenant_id: tenantId,
            request_id: `req-empty-${i}`,
            task: {
              type: 'text.generate',
              payload: {},  // Empty payload
            },
          },
          {
            headers: {
              'X-Tenant-ID': tenantId,
            },
          },
        ),
      );
    }

    const responses = await Promise.all(requests);

    // All requests should be rejected (400 or 422)
    responses.forEach((res) => {
      expect([400, 422]).toContain(res.status);
      expect(res.data).toHaveProperty('error');
      expect(res.data.error).toMatch(/payload|empty|too.small/i);
    });
  });

  it('should log abuse events for empty payloads', async () => {
    // Send empty payload request
    const res = await client.post(
      '/api/v1/routes/decide',
      {
        version: '1',
        tenant_id: tenantId,
        request_id: 'req-empty-log',
        task: {
          type: 'text.generate',
          payload: {},  // Empty payload
        },
      },
      {
        headers: {
          'X-Tenant-ID': tenantId,
        },
      },
    );

    // Request should be rejected
    expect([400, 422]).toContain(res.status);

    // Check metrics endpoint for abuse event
    await sleep(1000); // Wait for metrics to be recorded
    const metricsRes = await client.get('/_metrics');
    expect(metricsRes.status).toBe(200);
    const metricsText = metricsRes.data;
    expect(metricsText).toContain('gateway_abuse_empty_payload_total');
  });
});

describe('Abuse Scenario 2: Targeted Tenant Attack', () => {
  const client = gateway();
  const targetTenantId = `tenant-target-${Date.now()}`;

  beforeAll(() => {
    // Set low per-tenant limit for testing
    process.env.GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT = '10';
    process.env.GATEWAY_RATE_LIMIT_TTL_SECONDS = '60';
  });

  it('should detect high-volume requests to single tenant', async () => {
    const requests = [];
    
    // Send 20 requests to same tenant (exceeding limit of 10)
    for (let i = 0; i < 20; i++) {
      requests.push(
        client.post(
          '/api/v1/routes/decide',
          {
            version: '1',
            tenant_id: targetTenantId,
            request_id: `req-target-${i}`,
            task: {
              type: 'text.generate',
              payload: { prompt: 'test' },
            },
          },
          {
            headers: {
              'X-Tenant-ID': targetTenantId,
            },
          },
        ),
      );
    }

    const responses = await Promise.all(requests);

    // First 10 should succeed (or 503 if Router unavailable)
    // Remaining should be 429 (rate limited)
    const successCount = responses.filter((res) => res.status === 200 || res.status === 503).length;
    const rateLimitedCount = responses.filter((res) => res.status === 429).length;

    expect(rateLimitedCount).toBeGreaterThan(0);
    expect(successCount + rateLimitedCount).toBe(20);
  });

  it('should log abuse events for targeted tenant attacks', async () => {
    // Send high-volume requests
    const requests = [];
    for (let i = 0; i < 15; i++) {
      requests.push(
        client.post(
          '/api/v1/routes/decide',
          {
            version: '1',
            tenant_id: targetTenantId,
            request_id: `req-target-log-${i}`,
            task: {
              type: 'text.generate',
              payload: { prompt: 'test' },
            },
          },
          {
            headers: {
              'X-Tenant-ID': targetTenantId,
            },
          },
        ),
      );
    }

    await Promise.all(requests);

    // Check metrics endpoint for abuse event
    await sleep(1000); // Wait for metrics to be recorded
    const metricsRes = await client.get('/_metrics');
    expect(metricsRes.status).toBe(200);
    const metricsText = metricsRes.data;
    expect(metricsText).toContain('gateway_abuse_targeted_tenant_total');
  });
});

describe('Abuse Scenario 3: Rate Limit Evasion (Multiple API Keys)', () => {
  const client = gateway();
  const tenantId = `tenant-evasion-${Date.now()}`;

  beforeAll(() => {
    // Set low per-tenant limit for testing
    process.env.GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT = '5';
    process.env.GATEWAY_RATE_LIMIT_TTL_SECONDS = '60';
  });

  it('should detect rate limit evasion with multiple API keys', async () => {
    const apiKeys = ['key-001', 'key-002', 'key-003', 'key-004', 'key-005'];
    const requests = [];
    
    // Send requests with different API keys but same tenant
    for (let i = 0; i < apiKeys.length; i++) {
      requests.push(
        client.post(
          '/api/v1/routes/decide',
          {
            version: '1',
            tenant_id: tenantId,
            request_id: `req-evasion-${i}`,
            task: {
              type: 'text.generate',
              payload: { prompt: 'test' },
            },
          },
          {
            headers: {
              'X-Tenant-ID': tenantId,
              'X-API-Key': apiKeys[i],  // Different API key
            },
          },
        ),
      );
    }

    const responses = await Promise.all(requests);

    // If aggregate per-tenant rate limiting is implemented:
    // Some requests should be 429 (aggregate limit exceeded)
    // If not implemented:
    // All requests may succeed (per-API-key limits not aggregated)

    // TODO: Verify aggregate rate limiting behavior
    // This test documents expected behavior once aggregate limiting is implemented
  });

  it('should log abuse events for rate limit evasion', async () => {
    const apiKeys = ['key-001', 'key-002', 'key-003'];
    const requests = [];
    
    for (let i = 0; i < apiKeys.length; i++) {
      requests.push(
        client.post(
          '/api/v1/routes/decide',
          {
            version: '1',
            tenant_id: tenantId,
            request_id: `req-evasion-log-${i}`,
            task: {
              type: 'text.generate',
              payload: { prompt: 'test' },
            },
          },
          {
            headers: {
              'X-Tenant-ID': tenantId,
              'X-API-Key': apiKeys[i],
            },
          },
        ),
      );
    }

    await Promise.all(requests);

    // Check metrics endpoint for evasion event
    await sleep(1000); // Wait for metrics to be recorded
    const metricsRes = await client.get('/_metrics');
    expect(metricsRes.status).toBe(200);
    const metricsText = metricsRes.data;
    expect(metricsText).toContain('gateway_abuse_rate_limit_evasion_total');
  });
});

describe('Abuse Scenario 4: Heavy Payload Attacks', () => {
  const client = gateway();
  const tenantId = `tenant-heavy-${Date.now()}`;

  it('should reject requests with payload exceeding size limit', async () => {
    // Create large payload (1MB+)
    const largePayload = {
      prompt: 'A'.repeat(1048576),  // 1MB of 'A' characters
    };

    const res = await client.post(
      '/api/v1/routes/decide',
      {
        version: '1',
        tenant_id: tenantId,
        request_id: 'req-heavy-001',
        task: {
          type: 'text.generate',
          payload: largePayload,
        },
      },
      {
        headers: {
          'X-Tenant-ID': tenantId,
        },
      },
    );

    // Request should be rejected (400 or 413)
    expect([400, 413, 422]).toContain(res.status);
    expect(res.data).toHaveProperty('error');
    expect(res.data.error).toMatch(/payload.*size|too.large|request.entity.too.large/i);
  });

  it('should detect heavy payload pattern (consistent large payloads)', async () => {
    const requests = [];
    
    // Send 10 requests with large payloads (500KB each)
    const largePayload = {
      prompt: 'A'.repeat(524288),  // 500KB
    };

    for (let i = 0; i < 10; i++) {
      requests.push(
        client.post(
          '/api/v1/routes/decide',
          {
            version: '1',
            tenant_id: tenantId,
            request_id: `req-heavy-${i}`,
            task: {
              type: 'text.generate',
              payload: largePayload,
            },
          },
          {
            headers: {
              'X-Tenant-ID': tenantId,
            },
          },
        ),
      );
    }

    const responses = await Promise.all(requests);

    // Requests may succeed (under 1MB limit) but should trigger abuse detection
    // Check metrics endpoint for heavy payload abuse event
    await sleep(1000); // Wait for metrics to be recorded
    const metricsRes = await client.get('/_metrics');
    expect(metricsRes.status).toBe(200);
    const metricsText = metricsRes.data;
    // Gateway may detect heavy payload pattern
    expect(metricsText).toMatch(/gateway_abuse_heavy_payload_total|router_abuse_heavy_payload_total/);
  });
});

describe('Abuse Scenario 5: Multi-Tenant Flood', () => {
  const client = gateway();
  const tenantIds = Array.from({ length: 20 }, (_, i) => `tenant-flood-${Date.now()}-${i}`);

  beforeAll(() => {
    // Set global limit for testing
    process.env.GATEWAY_RATE_LIMIT_GLOBAL = '50';
    process.env.GATEWAY_RATE_LIMIT_TTL_SECONDS = '60';
  });

  it('should detect multi-tenant flood (distributed attack)', async () => {
    const requests = [];
    
    // Send 3 requests per tenant (20 tenants = 60 total requests)
    // This should exceed global limit of 50
    for (const tenantId of tenantIds) {
      for (let i = 0; i < 3; i++) {
        requests.push(
          client.post(
            '/api/v1/routes/decide',
            {
              version: '1',
              tenant_id: tenantId,
              request_id: `req-flood-${tenantId}-${i}`,
              task: {
                type: 'text.generate',
                payload: { prompt: 'test' },
              },
            },
            {
              headers: {
                'X-Tenant-ID': tenantId,
              },
            },
          ),
        );
      }
    }

    const responses = await Promise.all(requests);

    // Some requests should be 429 (global limit exceeded)
    const rateLimitedCount = responses.filter((res) => res.status === 429).length;
    expect(rateLimitedCount).toBeGreaterThan(0);
  });

  it('should log abuse events for multi-tenant flood', async () => {
    const requests = [];
    
    // Send requests from multiple tenants
    for (const tenantId of tenantIds.slice(0, 10)) {
      requests.push(
        client.post(
          '/api/v1/routes/decide',
          {
            version: '1',
            tenant_id: tenantId,
            request_id: `req-flood-log-${tenantId}`,
            task: {
              type: 'text.generate',
              payload: { prompt: 'test' },
            },
          },
          {
            headers: {
              'X-Tenant-ID': tenantId,
            },
          },
        ),
      );
    }

    await Promise.all(requests);

    // Check metrics endpoint for multi-tenant flood event
    await sleep(1000); // Wait for metrics to be recorded
    const metricsRes = await client.get('/_metrics');
    expect(metricsRes.status).toBe(200);
    const metricsText = metricsRes.data;
    expect(metricsText).toContain('gateway_abuse_multi_tenant_flood_total');
    // Verify: gateway_abuse_multi_tenant_flood_total incremented
  });
});

describe('Abuse Scenario 6: Combined Attack (Multiple Vectors)', () => {
  const client = gateway();
  const tenantId = `tenant-combined-${Date.now()}`;

  it('should detect combined abuse patterns', async () => {
    const requests = [];
    
    // Combine multiple abuse vectors:
    // 1. Empty payloads
    // 2. High volume
    // 3. Large payloads (when not empty)
    
    // Empty payload flood
    for (let i = 0; i < 5; i++) {
      requests.push(
        client.post(
          '/api/v1/routes/decide',
          {
            version: '1',
            tenant_id: tenantId,
            request_id: `req-combined-empty-${i}`,
            task: {
              type: 'text.generate',
              payload: {},  // Empty
            },
          },
          {
            headers: {
              'X-Tenant-ID': tenantId,
            },
          },
        ),
      );
    }

    // High volume with valid payloads
    for (let i = 0; i < 20; i++) {
      requests.push(
        client.post(
          '/api/v1/routes/decide',
          {
            version: '1',
            tenant_id: tenantId,
            request_id: `req-combined-volume-${i}`,
            task: {
              type: 'text.generate',
              payload: { prompt: 'test' },
            },
          },
          {
            headers: {
              'X-Tenant-ID': tenantId,
            },
          },
        ),
      );
    }

    const responses = await Promise.all(requests);

    // Empty payloads should be rejected (400/422)
    const emptyPayloadResponses = responses.slice(0, 5);
    emptyPayloadResponses.forEach((res) => {
      expect([400, 422]).toContain(res.status);
    });

    // High volume should trigger rate limiting (429)
    const volumeResponses = responses.slice(5);
    const rateLimitedCount = volumeResponses.filter((res) => res.status === 429).length;
    expect(rateLimitedCount).toBeGreaterThan(0);
  });
});

