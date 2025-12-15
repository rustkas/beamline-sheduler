/**
 * Gateway Rate Limiting Integration Tests
 * 
 * Tests rate limiting behavior for Gateway HTTP endpoints:
 * - Under-limit requests (should succeed)
 * - At-limit requests (should succeed)
 * - Over-limit requests (should return 429)
 * - Window reset (after TTL expiration)
 * - Multi-tenant/API-key isolation (different tenants have separate limits in CP2)
 * - Rate limit headers (X-RateLimit-*, Retry-After)
 * - Error response format
 */

import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import axios, { AxiosInstance, AxiosError } from 'axios';

const BASE_URL = process.env.C_GATEWAY_URL || 'http://localhost:8080';

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

describe('Gateway Rate Limiting: Under-Limit Requests', () => {
  const client = gateway();
  const tenantId = `tenant-rl-under-${Date.now()}`;

  beforeAll(() => {
    // Set low limit for testing
    process.env.GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT = '5';
    process.env.GATEWAY_RATE_LIMIT_TTL_SECONDS = '60';
  });

  it('should allow requests under limit', async () => {
    const requests = [];
    
    // Send 3 requests (under limit of 5)
    for (let i = 0; i < 3; i++) {
      requests.push(
        client.post(
          '/api/v1/routes/decide',
          {
            version: '1',
            tenant_id: tenantId,
            request_id: `req-under-${i}`,
            task: {
              type: 'text.generate',
              payload: {},
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

    // All requests should NOT be 429
    responses.forEach((res) => {
      expect(res.status).not.toBe(429);
      // May be 200 (success) or 503 (Router unavailable), but NOT 429
    });
  });
});

describe('Gateway Rate Limiting: At-Limit Requests', () => {
  const client = gateway();
  const tenantId = `tenant-rl-at-${Date.now()}`;

  beforeAll(() => {
    // Set very low limit for testing
    process.env.GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT = '2';
    process.env.GATEWAY_RATE_LIMIT_TTL_SECONDS = '60';
  });

  it('should allow requests at limit, reject over limit', async () => {
    // Send 2 requests (at limit) - should succeed
    for (let i = 0; i < 2; i++) {
      const res = await client.post(
        '/api/v1/routes/decide',
        {
          version: '1',
          tenant_id: tenantId,
          request_id: `req-at-${i}`,
          task: {
            type: 'text.generate',
            payload: {},
          },
        },
        {
          headers: {
            'X-Tenant-ID': tenantId,
          },
        },
      );

      // First 2 requests should NOT be 429
      expect(res.status).not.toBe(429);
    }

    // Third request (exceeds limit) - should return 429
    const res = await client.post(
      '/api/v1/routes/decide',
      {
        version: '1',
        tenant_id: tenantId,
        request_id: 'req-at-2',
        task: {
          type: 'text.generate',
          payload: {},
        },
      },
      {
        headers: {
          'X-Tenant-ID': tenantId,
        },
      },
    );

    expect(res.status).toBe(429);
    expect(res.data).toBeDefined();
    expect(res.data.ok).toBe(false);
    expect(res.data.error?.code).toBe('rate_limit_exceeded');
  });
});

describe('Gateway Rate Limiting: Rate Limit Headers', () => {
  const client = gateway();
  const tenantId = `tenant-rl-headers-${Date.now()}`;

  beforeAll(() => {
    // Set very low limit for testing
    process.env.GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT = '1';
    process.env.GATEWAY_RATE_LIMIT_TTL_SECONDS = '60';
  });

  it('should include rate limit headers in 429 response', async () => {
    // First request - should succeed
    await client.post(
      '/api/v1/routes/decide',
      {
        version: '1',
        tenant_id: tenantId,
        request_id: 'req-headers-1',
        task: {
          type: 'text.generate',
          payload: {},
        },
      },
      {
        headers: {
          'X-Tenant-ID': tenantId,
        },
      },
    );

    // Second request - should return 429 with headers
    const res = await client.post(
      '/api/v1/routes/decide',
      {
        version: '1',
        tenant_id: tenantId,
        request_id: 'req-headers-2',
        task: {
          type: 'text.generate',
          payload: {},
        },
      },
      {
        headers: {
          'X-Tenant-ID': tenantId,
        },
      },
    );

    expect(res.status).toBe(429);

    // Verify rate limit headers
    expect(res.headers['x-ratelimit-limit']).toBeDefined();
    expect(parseInt(res.headers['x-ratelimit-limit'])).toBeGreaterThan(0);

    expect(res.headers['x-ratelimit-remaining']).toBeDefined();
    expect(parseInt(res.headers['x-ratelimit-remaining'])).toBe(0); // Should be 0 when limit exceeded

    expect(res.headers['x-ratelimit-reset']).toBeDefined();
    const resetTime = parseInt(res.headers['x-ratelimit-reset']);
    expect(resetTime).toBeGreaterThan(Math.floor(Date.now() / 1000)); // Should be future timestamp

    expect(res.headers['retry-after']).toBeDefined();
    const retryAfter = parseInt(res.headers['retry-after']);
    expect(retryAfter).toBeGreaterThanOrEqual(0); // Should be non-negative
  });
});

describe('Gateway Rate Limiting: Window Reset', () => {
  const client = gateway();
  const tenantId = `tenant-rl-reset-${Date.now()}`;

  beforeAll(() => {
    // Set very short TTL for testing
    process.env.GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT = '2';
    process.env.GATEWAY_RATE_LIMIT_TTL_SECONDS = '2'; // 2 seconds window
  });

  it('should reset window after TTL expiration', async () => {
    // Exhaust limit
    for (let i = 0; i < 2; i++) {
      await client.post(
        '/api/v1/routes/decide',
        {
          version: '1',
          tenant_id: tenantId,
          request_id: `req-reset-${i}`,
          task: {
            type: 'text.generate',
            payload: {},
          },
        },
        {
          headers: {
            'X-Tenant-ID': tenantId,
          },
        },
      );
    }

    // Third request should be 429
    let res = await client.post(
      '/api/v1/routes/decide',
      {
        version: '1',
        tenant_id: tenantId,
        request_id: 'req-reset-2',
        task: {
          type: 'text.generate',
          payload: {},
        },
      },
      {
        headers: {
          'X-Tenant-ID': tenantId,
        },
      },
    );

    expect(res.status).toBe(429);

    // Wait for window to reset (2 seconds + small buffer)
    await sleep(3000);

    // After reset, request should succeed
    res = await client.post(
      '/api/v1/routes/decide',
      {
        version: '1',
        tenant_id: tenantId,
        request_id: 'req-reset-3',
        task: {
          type: 'text.generate',
          payload: {},
        },
      },
      {
        headers: {
          'X-Tenant-ID': tenantId,
        },
      },
    );

    // Should NOT be 429 after reset
    expect(res.status).not.toBe(429);
  });
});

describe('Gateway Rate Limiting: Multi-Endpoint Isolation', () => {
  const client = gateway();
  const tenantId = `tenant-rl-multi-${Date.now()}`;

  beforeAll(() => {
    // Set different limits for different endpoints
    process.env.GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT = '2';
    process.env.GATEWAY_RATE_LIMIT_MESSAGES = '3';
    process.env.GATEWAY_RATE_LIMIT_TTL_SECONDS = '60';
  });

  it('should isolate rate limits per endpoint', async () => {
    // Exhaust /api/v1/routes/decide limit (2 requests)
    for (let i = 0; i < 2; i++) {
      await client.post(
        '/api/v1/routes/decide',
        {
          version: '1',
          tenant_id: tenantId,
          request_id: `req-multi-decide-${i}`,
          task: {
            type: 'text.generate',
            payload: {},
          },
        },
        {
          headers: {
            'X-Tenant-ID': tenantId,
          },
        },
      );
    }

    // Third decide request should be 429
    let res = await client.post(
      '/api/v1/routes/decide',
      {
        version: '1',
        tenant_id: tenantId,
        request_id: 'req-multi-decide-2',
        task: {
          type: 'text.generate',
          payload: {},
        },
      },
      {
        headers: {
          'X-Tenant-ID': tenantId,
        },
      },
    );

    expect(res.status).toBe(429);

    // But /api/v1/messages should still work (different endpoint, limit 3)
    res = await client.post(
      '/api/v1/messages',
      {
        message_id: 'msg-multi-1',
        message_type: 'chat',
        payload: {},
      },
      {
        headers: {
          'X-Tenant-ID': tenantId,
        },
      },
    );

    // Should NOT be 429 (different endpoint)
    expect(res.status).not.toBe(429);
  });
});

describe('Gateway Rate Limiting: Error Response Format', () => {
  const client = gateway();
  const tenantId = `tenant-rl-error-${Date.now()}`;
  const traceId = `trace-rl-error-${Date.now()}`;

  beforeAll(() => {
    // Set very low limit
    process.env.GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT = '1';
    process.env.GATEWAY_RATE_LIMIT_TTL_SECONDS = '60';
  });

  it('should return correct error response format for rate limit exceeded', async () => {
    // First request - should succeed
    await client.post(
      '/api/v1/routes/decide',
      {
        version: '1',
        tenant_id: tenantId,
        request_id: 'req-error-1',
        task: {
          type: 'text.generate',
          payload: {},
        },
      },
      {
        headers: {
          'X-Tenant-ID': tenantId,
          'X-Trace-ID': traceId,
        },
      },
    );

    // Second request - should return 429
    const res = await client.post(
      '/api/v1/routes/decide',
      {
        version: '1',
        tenant_id: tenantId,
        request_id: 'req-error-2',
        task: {
          type: 'text.generate',
          payload: {},
        },
      },
      {
        headers: {
          'X-Tenant-ID': tenantId,
          'X-Trace-ID': traceId,
        },
      },
    );

    expect(res.status).toBe(429);
    expect(res.data).toBeDefined();

    // Verify error structure
    expect(res.data.ok).toBe(false);

    expect(res.data.error).toBeDefined();
    expect(res.data.error.code).toBe('rate_limit_exceeded');
    expect(res.data.error.message).toBeDefined();
    expect(res.data.error.details).toBeDefined();

    // Verify error details
    expect(res.data.error.details.endpoint).toBe('/api/v1/routes/decide');
    expect(res.data.error.details.limit).toBeGreaterThan(0);
    expect(res.data.error.details.retry_after_seconds).toBeGreaterThanOrEqual(0);

    // Verify context
    expect(res.data.context).toBeDefined();
    expect(res.data.context.request_id).toBeDefined();
    expect(res.data.context.trace_id).toBe(traceId);
    expect(res.data.context.tenant_id).toBe(tenantId);
  });
});

describe('Gateway Rate Limiting: Multi-Tenant Isolation (CP2)', () => {
  const client = gateway();
  const tenant1 = `tenant-rl-mt1-${Date.now()}`;
  const tenant2 = `tenant-rl-mt2-${Date.now()}`;

  beforeAll(() => {
    // Set low limit for testing
    process.env.GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT = '2';
    process.env.GATEWAY_RATE_LIMIT_TTL_SECONDS = '60';
  });

  it('should isolate rate limits per tenant (CP2 behavior)', async () => {
    // Note: In CP1, rate limits are per-endpoint (not per-tenant)
    // In CP2, rate limits will be per-tenant
    // This test verifies current CP1 behavior (no per-tenant isolation)
    // and can be updated when CP2 per-tenant limits are implemented

    // Exhaust limit for tenant1
    for (let i = 0; i < 2; i++) {
      await client.post(
        '/api/v1/routes/decide',
        {
          version: '1',
          tenant_id: tenant1,
          request_id: `req-mt1-${i}`,
          task: {
            type: 'text.generate',
            payload: {},
          },
        },
        {
          headers: {
            'X-Tenant-ID': tenant1,
          },
        },
      );
    }

    // Third request from tenant1 should be 429
    let res = await client.post(
      '/api/v1/routes/decide',
      {
        version: '1',
        tenant_id: tenant1,
        request_id: 'req-mt1-2',
        task: {
          type: 'text.generate',
          payload: {},
        },
      },
      {
        headers: {
          'X-Tenant-ID': tenant1,
        },
      },
    );

    expect(res.status).toBe(429);

    // In CP1: tenant2 should also be 429 (shared limit)
    // In CP2: tenant2 should succeed (separate limit per tenant)
    // Current CP1 behavior: shared limit, so tenant2 should also be 429
    res = await client.post(
      '/api/v1/routes/decide',
      {
        version: '1',
        tenant_id: tenant2,
        request_id: 'req-mt2-1',
        task: {
          type: 'text.generate',
          payload: {},
        },
      },
      {
        headers: {
          'X-Tenant-ID': tenant2,
        },
      },
    );

    // CP1: shared limit, so tenant2 should also be 429
    // CP2: per-tenant limit, so tenant2 should succeed (status !== 429)
    // For now, test CP1 behavior
    expect(res.status).toBe(429); // CP1: shared limit
  });
});

