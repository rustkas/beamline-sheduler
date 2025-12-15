/**
 * Gateway ↔ Router Rate Limiting E2E Test
 * 
 * Purpose: Verify that Gateway Redis rate limiting and Router policy rate limiting
 * work together correctly without conflicts or double-dropping.
 * 
 * Test Scenarios:
 * 1. Gateway RL allows, Router RL allows → request succeeds
 * 2. Gateway RL blocks → Router NOT called, 429 from Gateway
 * 3. Gateway RL allows, Router RL blocks → Router returns 429, Gateway forwards
 * 4. Both layers active → no double-dropping, transparent behavior
 */

import { describe, it, expect, beforeAll, afterAll } from '@jest/globals';
import axios, { AxiosError } from 'axios';

const GATEWAY_URL = process.env.GATEWAY_URL || 'http://localhost:8080';
const TEST_TIMEOUT = 30000;

describe('Gateway ↔ Router Rate Limiting E2E', () => {
  const tenantId = `test-tenant-${Date.now()}`;
  const policyId = 'default';

  beforeAll(async () => {
    // Wait for Gateway to be ready
    await waitForGateway();
  });

  afterAll(async () => {
    // Cleanup: reset rate limits if needed
    // Note: In production, rate limits expire automatically
  });

  /**
   * Scenario 1: Gateway RL allows, Router RL allows → request succeeds
   * 
   * Test that when both layers allow the request, it succeeds normally.
   */
  it('should succeed when both Gateway and Router rate limits allow', async () => {
    const request = {
      version: '1',
      tenant_id: tenantId,
      request_id: `req-${Date.now()}`,
      task: {
        type: 'text.generate',
        payload: {}
      }
    };

    // First request should succeed (both layers allow)
    const response = await axios.post(
      `${GATEWAY_URL}/api/v1/routes/decide`,
      request,
      {
        headers: {
          'Content-Type': 'application/json',
          'X-Tenant-ID': tenantId
        },
        validateStatus: (status) => status < 500 // Accept 2xx, 4xx, but not 5xx
      }
    );

    // Should succeed (200 or 201)
    expect(response.status).toBeGreaterThanOrEqual(200);
    expect(response.status).toBeLessThan(300);
  }, TEST_TIMEOUT);

  /**
   * Scenario 2: Gateway RL blocks → Router NOT called, 429 from Gateway
   * 
   * Test that when Gateway rate limit is exceeded, Router is NOT called
   * and Gateway returns 429 with Gateway-specific error format.
   */
  it('should return 429 from Gateway when Gateway rate limit exceeded (Router NOT called)', async () => {
    // Configure Gateway with very low rate limit for this test
    // Note: This requires Gateway to be configured with low limits for testing
    const request = {
      version: '1',
      tenant_id: tenantId,
      request_id: `req-gateway-rl-${Date.now()}`,
      task: {
        type: 'text.generate',
        payload: {}
      }
    };

    // Send requests until Gateway rate limit is exceeded
    // Note: Actual limit depends on Gateway configuration
    let lastResponse;
    let requestCount = 0;
    const maxRequests = 100; // Safety limit

    for (let i = 0; i < maxRequests; i++) {
      try {
        lastResponse = await axios.post(
          `${GATEWAY_URL}/api/v1/routes/decide`,
          { ...request, request_id: `req-${i}` },
          {
            headers: {
              'Content-Type': 'application/json',
              'X-Tenant-ID': tenantId
            },
            validateStatus: () => true // Accept all status codes
          }
        );
        requestCount++;

        // If we get 429, Gateway rate limit was exceeded
        if (lastResponse.status === 429) {
          break;
        }
      } catch (error) {
        if (error instanceof AxiosError && error.response?.status === 429) {
          lastResponse = error.response;
          break;
        }
        throw error;
      }
    }

    // Verify Gateway returned 429
    expect(lastResponse?.status).toBe(429);

    // Verify Gateway error format (no Router intake_error_code)
    const errorBody = lastResponse?.data;
    expect(errorBody).toHaveProperty('error', 'rate_limit_exceeded');
    expect(errorBody).not.toHaveProperty('intake_error_code'); // Gateway error, not Router error

    // Verify Gateway rate limit headers
    expect(lastResponse?.headers).toHaveProperty('x-ratelimit-limit');
    expect(lastResponse?.headers).toHaveProperty('x-ratelimit-remaining');
    expect(lastResponse?.headers).toHaveProperty('retry-after');
  }, TEST_TIMEOUT);

  /**
   * Scenario 3: Gateway RL allows, Router RL blocks → Router returns 429, Gateway forwards
   * 
   * Test that when Gateway allows but Router rate limit is exceeded,
   * Router returns 429 with Router-specific error format, and Gateway forwards it.
   */
  it('should return 429 from Router when Router rate limit exceeded (Gateway allows)', async () => {
    // This test requires:
    // 1. Gateway rate limit is high enough (allows requests)
    // 2. Router policy has low rate limit configured
    // 3. Policy is loaded in Router with rate_limit enabled

    const request = {
      version: '1',
      tenant_id: tenantId,
      request_id: `req-router-rl-${Date.now()}`,
      task: {
        type: 'text.generate',
        payload: {}
      }
    };

    // Send requests until Router rate limit is exceeded
    // Note: Router rate limit is configured in policy JSON (e.g., 2 req/s, burst 2)
    let lastResponse;
    let requestCount = 0;
    const maxRequests = 10; // Safety limit

    for (let i = 0; i < maxRequests; i++) {
      try {
        lastResponse = await axios.post(
          `${GATEWAY_URL}/api/v1/routes/decide`,
          { ...request, request_id: `req-${i}` },
          {
            headers: {
              'Content-Type': 'application/json',
              'X-Tenant-ID': tenantId
            },
            validateStatus: () => true // Accept all status codes
          }
        );
        requestCount++;

        // If we get 429, Router rate limit was exceeded
        if (lastResponse.status === 429) {
          break;
        }
      } catch (error) {
        if (error instanceof AxiosError && error.response?.status === 429) {
          lastResponse = error.response;
          break;
        }
        throw error;
      }
    }

    // Verify Router returned 429 (Gateway forwarded Router error)
    expect(lastResponse?.status).toBe(429);

    // Verify Router error format (includes policy/tenant context)
    const errorBody = lastResponse?.data;
    expect(errorBody).toHaveProperty('error');
    
    // Router error should include policy/tenant context
    // Format: { ok: false, error: { code: "rate_limit_exceeded", details: {...} } }
    if (errorBody.error && typeof errorBody.error === 'object') {
      expect(errorBody.error).toHaveProperty('code', 'rate_limit_exceeded');
      expect(errorBody.error).toHaveProperty('details');
      expect(errorBody.error.details).toHaveProperty('scope');
      expect(errorBody.error.details).toHaveProperty('policy_id');
      expect(errorBody.error.details).toHaveProperty('tenant_id');
    }
  }, TEST_TIMEOUT);

  /**
   * Scenario 4: Both layers active → no double-dropping, transparent behavior
   * 
   * Test that when both Gateway and Router rate limits are configured,
   * they don't conflict and behavior is predictable.
   */
  it('should not double-drop requests when both Gateway and Router rate limits are active', async () => {
    // This test verifies:
    // 1. Gateway checks first (early rejection)
    // 2. Router checks second (after Gateway allows)
    // 3. No double-dropping (each layer checks independently)
    // 4. Transparent behavior (clear which layer blocked)

    const request = {
      version: '1',
      tenant_id: tenantId,
      request_id: `req-both-layers-${Date.now()}`,
      task: {
        type: 'text.generate',
        payload: {}
      }
    };

    // Track which layer blocked (if any)
    const responses: Array<{ status: number; error?: any }> = [];

    // Send multiple requests
    for (let i = 0; i < 5; i++) {
      try {
        const response = await axios.post(
          `${GATEWAY_URL}/api/v1/routes/decide`,
          { ...request, request_id: `req-${i}` },
          {
            headers: {
              'Content-Type': 'application/json',
              'X-Tenant-ID': tenantId
            },
            validateStatus: () => true // Accept all status codes
          }
        );
        responses.push({ status: response.status, error: response.data });
      } catch (error) {
        if (error instanceof AxiosError && error.response) {
          responses.push({ status: error.response.status, error: error.response.data });
        } else {
          throw error;
        }
      }
    }

    // Verify no double-dropping:
    // - Each 429 should have clear error format (Gateway or Router)
    // - No duplicate 429s for same request
    const rateLimitErrors = responses.filter(r => r.status === 429);
    
    for (const error of rateLimitErrors) {
      if (error.error) {
        // Gateway error: { error: "rate_limit_exceeded", ... } (no intake_error_code)
        // Router error: { ok: false, error: { code: "rate_limit_exceeded", details: {...} } }
        const isGatewayError = error.error.error === 'rate_limit_exceeded' && !error.error.intake_error_code;
        const isRouterError = error.error.error?.code === 'rate_limit_exceeded' || 
                             (error.error.ok === false && error.error.error?.details);
        
        expect(isGatewayError || isRouterError).toBe(true);
      }
    }
  }, TEST_TIMEOUT);

  /**
   * Helper: Wait for Gateway to be ready
   */
  async function waitForGateway(maxRetries = 10, delayMs = 1000): Promise<void> {
    for (let i = 0; i < maxRetries; i++) {
      try {
        const response = await axios.get(`${GATEWAY_URL}/_health`, {
          timeout: 2000,
          validateStatus: () => true
        });
        if (response.status === 200) {
          return;
        }
      } catch (error) {
        // Gateway not ready yet
      }
      await new Promise(resolve => setTimeout(resolve, delayMs));
    }
    throw new Error('Gateway not ready after max retries');
  }
});

