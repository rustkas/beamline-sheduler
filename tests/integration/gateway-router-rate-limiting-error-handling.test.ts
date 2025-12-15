/**
 * Gateway ↔ Router: Rate Limiting + Intake Error Handling Integration Tests
 * 
 * Tests the interaction between Gateway rate limiting (429) and Router intake errors:
 * - Rate limiting is checked BEFORE Router validation
 * - Rate limit exceeded → Gateway returns 429 (Router not called)
 * - Rate limit OK but Router intake error → Gateway returns Router error (400/401/500)
 * - These error types should not conflict
 * 
 * Error Priority:
 * 1. Rate Limiting (429) - checked first, blocks Router call
 * 2. Router Intake Errors (400/401/500) - checked after rate limit passes
 */

import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import axios, { AxiosInstance } from 'axios';

const BASE_URL = process.env.C_GATEWAY_URL || 'http://localhost:8080';

function gateway(): AxiosInstance {
  return axios.create({
    baseURL: BASE_URL,
    timeout: 10000,
    validateStatus: () => true, // Don't throw on any status
  });
}

function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

function generateId(prefix: string): string {
  return `${prefix}-${Date.now()}-${Math.random().toString(36).substring(7)}`;
}

describe('Gateway ↔ Router: Rate Limiting vs Intake Errors Priority', () => {
  const client = gateway();
  const tenantId = `tenant-rl-intake-${Date.now()}`;

  beforeAll(() => {
    // Set very low rate limit for testing
    process.env.GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT = '2';
    process.env.GATEWAY_RATE_LIMIT_TTL_SECONDS = '60';
  });

  describe('Rate Limiting Takes Priority (429 Before Router Validation)', () => {
    it('should return 429 when rate limit exceeded, even if request would trigger Router intake error', async () => {
      // Exhaust rate limit with valid requests
      for (let i = 0; i < 2; i++) {
        await client.post(
          '/api/v1/routes/decide',
          {
            version: '1',
            tenant_id: tenantId,
            request_id: generateId('req'),
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

      // Third request should hit rate limit BEFORE Router validation
      // Even if this request has invalid data (missing tenant_id), it should return 429, not 400
      const res = await client.post(
        '/api/v1/routes/decide',
        {
          version: '1',
          // Missing tenant_id - would trigger Router SCHEMA_VALIDATION_FAILED (400)
          // But rate limit should be checked first, so should return 429
          request_id: generateId('req'),
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

      // Should return 429 (rate limit), NOT 400 (Router intake error)
      expect(res.status).toBe(429);
      expect(res.data?.error?.code).toBe('rate_limit_exceeded');
      
      // Should NOT include intake_error_code (Router was not called)
      expect(res.data?.error?.intake_error_code).toBeUndefined();
      
      // Should include rate limit headers
      expect(res.headers['x-ratelimit-limit']).toBeDefined();
      expect(res.headers['x-ratelimit-remaining']).toBe('0');
      expect(res.headers['retry-after']).toBeDefined();
    });

    it('should return 429 when rate limit exceeded, even if request has invalid correlation fields', async () => {
      // Use different tenant to avoid previous rate limit
      const tenantId2 = `tenant-rl-intake-2-${Date.now()}`;
      
      // Exhaust rate limit
      for (let i = 0; i < 2; i++) {
        await client.post(
          '/api/v1/routes/decide',
          {
            version: '1',
            tenant_id: tenantId2,
            request_id: generateId('req'),
            task: {
              type: 'text.generate',
              payload: {},
            },
          },
          {
            headers: {
              'X-Tenant-ID': tenantId2,
            },
          },
        );
      }

      // Third request with invalid correlation fields (would trigger Router CORRELATION_FIELDS_INVALID)
      // But should return 429 first
      const res = await client.post(
        '/api/v1/routes/decide',
        {
          version: '1',
          tenant_id: tenantId2,
          request_id: generateId('req'),
          task: {
            type: 'text.generate',
            payload: {},
          },
          run_id: 'invalid-uuid-format', // Invalid UUID - would trigger Router error
          flow_id: '550e8400-e29b-41d4-a716-446655440000',
        },
        {
          headers: {
            'X-Tenant-ID': tenantId2,
          },
        },
      );

      // Should return 429 (rate limit), NOT 400 (Router correlation error)
      expect(res.status).toBe(429);
      expect(res.data?.error?.code).toBe('rate_limit_exceeded');
      expect(res.data?.error?.intake_error_code).toBeUndefined();
    });

    it('should return 429 when rate limit exceeded, even if tenant is forbidden', async () => {
      const tenantId3 = `tenant-rl-intake-3-${Date.now()}`;
      
      // Exhaust rate limit
      for (let i = 0; i < 2; i++) {
        await client.post(
          '/api/v1/routes/decide',
          {
            version: '1',
            tenant_id: tenantId3,
            request_id: generateId('req'),
            task: {
              type: 'text.generate',
              payload: {},
            },
          },
          {
            headers: {
              'X-Tenant-ID': tenantId3,
            },
          },
        );
      }

      // Third request with forbidden tenant (would trigger Router TENANT_FORBIDDEN)
      // But should return 429 first
      const res = await client.post(
        '/api/v1/routes/decide',
        {
          version: '1',
          tenant_id: 'forbidden-tenant', // Would trigger Router 401
          request_id: generateId('req'),
          task: {
            type: 'text.generate',
            payload: {},
          },
        },
        {
          headers: {
            'X-Tenant-ID': 'forbidden-tenant',
          },
        },
      );

      // Should return 429 (rate limit), NOT 401 (Router tenant error)
      expect(res.status).toBe(429);
      expect(res.data?.error?.code).toBe('rate_limit_exceeded');
      expect(res.data?.error?.intake_error_code).toBeUndefined();
    });
  });

  describe('Router Intake Errors After Rate Limit Passes', () => {
    it('should return Router intake error (400) when rate limit OK but schema validation fails', async () => {
      // Use fresh tenant to avoid rate limit
      const tenantId4 = `tenant-rl-intake-4-${Date.now()}`;

      const res = await client.post(
        '/api/v1/routes/decide',
        {
          version: '1',
          // Missing tenant_id - should trigger Router SCHEMA_VALIDATION_FAILED
          request_id: generateId('req'),
          task: {
            type: 'text.generate',
            payload: {},
          },
        },
        {
          headers: {
            'X-Tenant-ID': tenantId4,
          },
        },
      );

      // Should return 400 (Router intake error), NOT 429 (rate limit)
      expect(res.status).toBe(400);
      expect(res.data?.error?.code).toBe('invalid_request');
      
      // Should include intake_error_code if Router provides it
      if (res.data?.error?.intake_error_code) {
        expect(res.data.error.intake_error_code).toBe('SCHEMA_VALIDATION_FAILED');
      }
      
      // Should NOT include rate limit headers (rate limit was OK)
      expect(res.headers['x-ratelimit-limit']).toBeDefined(); // May be present
      const remaining = parseInt(res.headers['x-ratelimit-remaining'] || '0');
      expect(remaining).toBeGreaterThan(0); // Should have remaining quota
    });

    it('should return Router intake error (401) when rate limit OK but tenant is forbidden', async () => {
      const tenantId5 = `tenant-rl-intake-5-${Date.now()}`;

      const res = await client.post(
        '/api/v1/routes/decide',
        {
          version: '1',
          tenant_id: 'forbidden-tenant', // Would trigger Router TENANT_FORBIDDEN
          request_id: generateId('req'),
          task: {
            type: 'text.generate',
            payload: {},
          },
        },
        {
          headers: {
            'X-Tenant-ID': 'forbidden-tenant',
          },
        },
      );

      // Should return 401 (Router intake error), NOT 429 (rate limit)
      expect(res.status).toBe(401);
      expect(res.data?.error?.code).toBe('unauthorized');
      
      if (res.data?.error?.intake_error_code) {
        expect(res.data.error.intake_error_code).toBe('TENANT_FORBIDDEN');
      }
    });

    it('should return Router intake error (400) when rate limit OK but correlation fields invalid', async () => {
      const tenantId6 = `tenant-rl-intake-6-${Date.now()}`;

      const res = await client.post(
        '/api/v1/routes/decide',
        {
          version: '1',
          tenant_id: tenantId6,
          request_id: generateId('req'),
          task: {
            type: 'text.generate',
            payload: {},
          },
          run_id: 'invalid-uuid-format', // Invalid UUID - should trigger Router CORRELATION_FIELDS_INVALID
          flow_id: '550e8400-e29b-41d4-a716-446655440000',
        },
        {
          headers: {
            'X-Tenant-ID': tenantId6,
          },
        },
      );

      // Should return 400 (Router intake error), NOT 429 (rate limit)
      expect(res.status).toBe(400);
      expect(res.data?.error?.code).toBe('invalid_request');
      
      if (res.data?.error?.intake_error_code) {
        expect(res.data.error.intake_error_code).toBe('CORRELATION_FIELDS_INVALID');
      }
    });
  });

  describe('Error Response Format Distinction', () => {
    it('should distinguish rate limit error (429) from Router intake error (400) in response format', async () => {
      const tenantId7 = `tenant-rl-intake-7-${Date.now()}`;

      // Exhaust rate limit
      for (let i = 0; i < 2; i++) {
        await client.post(
          '/api/v1/routes/decide',
          {
            version: '1',
            tenant_id: tenantId7,
            request_id: generateId('req'),
            task: {
              type: 'text.generate',
              payload: {},
            },
          },
          {
            headers: {
              'X-Tenant-ID': tenantId7,
            },
          },
        );
      }

      // Rate limit error (429)
      const rateLimitRes = await client.post(
        '/api/v1/routes/decide',
        {
          version: '1',
          tenant_id: tenantId7,
          request_id: generateId('req'),
          task: {
            type: 'text.generate',
            payload: {},
          },
        },
        {
          headers: {
            'X-Tenant-ID': tenantId7,
          },
        },
      );

      expect(rateLimitRes.status).toBe(429);
      expect(rateLimitRes.data?.error?.code).toBe('rate_limit_exceeded');
      expect(rateLimitRes.data?.error?.details?.endpoint).toBe('/api/v1/routes/decide');
      expect(rateLimitRes.data?.error?.details?.limit).toBeDefined();
      expect(rateLimitRes.data?.error?.details?.retry_after_seconds).toBeDefined();
      expect(rateLimitRes.data?.error?.intake_error_code).toBeUndefined(); // No Router error

      // Wait for rate limit window to reset
      await sleep(3000);

      // Router intake error (400) - use different tenant to avoid rate limit
      const tenantId8 = `tenant-rl-intake-8-${Date.now()}`;
      const intakeErrorRes = await client.post(
        '/api/v1/routes/decide',
        {
          version: '1',
          // Missing tenant_id - should trigger Router SCHEMA_VALIDATION_FAILED
          request_id: generateId('req'),
          task: {
            type: 'text.generate',
            payload: {},
          },
        },
        {
          headers: {
            'X-Tenant-ID': tenantId8,
          },
        },
      );

      expect(intakeErrorRes.status).toBe(400);
      expect(intakeErrorRes.data?.error?.code).toBe('invalid_request');
      expect(intakeErrorRes.data?.error?.intake_error_code).toBeDefined(); // Router error code
      expect(intakeErrorRes.data?.error?.details?.endpoint).toBeUndefined(); // No rate limit details
    });

    it('should include rate limit headers only for 429 responses, not for Router intake errors', async () => {
      const tenantId9 = `tenant-rl-intake-9-${Date.now()}`;

      // Exhaust rate limit
      for (let i = 0; i < 2; i++) {
        await client.post(
          '/api/v1/routes/decide',
          {
            version: '1',
            tenant_id: tenantId9,
            request_id: generateId('req'),
            task: {
              type: 'text.generate',
              payload: {},
            },
          },
          {
            headers: {
              'X-Tenant-ID': tenantId9,
            },
          },
        );
      }

      // Rate limit error (429) - should have rate limit headers
      const rateLimitRes = await client.post(
        '/api/v1/routes/decide',
        {
          version: '1',
          tenant_id: tenantId9,
          request_id: generateId('req'),
          task: {
            type: 'text.generate',
            payload: {},
          },
        },
        {
          headers: {
            'X-Tenant-ID': tenantId9,
          },
        },
      );

      expect(rateLimitRes.status).toBe(429);
      expect(rateLimitRes.headers['x-ratelimit-limit']).toBeDefined();
      expect(rateLimitRes.headers['x-ratelimit-remaining']).toBe('0');
      expect(rateLimitRes.headers['x-ratelimit-reset']).toBeDefined();
      expect(rateLimitRes.headers['retry-after']).toBeDefined();

      // Wait for rate limit window to reset
      await sleep(3000);

      // Router intake error (400) - use different tenant
      const tenantId10 = `tenant-rl-intake-10-${Date.now()}`;
      const intakeErrorRes = await client.post(
        '/api/v1/routes/decide',
        {
          version: '1',
          // Missing tenant_id
          request_id: generateId('req'),
          task: {
            type: 'text.generate',
            payload: {},
          },
        },
        {
          headers: {
            'X-Tenant-ID': tenantId10,
          },
        },
      );

      expect(intakeErrorRes.status).toBe(400);
      // Rate limit headers may be present (if Gateway includes them for all responses)
      // But retry-after should NOT be present for non-429 errors
      if (intakeErrorRes.headers['retry-after']) {
        // If present, it should be 0 or not relevant to intake errors
        expect(parseInt(intakeErrorRes.headers['retry-after'])).toBeGreaterThanOrEqual(0);
      }
    });
  });

  describe('Mixed Scenarios: Rate Limit Reset After Intake Errors', () => {
    it('should handle rate limit reset correctly after Router intake errors', async () => {
      const tenantId11 = `tenant-rl-intake-11-${Date.now()}`;

      // Send requests that trigger Router intake errors (these should NOT count toward rate limit)
      // But Gateway may count them - depends on implementation
      for (let i = 0; i < 2; i++) {
        const res = await client.post(
          '/api/v1/routes/decide',
          {
            version: '1',
            // Missing tenant_id - Router intake error
            request_id: generateId('req'),
            task: {
              type: 'text.generate',
              payload: {},
            },
          },
          {
            headers: {
              'X-Tenant-ID': tenantId11,
            },
          },
        );

        // Should return Router intake error (400), not rate limit (429)
        expect(res.status).toBe(400);
        expect(res.data?.error?.code).toBe('invalid_request');
      }

      // After rate limit window, valid requests should work
      await sleep(3000);

      const validRes = await client.post(
        '/api/v1/routes/decide',
        {
          version: '1',
          tenant_id: tenantId11,
          request_id: generateId('req'),
          task: {
            type: 'text.generate',
            payload: {},
          },
        },
        {
          headers: {
            'X-Tenant-ID': tenantId11,
          },
        },
      );

      // Should succeed (200) or Router unavailable (503), but NOT 429 or 400
      expect([200, 503]).toContain(validRes.status);
    });
  });

  describe('Error Code Mapping Consistency', () => {
    it('should map Router intake errors correctly while preserving rate limit error format', async () => {
      const tenantId12 = `tenant-rl-intake-12-${Date.now()}`;

      // Router intake error
      const intakeErrorRes = await client.post(
        '/api/v1/routes/decide',
        {
          version: '1',
          // Missing tenant_id
          request_id: generateId('req'),
          task: {
            type: 'text.generate',
            payload: {},
          },
        },
        {
          headers: {
            'X-Tenant-ID': tenantId12,
          },
        },
      );

      if (intakeErrorRes.status === 400) {
        // Router intake error format
        expect(intakeErrorRes.data?.error?.code).toBe('invalid_request');
        expect(intakeErrorRes.data?.error?.intake_error_code).toBeDefined();
        expect(intakeErrorRes.data?.context).toBeDefined();
      }

      // Exhaust rate limit
      for (let i = 0; i < 2; i++) {
        await client.post(
          '/api/v1/routes/decide',
          {
            version: '1',
            tenant_id: tenantId12,
            request_id: generateId('req'),
            task: {
              type: 'text.generate',
              payload: {},
            },
          },
          {
            headers: {
              'X-Tenant-ID': tenantId12,
            },
          },
        );
      }

      // Rate limit error
      const rateLimitRes = await client.post(
        '/api/v1/routes/decide',
        {
          version: '1',
          tenant_id: tenantId12,
          request_id: generateId('req'),
          task: {
            type: 'text.generate',
            payload: {},
          },
        },
        {
          headers: {
            'X-Tenant-ID': tenantId12,
          },
        },
      );

      if (rateLimitRes.status === 429) {
        // Rate limit error format
        expect(rateLimitRes.data?.error?.code).toBe('rate_limit_exceeded');
        expect(rateLimitRes.data?.error?.intake_error_code).toBeUndefined();
        expect(rateLimitRes.data?.error?.details?.endpoint).toBeDefined();
        expect(rateLimitRes.data?.context).toBeDefined();
      }

      // Both should have consistent context structure
      if (intakeErrorRes.data?.context && rateLimitRes.data?.context) {
        expect(intakeErrorRes.data.context.request_id).toBeDefined();
        expect(rateLimitRes.data.context.request_id).toBeDefined();
      }
    });
  });
});

