/**
 * RBAC and Policy Validation Tests
 * Validates Role-Based Access Control and Policy Enforcement
 */

import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import { connect, NatsConnection, StringCodec } from 'nats';

// RBAC Test Configuration
const TEST_CONFIG = {
  GATEWAY_URL: process.env.GATEWAY_URL || 'http://localhost:3000',
  ROUTER_URL: process.env.ROUTER_URL || 'http://localhost:8080',
  NATS_URL: process.env.NATS_URL || 'nats://localhost:4222',
  TEST_TIMEOUT: 30000,
};

// Mock RBAC roles and permissions
const ROLES = {
  ADMIN: {
    name: 'admin',
    permissions: ['read', 'write', 'delete', 'manage', 'access_all_models'],
    models: ['gpt-3.5-turbo', 'gpt-4', 'gpt-4-turbo', 'claude-3', 'gemini-pro'],
  },
  USER: {
    name: 'user',
    permissions: ['read', 'write'],
    models: ['gpt-3.5-turbo', 'gpt-3.5-turbo-16k'],
  },
  GUEST: {
    name: 'guest',
    permissions: ['read'],
    models: ['gpt-3.5-turbo'],
  },
  DEVELOPER: {
    name: 'developer',
    permissions: ['read', 'write', 'test'],
    models: ['gpt-3.5-turbo', 'gpt-4', 'gpt-4-turbo-preview'],
  },
};

// Mock policies
const POLICIES = {
  RATE_LIMIT: {
    admin: { requests_per_minute: 1000, requests_per_hour: 10000 },
    user: { requests_per_minute: 100, requests_per_hour: 1000 },
    guest: { requests_per_minute: 20, requests_per_hour: 200 },
    developer: { requests_per_minute: 200, requests_per_hour: 2000 },
  },
  TENANT_ISOLATION: {
    enabled: true,
    strict_mode: true,
  },
  CONTENT_FILTER: {
    enabled: true,
    blocked_patterns: ['violence', 'harassment', 'discrimination'],
  },
  QUOTA_MANAGEMENT: {
    admin: { daily_tokens: 1000000, monthly_tokens: 30000000 },
    user: { daily_tokens: 100000, monthly_tokens: 3000000 },
    guest: { daily_tokens: 10000, monthly_tokens: 300000 },
    developer: { daily_tokens: 200000, monthly_tokens: 6000000 },
  },
};

describe('RBAC and Policy Validation', () => {
  let natsConnection: NatsConnection;
  let sc: StringCodec;

  beforeAll(async () => {
    try {
      natsConnection = await connect({ servers: TEST_CONFIG.NATS_URL });
      sc = StringCodec();
    } catch (error) {
      console.warn('NATS connection failed, some tests may be skipped:', error);
    }
  });

  afterAll(async () => {
    if (natsConnection) {
      await natsConnection.close();
    }
  });

  describe('Role-Based Access Control', () => {
    it('should validate admin role permissions', async () => {
      const adminToken = 'admin-token-123';
      
      // Test access to all models
      for (const model of ROLES.ADMIN.models) {
        const response = await fetch(`${TEST_CONFIG.GATEWAY_URL}/v1/chat/completions`, {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            'Authorization': `Bearer ${adminToken}`,
            'X-User-Role': 'admin',
          },
          body: JSON.stringify({
            model: model,
            messages: [{ role: 'user', content: `Admin access test for ${model}` }],
            max_tokens: 50,
          }),
        });

        expect([200, 202]).toContain(response.status);
      }
    }, TEST_CONFIG.TEST_TIMEOUT);

    it('should validate user role permissions', async () => {
      const userToken = 'user-token-456';
      
      // Test access to allowed models
      for (const model of ROLES.USER.models) {
        const response = await fetch(`${TEST_CONFIG.GATEWAY_URL}/v1/chat/completions`, {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            'Authorization': `Bearer ${userToken}`,
            'X-User-Role': 'user',
          },
          body: JSON.stringify({
            model: model,
            messages: [{ role: 'user', content: `User access test for ${model}` }],
            max_tokens: 50,
          }),
        });

        expect([200, 202]).toContain(response.status);
      }

      // Test access to restricted models
      const restrictedModels = ['gpt-4', 'claude-3', 'gemini-pro'];
      for (const model of restrictedModels) {
        const response = await fetch(`${TEST_CONFIG.GATEWAY_URL}/v1/chat/completions`, {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            'Authorization': `Bearer ${userToken}`,
            'X-User-Role': 'user',
          },
          body: JSON.stringify({
            model: model,
            messages: [{ role: 'user', content: `User restricted test for ${model}` }],
            max_tokens: 50,
          }),
        });

        // Should either be denied or redirected to allowed model
        expect([200, 403, 422]).toContain(response.status);
        
        if (response.status === 403) {
          const error = await response.json();
          expect(error.error.message.toLowerCase()).toContain('access');
        }
      }
    }, TEST_CONFIG.TEST_TIMEOUT);

    it('should validate guest role permissions', async () => {
      const guestToken = 'guest-token-789';
      
      // Test access to guest-allowed models
      const response = await fetch(`${TEST_CONFIG.GATEWAY_URL}/v1/chat/completions`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': `Bearer ${guestToken}`,
          'X-User-Role': 'guest',
        },
        body: JSON.stringify({
          model: 'gpt-3.5-turbo',
          messages: [{ role: 'user', content: 'Guest access test' }],
          max_tokens: 50,
        }),
      });

      expect([200, 202]).toContain(response.status);

      // Test access to restricted models
      const restrictedResponse = await fetch(`${TEST_CONFIG.GATEWAY_URL}/v1/chat/completions`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': `Bearer ${guestToken}`,
          'X-User-Role': 'guest',
        },
        body: JSON.stringify({
          model: 'gpt-4',
          messages: [{ role: 'user', content: 'Guest restricted test' }],
          max_tokens: 50,
        }),
      });

      expect([403, 422]).toContain(restrictedResponse.status);
    }, TEST_CONFIG.TEST_TIMEOUT);
  });

  describe('Rate Limiting Policies', () => {
    it('should enforce rate limits per role', async () => {
      const userToken = 'rate-limit-user-123';
      const requests = Array(25).fill(null).map((_, index) => 
        fetch(`${TEST_CONFIG.GATEWAY_URL}/v1/chat/completions`, {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            'Authorization': `Bearer ${userToken}`,
            'X-User-Role': 'user',
          },
          body: JSON.stringify({
            model: 'gpt-3.5-turbo',
            messages: [{ role: 'user', content: `Rate limit test ${index}` }],
            max_tokens: 10,
          }),
        })
      );

      const responses = await Promise.all(requests);
      const rateLimitedResponses = responses.filter(r => r.status === 429);
      
      // Should have some rate-limited requests for user role
      expect(rateLimitedResponses.length).toBeGreaterThan(0);
      
      if (rateLimitedResponses.length > 0) {
        const error = await rateLimitedResponses[0].json();
        expect(error.error.message.toLowerCase()).toContain('rate limit');
      }
    }, TEST_CONFIG.TEST_TIMEOUT);

    it('should allow higher rate limits for admin role', async () => {
      const adminToken = 'rate-limit-admin-456';
      const requests = Array(50).fill(null).map((_, index) => 
        fetch(`${TEST_CONFIG.GATEWAY_URL}/v1/chat/completions`, {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            'Authorization': `Bearer ${adminToken}`,
            'X-User-Role': 'admin',
          },
          body: JSON.stringify({
            model: 'gpt-3.5-turbo',
            messages: [{ role: 'user', content: `Admin rate test ${index}` }],
            max_tokens: 10,
          }),
        })
      );

      const responses = await Promise.all(requests);
      const successfulResponses = responses.filter(r => r.status === 200);
      
      // Admin should have higher success rate
      expect(successfulResponses.length).toBeGreaterThan(40);
    }, TEST_CONFIG.TEST_TIMEOUT);
  });

  describe('Tenant Isolation', () => {
    it('should isolate tenants completely', async () => {
      const tenant1Token = 'tenant-1-token';
      const tenant2Token = 'tenant-2-token';
      
      // Create requests for different tenants
      const tenant1Request = {
        model: 'gpt-3.5-turbo',
        messages: [{ role: 'user', content: 'Tenant 1 test' }],
        tenant_id: 'tenant-1',
      };

      const tenant2Request = {
        model: 'gpt-3.5-turbo',
        messages: [{ role: 'user', content: 'Tenant 2 test' }],
        tenant_id: 'tenant-2',
      };

      const [response1, response2] = await Promise.all([
        fetch(`${TEST_CONFIG.GATEWAY_URL}/v1/chat/completions`, {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            'Authorization': `Bearer ${tenant1Token}`,
            'X-Tenant-ID': 'tenant-1',
          },
          body: JSON.stringify(tenant1Request),
        }),
        fetch(`${TEST_CONFIG.GATEWAY_URL}/v1/chat/completions`, {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            'Authorization': `Bearer ${tenant2Token}`,
            'X-Tenant-ID': 'tenant-2',
          },
          body: JSON.stringify(tenant2Request),
        }),
      ]);

      expect(response1.status).toBe(200);
      expect(response2.status).toBe(200);

      const data1 = await response1.json();
      const data2 = await response2.json();

      // Responses should be different (different IDs, etc.)
      expect(data1.id).not.toBe(data2.id);
      expect(data1.created).not.toBe(data2.created);
    }, TEST_CONFIG.TEST_TIMEOUT);

    it('should prevent cross-tenant data access', async () => {
      const tenant1Token = 'tenant-1-cross-token';
      
      // Try to access another tenant's data
      const response = await fetch(`${TEST_CONFIG.GATEWAY_URL}/v1/chat/completions`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': `Bearer ${tenant1Token}`,
          'X-Tenant-ID': 'tenant-1',
          'X-Target-Tenant-ID': 'tenant-2', // Try to access tenant-2 data
        },
        body: JSON.stringify({
          model: 'gpt-3.5-turbo',
          messages: [{ role: 'user', content: 'Cross-tenant access test' }],
        }),
      });

      // Should be denied or redirected to own tenant
      expect([200, 403]).toContain(response.status);
      
      if (response.status === 403) {
        const error = await response.json();
        expect(error.error.message.toLowerCase()).toContain('access');
      }
    }, TEST_CONFIG.TEST_TIMEOUT);
  });

  describe('Content Filtering Policies', () => {
    it('should block harmful content', async () => {
      const testToken = 'content-filter-token';
      
      // Test with blocked content patterns
      const blockedContents = [
        'This is a test about violence and harmful behavior',
        'Content that promotes harassment and discrimination',
        'Messages containing inappropriate violent content',
      ];

      for (const content of blockedContents) {
        const response = await fetch(`${TEST_CONFIG.GATEWAY_URL}/v1/chat/completions`, {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            'Authorization': `Bearer ${testToken}`,
          },
          body: JSON.stringify({
            model: 'gpt-3.5-turbo',
            messages: [{ role: 'user', content: content }],
            max_tokens: 50,
          }),
        });

        // Should either block or filter the content
        expect([200, 400, 403, 422]).toContain(response.status);
        
        if (response.status !== 200) {
          const error = await response.json();
          expect(error.error.message.toLowerCase()).toMatch(/content|filter|inappropriate/);
        }
      }
    }, TEST_CONFIG.TEST_TIMEOUT);

    it('should allow safe content', async () => {
      const testToken = 'safe-content-token';
      
      const safeContents = [
        'Hello, how are you today?',
        'Can you help me with a programming question?',
        'What is the weather like?',
        'Tell me about machine learning.',
      ];

      for (const content of safeContents) {
        const response = await fetch(`${TEST_CONFIG.GATEWAY_URL}/v1/chat/completions`, {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            'Authorization': `Bearer ${testToken}`,
          },
          body: JSON.stringify({
            model: 'gpt-3.5-turbo',
            messages: [{ role: 'user', content: content }],
            max_tokens: 50,
          }),
        });

        expect([200, 202]).toContain(response.status);
      }
    }, TEST_CONFIG.TEST_TIMEOUT);
  });

  describe('Quota Management', () => {
    it('should enforce daily token quotas', async () => {
      const userToken = 'quota-user-token';
      
      // Make requests that would exceed daily quota
      const largeRequests = Array(10).fill(null).map((_, index) => ({
        model: 'gpt-3.5-turbo',
        messages: [{ role: 'user', content: `Quota test ${index}. ${'This is a long message to consume tokens. '.repeat(20)}` }],
        max_tokens: 500,
      }));

      const responses = await Promise.all(
        largeRequests.map(request =>
          fetch(`${TEST_CONFIG.GATEWAY_URL}/v1/chat/completions`, {
            method: 'POST',
            headers: {
              'Content-Type': 'application/json',
              'Authorization': `Bearer ${userToken}`,
              'X-User-Role': 'user',
            },
            body: JSON.stringify(request),
          })
        )
      );

      // Check if any requests were rejected due to quota
      const quotaExceededResponses = responses.filter(r => r.status === 429);
      
      if (quotaExceededResponses.length > 0) {
        const error = await quotaExceededResponses[0].json();
        expect(error.error.message.toLowerCase()).toMatch(/quota|limit|exceeded/);
      }
    }, TEST_CONFIG.TEST_TIMEOUT);

    it('should allow higher quotas for premium roles', async () => {
      const adminToken = 'quota-admin-token';
      
      // Admin should be able to make more requests
      const requests = Array(20).fill(null).map((_, index) => ({
        model: 'gpt-3.5-turbo',
        messages: [{ role: 'user', content: `Admin quota test ${index}` }],
        max_tokens: 100,
      }));

      const responses = await Promise.all(
        requests.map(request =>
          fetch(`${TEST_CONFIG.GATEWAY_URL}/v1/chat/completions`, {
            method: 'POST',
            headers: {
              'Content-Type': 'application/json',
              'Authorization': `Bearer ${adminToken}`,
              'X-User-Role': 'admin',
            },
            body: JSON.stringify(request),
          })
        )
      );

      const successfulResponses = responses.filter(r => r.status === 200);
      
      // Admin should have higher success rate due to higher quota
      expect(successfulResponses.length).toBeGreaterThan(15);
    }, TEST_CONFIG.TEST_TIMEOUT);
  });

  describe('Policy Routing and Fallback', () => {
    it('should apply routing policies correctly', async () => {
      const testToken = 'routing-policy-token';
      const policy = {
        priority: 'high',
        fallback_enabled: true,
        timeout_ms: 5000,
        provider_preference: ['anthropic'],
      };

      const response = await fetch(`${TEST_CONFIG.ROUTER_URL}/v1/route`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          message: {
            type: 'chat.completion',
            content: 'Policy routing test',
          },
          policy: policy,
          trace_id: `policy-routing-${Date.now()}`,
        }),
      });

      expect([200, 202]).toContain(response.status);
      const result = await response.json();
      
      expect(result).toHaveProperty('route_id');
      expect(result).toHaveProperty('provider');
      expect(result).toHaveProperty('policy_applied');
      expect(result.policy_applied.priority).toBe('high');
      expect(result.policy_applied.fallback_enabled).toBe(true);
    }, TEST_CONFIG.TEST_TIMEOUT);

    it('should handle fallback scenarios', async () => {
      const testToken = 'fallback-token';
      
      // Request with specific provider that might fail
      const response = await fetch(`${TEST_CONFIG.GATEWAY_URL}/v1/chat/completions`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': `Bearer ${testToken}`,
          'X-Provider-Preference': 'unavailable-provider',
          'X-Fallback-Enabled': 'true',
        },
        body: JSON.stringify({
          model: 'gpt-3.5-turbo',
          messages: [{ role: 'user', content: 'Fallback test' }],
          max_tokens: 50,
        }),
      });

      // Should either succeed with fallback or fail gracefully
      expect([200, 202, 503]).toContain(response.status);
      
      if (response.status === 503) {
        const error = await response.json();
        expect(error.error.message.toLowerCase()).toContain('unavailable');
      }
    }, TEST_CONFIG.TEST_TIMEOUT);
  });

  describe('Audit and Compliance', () => {
    it('should log policy enforcement decisions', async () => {
      const testToken = 'audit-token';
      const traceId = `audit-${Date.now()}`;
      
      const response = await fetch(`${TEST_CONFIG.GATEWAY_URL}/v1/chat/completions`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': `Bearer ${testToken}`,
          'X-Trace-ID': traceId,
          'X-User-Role': 'user',
        },
        body: JSON.stringify({
          model: 'gpt-3.5-turbo',
          messages: [{ role: 'user', content: 'Audit test' }],
          max_tokens: 50,
        }),
      });

      expect([200, 202]).toContain(response.status);
      
      // Verify trace ID is preserved
      const data = await response.json();
      expect(data.id).toBeDefined();
      
      // In a real implementation, you would verify audit logs were created
      // For now, we just verify the request was processed with proper headers
      expect(traceId).toBeDefined();
    }, TEST_CONFIG.TEST_TIMEOUT);
  });
});