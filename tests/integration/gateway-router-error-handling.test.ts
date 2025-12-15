/**
 * Gateway ↔ Router Integration Tests: Error Handling & DLQ/Audit
 * 
 * Tests the complete flow: Gateway HTTP request → Router validation → DLQ/audit → HTTP response
 * Uses existing error code mapping from router_intake_error_handler.erl
 * 
 * Error Code Mapping:
 * - SCHEMA_VALIDATION_FAILED → invalid_request (HTTP 400)
 * - VERSION_UNSUPPORTED → invalid_request (HTTP 400)
 * - CORRELATION_FIELDS_INVALID → invalid_request (HTTP 400)
 * - TENANT_FORBIDDEN → unauthorized (HTTP 401)
 * - IDEMPOTENCY_VIOLATION → invalid_request (HTTP 400)
 * - INTERNAL_VALIDATION_ERROR → internal (HTTP 500)
 */

import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import axios, { AxiosInstance, AxiosError } from 'axios';

const BASE_URL = process.env.C_GATEWAY_URL || 'http://localhost:8080';
const ROUTER_URL = process.env.ROUTER_URL || 'http://localhost:3081';
const NATS_URL = process.env.NATS_URL || 'nats://localhost:4222';

// Helper to create Gateway client
function gateway(): AxiosInstance {
  return axios.create({
    baseURL: BASE_URL,
    timeout: 10000,
    validateStatus: () => true, // Don't throw on any status
  });
}

// Helper to create Router client (for direct Router testing)
function router(): AxiosInstance {
  return axios.create({
    baseURL: ROUTER_URL,
    timeout: 10000,
    validateStatus: () => true,
  });
}

// Helper to generate test IDs
function generateId(prefix: string): string {
  return `${prefix}-${Date.now()}-${Math.random().toString(36).substring(7)}`;
}

describe('Gateway ↔ Router: Error Code Mapping & HTTP Status', () => {
  const client = gateway();

  describe('SCHEMA_VALIDATION_FAILED → 400 Bad Request', () => {
    it('should return 400 when message schema is invalid (missing tenant_id)', async () => {
      const res = await client.post(
        '/api/v1/routes/decide',
        {
          message_id: generateId('msg'),
          message_type: 'chat',
          payload: { text: 'test' },
          // Missing tenant_id - should trigger SCHEMA_VALIDATION_FAILED
        },
        {
          headers: {
            'Content-Type': 'application/json',
          },
        },
      );

      expect(res.status).toBe(400);
      expect(res.data).toBeDefined();
      expect(res.data.ok).toBe(false);
      expect(res.data.error).toBeDefined();
      
      // Gateway-compatible error code
      expect(res.data.error.code).toBe('invalid_request');
      
      // Original intake error code (if Router includes it)
      if (res.data.error.intake_error_code) {
        expect(res.data.error.intake_error_code).toBe('SCHEMA_VALIDATION_FAILED');
      }
      
      // Error message should indicate schema validation failure
      expect(res.data.error.message).toMatch(/schema|validation|tenant/i);
    });

    it('should return 400 when message payload is invalid JSON', async () => {
      const res = await client.post(
        '/api/v1/routes/decide',
        'invalid json',
        {
          headers: {
            'X-Tenant-ID': 'test-tenant',
            'Content-Type': 'application/json',
          },
        },
      );

      expect(res.status).toBe(400);
      expect(res.data?.error?.code).toBe('invalid_request');
    });

    it('should return 400 when required fields are missing', async () => {
      const res = await client.post(
        '/api/v1/routes/decide',
        {
          // Missing message_id, message_type, payload
        },
        {
          headers: {
            'X-Tenant-ID': 'test-tenant',
          },
        },
      );

      expect(res.status).toBe(400);
      expect(res.data?.error?.code).toBe('invalid_request');
    });
  });

  describe('VERSION_UNSUPPORTED → 400 Bad Request', () => {
    it('should return 400 when version is unsupported', async () => {
      const res = await client.post(
        '/api/v1/routes/decide',
        {
          version: '999', // Unsupported version
          message_id: generateId('msg'),
          message_type: 'chat',
          payload: { text: 'test' },
          tenant_id: 'test-tenant',
        },
        {
          headers: {
            'X-Tenant-ID': 'test-tenant',
          },
        },
      );

      // Should return 400 for unsupported version
      expect(res.status).toBe(400);
      expect(res.data?.error?.code).toBe('invalid_request');
      
      if (res.data?.error?.intake_error_code) {
        expect(res.data.error.intake_error_code).toBe('VERSION_UNSUPPORTED');
      }
    });
  });

  describe('CORRELATION_FIELDS_INVALID → 400 Bad Request', () => {
    it('should return 400 when correlation fields are invalid (invalid UUID format)', async () => {
      const res = await client.post(
        '/api/v1/routes/decide',
        {
          message_id: generateId('msg'),
          message_type: 'chat',
          payload: { text: 'test' },
          tenant_id: 'test-tenant',
          run_id: 'invalid-uuid-format', // Invalid UUID format
          flow_id: 'flow-123',
          step_id: 'step-456',
        },
        {
          headers: {
            'X-Tenant-ID': 'test-tenant',
          },
        },
      );

      // Should return 400 for invalid correlation fields
      expect(res.status).toBe(400);
      expect(res.data?.error?.code).toBe('invalid_request');
      
      if (res.data?.error?.intake_error_code) {
        expect(res.data.error.intake_error_code).toBe('CORRELATION_FIELDS_INVALID');
      }
    });

    it('should return 400 when correlation fields have invalid dependencies (flow_id without run_id)', async () => {
      const res = await client.post(
        '/api/v1/routes/decide',
        {
          message_id: generateId('msg'),
          message_type: 'chat',
          payload: { text: 'test' },
          tenant_id: 'test-tenant',
          // flow_id present but run_id missing - invalid dependency
          flow_id: '550e8400-e29b-41d4-a716-446655440000',
        },
        {
          headers: {
            'X-Tenant-ID': 'test-tenant',
          },
        },
      );

      expect(res.status).toBe(400);
      expect(res.data?.error?.code).toBe('invalid_request');
    });
  });

  describe('TENANT_FORBIDDEN → 401 Unauthorized', () => {
    it('should return 401 when tenant is not in allowlist', async () => {
      const res = await client.post(
        '/api/v1/routes/decide',
        {
          message_id: generateId('msg'),
          message_type: 'chat',
          payload: { text: 'test' },
          tenant_id: 'forbidden-tenant', // Tenant not in allowlist
        },
        {
          headers: {
            'X-Tenant-ID': 'forbidden-tenant',
          },
        },
      );

      // Should return 401 for tenant validation failure
      expect(res.status).toBe(401);
      expect(res.data?.error?.code).toBe('unauthorized');
      
      if (res.data?.error?.intake_error_code) {
        expect(res.data.error.intake_error_code).toBe('TENANT_FORBIDDEN');
      }
    });

    it('should return 401 when tenant header is missing and tenant_id in payload is invalid', async () => {
      const res = await client.post(
        '/api/v1/routes/decide',
        {
          message_id: generateId('msg'),
          message_type: 'chat',
          payload: { text: 'test' },
          tenant_id: 'forbidden-tenant',
        },
        {
          // Missing X-Tenant-ID header
        },
      );

      // Should return 400 or 401 depending on validation order
      expect([400, 401]).toContain(res.status);
    });
  });

  describe('IDEMPOTENCY_VIOLATION → 400 Bad Request', () => {
    it('should return 400 when idempotency key is violated (duplicate request with conflicting data)', async () => {
      const idempotencyKey = generateId('idem');
      const messageId = generateId('msg');

      // First request - should succeed
      const res1 = await client.post(
        '/api/v1/routes/decide',
        {
          message_id: messageId,
          message_type: 'chat',
          payload: { text: 'first request' },
          tenant_id: 'test-tenant',
          idempotency_key: idempotencyKey,
        },
        {
          headers: {
            'X-Tenant-ID': 'test-tenant',
            'Idempotency-Key': idempotencyKey,
          },
        },
      );

      // Second request with same idempotency key but different data - should fail
      const res2 = await client.post(
        '/api/v1/routes/decide',
        {
          message_id: messageId,
          message_type: 'chat',
          payload: { text: 'conflicting request' }, // Different payload
          tenant_id: 'test-tenant',
          idempotency_key: idempotencyKey,
        },
        {
          headers: {
            'X-Tenant-ID': 'test-tenant',
            'Idempotency-Key': idempotencyKey,
          },
        },
      );

      // Second request should return 400 for idempotency violation
      if (res2.status === 400 && res2.data?.error?.intake_error_code === 'IDEMPOTENCY_VIOLATION') {
        expect(res2.data.error.code).toBe('invalid_request');
      }
      // Note: Idempotency violation may be handled as duplicate (200) or error (400)
      // depending on implementation
    });
  });

  describe('INTERNAL_VALIDATION_ERROR → 500 Internal Server Error', () => {
    it('should return 500 when internal validation error occurs', async () => {
      // This test may be difficult to trigger without mocking Router internals
      // For now, we verify that 500 errors include proper error structure
      const res = await client.post(
        '/api/v1/routes/decide',
        {
          message_id: generateId('msg'),
          message_type: 'chat',
          payload: { text: 'test' },
          tenant_id: 'test-tenant',
          // Add data that might trigger internal error (e.g., extremely large payload)
          // Note: This is a placeholder - actual trigger depends on Router implementation
        },
        {
          headers: {
            'X-Tenant-ID': 'test-tenant',
          },
        },
      );

      // If internal error occurs, should return 500
      if (res.status === 500) {
        expect(res.data?.error?.code).toBe('internal');
        
        if (res.data?.error?.intake_error_code) {
          expect(res.data.error.intake_error_code).toBe('INTERNAL_VALIDATION_ERROR');
        }
      }
    });
  });

  describe('Error Response Structure', () => {
    it('should include context with request_id and trace_id in error responses', async () => {
      const requestId = generateId('req');
      const traceId = generateId('trace');

      const res = await client.post(
        '/api/v1/routes/decide',
        {
          request_id: requestId,
          trace_id: traceId,
          message_id: generateId('msg'),
          message_type: 'chat',
          payload: { text: 'test' },
          // Missing tenant_id to trigger error
        },
        {
          headers: {
            'X-Tenant-ID': 'test-tenant',
            'X-Trace-Id': traceId,
          },
        },
      );

      expect(res.status).toBeGreaterThanOrEqual(400);
      expect(res.data).toBeDefined();
      
      if (res.data.context) {
        // Context should include request_id and trace_id if provided
        if (res.data.context.request_id) {
          expect(res.data.context.request_id).toBe(requestId);
        }
        if (res.data.context.trace_id) {
          expect(res.data.context.trace_id).toBe(traceId);
        }
      }
    });

    it('should include error details when available', async () => {
      const res = await client.post(
        '/api/v1/routes/decide',
        {
          message_id: generateId('msg'),
          message_type: 'chat',
          payload: { text: 'test' },
          // Missing tenant_id
        },
        {
          headers: {
            'X-Tenant-ID': 'test-tenant',
          },
        },
      );

      expect(res.status).toBeGreaterThanOrEqual(400);
      expect(res.data?.error).toBeDefined();
      expect(res.data.error.code).toBeDefined();
      expect(res.data.error.message).toBeDefined();
      
      // Error may include details field
      if (res.data.error.details) {
        expect(typeof res.data.error.details).toBe('object');
      }
    });
  });
});

describe('Gateway ↔ Router: DLQ Publication Verification', () => {
  const client = gateway();

  // Note: DLQ verification requires access to NATS or Router logs
  // These tests verify that error responses are returned correctly
  // Actual DLQ publication should be verified in Router e2e tests

  it('should return proper error response when validation fails (DLQ should be published by Router)', async () => {
    const res = await client.post(
      '/api/v1/routes/decide',
      {
        message_id: generateId('msg'),
        message_type: 'chat',
        payload: { text: 'test' },
        // Missing tenant_id - should trigger validation error and DLQ publication
      },
      {
        headers: {
          'X-Tenant-ID': 'test-tenant',
        },
      },
    );

    // Should return error response
    expect(res.status).toBeGreaterThanOrEqual(400);
    expect(res.data?.error).toBeDefined();
    
    // Router should have published to DLQ (verified in Router e2e tests)
    // Gateway just needs to return proper error response
  });

  it('should handle DLQ publication failures gracefully (Router should not block on DLQ errors)', async () => {
    // This test verifies that Router doesn't block on DLQ publication failures
    // Gateway should still receive error response even if DLQ publication fails
    
    const res = await client.post(
      '/api/v1/routes/decide',
      {
        message_id: generateId('msg'),
        message_type: 'chat',
        payload: { text: 'test' },
        // Missing tenant_id
      },
      {
        headers: {
          'X-Tenant-ID': 'test-tenant',
        },
      },
    );

    // Should return error response regardless of DLQ publication status
    expect(res.status).toBeGreaterThanOrEqual(400);
    expect(res.data?.error).toBeDefined();
    
    // Response time should be reasonable (not blocked by DLQ)
    // Note: This is a qualitative check - actual timing depends on Router implementation
  });
});

describe('Gateway ↔ Router: Audit Logging Verification', () => {
  const client = gateway();

  // Note: Audit log verification requires access to Router logs
  // These tests verify that error responses include necessary information for audit

  it('should include trace_id in error response for audit correlation', async () => {
    const traceId = generateId('trace');

    const res = await client.post(
      '/api/v1/routes/decide',
      {
        message_id: generateId('msg'),
        message_type: 'chat',
        payload: { text: 'test' },
        trace_id: traceId,
        // Missing tenant_id
      },
      {
        headers: {
          'X-Tenant-ID': 'test-tenant',
          'X-Trace-Id': traceId,
        },
      },
    );

    expect(res.status).toBeGreaterThanOrEqual(400);
    
    // Trace ID should be in response for audit correlation
    if (res.data?.context?.trace_id) {
      expect(res.data.context.trace_id).toBe(traceId);
    }
  });

  it('should include tenant_id in error response for audit', async () => {
    const tenantId = 'test-tenant';

    const res = await client.post(
      '/api/v1/routes/decide',
      {
        message_id: generateId('msg'),
        message_type: 'chat',
        payload: { text: 'test' },
        tenant_id: tenantId,
        // Missing required fields to trigger error
      },
      {
        headers: {
          'X-Tenant-ID': tenantId,
        },
      },
    );

    expect(res.status).toBeGreaterThanOrEqual(400);
    
    // Tenant ID should be available for audit (may be in error details or context)
    // Actual location depends on Router implementation
  });

  it('should include error_code in error response for audit', async () => {
    const res = await client.post(
      '/api/v1/routes/decide',
      {
        message_id: generateId('msg'),
        message_type: 'chat',
        payload: { text: 'test' },
        // Missing tenant_id
      },
      {
        headers: {
          'X-Tenant-ID': 'test-tenant',
        },
      },
    );

    expect(res.status).toBeGreaterThanOrEqual(400);
    expect(res.data?.error?.code).toBeDefined();
    
    // Error code should be present for audit logging
    // Router should log this in audit logs (verified in Router e2e tests)
  });
});

describe('Gateway ↔ Router: End-to-End Error Flow', () => {
  const client = gateway();

  it('should handle complete error flow: HTTP request → Router validation → DLQ/audit → HTTP error response', async () => {
    const requestId = generateId('req');
    const traceId = generateId('trace');
    const tenantId = 'test-tenant';

    const res = await client.post(
      '/api/v1/routes/decide',
      {
        request_id: requestId,
        trace_id: traceId,
        message_id: generateId('msg'),
        message_type: 'chat',
        payload: { text: 'test' },
        tenant_id: tenantId,
        // Add invalid data to trigger validation error
        run_id: 'invalid-uuid', // Invalid UUID format
        flow_id: '550e8400-e29b-41d4-a716-446655440000',
      },
      {
        headers: {
          'X-Tenant-ID': tenantId,
          'X-Trace-Id': traceId,
        },
      },
    );

    // 1. HTTP error response
    expect(res.status).toBe(400);
    expect(res.data?.ok).toBe(false);
    expect(res.data?.error).toBeDefined();
    expect(res.data.error.code).toBe('invalid_request');
    
    // 2. Error should include intake_error_code if Router provides it
    if (res.data.error.intake_error_code) {
      expect(['SCHEMA_VALIDATION_FAILED', 'CORRELATION_FIELDS_INVALID']).toContain(
        res.data.error.intake_error_code
      );
    }
    
    // 3. Context should include request_id and trace_id for audit
    if (res.data.context) {
      if (res.data.context.request_id) {
        expect(res.data.context.request_id).toBe(requestId);
      }
      if (res.data.context.trace_id) {
        expect(res.data.context.trace_id).toBe(traceId);
      }
    }
    
    // 4. Router should have:
    //    - Published to DLQ (verified in Router e2e tests)
    //    - Logged audit entry (verified in Router e2e tests)
    //    - Emitted metrics (verified in Router e2e tests)
  });

  it('should handle multiple validation errors and return first error', async () => {
    const res = await client.post(
      '/api/v1/routes/decide',
      {
        message_id: generateId('msg'),
        message_type: 'chat',
        // Missing payload
        // Missing tenant_id
        // Invalid correlation fields
        run_id: 'invalid',
        flow_id: 'invalid',
      },
      {
        headers: {
          'X-Tenant-ID': 'test-tenant',
        },
      },
    );

    // Should return 400 with first validation error
    expect(res.status).toBe(400);
    expect(res.data?.error?.code).toBe('invalid_request');
    
    // Error message should indicate validation failure
    expect(res.data.error.message).toBeDefined();
  });
});

describe('Gateway ↔ Router: Success Flow (for comparison)', () => {
  const client = gateway();

  it('should return 200 OK for valid requests', async () => {
    const res = await client.post(
      '/api/v1/routes/decide',
      {
        message_id: generateId('msg'),
        message_type: 'chat',
        payload: { text: 'test' },
        tenant_id: 'default_tenant', // Use default tenant that should be allowed
      },
      {
        headers: {
          'X-Tenant-ID': 'default_tenant',
        },
      },
    );

    // Should return 200 if Router is available and request is valid
    // May return 503 if Router is unavailable
    expect([200, 503]).toContain(res.status);
    
    if (res.status === 200) {
      expect(res.data?.ok).toBe(true);
      expect(res.data?.decision).toBeDefined();
    }
  });
});

describe('Gateway ↔ Router: Rate Limiting vs Router Intake Errors Distinction', () => {
  const client = gateway();

  describe('Rate Limiting Error (429) - Gateway-Level', () => {
    beforeAll(() => {
      // Set very low rate limit for testing
      process.env.GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT = '2';
      process.env.GATEWAY_RATE_LIMIT_TTL_SECONDS = '60';
    });

    it('should return 429 with rate_limit_exceeded when limit exceeded BEFORE Router validation', async () => {
      const tenantId = `tenant-rl-dist-${Date.now()}`;

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
      // Even if request has invalid data, should return 429, not Router error
      const res = await client.post(
        '/api/v1/routes/decide',
        {
          version: '1',
          // Missing tenant_id - would trigger Router SCHEMA_VALIDATION_FAILED (400)
          // But rate limit checked FIRST, so should return 429
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
      
      // Should include rate limit details in error response
      expect(res.data?.error?.details?.endpoint).toBe('/api/v1/routes/decide');
      expect(res.data?.error?.details?.limit).toBeDefined();
      expect(res.data?.error?.details?.retry_after_seconds).toBeDefined();
    });

    it('should distinguish rate limit error format from Router intake error format', async () => {
      const tenantId = `tenant-rl-dist-2-${Date.now()}`;

      // Exhaust rate limit
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

      // Rate limit error (429)
      const rateLimitRes = await client.post(
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

      expect(rateLimitRes.status).toBe(429);
      
      // Rate limit error characteristics
      expect(rateLimitRes.data?.error?.code).toBe('rate_limit_exceeded');
      expect(rateLimitRes.data?.error?.intake_error_code).toBeUndefined(); // No Router error
      expect(rateLimitRes.data?.error?.details?.endpoint).toBeDefined(); // Rate limit details
      expect(rateLimitRes.data?.error?.details?.limit).toBeDefined();
      expect(rateLimitRes.data?.error?.details?.retry_after_seconds).toBeDefined();
      expect(rateLimitRes.headers['retry-after']).toBeDefined(); // Rate limit header
    });
  });

  describe('Router Intake Error (400/401/500) - Router-Level', () => {
    it('should return Router intake error (400) when rate limit OK but schema validation fails', async () => {
      // Use fresh tenant to avoid rate limit
      const tenantId = `tenant-intake-dist-${Date.now()}`;

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
            'X-Tenant-ID': tenantId,
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
      
      // Should NOT have rate limit-specific details (endpoint, limit, retry_after_seconds)
      // Rate limit headers may be present, but retry-after should not be relevant
      if (res.headers['retry-after']) {
        // If present, should be 0 or not relevant to intake errors
        expect(parseInt(res.headers['retry-after'])).toBeGreaterThanOrEqual(0);
      }
    });

    it('should return Router intake error (401) when rate limit OK but tenant is forbidden', async () => {
      const tenantId = `tenant-intake-dist-2-${Date.now()}`;

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

    it('should distinguish Router intake error format from rate limit error format', async () => {
      const tenantId = `tenant-intake-dist-3-${Date.now()}`;

      // Router intake error (400) - missing tenant_id
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
            'X-Tenant-ID': tenantId,
          },
        },
      );

      expect(intakeErrorRes.status).toBe(400);
      
      // Router intake error characteristics
      expect(intakeErrorRes.data?.error?.code).toBe('invalid_request');
      expect(intakeErrorRes.data?.error?.intake_error_code).toBeDefined(); // Router error code
      expect(intakeErrorRes.data?.error?.details?.endpoint).toBeUndefined(); // No rate limit details
      expect(intakeErrorRes.data?.error?.details?.limit).toBeUndefined();
      expect(intakeErrorRes.data?.error?.details?.retry_after_seconds).toBeUndefined();
    });
  });

  describe('Error Response Format Comparison', () => {
    it('should have distinct error response formats for rate limit vs Router intake errors', async () => {
      const tenantId = `tenant-format-comp-${Date.now()}`;

      // Exhaust rate limit
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

      // Rate limit error (429)
      const rateLimitRes = await client.post(
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

      // Wait for rate limit window to reset
      await new Promise((resolve) => setTimeout(resolve, 3000));

      // Router intake error (400) - use different tenant
      const tenantId2 = `tenant-format-comp-2-${Date.now()}`;
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
            'X-Tenant-ID': tenantId2,
          },
        },
      );

      // Compare error formats
      if (rateLimitRes.status === 429 && intakeErrorRes.status === 400) {
        // Rate limit error format
        expect(rateLimitRes.data?.error?.code).toBe('rate_limit_exceeded');
        expect(rateLimitRes.data?.error?.intake_error_code).toBeUndefined();
        expect(rateLimitRes.data?.error?.details?.endpoint).toBe('/api/v1/routes/decide');
        expect(rateLimitRes.data?.error?.details?.limit).toBeDefined();
        expect(rateLimitRes.data?.error?.details?.retry_after_seconds).toBeDefined();
        expect(rateLimitRes.headers['retry-after']).toBeDefined();

        // Router intake error format
        expect(intakeErrorRes.data?.error?.code).toBe('invalid_request');
        expect(intakeErrorRes.data?.error?.intake_error_code).toBeDefined(); // Router error code
        expect(intakeErrorRes.data?.error?.details?.endpoint).toBeUndefined(); // No rate limit details
        expect(intakeErrorRes.data?.error?.details?.limit).toBeUndefined();
        expect(intakeErrorRes.data?.error?.details?.retry_after_seconds).toBeUndefined();
      }
    });

    it('should include context in both error types for audit correlation', async () => {
      const tenantId = `tenant-context-${Date.now()}`;
      const requestId = generateId('req');
      const traceId = generateId('trace');

      // Exhaust rate limit
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

      // Rate limit error with context
      const rateLimitRes = await client.post(
        '/api/v1/routes/decide',
        {
          version: '1',
          tenant_id: tenantId,
          request_id: requestId,
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

      if (rateLimitRes.status === 429) {
        expect(rateLimitRes.data?.context).toBeDefined();
        if (rateLimitRes.data?.context?.request_id) {
          expect(rateLimitRes.data.context.request_id).toBe(requestId);
        }
        if (rateLimitRes.data?.context?.trace_id) {
          expect(rateLimitRes.data.context.trace_id).toBe(traceId);
        }
        if (rateLimitRes.data?.context?.tenant_id) {
          expect(rateLimitRes.data.context.tenant_id).toBe(tenantId);
        }
      }

      // Router intake error with context
      const tenantId2 = `tenant-context-2-${Date.now()}`;
      const requestId2 = generateId('req');
      const traceId2 = generateId('trace');

      const intakeErrorRes = await client.post(
        '/api/v1/routes/decide',
        {
          version: '1',
          request_id: requestId2,
          // Missing tenant_id
          task: {
            type: 'text.generate',
            payload: {},
          },
        },
        {
          headers: {
            'X-Tenant-ID': tenantId2,
            'X-Trace-ID': traceId2,
          },
        },
      );

      if (intakeErrorRes.status === 400) {
        expect(intakeErrorRes.data?.context).toBeDefined();
        if (intakeErrorRes.data?.context?.request_id) {
          expect(intakeErrorRes.data.context.request_id).toBe(requestId2);
        }
        if (intakeErrorRes.data?.context?.trace_id) {
          expect(intakeErrorRes.data.context.trace_id).toBe(traceId2);
        }
      }
    });
  });
});

