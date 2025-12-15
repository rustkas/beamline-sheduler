import { describe, it, expect } from 'vitest';
import axios from 'axios';

const BASE_URL = process.env.C_GATEWAY_URL || 'http://localhost:8080';

function gateway() {
  return axios.create({
    baseURL: BASE_URL,
    timeout: 5000,
    validateStatus: () => true,
  });
}

describe('C-Gateway HTTP integration: /api/v1/routes/decide', () => {
  const client = gateway();

  it('POST /api/v1/routes/decide - happy path (stub or real)', async () => {
    const res = await client.post(
      '/api/v1/routes/decide',
      {
        message_id: 'msg-int-001',
        message_type: 'chat',
        payload: { text: 'hello from test' },
        metadata: {},
        policy_id: 'default',
        context: {},
      },
      {
        headers: {
          'X-Tenant-ID': 'tenant-int',
        },
      },
    );

    // Depending on environment we can get 200 (Router stub/real) or 503 if Router is down.
    expect([200, 503]).toContain(res.status);
    if (res.status === 200) {
      expect(res.headers['content-type']).toMatch(/application\/json/);
      expect(res.data).toBeDefined();
    }
  });

  it('POST /api/v1/routes/decide - missing tenant header -> 400', async () => {
    const res = await client.post('/api/v1/routes/decide', {
      message_id: 'msg-int-002',
      message_type: 'chat',
      payload: {},
    });

    expect(res.status).toBe(400);
    expect(res.data).toBeDefined();
    // error.code from send_error_response
    expect(res.data.error?.code).toBe('invalid_request');
  });

  it('POST /api/v1/routes/decide - empty body -> 400', async () => {
    const res = await client.post(
      '/api/v1/routes/decide',
      '',
      {
        headers: {
          'X-Tenant-ID': 'tenant-int',
          'Content-Type': 'application/json',
        },
      },
    );

    // http_server.c treats empty body as invalid_request
    expect(res.status).toBe(400);
  });
});

describe('C-Gateway HTTP integration: GET /api/v1/routes/decide/:message_id', () => {
  const client = gateway();

  it('GET /api/v1/routes/decide/:message_id - missing tenant -> 400', async () => {
    const res = await client.get('/api/v1/routes/decide/msg-int-003');

    expect(res.status).toBe(400);
    expect(res.data?.error?.code).toBe('invalid_request');
  });

  it('GET /api/v1/routes/decide/:message_id - with tenant header', async () => {
    const res = await client.get('/api/v1/routes/decide/msg-int-004', {
      headers: {
        'X-Tenant-ID': 'tenant-int',
      },
    });

    // With real Router we expect 200/404, with Router down we may get 503
    expect([200, 404, 503]).toContain(res.status);
    if (res.status === 200) {
      expect(res.headers['content-type']).toMatch(/application\/json/);
    }
  });
});
