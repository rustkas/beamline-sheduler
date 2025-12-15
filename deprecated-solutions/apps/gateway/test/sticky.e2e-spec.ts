import { Test, TestingModule } from '@nestjs/testing';
import { INestApplication } from '@nestjs/common';
import request from 'supertest';
import { AppModule } from '../src/app.module';

describe('Sticky Sessions (e2e)', () => {
  let app: INestApplication;

  beforeEach(async () => {
    process.env.STICKY_SESSION_KEY = 'session_id';
    process.env.STICKY_TTL_MS = '100';
    const moduleFixture: TestingModule = await Test.createTestingModule({
      imports: [AppModule],
    }).compile();
    app = moduleFixture.createNestApplication();
    await app.init();
  });

  afterEach(async () => {
    await app.close();
  });

  it('cold start → miss; warm cache → hit; TTL expiry → miss', async () => {
    const server = app.getHttpServer();
    const base = {
      message: {
        message_id: 'sticky-msg-1',
        tenant_id: 'tenant-sticky',
        message_type: 'chat',
        payload: '{"text":"hi"}',
      },
      context: { session_id: 'sess-1' },
    };

    await request(server).post('/api/v1/routes/decide').send(base).expect(200);
    await request(server).post('/api/v1/routes/decide').send(base).expect(200);

    const store1 = require('../src/observability/internal-metrics.store');
    expect(store1.InternalMetrics.sticky_miss_total).toBeGreaterThanOrEqual(1);
    expect(store1.InternalMetrics.sticky_hits_total).toBeGreaterThanOrEqual(1);

    await new Promise((r) => setTimeout(r, 120));
    await request(server).post('/api/v1/routes/decide').send(base).expect(200);
    const store2 = require('../src/observability/internal-metrics.store');
    expect(store2.InternalMetrics.sticky_miss_total).toBeGreaterThanOrEqual(2);
  });

  it('key rotation changes mapping', async () => {
    const server = app.getHttpServer();
    const base = {
      message: {
        message_id: 'sticky-msg-2',
        tenant_id: 'tenant-sticky2',
        message_type: 'chat',
        payload: '{"text":"hi"}',
      },
      context: { session_id: 'sess-rot' },
    } as any;

    await request(server).post('/api/v1/routes/decide').send(base).expect(200);

    process.env.STICKY_SESSION_KEY = 'session_v2';
    const rotated = { ...base, context: { session_v2: 'sess-rot' } };
    await request(server).post('/api/v1/routes/decide').send(rotated).expect(200);

    const store = require('../src/observability/internal-metrics.store');
    expect(store.InternalMetrics.sticky_miss_total).toBeGreaterThanOrEqual(1);
  });
});