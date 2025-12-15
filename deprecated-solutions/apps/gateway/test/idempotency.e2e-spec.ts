import { Test, TestingModule } from '@nestjs/testing';
import { INestApplication } from '@nestjs/common';
import request from 'supertest';
import { AppModule } from '../src/app.module';

describe('HTTP Idempotency (e2e)', () => {
  let app: INestApplication;

  beforeEach(async () => {
    const moduleFixture: TestingModule = await Test.createTestingModule({
      imports: [AppModule],
    }).compile();

    app = moduleFixture.createNestApplication();
    await app.init();
  });

  afterEach(async () => {
    await app.close();
  });

  it('same tenant + same assignment_id increments idempotency hit/miss counters', async () => {
    const payload = {
      assignment_id: 'idem-001',
      message: {
        message_id: 'm-idem-1',
        tenant_id: 'tenant-A',
        message_type: 'chat',
        payload: '{"text":"hello"}',
      },
    };

    const server = app.getHttpServer();

    await request(server).post('/api/v1/routes/decide').send(payload).expect(200);
    await request(server).post('/api/v1/routes/decide').send(payload).expect(200);

    const store = require('../src/observability/internal-metrics.store');
    expect(store.InternalMetrics.idempotency_miss_total).toBeGreaterThan(0);
    expect(store.InternalMetrics.idempotency_hit_total).toBeGreaterThan(0);
  });

  it('different tenant + same assignment_id do not collide', async () => {
    const server = app.getHttpServer();
    const base = {
      assignment_id: 'idem-002',
      message: {
        message_id: 'm-idem-2',
        message_type: 'chat',
        payload: '{"text":"hello"}',
      },
    } as any;

    await request(server)
      .post('/api/v1/routes/decide')
      .send({ ...base, message: { ...base.message, tenant_id: 'tenant-A' } })
      .expect(200);

    await request(server)
      .post('/api/v1/routes/decide')
      .send({ ...base, message: { ...base.message, tenant_id: 'tenant-B' } })
      .expect(200);

    const store = require('../src/observability/internal-metrics.store');
    // Expect at least two misses due to separate tenants
    expect(store.InternalMetrics.idempotency_miss_total).toBeGreaterThanOrEqual(2);
  });
});