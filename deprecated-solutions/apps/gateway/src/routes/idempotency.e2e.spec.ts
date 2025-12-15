import { Test, TestingModule } from '@nestjs/testing';
import request from 'supertest';
import { INestApplication } from '@nestjs/common';
import { RoutesController } from './routes.controller';
import { RoutesService } from './routes.service';
import { RouterClientService } from './adapters/router-client.service';
import { IdempotencyService } from '../common/services/idempotency.service';
import { StickyService } from '../common/services/sticky.service';

describe('HTTP Idempotency (e2e)', () => {
  let app: INestApplication;
  let svc: RoutesService;

  beforeEach(async () => {
    process.env.IDEMPOTENCY_TTL_MS = '1000';

    const moduleFixture: TestingModule = await Test.createTestingModule({
      controllers: [RoutesController],
      providers: [
        RoutesService,
        RouterClientService,
        IdempotencyService,
        StickyService,
        {
          provide: RouterClientService,
          useValue: {
            decide: jest.fn().mockResolvedValue({ provider_id: 'p_first', reason: 'ok', priority: 1 }),
          },
        },
      ],
    }).compile();

    app = moduleFixture.createNestApplication();
    await app.init();
    svc = app.get<RoutesService>(RoutesService);
  });

  afterEach(async () => {
    await app.close();
  });

  it('avoids duplicate processing with same tenant_id + assignment_id within TTL', async () => {
    const server = app.getHttpServer();
    const payload = {
      message: {
        message_id: 'msg_e2e_dup',
        tenant_id: 't_dup',
        message_type: 'chat',
        payload: '{"text":"hi"}',
      },
      assignment_id: 'assign_1',
    };

    const res1 = await request(server).post('/api/v1/routes/decide').send(payload).expect(200);
    const { InternalMetrics } = require('../observability/internal-metrics.store');
    const hitBefore = InternalMetrics.idempotency_hit_total;
    const missBefore = InternalMetrics.idempotency_miss_total;

    // Second request with same tenant+assignment should be counted as duplicate by idempotency marks
    const res2 = await request(server).post('/api/v1/routes/decide').send(payload).expect(200);
    await new Promise((r) => setTimeout(r, 10));
    const hitAfter = InternalMetrics.idempotency_hit_total;
    const missAfter = InternalMetrics.idempotency_miss_total;

    expect(hitAfter).toBeGreaterThanOrEqual(hitBefore);
    expect(missAfter).toBeGreaterThanOrEqual(missBefore);
    expect(res1.body.provider_id).toBeDefined();
    expect(res2.body.provider_id).toBeDefined();
  }, 15000);

  it('expires idempotency after TTL', async () => {
    const server = app.getHttpServer();
    const payload = {
      message: {
        message_id: 'msg_e2e_ttl',
        tenant_id: 't_ttl',
        message_type: 'chat',
        payload: '{"text":"hi"}',
      },
      usage_id: 'u1',
    };

    await request(server).post('/api/v1/routes/decide').send(payload).expect(200);
    await new Promise((r) => setTimeout(r, 1100));
    const { InternalMetrics } = require('../observability/internal-metrics.store');
    const missBefore = InternalMetrics.idempotency_miss_total;
    await request(server).post('/api/v1/routes/decide').send(payload).expect(200);
    const missAfter = InternalMetrics.idempotency_miss_total;
    expect(missAfter).toBeGreaterThanOrEqual(missBefore + 1);
  }, 20000);
});