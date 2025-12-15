import { Test, TestingModule } from '@nestjs/testing';
import { INestApplication, ValidationPipe } from '@nestjs/common';
import request from 'supertest';
import { RoutesController } from '../src/routes/routes.controller';
import { RoutesService } from '../src/routes/routes.service';
import { RouterClientService } from '../src/routes/adapters/router-client.service';
import { validRouteRequest, routeRequestWithSticky } from './fixtures/messages.fixture';

describe('RoutesController (e2e)', () => {
  let app: INestApplication;

  beforeEach(async () => {
    const moduleFixture: TestingModule = await Test.createTestingModule({
      controllers: [RoutesController],
      providers: [RoutesService, RouterClientService],
    }).compile();

    app = moduleFixture.createNestApplication();
    app.useGlobalPipes(
      new ValidationPipe({
        whitelist: true,
        forbidNonWhitelisted: true,
        transform: true,
      }),
    );
    await app.init();
  });

  afterEach(async () => {
    await app.close();
  });

  describe('POST /api/v1/routes/decide', () => {
    it('should return route decision (happy path)', () => {
      return request(app.getHttpServer())
        .post('/api/v1/routes/decide')
        .send(validRouteRequest)
        .expect(200)
        .expect((res: any) => {
          expect(res.body).toHaveProperty('provider_id');
          expect(res.body).toHaveProperty('reason');
          expect(res.body).toHaveProperty('priority');
          expect(res.body).toHaveProperty('expected_latency_ms');
          expect(res.body).toHaveProperty('expected_cost');
        });
    });

    it('should return route decision with sticky context', () => {
      return request(app.getHttpServer())
        .post('/api/v1/routes/decide')
        .send(routeRequestWithSticky)
        .expect(200)
        .expect((res: any) => {
          expect(res.body).toHaveProperty('provider_id');
          // Mock returns 'provider-sticky' when context.user_id is present
          expect(res.body.provider_id).toBe('provider-sticky');
        });
    });

    it('should return 400 for invalid request', () => {
      return request(app.getHttpServer())
        .post('/api/v1/routes/decide')
        .send({
          // Missing required message field
          policy_id: 'default',
        })
        .expect(400);
    });

    it('should return 400 for invalid message', () => {
      return request(app.getHttpServer())
        .post('/api/v1/routes/decide')
        .send({
          message: {
            // Missing required fields
            message_id: 'msg_123',
          },
        })
        .expect(400);
    });

    it('should handle optional policy_id', () => {
      return request(app.getHttpServer())
        .post('/api/v1/routes/decide')
        .send({
          message: validRouteRequest.message,
          // No policy_id
        })
        .expect(200);
    });

    it('should handle optional context', () => {
      return request(app.getHttpServer())
        .post('/api/v1/routes/decide')
        .send({
          message: validRouteRequest.message,
          policy_id: 'default',
          // No context
        })
        .expect(200);
    });

    it('should exercise sticky hit/miss with session_id and idempotency caching', async () => {
      const cold = { ...routeRequestWithSticky };
      // Cold: first request
      const res1 = await request(app.getHttpServer())
        .post('/api/v1/routes/decide')
        .send(cold)
        .expect(200);
      expect(res1.body).toHaveProperty('provider_id');

      // Warm: same session → expect same provider (sticky hit)
      const res2 = await request(app.getHttpServer())
        .post('/api/v1/routes/decide')
        .send(cold)
        .expect(200);
      expect(res2.body.provider_id).toBe(res1.body.provider_id);

      // Idempotency: same tenant + same message_id returns cached decision
      const idemReq = { ...validRouteRequest };
      const r1 = await request(app.getHttpServer())
        .post('/api/v1/routes/decide')
        .send(idemReq)
        .expect(200);
      const r2 = await request(app.getHttpServer())
        .post('/api/v1/routes/decide')
        .send(idemReq)
        .expect(200);
      expect(r2.body).toEqual(r1.body);
    });
  });

  describe('GET /api/v1/routes/decide/:messageId', () => {
    it('should return route decision for message ID (happy path)', () => {
      const payload = {
        ...validRouteRequest,
        message: { ...validRouteRequest.message, message_id: 'get_happy_001' },
      };
      return request(app.getHttpServer())
        .post('/api/v1/routes/decide')
        .send(payload)
        .expect(200)
        .then(() =>
          request(app.getHttpServer())
            .get('/api/v1/routes/decide/get_happy_001')
            .expect(200)
            .expect((res: any) => {
              expect(res.body).toHaveProperty('provider_id');
              expect(res.body).toHaveProperty('reason');
              expect(res.body).toHaveProperty('priority');
            }),
        );
    });

    it('should return consistent decision for same message ID', async () => {
      const payload = {
        ...validRouteRequest,
        message: { ...validRouteRequest.message, message_id: 'msg_consistent' },
      };
      await request(app.getHttpServer())
        .post('/api/v1/routes/decide')
        .send(payload)
        .expect(200);

      const response1 = await request(app.getHttpServer())
        .get('/api/v1/routes/decide/msg_consistent')
        .expect(200);

      const response2 = await request(app.getHttpServer())
        .get('/api/v1/routes/decide/msg_consistent')
        .expect(200);

      expect(response1.body).toEqual(response2.body);
    });
  });
});
