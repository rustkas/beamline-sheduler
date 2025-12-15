import { Test, TestingModule } from '@nestjs/testing';
import { INestApplication } from '@nestjs/common';
import request from 'supertest';
import { ThrottlerModule, ThrottlerGuard } from '@nestjs/throttler';
import { RateLimitGuard } from '../src/common/guards/rate-limit.guard';

// Minimal app to exercise throttling on real HTTP endpoints
import { Controller, Post } from '@nestjs/common';

@Controller('api/v1/messages')
class MessagesController {
  @Post()
  create() {
    return { status: 'accepted' };
  }
}

@Controller('api/v1/routes')
class RoutesController {
  @Post('decide')
  decide() {
    return { provider_id: 'test-provider' };
  }
}

describe('Rate Limiting (e2e)', () => {
  let app: INestApplication;

  beforeAll(async () => {
    const moduleFixture: TestingModule = await Test.createTestingModule({
      imports: [
        ThrottlerModule.forRoot({
          throttlers: [
            {
              ttl: 2000, // 2 seconds in milliseconds
              limit: 3,
            },
          ],
        }),
      ],
      controllers: [MessagesController, RoutesController],
      providers: [
        {
          provide: ThrottlerGuard,
          useClass: RateLimitGuard,
        },
      ],
    }).compile();

    app = moduleFixture.createNestApplication();
    app.useGlobalGuards(app.get(ThrottlerGuard));
    await app.init();
  });

  afterAll(async () => {
    await app.close();
  });

  it('returns 429 with correct headers and body when exceeding global limit', async () => {
    const server = app.getHttpServer();

    // 3 allowed within window
    await request(server).post('/api/v1/messages').set('x-tenant-id', 't-rl').send({}).expect(201);
    await request(server).post('/api/v1/messages').set('x-tenant-id', 't-rl').send({}).expect(201);
    await request(server).post('/api/v1/messages').set('x-tenant-id', 't-rl').send({}).expect(201);

    // 4th should be throttled
    const traceId = 'e2e-trace-456';
    const res = await request(server)
      .post('/api/v1/messages')
      .set('x-tenant-id', 't-rl')
      .set('x-trace-id', traceId)
      .send({})
      .expect(429);

    // Verify rate limit headers
    expect(res.headers['retry-after']).toBeDefined();
    expect(parseInt(res.headers['retry-after'], 10)).toBeGreaterThanOrEqual(1);
    expect(res.headers['x-ratelimit-limit']).toBe('3');
    expect(res.headers['x-ratelimit-remaining']).toBe('0');
    expect(res.headers['x-ratelimit-reset']).toBeDefined();

    // Verify unified JSON error response
    expect(res.body).toMatchObject({
      code: 'RATE_LIMITED',
      message: 'Rate limit exceeded',
      details: {
        tenant_id: 't-rl',
        endpoint: expect.stringContaining('messages'),
        limit: 3,
        remaining: 0,
        window_seconds: expect.any(Number),
        reset_at: expect.any(String),
        trace_id: traceId,
      },
      retry_after_ms: expect.any(Number),
      timestamp: expect.any(String),
    });
  });

  it('isolates rate limits per tenant', async () => {
    const server = app.getHttpServer();

    // Tenant t-a exhausts limit
    await request(server).post('/api/v1/messages').set('x-tenant-id', 't-a').send({}).expect(201);
    await request(server).post('/api/v1/messages').set('x-tenant-id', 't-a').send({}).expect(201);
    await request(server).post('/api/v1/messages').set('x-tenant-id', 't-a').send({}).expect(201);
    await request(server).post('/api/v1/messages').set('x-tenant-id', 't-a').send({}).expect(429);

    // Tenant t-b should still have full quota
    await request(server).post('/api/v1/messages').set('x-tenant-id', 't-b').send({}).expect(201);
    await request(server).post('/api/v1/messages').set('x-tenant-id', 't-b').send({}).expect(201);
    await request(server).post('/api/v1/messages').set('x-tenant-id', 't-b').send({}).expect(201);
  });

  it('isolates rate limits per endpoint', async () => {
    const server = app.getHttpServer();

    // Exhaust messages endpoint for tenant t-c
    await request(server).post('/api/v1/messages').set('x-tenant-id', 't-c').send({}).expect(201);
    await request(server).post('/api/v1/messages').set('x-tenant-id', 't-c').send({}).expect(201);
    await request(server).post('/api/v1/messages').set('x-tenant-id', 't-c').send({}).expect(201);
    await request(server).post('/api/v1/messages').set('x-tenant-id', 't-c').send({}).expect(429);

    // routes/decide should have separate quota
    await request(server).post('/api/v1/routes/decide').set('x-tenant-id', 't-c').send({}).expect(200);
    await request(server).post('/api/v1/routes/decide').set('x-tenant-id', 't-c').send({}).expect(200);
    await request(server).post('/api/v1/routes/decide').set('x-tenant-id', 't-c').send({}).expect(200);
  });

  it('validates TTL units correctly (milliseconds in config, seconds in headers)', async () => {
    const server = app.getHttpServer();

    // Exhaust limit
    await request(server).post('/api/v1/messages').set('x-tenant-id', 't-ttl').send({}).expect(201);
    await request(server).post('/api/v1/messages').set('x-tenant-id', 't-ttl').send({}).expect(201);
    await request(server).post('/api/v1/messages').set('x-tenant-id', 't-ttl').send({}).expect(201);

    const res = await request(server)
      .post('/api/v1/messages')
      .set('x-tenant-id', 't-ttl')
      .send({})
      .expect(429);

    // TTL is 2000ms = 2 seconds, so retry-after should be >= 1 second
    const retryAfter = parseInt(res.headers['retry-after'], 10);
    expect(retryAfter).toBeGreaterThanOrEqual(1);
    expect(retryAfter).toBeLessThanOrEqual(3); // Allow some tolerance

    // window_seconds should match
    expect(res.body.details.window_seconds).toBeGreaterThanOrEqual(1);
    expect(res.body.details.window_seconds).toBeLessThanOrEqual(3);

    // retry_after_ms should be in milliseconds
    expect(res.body.retry_after_ms).toBeGreaterThanOrEqual(1000);
    expect(res.body.retry_after_ms).toBeLessThanOrEqual(3000);
  });
});

