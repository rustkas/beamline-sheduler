import { ThrottlerGuard, ThrottlerModule, ThrottlerModuleOptions } from '@nestjs/throttler';
import { Test } from '@nestjs/testing';
import { INestApplication } from '@nestjs/common';
import request from 'supertest';
import { RateLimitGuard } from './rate-limit.guard';

// Minimal controller to exercise guard behavior
import { Controller, Get } from '@nestjs/common';

@Controller('rl-spec')
class RlSpecController {
  @Get('ping')
  ping() {
    return { ok: true };
  }

  @Get('pong')
  pong() {
    return { ok: true };
  }
}

describe('RateLimitGuard (unit)', () => {
  let app: INestApplication;

  beforeAll(async () => {
    const throttlerOptions: ThrottlerModuleOptions = {
      throttlers: [
        {
          ttl: 2000, // 2 second window for tests (in milliseconds)
          limit: 2, // allow 2 requests per window
        },
      ],
    };

    const moduleRef = await Test.createTestingModule({
      imports: [ThrottlerModule.forRoot(throttlerOptions)],
      controllers: [RlSpecController],
      providers: [
        {
          provide: ThrottlerGuard,
          useClass: RateLimitGuard,
        },
      ],
    }).compile();

    app = moduleRef.createNestApplication();
    app.useGlobalGuards(app.get(ThrottlerGuard));
    await app.init();
  });

  afterAll(async () => {
    await app.close();
  });

  it('allows requests under limit', async () => {
    await request(app.getHttpServer()).get('/rl-spec/ping').set('x-tenant-id', 't-1').expect(200);
    await request(app.getHttpServer()).get('/rl-spec/ping').set('x-tenant-id', 't-1').expect(200);
  });

  it('returns 429 with correct headers and body on exceeding limit', async () => {
    // Two allowed; third should be throttled within same window
    await request(app.getHttpServer()).get('/rl-spec/ping').set('x-tenant-id', 't-2').expect(200);
    await request(app.getHttpServer()).get('/rl-spec/ping').set('x-tenant-id', 't-2').expect(200);

    const traceId = 'test-trace-123';
    const res = await request(app.getHttpServer())
      .get('/rl-spec/ping')
      .set('x-tenant-id', 't-2')
      .set('x-trace-id', traceId)
      .expect(429);

    // Verify rate limit headers use actual limit/TTL from throttlerLimitDetail
    expect(res.headers['retry-after']).toBeDefined();
    expect(parseInt(res.headers['retry-after'], 10)).toBeGreaterThanOrEqual(1); // At least 1 second (2s TTL / 1000ms)
    expect(res.headers['x-ratelimit-limit']).toBe('2'); // Actual limit from throttler
    expect(res.headers['x-ratelimit-remaining']).toBe('0');
    expect(res.headers['x-ratelimit-reset']).toBeDefined();
    const resetAt = parseInt(res.headers['x-ratelimit-reset'], 10);
    expect(resetAt).toBeGreaterThan(Math.floor(Date.now() / 1000));

    // Verify unified JSON error response
    expect(res.body).toMatchObject({
      code: 'RATE_LIMITED',
      message: 'Rate limit exceeded',
      details: {
        tenant_id: 't-2',
        endpoint: expect.any(String),
        limit: 2,
        remaining: 0,
        window_seconds: expect.any(Number),
        reset_at: expect.any(String),
        trace_id: traceId,
      },
      retry_after_ms: expect.any(Number),
      timestamp: expect.any(String),
    });

    // Verify window_seconds matches TTL (2 seconds = 2000ms)
    expect(res.body.details.window_seconds).toBeGreaterThanOrEqual(1);
    expect(res.body.retry_after_ms).toBeGreaterThanOrEqual(1000);
  });

  it('tracks per-tenant keys independently', async () => {
    // Tenant t-3 should not be affected by t-2 counters
    await request(app.getHttpServer()).get('/rl-spec/ping').set('x-tenant-id', 't-3').expect(200);
    await request(app.getHttpServer()).get('/rl-spec/ping').set('x-tenant-id', 't-3').expect(200);
  });

  it('tracks per-endpoint keys independently', async () => {
    // Different endpoints should have separate counters
    await request(app.getHttpServer()).get('/rl-spec/ping').set('x-tenant-id', 't-4').expect(200);
    await request(app.getHttpServer()).get('/rl-spec/ping').set('x-tenant-id', 't-4').expect(200);
    await request(app.getHttpServer()).get('/rl-spec/ping').set('x-tenant-id', 't-4').expect(429); // ping exhausted

    // pong should still work (different endpoint)
    await request(app.getHttpServer()).get('/rl-spec/pong').set('x-tenant-id', 't-4').expect(200);
    await request(app.getHttpServer()).get('/rl-spec/pong').set('x-tenant-id', 't-4').expect(200);
  });

  it('handles missing trace_id gracefully', async () => {
    await request(app.getHttpServer()).get('/rl-spec/ping').set('x-tenant-id', 't-5').expect(200);
    await request(app.getHttpServer()).get('/rl-spec/ping').set('x-tenant-id', 't-5').expect(200);

    const res = await request(app.getHttpServer())
      .get('/rl-spec/ping')
      .set('x-tenant-id', 't-5')
      .expect(429);

    // trace_id should be undefined if not provided
    expect(res.body.details.trace_id).toBeUndefined();
  });

  it('uses anonymous tenant when no tenant header provided', async () => {
    await request(app.getHttpServer()).get('/rl-spec/ping').expect(200);
    await request(app.getHttpServer()).get('/rl-spec/ping').expect(200);

    const res = await request(app.getHttpServer()).get('/rl-spec/ping').expect(429);
    expect(res.body.details.tenant_id).toBe('anonymous');
  });
});
