import { Test, TestingModule } from '@nestjs/testing';
import { INestApplication, ValidationPipe } from '@nestjs/common';
import request from 'supertest';
import { AppModule } from '../src/app.module';

describe('CP-phase Metrics (e2e)', () => {
  let app: INestApplication;

  beforeEach(async () => {
    const moduleFixture: TestingModule = await Test.createTestingModule({
      imports: [AppModule],
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

  it('exposes cp_phase labels in /metrics after a request', async () => {
    const server = app.getHttpServer();
    const payload = {
      message: {
        message_id: 'cp-metrics-1',
        tenant_id: 'tenant-m',
        message_type: 'chat',
        payload: '{"text":"hi"}',
      },
    };

    await request(server)
      .post('/api/v1/routes/decide')
      .set('x-cp-phase', 'cp2')
      .send(payload)
      .expect(200);

    const res = await request(server).get('/metrics').expect(200);
    const text: string = (res as any).text || (res as any).body || '';
    expect(text).toContain('beamline_gateway_cp_requests_total');
    expect(text).toContain('cp_phase="cp2"');
    expect(text).toContain('beamline_gateway_cp_latency_seconds');
  });
});