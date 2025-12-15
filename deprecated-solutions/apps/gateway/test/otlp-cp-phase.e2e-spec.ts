import { Test, TestingModule } from '@nestjs/testing';
import { INestApplication } from '@nestjs/common';
import request from 'supertest';
import http from 'http';
import { AppModule } from '../src/app.module';

function startMockOTLPServer(port: number): Promise<{ server: http.Server; traces: string[] }> {
  const traces: string[] = [];
  const server = http.createServer((req, res) => {
    if (req.method === 'POST' && req.url && req.url.includes('/v1/traces')) {
      let body = '';
      req.on('data', (chunk) => (body += chunk.toString('utf-8')));
      req.on('end', () => {
        traces.push(body);
        res.writeHead(200);
        res.end('OK');
      });
      return;
    }
    res.writeHead(404);
    res.end();
  });
  return new Promise((resolve) => server.listen(port, '127.0.0.1', () => resolve({ server, traces })));
}

describe('OTLP cp_phase propagation (e2e)', () => {
  let app: INestApplication;
  let otlp: { server: http.Server; traces: string[] };

  beforeEach(async () => {
    otlp = await startMockOTLPServer(4318);
    process.env.OTLP_ENDPOINT = 'http://127.0.0.1:4318';
    const moduleFixture: TestingModule = await Test.createTestingModule({ imports: [AppModule] }).compile();
    app = moduleFixture.createNestApplication();
    await app.init();
  });

  afterEach(async () => {
    await app.close();
    await new Promise((r) => otlp.server.close(r));
  });

  it('includes cp_phase in HTTP and NATS spans', async () => {
    const server = app.getHttpServer();
    const payload = {
      message: {
        message_id: 'otlp-cp2-1',
        tenant_id: 'tenant-otlp',
        message_type: 'chat',
        payload: '{"text":"hi"}',
      },
    };
    await request(server).post('/api/v1/routes/decide').set('x-cp-phase', 'cp2').send(payload).expect(200);

    await new Promise((r) => setTimeout(r, 500));
    const joined = otlp.traces.join('\n');
    expect(joined).toContain('beamline-gateway');
    expect(joined).toMatch(/cp_phase.*cp2/);
  });
});