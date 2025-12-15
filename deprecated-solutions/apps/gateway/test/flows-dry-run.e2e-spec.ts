import { Test } from '@nestjs/testing';
import { INestApplication, ValidationPipe } from '@nestjs/common';
import request from 'supertest';
import { AppModule } from '../src/app.module';

describe('/api/v1/flows/dry-run (e2e)', () => {
  let app: INestApplication;

  beforeAll(async () => {
    const moduleFixture = await Test.createTestingModule({ imports: [AppModule] }).compile();
    app = moduleFixture.createNestApplication();
    app.useGlobalPipes(new ValidationPipe({ whitelist: true, transform: true }));
    await app.init();
  });

  afterAll(async () => {
    await app.close();
  });

  it('should compile and simulate valid flow', async () => {
    const flow = {
      id: 'f1',
      version: 'v1',
      steps: [
        { id: 's1', type: 'http.request', inputs: { url: 'https://example.com' } },
        { id: 's2', type: 'sql.query', inputs: { sql: 'select 1' } },
      ],
      edges: [ { from: 's1', to: 's2' } ],
    };
    const res = await request(app.getHttpServer())
      .post('/api/v1/flows/dry-run')
      .send({ definition: flow })
      .expect(200);
    expect(res.body.compiled).toBe(true);
    expect(res.body.steps.length).toBe(2);
    expect(res.body.total_latency_ms).toBeGreaterThan(0);
  });

  it('should report compile errors for invalid flow', async () => {
    const flow = {
      id: 'f2',
      version: 'v1',
      steps: [ { id: 's1', type: 'http.request' } ],
      edges: [ { from: 's1', to: 'missing' } ],
    };
    const res = await request(app.getHttpServer())
      .post('/api/v1/flows/dry-run')
      .send({ definition: flow })
      .expect(200);
    expect(res.body.compiled).toBe(false);
    expect(res.body.compile_errors.length).toBeGreaterThan(0);
    expect(res.body.steps.length).toBe(0);
  });
});
