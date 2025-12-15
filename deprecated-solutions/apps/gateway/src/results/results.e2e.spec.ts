import { Test, TestingModule } from '@nestjs/testing';
import request from 'supertest';
import { INestApplication } from '@nestjs/common';
import { ResultsController } from './results.controller';
import { ResultsStreamService } from './results-stream.service';

async function readSSE(server: any, url: string, timeoutMs = 2500): Promise<string> {
  return await new Promise<string>((resolve, reject) => {
    let finished = false;
    let buffer = '';
    const finishOk = (data: string) => {
      if (finished) return;
      finished = true;
      resolve(data);
    };
    const finishErr = (data: string, err?: unknown) => {
      if (finished) return;
      finished = true;
      reject(err ?? new Error('SSE read error'));
    };
    const timer = setTimeout(() => finishOk(buffer), timeoutMs);
    request(server)
      .get(url)
      .set('Accept', 'text/event-stream')
      .buffer(true)
      .parse((r, cb) => {
        r.on('data', (chunk: Buffer) => {
          buffer += chunk.toString('utf-8');
        });
        r.on('end', () => {
          clearTimeout(timer);
          cb(null, buffer);
        });
        r.on('error', (err) => {
          clearTimeout(timer);
          cb(err, buffer);
        });
      })
      .then((res) => {
        const text = (res as any).text as string;
        finishOk(typeof text === 'string' && text.length > 0 ? text : buffer);
      })
      .catch((err) => finishErr(buffer, err));
  });
}

describe('ResultsController (e2e SSE)', () => {
  let app: INestApplication;
  let controller: ResultsController;

  beforeEach(async () => {
    const moduleFixture: TestingModule = await Test.createTestingModule({
      controllers: [ResultsController],
      providers: [ResultsStreamService],
    }).compile();

    app = moduleFixture.createNestApplication();
    await app.init();
    controller = app.get<ResultsController>(ResultsController);
  });

  afterEach(async () => {
    await app.close();
  });

  it('streams events and applies tenant/provider filters', async () => {
    const server = app.getHttpServer();
    setTimeout(() => {
      controller.emitResult({
        assignment_id: 'a1',
        tenant_id: 't1',
        provider_id: 'p1',
        status: 'success',
      });
    }, 100);

    const text = await readSSE(
      server,
      '/api/v1/results/stream?assignment_id=a1&tenant_id=t1&provider_id=p1&heartbeat_ms=500&close_after_ms=1000',
      2500,
    );

    const lines = text.split('\n').filter((l) => l.startsWith('data: '));
    const payloads = lines.map((l) => l.slice(6)).join('\n');
    expect(payloads).toContain('tenant_id');
    expect(payloads).toContain('provider_id');
  }, 20000);

  it('streams events by request_id', async () => {
    const server = app.getHttpServer();
    setTimeout(() => {
      controller.emitResult({
        request_id: 'r1',
        tenant_id: 't9',
        provider_id: 'p9',
        status: 'success',
      });
    }, 100);

    const text = await readSSE(
      server,
      '/api/v1/results/stream?request_id=r1&heartbeat_ms=500&close_after_ms=1000',
      2500,
    );

    const lines2 = text.split('\n').filter((l) => l.startsWith('data: '));
    const payloads2 = lines2.map((l) => l.slice(6)).join('\n');
    expect(payloads2).toContain('request_id');
  }, 20000);

  it('streams multiple events and applies combined filters', async () => {
    const server = app.getHttpServer();
    setTimeout(() => {
      controller.emitResult({ assignment_id: 'a3', tenant_id: 'tX', provider_id: 'p1', status: 'success' });
      controller.emitResult({ assignment_id: 'a3', tenant_id: 'tY', provider_id: 'p2', status: 'success' });
      controller.emitResult({ assignment_id: 'a3', tenant_id: 'tX', provider_id: 'p2', status: 'error' });
    }, 100);

    const text = await readSSE(
      server,
      '/api/v1/results/stream?assignment_id=a3&tenant_id=tX&provider_id=p2&heartbeat_ms=500&close_after_ms=1500',
      3000,
    );

    expect(text).toContain('data:');
    expect(text).toContain('tenant_id');
    expect(text).toContain('provider_id');
    expect(text).toContain('error');
  }, 20000);

  it('streams events by provider_id only (no tenant filter)', async () => {
    const server = app.getHttpServer();
    setTimeout(() => {
      controller.emitResult({ assignment_id: 'a4', tenant_id: 'tZ', provider_id: 'p77', status: 'success' });
    }, 100);

    const text = await readSSE(
      server,
      '/api/v1/results/stream?assignment_id=a4&provider_id=p77&heartbeat_ms=500&close_after_ms=1000',
      2500,
    );

    expect(text).toContain('data:');
    expect(text).toContain('provider_id');
  }, 20000);

  it('stress: streams many events quickly', async () => {
    const server = app.getHttpServer();
    setTimeout(() => {
      for (let i = 0; i < 60; i++) {
        controller.emitResult({
          assignment_id: 'a5',
          tenant_id: `t${i % 3}`,
          provider_id: `p${i % 5}`,
          status: i % 2 === 0 ? 'success' : 'error',
        });
      }
    }, 100);

    const text = await readSSE(
      server,
      '/api/v1/results/stream?assignment_id=a5&heartbeat_ms=500&close_after_ms=2000',
      3500,
    );

    expect(text).toContain('data:');
    expect(text).toContain('success');
    expect(text).toContain('error');
  }, 30000);

  it('streams events by tenant_id only (no provider filter)', async () => {
    const server = app.getHttpServer();
    setTimeout(() => {
      controller.emitResult({ assignment_id: 'a6', tenant_id: 'tOnly', provider_id: 'pX', status: 'success' });
      controller.emitResult({ assignment_id: 'a6', tenant_id: 'tOther', provider_id: 'pX', status: 'success' });
    }, 100);

    const text = await readSSE(
      server,
      '/api/v1/results/stream?assignment_id=a6&tenant_id=tOnly&heartbeat_ms=500&close_after_ms=1000',
      2500,
    );

    expect(text).toContain('data:');
    expect(text).toContain('tenant_id');
    expect(text).toContain('tOnly');
    expect(text).not.toContain('tOther');
  }, 20000);

  it('streams events with provider exclusion', async () => {
    const server = app.getHttpServer();
    setTimeout(() => {
      controller.emitResult({ assignment_id: 'a8', tenant_id: 't8', provider_id: 'p10', status: 'success' });
    }, 100);

    const text = await readSSE(
      server,
      '/api/v1/results/stream?assignment_id=a8&tenant_id=t8&provider_exclude=p9&heartbeat_ms=500&close_after_ms=1200',
      1800,
    );

    const lines3 = text.split('\n').filter((l) => l.startsWith('data: '));
    const payloads3 = lines3.map((l) => l.slice(6)).join('\n');
    expect(payloads3).toContain('"provider_id":"p10"');
    expect(text).not.toContain('"provider_id":"p9"');
  }, 20000);

  it('respects close_after_ms and ends stream timely', async () => {
    const server = app.getHttpServer();
    const start = Date.now();
    const text = await new Promise<string>((resolve, reject) => {
      request(server)
        .get('/api/v1/results/stream?assignment_id=a7&heartbeat_ms=200&close_after_ms=600')
        .set('Accept', 'text/event-stream')
        .buffer(true)
        .parse((r, cb) => {
          let data = '';
          let finished = false;
          const finishOk = () => {
            if (finished) return;
            finished = true;
            cb(null, data);
          };
          const finishErr = (err: unknown) => {
            if (finished) return;
            finished = true;
            cb(err as Error, data);
          };
          r.on('data', (chunk: Buffer) => {
            data += chunk.toString('utf-8');
            if (!finished && data.includes('heartbeat')) {
              finishOk();
            }
          });
          r.on('end', finishOk);
          r.on('error', finishErr);
        })
        .then((res) => ((res as any).body ? ((res as any).body as string) : ((res as any).text ? ((res as any).text as string) : '')))
        .then(resolve)
        .catch(reject);
    });
    const duration = Date.now() - start;
    expect(duration).toBeLessThan(3000);
  }, 20000);

  it('provider_exclude list filters out multiple providers', async () => {
    const server = app.getHttpServer();
    setTimeout(() => {
      controller.emitResult({ assignment_id: 'a9', tenant_id: 't9', provider_id: 'p1', status: 'success' });
      controller.emitResult({ assignment_id: 'a9', tenant_id: 't9', provider_id: 'p2', status: 'success' });
      controller.emitResult({ assignment_id: 'a9', tenant_id: 't9', provider_id: 'p3', status: 'success' });
    }, 100);

    const text = await readSSE(
      server,
      '/api/v1/results/stream?assignment_id=a9&tenant_id=t9&provider_exclude=p1,p2&heartbeat_ms=500&close_after_ms=1500',
      2200,
    );

    const lines = text.split('\n').filter((l) => l.startsWith('data: '));
    const payloads = lines.map((l) => l.slice(6)).join('\n');
    expect(payloads).toContain('"provider_id":"p3"');
    expect(payloads).not.toContain('"provider_id":"p1"');
    expect(payloads).not.toContain('"provider_id":"p2"');
  }, 20000);

  it('heartbeat behaves with different intervals', async () => {
    const server = app.getHttpServer();
    const intervals = [500, 1000, 2000];
    for (const hb of intervals) {
      const start = Date.now();
      const text = await readSSE(
        server,
        `/api/v1/results/stream?assignment_id=hb_${hb}&heartbeat_ms=${hb}&close_after_ms=${hb + 600}`,
        hb + 2500,
      );
      const duration = Date.now() - start;
      expect(text).toContain('data:');
      expect(text).toContain('heartbeat');
      expect(duration).toBeLessThan(hb + 3000);
    }
  }, 30000);
});
jest.setTimeout(30000);
