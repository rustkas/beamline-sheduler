import { Test, TestingModule } from '@nestjs/testing';
import request from 'supertest';
import { INestApplication } from '@nestjs/common';
import { AppModule } from '../app.module';

jest.mock('nats', () => {
  const makeHeaders = () => {
    const m = new Map<string, string>();
    return {
      set: (k: string, v: string) => {
        m.set(k, v);
      },
      get: (k: string) => m.get(k) as string,
    } as unknown as Map<string, string>;
  };
  const sc = {
    encode: (s: string) => Buffer.from(s, 'utf-8'),
    decode: (b: Buffer) => b.toString('utf-8'),
  };
  let nakCount = 0;
  (global as any).__nakCount = 0;
  return {
    headers: makeHeaders,
    StringCodec: () => sc,
    connect: async () => {
      return {
        request: async (_subject: string, data: Buffer) => ({ data }),
        publish: (_subject: string, _data: Buffer, _opts?: unknown) => {},
        subscribe: (_subject: string, _opts?: unknown) => ({ async *[Symbol.asyncIterator]() {} }),
        jetstream: () => ({
          subscribe: (_subject: string, _opts?: unknown) => ({
            async *[Symbol.asyncIterator]() {
              yield {
                data: sc.encode(
                  JSON.stringify({ assignment_id: 'a_js', tenant_id: 't_js', provider_id: 'p_js', status: 'success' }),
                ),
                headers: makeHeaders(),
                ack: () => {},
                nak: () => {
                  nakCount += 1;
                  (global as any).__nakCount = nakCount;
                },
              } as any;
            },
          }),
        }),
        close: async () => {},
      };
    },
  };
});

describe('OTLP Tracing (mock) publish/consume', () => {
  let app: INestApplication;

  beforeEach(async () => {
    process.env.ROUTER_CLIENT = 'nats';
    process.env.NATS_URL = 'nats://localhost:4222';
    process.env.CAF_RESULT_SUBJECT = 'caf.exec.result.v1';
    process.env.NATS_JS_DURABLE_RESULTS = 'router-results';
    process.env.ENABLE_TRACE_TEST_COLLECTOR = 'true';

    const moduleFixture: TestingModule = await Test.createTestingModule({ imports: [AppModule] })
      .overrideProvider(require('../auth/router-admin-grpc.service').RouterAdminGrpcService)
      .useValue({
        onModuleInit: jest.fn(),
        authenticate: jest.fn().mockResolvedValue({ authenticated: true, roles: [], permissions: [], trace_id: 't' }),
        authorize: jest.fn().mockResolvedValue({ authorized: true, trace_id: 't' }),
      })
      .overrideProvider(require('../router-admin/router-admin-client.service').RouterAdminClientService)
      .useValue({
        onModuleInit: jest.fn(),
        connect: jest.fn().mockResolvedValue(true),
        isConnected: jest.fn().mockReturnValue(true),
      })
      .compile();
    app = moduleFixture.createNestApplication();
    await app.init();
  });

  afterEach(async () => {
    await app.close();
  });

  it('produces http.request → nats.request → nats.consume spans', async () => {
    const server = app.getHttpServer();
    const payload = {
      message: {
        message_id: 'msg_trace',
        tenant_id: 't_trace',
        message_type: 'chat',
        payload: '{"text":"hi"}',
      },
      cp_phase: 'cp2',
    } as any;

    await request(server).post('/api/v1/routes/decide').send(payload).expect(200);

    const collector = (global as any).__traceCollector as { spans?: Array<{ name: string; attrs?: Record<string, string> }> };
    const names = new Set((collector?.spans || []).map((s) => s.name));
    expect(names.has('nats.request')).toBe(true);
    expect(names.has('nats.consume')).toBe(true);
  }, 20000);
});