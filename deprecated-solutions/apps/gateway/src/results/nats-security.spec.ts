import { Test, TestingModule } from '@nestjs/testing';
import { NatsResultsSubscriber } from './nats-results.subscriber';
import { ResultsStreamService } from './results-stream.service';
import { InternalMetrics } from '../observability/internal-metrics.store';

jest.mock('nats', () => {
  const sc = {
    encode: (s: string) => Buffer.from(s, 'utf-8'),
    decode: (b: Buffer) => b.toString('utf-8'),
  };
  const makeHeaders = () => {
    const map = new Map<string, string>();
    return {
      set: (k: string, v: string) => map.set(k, v),
      get: (k: string) => map.get(k) as string,
    } as unknown as Map<string, string>;
  };
  return {
    StringCodec: () => sc,
    connect: async () => ({
      jetstream: () => ({
        subscribe: (_subject: string, _opts?: unknown) => ({
          async *[Symbol.asyncIterator]() {
            // Message with mismatched tenant_id → should NAK
            yield {
              data: sc.encode(JSON.stringify({ assignment_id: 'a1', tenant_id: 'bad_tenant', status: 'success' })),
              headers: makeHeaders(),
              ack: () => {},
              nak: () => {
                InternalMetrics.nak_total += 1;
              },
            } as any;
            // Message with subject not in allowlist → should NAK
            yield {
              data: sc.encode(JSON.stringify({ assignment_id: 'a2', tenant_id: 't1', status: 'success' })),
              headers: makeHeaders(),
              ack: () => {},
              nak: () => {
                InternalMetrics.nak_total += 1;
              },
            } as any;
          },
        }),
      }),
      close: async () => {},
    }),
  };
});

describe('NATS Security (allowlist/tenant validation)', () => {
  it('NAKs messages violating allowlist or tenant check', async () => {
    process.env.NATS_SUBJECT_ALLOWLIST = 'caf.exec.result.v1';
    const sub = new NatsResultsSubscriber(new ResultsStreamService());
    await sub.onModuleInit();
    // Allow async iterator to process messages
    await new Promise((r) => setTimeout(r, 600));
    // Expect NAK increments
    expect(InternalMetrics.nak_total).toBeGreaterThanOrEqual(2);
    await sub.onModuleDestroy();
  }, 10000);
});