import { NatsResultsSubscriber } from './nats-results.subscriber';
import { ResultsStreamService } from './results-stream.service';

describe('Usage Tracking (unit)', () => {
  it('emits usage at most once per tenant+assignment/request', async () => {
    const stream = new ResultsStreamService();
    const subscriber = new NatsResultsSubscriber(stream);
    const publishes: any[] = [];
    (subscriber as any).nc = {
      publish: (_subj: string, _data: Uint8Array, _opts?: Record<string, unknown>) => {
        publishes.push({ subj: _subj });
      },
    };
    const sc = { encode: (s: string) => new TextEncoder().encode(s) };
    const natsLib = { headers: () => ({ set: (_k: string, _v: string) => {} }) } as any;
    const headers = new Map<string, string>();
    headers.set('tenant_id', 't1');
    const payload = { assignment_id: 'a1', provider_id: 'p1', status: 'success' } as any;
    const msg: any = { headers: new Map<string, string>() };

    const first = await (subscriber as any).maybePublishUsage(headers, payload, sc, natsLib, msg);
    const second = await (subscriber as any).maybePublishUsage(headers, payload, sc, natsLib, msg);
    expect(first).toBe(true);
    expect(second).toBe(false);
    expect(publishes.length).toBe(1);
  });
});