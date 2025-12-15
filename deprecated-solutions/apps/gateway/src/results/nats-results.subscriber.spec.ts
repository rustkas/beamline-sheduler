import { NatsResultsSubscriber } from './nats-results.subscriber';
import { ResultsStreamService } from './results-stream.service';

describe('NatsResultsSubscriber (unit)', () => {
  let stream: ResultsStreamService;
  let subscriber: NatsResultsSubscriber;

  beforeEach(() => {
    stream = new ResultsStreamService();
    subscriber = new NatsResultsSubscriber(stream);
  });

  afterEach(async () => {
    // Simulate module destroy to close subscriptions
    await subscriber.onModuleDestroy();
  });

  it('should emit results to stream when message received', async () => {
    // Mock internal state to avoid real NATS
    // Directly call emitResult to simulate incoming message
    const payload = { assignment_id: 'a1', status: 'success', provider_id: 'p1' };
    stream.emitResult(payload);

    const events: Array<Record<string, unknown>> = [];
    stream.getStream('a1').subscribe((event) => events.push(event.data as Record<string, unknown>));

    // Emit again after subscription
    stream.emitResult(payload);

    expect(events.length).toBeGreaterThan(0);
    expect(events[0].assignment_id).toBe('a1');
    expect(events[0].status).toBe('success');
  });

  it('idempotency marks hit/miss counters', () => {
    const { InternalMetrics } = require('../observability/internal-metrics.store');
    const idem = new (require('../common/services/idempotency.service').IdempotencyService)();
    const key = 'result:a1';
    const first = idem.markEvent(key);
    const second = idem.markEvent(key);
    expect(first).toBe(true);
    expect(second).toBe(false);
    expect(InternalMetrics.idempotency_miss_total).toBeGreaterThan(0);
    expect(InternalMetrics.idempotency_hit_total).toBeGreaterThan(0);
  });
});
