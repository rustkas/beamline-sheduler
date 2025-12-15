export const InternalMetrics = {
  requests_total: 0,
  decisions_total: 0,
  http_retries_total: 0,
  nats_retries_total: 0,
  sticky_hits_total: 0,
  sticky_miss_total: 0,
  nak_total: 0,
  redelivery_total: 0,
  maxdeliver_exhausted_total: 0,
  consumption_latency_ms_total: 0,
  consumption_latency_events: 0,
  idempotency_hit_total: 0,
  idempotency_miss_total: 0,
};

export function incRequest(): void {
  InternalMetrics.requests_total += 1;
}

export function incDecision(): void {
  InternalMetrics.decisions_total += 1;
}

export function incHttpRetry(): void {
  InternalMetrics.http_retries_total += 1;
}

export function incNatsRetry(): void {
  InternalMetrics.nats_retries_total += 1;
}

export function incStickyHit(): void {
  InternalMetrics.sticky_hits_total += 1;
}

export function incStickyMiss(): void {
  InternalMetrics.sticky_miss_total += 1;
}
export function incNak(): void {
  InternalMetrics.nak_total += 1;
}

export function incRedelivery(): void {
  InternalMetrics.redelivery_total += 1;
}

export function incMaxDeliverExhausted(): void {
  InternalMetrics.maxdeliver_exhausted_total += 1;
}

export function addConsumptionLatency(ms: number): void {
  if (Number.isFinite(ms) && ms >= 0) {
    InternalMetrics.consumption_latency_ms_total += ms;
    InternalMetrics.consumption_latency_events += 1;
  }
}

export function incIdempotencyHit(): void {
  InternalMetrics.idempotency_hit_total += 1;
}

export function incIdempotencyMiss(): void {
  InternalMetrics.idempotency_miss_total += 1;
}

export function getIdempotencyHits(): number {
  return InternalMetrics.idempotency_hit_total;
}

export function getIdempotencyMisses(): number {
  return InternalMetrics.idempotency_miss_total;
}
