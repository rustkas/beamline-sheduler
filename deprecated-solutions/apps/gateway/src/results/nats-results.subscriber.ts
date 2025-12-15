import { Injectable, OnModuleInit, OnModuleDestroy } from '@nestjs/common';
import { ResultsStreamService } from './results-stream.service';
import { IdempotencyService } from '../common/services/idempotency.service';
import { MetricsService } from '../observability/metrics.service';
import { TracingService } from '../observability/tracing.service';

interface NatsSubscription extends AsyncIterable<{ data: Uint8Array }> {
  drain: () => Promise<void>;
}

interface NatsClient {
  subscribe: (subject: string) => NatsSubscription;
  request?: (
    subject: string,
    data: Uint8Array,
    opts: Record<string, unknown>,
  ) => Promise<{ data: Uint8Array }>;
  publish?: (subject: string, data: Uint8Array, opts?: Record<string, unknown>) => void;
  close: () => Promise<void>;
}

interface NatsLib {
  connect: (opts: { servers: string }) => Promise<NatsClient>;
  StringCodec: () => { encode: (s: string) => Uint8Array; decode: (b: Uint8Array) => string };
  headers?: () => { set: (k: string, v: string) => void };
}

@Injectable()
export class NatsResultsSubscriber implements OnModuleInit, OnModuleDestroy {
  private nc!: NatsClient;
  private sub!: NatsSubscription;

  constructor(
    private readonly stream: ResultsStreamService,
    private readonly idempotency: IdempotencyService = new IdempotencyService(),
    private readonly metrics: MetricsService = new MetricsService(),
    private readonly tracing: TracingService = new TracingService(),
  ) {}

  private validateAndEmit(headers: Map<string, string> | undefined, payload: Record<string, unknown>): boolean {
    const version = headers?.get?.('version') || String(payload.version || '1');
    const tenantId = headers?.get?.('tenant_id') || String(payload.tenant_id || '');
    // Require JetStream message id when headers are present
    try {
      const msgId = headers?.get?.('Nats-Msg-Id') || headers?.get?.('nats-msg-id');
      if (headers && !msgId) {
        const { incNak } = require('../observability/internal-metrics.store');
        incNak();
        return false;
      }
    } catch {}
    const deliveredStr = headers?.get?.('Nats-Delivered') || headers?.get?.('nats-delivered');
    const delivered = deliveredStr ? parseInt(deliveredStr, 10) : undefined;
    const maxDeliverCfg = parseInt(process.env.NATS_MAX_DELIVER ?? '5', 10);
    if (typeof delivered === 'number') {
      const { incRedelivery, incMaxDeliverExhausted } = require('../observability/internal-metrics.store');
      if (delivered > 1) incRedelivery();
      if (delivered >= maxDeliverCfg) incMaxDeliverExhausted();
    }
    if (version !== '1' || !tenantId) {
      const { incNak } = require('../observability/internal-metrics.store');
      incNak();
      return false;
    }
    this.stream.emitResult(payload);
    return true;
  }

  async maybePublishUsage(
    headers: Map<string, string> | undefined,
    payload: Record<string, unknown>,
    sc: { encode: (s: string) => Uint8Array },
    natsLib: { headers?: () => { set: (k: string, v: string) => void } },
    msg: any,
  ): Promise<boolean> {
    if (process.env.ENABLE_USAGE_PUBLISH === 'false') return false;
    const assignmentId = (payload as any)?.assignment_id;
    const requestId = (payload as any)?.request_id;
    const usageKey = assignmentId
      ? `usage:${assignmentId}`
      : requestId
      ? `usage:${requestId}`
      : '';
    const tenantForUsage = headers?.get?.('tenant_id') || (payload as any)?.tenant_id || '';
    const compositeKey = usageKey && tenantForUsage ? `${tenantForUsage}:${usageKey}` : usageKey;
    const publishAllowed = compositeKey ? this.idempotency.markEvent(compositeKey) : true;
    if (!publishAllowed) return false;
    const evt = {
      version: '1',
      tenant_id: headers?.get?.('tenant_id') || (payload as any)?.tenant_id,
      provider_id: (payload as any)?.provider_id,
      event_type: 'result',
      status: (payload as any)?.status,
      latency_ms: (payload as any)?.latency_ms,
      cost: (payload as any)?.cost,
      trace_id: headers?.get?.('trace_id') || (payload as any)?.trace_id,
      timestamp: Date.now(),
      assignment_id: (payload as any)?.assignment_id,
      request_id: (payload as any)?.request_id,
    };
    const subj = process.env.USAGE_SUBJECT || 'beamline.usage.v1.metered';
    if (this.nc && typeof (this.nc as any).publish === 'function') {
      const opts: Record<string, unknown> = {};
      try {
        if (natsLib.headers) {
          const hdrs = natsLib.headers();
          const tid = String(evt.trace_id || this.tracing.generateTraceId());
          const sid = this.tracing.generateSpanId();
          hdrs.set('traceparent', this.tracing.buildTraceparent(tid, sid));
          const cpPhaseHdr2 = msg?.headers?.get?.('cp_phase');
          if (cpPhaseHdr2) hdrs.set('cp_phase', String(cpPhaseHdr2));
          opts.headers = hdrs;
        }
      } catch {}
      const spanPub = this.tracing.startSpan('nats.publish', { subject: subj });
      try {
        (this.nc as any).publish(subj, sc.encode(JSON.stringify(evt)), opts);
        spanPub.setAttribute('tenant_id', String(evt.tenant_id || ''));
        const cpPhaseAttr = msg?.headers?.get?.('cp_phase');
        if (cpPhaseAttr) spanPub.setAttribute('cp_phase', String(cpPhaseAttr));
        this.tracing.endSpan(spanPub, true);
        return true;
      } catch {
        this.tracing.endSpan(spanPub, false);
        return false;
      }
    }
    return false;
  }

  async onModuleInit(): Promise<void> {
    let nats: any;
    try {
      // eslint-disable-next-line @typescript-eslint/no-var-requires
      nats = require('nats');
    } catch {
      return;
    }
    const url = process.env.NATS_URL || 'nats://localhost:4222';
    this.nc = await nats.connect({ servers: url });
    const subject = process.env.CAF_RESULT_SUBJECT || 'caf.exec.result.v1';
    try {
      const js = (this.nc as any).jetstream();
      const durable = process.env.NATS_JS_DURABLE_RESULTS || 'router-results';
      const maxDeliver = parseInt(process.env.NATS_MAX_DELIVER ?? '5', 10);
      const ackWaitMs = parseInt(process.env.NATS_ACK_WAIT_MS ?? '30000', 10);
      const backoffMs = String(process.env.NATS_BACKOFF_MS || '1000,2000,4000')
        .split(',')
        .map((s) => parseInt(s.trim(), 10))
        .filter((n) => Number.isFinite(n) && n > 0);
      this.sub = await js.subscribe(subject, {
        config: {
          durable_name: durable,
          ack_policy: 'explicit',
          deliver_policy: 'all',
          max_deliver: maxDeliver,
          ack_wait: ackWaitMs * 1000000,
          backoff: backoffMs.map((ms: number) => ms * 1000000),
        },
      });
    } catch {
      this.sub = this.nc.subscribe(subject);
    }
    const sc = nats.StringCodec();
    (async () => {
      for await (const m of this.sub) {
        try {
          const subjectName = process.env.CAF_RESULT_SUBJECT || 'caf.exec.result.v1';
          const span = this.tracing.startSpan('nats.consume', { subject: subjectName });
          try {
            const cpPhaseHdr = (m as any)?.headers?.get?.('cp_phase');
            if (cpPhaseHdr) span.setAttribute('cp_phase', String(cpPhaseHdr));
          } catch {}
          const allowlistRaw = process.env.NATS_SUBJECT_ALLOWLIST || subjectName;
          const allowlist = new Set(
            allowlistRaw
              .split(',')
              .map((s) => s.trim())
              .filter((s) => s.length > 0),
          );
          if (!allowlist.has(subjectName)) {
            try {
              if (typeof (m as any).nak === 'function') (m as any).nak();
            } catch {}
            this.tracing.endSpan(span, false);
            continue;
          }
          const start = Date.now();
          const payload = JSON.parse(sc.decode(m.data));
          // Validate headers (if available) with fallback to payload
          const headers = (m as any).headers as Map<string, string> | undefined;
          try {
            if (headers && typeof headers.set === 'function') {
              const traceId = String((payload as any)?.trace_id || this.tracing.generateTraceId());
              const spanId = this.tracing.generateSpanId();
              const tp = this.tracing.buildTraceparent(traceId, spanId);
              headers.set('traceparent', tp);
            }
          } catch {}
          // Idempotency for results (assignment_id or request_id)
          const assignmentId = (payload as any)?.assignment_id;
          const requestId = (payload as any)?.request_id;
          const idemKey = assignmentId ? `result:${assignmentId}` : requestId ? `result:${requestId}` : '';
          if (idemKey && !this.idempotency.markEvent(idemKey)) {
            this.tracing.endSpan(span, true);
            continue;
          }
          // Tenant allowlist enforcement (Gateway)
          try {
            const tenantAllowRaw = process.env.GATEWAY_TENANT_ALLOWLIST || '';
            const tenantAllow = new Set(
              tenantAllowRaw
                .split(',')
                .map((s) => s.trim())
                .filter((s) => s.length > 0),
            );
            const tId = headers?.get?.('tenant_id') || (payload as any)?.tenant_id;
            if (tenantAllow.size > 0 && tId && !tenantAllow.has(String(tId))) {
              try {
                if (typeof (m as any).nak === 'function') (m as any).nak();
              } catch {}
              this.tracing.endSpan(span, false);
              continue;
            }
          } catch {}
          const ok = this.validateAndEmit(headers, payload);
          const consumeSpan = this.tracing.startSpan('nats.consume', { subject: subjectName });
          try {
            const subject = process.env.CAF_RESULT_SUBJECT || 'caf.exec.result.v1';
            const tenant = headers?.get?.('tenant_id') || (payload as any)?.tenant_id;
            const redeliveryStr = headers?.get?.('Nats-Delivered') || headers?.get?.('nats-delivered');
            const redelivered = !!(redeliveryStr && parseInt(redeliveryStr, 10) > 1);
            const maxDeliverCfg = parseInt(process.env.NATS_MAX_DELIVER ?? '5', 10);
            const exhausted = !!(redeliveryStr && parseInt(redeliveryStr, 10) >= maxDeliverCfg);
            this.metrics.incNatsConsumption(subject, String(tenant || ''), ok ? 'ok' : 'nak', redelivered, exhausted);
            if (redeliveryStr) span.setAttribute('redelivery_count', String(parseInt(redeliveryStr, 10)));
            const durable = process.env.NATS_JS_DURABLE_RESULTS || 'router-results';
            span.setAttribute('durable', durable);
          } catch {}
          // ACK/NAK integration if client supports it
          try {
            if (!ok && typeof (m as any).nak === 'function') {
              const delayMs = parseInt(process.env.NATS_NAK_DELAY_MS ?? '0', 10);
              if (delayMs > 0 && typeof (m as any).nak === 'function') {
                (m as any).nak(delayMs);
              } else {
                (m as any).nak();
              }
            } else if (ok && typeof (m as any).ack === 'function') {
              (m as any).ack();
            }
          } catch {}
          // Optional Usage publish
          if (ok) {
            try {
              const published = await this.maybePublishUsage(headers, payload, sc, nats, m as any);
              if (!published) {
                // continue loop without marking failure
              }
            } catch {}
          }
          const { addConsumptionLatency } = require('../observability/internal-metrics.store');
          const elapsed = Date.now() - start;
          addConsumptionLatency(elapsed);
          try {
            const subject = process.env.CAF_RESULT_SUBJECT || 'caf.exec.result.v1';
            this.metrics.observeNatsLatency(subject, elapsed);
          } catch {}
          this.tracing.endSpan(consumeSpan, true);
          this.tracing.endSpan(span, true);
        } catch {
          // ignore invalid JSON
        }
      }
    })();
  }

  async onModuleDestroy(): Promise<void> {
    try {
      if (this.sub) await this.sub.drain();
      if (this.nc) await this.nc.close();
    } catch {
      // ignore
    }
  }
}
