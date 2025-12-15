/**
 * Metrics Service for NestJS Gateway
 * Provides Prometheus metrics integration
 * Stub implementation - replace with @willsoto/nestjs-prometheus
 */

import { Injectable } from '@nestjs/common';
import { Counter, Histogram, Gauge, Registry, collectDefaultMetrics } from 'prom-client';

@Injectable()
export class MetricsService {
  private readonly registry = new Registry();
  private readonly requestCounter: Counter;
  private readonly latencyHistogram: Histogram;
  private readonly activeRequestsGauge: Gauge;
  private readonly natsConsumptionCounter: Counter;
  private readonly natsLatencyHistogram: Histogram;
  private readonly natsAgg: Map<string, Map<string, { ok: number; nak: number; redelivered: number; exhausted: number }>> = new Map();
  private readonly cpRequestCounter: Counter;
  private readonly cpLatencyHistogram: Histogram;
  private readonly stickyHitsCounter: Counter;
  private readonly stickyMissesCounter: Counter;

  constructor() {
    collectDefaultMetrics({ register: this.registry, prefix: 'beamline_gateway_' });
    this.requestCounter = new Counter({
      name: 'beamline_gateway_requests_total',
      help: 'Total number of gateway requests',
      labelNames: ['method', 'route', 'status'],
      registers: [this.registry],
    });
    this.latencyHistogram = new Histogram({
      name: 'beamline_gateway_latency_seconds',
      help: 'Gateway request latency',
      labelNames: ['method', 'route'],
      buckets: [0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1.0, 5.0],
      registers: [this.registry],
    });
    this.activeRequestsGauge = new Gauge({
      name: 'beamline_gateway_active_requests',
      help: 'Current number of active requests',
      registers: [this.registry],
    });
    this.natsConsumptionCounter = new Counter({
      name: 'beamline_gateway_nats_consumption_total',
      help: 'NATS consumption events',
      labelNames: ['subject', 'tenant', 'result', 'redelivered', 'exhausted'],
      registers: [this.registry],
    });
    this.natsLatencyHistogram = new Histogram({
      name: 'beamline_gateway_nats_consumption_latency_ms',
      help: 'NATS consumption latency (ms)',
      labelNames: ['subject'],
      buckets: [1, 5, 10, 25, 50, 100, 250, 500, 1000, 2000, 5000],
      registers: [this.registry],
    });
    this.cpRequestCounter = new Counter({
      name: 'beamline_gateway_cp_requests_total',
      help: 'CP-phase requests',
      labelNames: ['cp_phase', 'route'],
      registers: [this.registry],
    });
    this.cpLatencyHistogram = new Histogram({
      name: 'beamline_gateway_cp_latency_seconds',
      help: 'CP-phase request latency',
      labelNames: ['cp_phase', 'route'],
      buckets: [0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1.0, 5.0],
      registers: [this.registry],
    });

    this.stickyHitsCounter = new Counter({
      name: 'beamline_gateway_sticky_hits_total',
      help: 'Sticky session cache hits',
      labelNames: ['subject', 'tenant'],
      registers: [this.registry],
    });

    this.stickyMissesCounter = new Counter({
      name: 'beamline_gateway_sticky_miss_total',
      help: 'Sticky session cache misses',
      labelNames: ['subject', 'tenant'],
      registers: [this.registry],
    });
  }

  incrementRequests(method: string, route: string, status: number): void {
    this.requestCounter.inc({ method, route, status: status.toString() });
  }

  recordLatency(method: string, route: string, latencySeconds: number): void {
    this.latencyHistogram.observe({ method, route }, latencySeconds);
  }

  incrementActiveRequests(): void {
    this.activeRequestsGauge.inc();
  }

  decrementActiveRequests(): void {
    this.activeRequestsGauge.dec();
  }

  async render(): Promise<string> {
    return await this.registry.metrics();
  }

  incNatsConsumption(subject: string, tenant: string | undefined, result: 'ok' | 'nak', redelivered: boolean, exhausted: boolean): void {
    const allowRaw = process.env.METRICS_TENANT_LABEL_ALLOWLIST || '';
    const allow = new Set(
      allowRaw
        .split(',')
        .map((s) => s.trim())
        .filter((s) => s.length > 0),
    );
    const tEffective = tenant && (allow.size === 0 || allow.has(tenant)) ? tenant : 'other';
    this.natsConsumptionCounter.inc({ subject, tenant: tEffective || 'unknown', result, redelivered: String(redelivered), exhausted: String(exhausted) });
    const t = tEffective || 'unknown';
    const bySubject = this.natsAgg.get(subject) || new Map<string, { ok: number; nak: number; redelivered: number; exhausted: number }>();
    const cur = bySubject.get(t) || { ok: 0, nak: 0, redelivered: 0, exhausted: 0 };
    if (result === 'ok') cur.ok += 1; else cur.nak += 1;
    if (redelivered) cur.redelivered += 1;
    if (exhausted) cur.exhausted += 1;
    bySubject.set(t, cur);
    this.natsAgg.set(subject, bySubject);
  }

  observeNatsLatency(subject: string, ms: number): void {
    if (Number.isFinite(ms) && ms >= 0) this.natsLatencyHistogram.observe({ subject }, ms);
  }

  getNatsSummary(): Record<string, Record<string, { ok: number; nak: number; redelivered: number; exhausted: number }>> {
    const out: Record<string, Record<string, { ok: number; nak: number; redelivered: number; exhausted: number }>> = {};
    for (const [subject, tenants] of this.natsAgg.entries()) {
      out[subject] = {};
      for (const [tenant, stats] of tenants.entries()) {
        out[subject][tenant] = stats;
      }
    }
    return out;
  }

  incCpRequest(cpPhase: string, route: string): void {
    this.cpRequestCounter.inc({ cp_phase: cpPhase, route });
  }

  observeCpLatency(cpPhase: string, route: string, latencySeconds: number): void {
    this.cpLatencyHistogram.observe({ cp_phase: cpPhase, route }, latencySeconds);
  }

  incStickyHit(subject: string, tenant: string | undefined): void {
    const allowRaw = process.env.METRICS_TENANT_LABEL_ALLOWLIST || '';
    const allow = new Set(
      allowRaw
        .split(',')
        .map((s) => s.trim())
        .filter((s) => s.length > 0),
    );
    const tEffective = tenant && (allow.size === 0 || allow.has(tenant)) ? tenant : 'other';
    this.stickyHitsCounter.inc({ subject, tenant: tEffective || 'unknown' });
  }

  incStickyMiss(subject: string, tenant: string | undefined): void {
    const allowRaw = process.env.METRICS_TENANT_LABEL_ALLOWLIST || '';
    const allow = new Set(
      allowRaw
        .split(',')
        .map((s) => s.trim())
        .filter((s) => s.length > 0),
    );
    const tEffective = tenant && (allow.size === 0 || allow.has(tenant)) ? tenant : 'other';
    this.stickyMissesCounter.inc({ subject, tenant: tEffective || 'unknown' });
  }
}
