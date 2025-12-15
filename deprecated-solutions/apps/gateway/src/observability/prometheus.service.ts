import { Injectable, OnModuleInit } from '@nestjs/common';
import { Registry, collectDefaultMetrics, Counter, Histogram, Gauge } from 'prom-client';
import { MetricsService } from './metrics.service';

/**
 * Prometheus Service
 * Collects and aggregates metrics for Prometheus export
 * CP2.4: Prometheus metrics export
 */
@Injectable()
export class PrometheusService implements OnModuleInit {
  private readonly registry: Registry;

  // Idempotency Metrics
  private idempotencyHits: Counter;
  private idempotencyMisses: Counter;
  private idempotencyEvictions: Counter;

  // Rate Limiting Metrics
  private rateLimitHits: Counter;
  private rateLimitRejects: Counter;

  // Request Metrics (using existing MetricsService)
  private httpRequests: Histogram;
  private activeConnections: Gauge;

  // Track previous values for delta calculation
  private previousIdempotencyHits: number = 0;
  private previousIdempotencyMisses: number = 0;
  private previousIdempotencyEvictions: number = 0;

  constructor(private readonly metricsService: MetricsService) {
    this.registry = new Registry();
    this.initializeMetrics();
    // Collect default Node.js metrics
    collectDefaultMetrics({ register: this.registry, prefix: 'gateway_' });
  }

  async onModuleInit(): Promise<void> {
    if (process.env.NODE_ENV !== 'test') {
      const timer = setInterval(() => this.updateMetrics(), 5000);
      if (typeof (timer as any).unref === 'function') {
        (timer as any).unref();
      }
    }
  }

  private initializeMetrics(): void {
    // Idempotency Metrics
    this.idempotencyHits = new Counter({
      name: 'beamline_gateway_idempotency_hits_total',
      help: 'Total number of idempotency cache hits',
      registers: [this.registry],
    });

    this.idempotencyMisses = new Counter({
      name: 'beamline_gateway_idempotency_miss_total',
      help: 'Total number of idempotency cache misses',
      registers: [this.registry],
    });

    this.idempotencyEvictions = new Counter({
      name: 'beamline_gateway_idempotency_store_errors_total',
      help: 'Total number of idempotency cache evictions',
      registers: [this.registry],
    });

    // Rate Limiting Metrics
    this.rateLimitHits = new Counter({
      name: 'gateway_rate_limit_hits_total',
      help: 'Total number of rate limit enforcements',
      registers: [this.registry],
      labelNames: ['tenant_id', 'endpoint'],
    });

    this.rateLimitRejects = new Counter({
      name: 'gateway_rate_limit_rejects_total',
      help: 'Total number of rate limit rejections',
      registers: [this.registry],
      labelNames: ['tenant_id', 'endpoint'],
    });

    // HTTP Request Metrics
    this.httpRequests = new Histogram({
      name: 'gateway_http_request_duration_seconds',
      help: 'HTTP request duration in seconds',
      registers: [this.registry],
      labelNames: ['method', 'route', 'status_code'],
      buckets: [0.1, 0.5, 1, 2, 5],
    });

    // Connection Metrics
    this.activeConnections = new Gauge({
      name: 'gateway_active_connections',
      help: 'Number of active HTTP connections',
      registers: [this.registry],
    });
  }

  /**
   * Update metrics from services
   * Called periodically to sync metrics from internal services
   */
  private updateMetrics(): void {
    try {
      // Update idempotency metrics from internal store
      // Note: IdempotencyService uses internal-metrics.store
      // We'll track the delta since last update
      const store = require('./internal-metrics.store');
      const getIdempotencyHits = typeof store.getIdempotencyHits === 'function' ? store.getIdempotencyHits : undefined;
      const getIdempotencyMisses = typeof store.getIdempotencyMisses === 'function' ? store.getIdempotencyMisses : undefined;
      if (!getIdempotencyHits || !getIdempotencyMisses) {
        return;
      }
      const currentHits = getIdempotencyHits();
      const currentMisses = getIdempotencyMisses();

      const hitsDelta = currentHits - this.previousIdempotencyHits;
      const missesDelta = currentMisses - this.previousIdempotencyMisses;

      if (hitsDelta > 0) {
        this.idempotencyHits.inc(hitsDelta);
        this.previousIdempotencyHits = currentHits;
      }

      if (missesDelta > 0) {
        this.idempotencyMisses.inc(missesDelta);
        this.previousIdempotencyMisses = currentMisses;
      }

      // Active connections gauge update skipped in tests
    } catch (error) {
      // Silently handle errors to prevent metrics update from breaking the service
      console.error('Error updating Prometheus metrics:', error);
    }
  }

  /**
   * Record rate limit hit
   */
  recordRateLimitHit(tenantId: string, endpoint: string): void {
    this.rateLimitHits.inc({ tenant_id: tenantId || 'unknown', endpoint });
  }

  /**
   * Record rate limit rejection
   */
  recordRateLimitReject(tenantId: string, endpoint: string): void {
    this.rateLimitRejects.inc({ tenant_id: tenantId || 'unknown', endpoint });
  }

  /**
   * Record HTTP request duration
   */
  recordHttpRequest(method: string, route: string, statusCode: number, durationSeconds: number): void {
    this.httpRequests.observe({ method, route, status_code: statusCode.toString() }, durationSeconds);
  }

  /**
   * Set active connections count
   */
  setActiveConnections(count: number): void {
    this.activeConnections.set(count);
  }

  /**
   * Get metrics in Prometheus format
   */
  async getMetrics(): Promise<string> {
    if (process.env.NODE_ENV !== 'test') {
      this.updateMetrics();
    }

    // Merge with existing MetricsService registry
    const existingMetrics = await this.metricsService.render();
    const prometheusMetrics = await this.registry.metrics();

    // Combine both metric outputs
    return `${prometheusMetrics}\n${existingMetrics}`;
  }

  /**
   * Get registry for direct metric access
   */
  getRegistry(): Registry {
    return this.registry;
  }
}
