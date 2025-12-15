/**
 * Prometheus Registry Wrapper
 * Singleton registry to prevent multiple registrations
 * Gracefully handles missing prom-client dependency
 */

let registry: any = null;
let hitsCounter: any = null;
let exceededCounter: any = null;

export function initPrometheus(): void {
  try {
    // eslint-disable-next-line @typescript-eslint/no-require-imports
    const { Registry, Counter } = require('prom-client');
    if (!registry) {
      registry = new Registry();
      hitsCounter = new Counter({
        name: 'gateway_rate_limit_hits_total',
        help: 'Total rate limit checks',
        labelNames: ['endpoint', 'tenant'],
        registers: [registry],
      });
      exceededCounter = new Counter({
        name: 'gateway_rate_limit_exceeded_total',
        help: 'Total rate limit exceeded events',
        labelNames: ['endpoint', 'tenant'],
        registers: [registry],
      });
    }
  } catch {
    // prom-client not installed - metrics disabled
    registry = null;
    hitsCounter = null;
    exceededCounter = null;
  }
}

export function incHits(endpoint: string, tenant: string): void {
  if (hitsCounter) {
    hitsCounter.inc({ endpoint, tenant });
  }
}

export function incExceeded(endpoint: string, tenant: string): void {
  if (exceededCounter) {
    exceededCounter.inc({ endpoint, tenant });
  }
}

// Initialize on module load
initPrometheus();

