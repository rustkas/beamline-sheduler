import { Injectable } from '@nestjs/common';

@Injectable()
export class IdempotencyService {
  private store = new Map<string, { ts: number; decision: unknown }>();
  private ttlMs = parseInt(process.env.IDEMPOTENCY_TTL_MS ?? '300000', 10); // 5 min
  private eventsStore = new Map<string, { ts: number }>();
  private cleanupIntervalMs = 60000;

  makeKey(tenantId: string, requestId?: string): string | null {
    if (!tenantId || !requestId) return null;
    return `${tenantId}:${requestId}`;
  }

  get(key: string): unknown | undefined {
    const entry = this.store.get(key);
    if (!entry) return undefined;
    if (Date.now() - entry.ts > this.ttlMs) {
      this.store.delete(key);
      return undefined;
    }
    return entry.decision;
  }

  set(key: string, decision: unknown): void {
    this.store.set(key, { ts: Date.now(), decision });
  }

  markEvent(key: string): boolean {
    if (!key) return false;
    const now = Date.now();
    const existing = this.eventsStore.get(key);
    if (existing && now - existing.ts <= this.ttlMs) {
      const { incIdempotencyHit } = require('../../observability/internal-metrics.store');
      incIdempotencyHit();
      return false;
    }
    this.eventsStore.set(key, { ts: now });
    const { incIdempotencyMiss } = require('../../observability/internal-metrics.store');
    incIdempotencyMiss();
    return true;
  }

  markHttpId(keyType: 'assignment_id' | 'ack_id' | 'usage_id', id?: string): boolean {
    if (!id) return false;
    return this.markEvent(`${keyType}:${id}`);
  }

  markHttpIdTenant(
    keyType: 'assignment_id' | 'ack_id' | 'usage_id',
    tenantId?: string,
    id?: string,
  ): boolean {
    if (!tenantId || !id) return false;
    return this.markEvent(`${tenantId}:${keyType}:${id}`);
  }

  constructor() {
    setInterval(() => {
      const now = Date.now();
      for (const [key, value] of this.store.entries()) {
        if (now - value.ts > this.ttlMs) this.store.delete(key);
      }
      for (const [key, value] of this.eventsStore.entries()) {
        if (now - value.ts > this.ttlMs) this.eventsStore.delete(key);
      }
    }, this.cleanupIntervalMs);
  }
}
