import { Injectable } from '@nestjs/common';

@Injectable()
export class StickyService {
  private store = new Map<string, { provider_id: string; ts: number }>();
  private ttlMs = parseInt(process.env.STICKY_TTL_MS ?? '3600000', 10);

  key(tenantId: string, sessionId?: string): string | null {
    if (!tenantId || !sessionId) return null;
    return `${tenantId}:${sessionId}`;
  }

  get(key: string): string | undefined {
    const entry = this.store.get(key);
    if (!entry) return undefined;
    if (Date.now() - entry.ts > this.ttlMs) {
      this.store.delete(key);
      return undefined;
    }
    return entry.provider_id;
  }

  set(key: string, providerId: string): void {
    this.store.set(key, { provider_id: providerId, ts: Date.now() });
  }

  clear(key: string): void {
    this.store.delete(key);
  }
}