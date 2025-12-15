import { Controller, Query, Sse, MessageEvent } from '@nestjs/common';
import { Observable, merge, interval, Subject, timer } from 'rxjs';
import { map, filter as rxFilter, shareReplay, takeUntil } from 'rxjs/operators';
import { ApiTags, ApiOperation, ApiResponse } from '@nestjs/swagger';
import { ResultsStreamService } from './results-stream.service';

@ApiTags('results')
@Controller('api/v1/results')
export class ResultsController {
  constructor(private readonly resultsStream: ResultsStreamService) {}

  @Sse('stream')
  @ApiOperation({ summary: 'Stream ExecResult by assignment_id or request_id' })
  @ApiResponse({ status: 200, description: 'SSE stream started' })
  stream(
    @Query('assignment_id') assignmentId?: string,
    @Query('request_id') requestId?: string,
    @Query('tenant_id') tenantId?: string,
    @Query('provider_id') providerId?: string,
    @Query('provider_exclude') providerExclude?: string,
    @Query('heartbeat_ms') heartbeatMs?: string,
    @Query('close_after_ms') closeAfterMs?: string,
  ): Observable<MessageEvent> {
    const key = assignmentId || requestId;
    if (!key) {
      const s = new Subject<MessageEvent>();
      s.next({ data: { error: 'missing assignment_id or request_id' } });
      s.complete();
      return s.asObservable();
    }
    const base = this.resultsStream.getStream(key).pipe(
      rxFilter((evt) => {
        const data = evt?.data as Record<string, unknown>;
        if (!data) return true;
        if (tenantId && data['tenant_id'] && data['tenant_id'] !== tenantId) return false;
        if (providerId && data['provider_id'] && data['provider_id'] !== providerId) return false;
        if (providerExclude) {
          const excludes = new Set(
            providerExclude
              .split(',')
              .map((s) => s.trim())
              .filter((s) => s.length > 0),
          );
          if (data['provider_id'] && excludes.has(String(data['provider_id']))) return false;
        }
        return true;
      }),
      shareReplay({ bufferSize: 1, refCount: true }),
    );
    const parsedHb = parseInt(heartbeatMs || '15000', 10);
    const hbInterval = isNaN(parsedHb) ? 15000 : Math.max(200, parsedHb);
    const heartbeat$ = interval(hbInterval).pipe(
      map(() => ({ data: { heartbeat: true, ts: Date.now() } }) as MessageEvent),
    );
    const merged = merge(base, heartbeat$);
    const closeMs = closeAfterMs ? Math.max(100, parseInt(closeAfterMs, 10)) : undefined;
    if (closeMs) {
      return merged.pipe(takeUntil(timer(closeMs)));
    }
    return merged;
  }

  // Internal helper for publishing result into SSE (to be called by NATS subscriber later)
  emitResult(result: Record<string, unknown>): void {
    this.resultsStream.emitResult(result);
  }
}
