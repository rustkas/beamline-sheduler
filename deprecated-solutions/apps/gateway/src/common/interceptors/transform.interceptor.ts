import { Injectable, NestInterceptor, ExecutionContext, CallHandler } from '@nestjs/common';
import { MetricsService } from '../../observability/metrics.service';
import { TracingService } from '../../observability/tracing.service';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';

/**
 * Transform Interceptor
 * Wraps successful responses in a standard format (optional)
 */
@Injectable()
export class TransformInterceptor implements NestInterceptor {
  constructor(private readonly metrics: MetricsService, private readonly tracing: TracingService) {}
  intercept(context: ExecutionContext, next: CallHandler): Observable<unknown> {
    const http = context.switchToHttp();
    const req = http.getRequest();
    const method: string = (req?.method || 'GET').toUpperCase();
    const route: string = req?.route?.path || req?.url || 'unknown';
    const start = process.hrtime.bigint();
    this.metrics.incrementActiveRequests();
    // Статус будет зафиксирован в MetricsInterceptor; здесь учитываем активный запрос
    const span = this.tracing.startSpan('http.request', { method, route });
    return next.handle().pipe(
      map((data) => {
        const res = http.getResponse();
        const traceIdHeader = req.headers['x-trace-id'] || req.headers['trace_id'];
        const traceId = traceIdHeader
          ? String(traceIdHeader)
          : globalThis.crypto && 'getRandomValues' in globalThis.crypto
            ? Array.from(globalThis.crypto.getRandomValues(new Uint8Array(16)))
                .map((b) => b.toString(16).padStart(2, '0'))
                .join('')
            : Array.from({ length: 16 }, () => Math.floor(Math.random() * 256))
                .map((b) => b.toString(16).padStart(2, '0'))
                .join('');
        res.setHeader('X-Trace-Id', String(traceId));
        if (data && typeof data === 'object' && !('trace_id' in data)) {
          return { ...data, trace_id: String(traceId) };
        }
        const end = process.hrtime.bigint();
        const latencySec = Number(end - start) / 1e9;
        this.metrics.recordLatency(method, route, latencySec);
        this.metrics.decrementActiveRequests();
        this.tracing.endSpan(span, true);
        return data;
      }),
    );
  }
}
