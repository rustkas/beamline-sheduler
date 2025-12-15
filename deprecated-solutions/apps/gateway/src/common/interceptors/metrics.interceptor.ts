import { Injectable, NestInterceptor, ExecutionContext, CallHandler, Optional, Inject } from '@nestjs/common';
import { Observable } from 'rxjs';
import { finalize } from 'rxjs/operators';
import { MetricsService } from '../../observability/metrics.service';
import { PrometheusService } from '../../observability/prometheus.service';
import { TracingService } from '../../observability/tracing.service';

@Injectable()
export class MetricsInterceptor implements NestInterceptor {
  constructor(
    private readonly metrics: MetricsService,
    private readonly tracing: TracingService,
    @Optional() @Inject(PrometheusService) private readonly prometheusService?: PrometheusService,
  ) {}

  intercept(context: ExecutionContext, next: CallHandler): Observable<unknown> {
    const http = context.switchToHttp();
    const req = http.getRequest();
    const method: string = (req?.method || 'GET').toUpperCase();
    const route: string = req?.route?.path || req?.url || 'unknown';
    const tenantId: string | undefined =
      (req?.headers?.['x-tenant-id'] as string | undefined) ||
      (req?.body?.message?.tenant_id as string | undefined);
    const startTime = Date.now();
    const cpPhase = (req?.headers?.['x-cp-phase'] as string | undefined) || 'cp2';
    const span = this.tracing.startSpan('http.metrics', { method, route });
    return next.handle().pipe(
      finalize(() => {
        const res = http.getResponse();
        const status: number = Number(res?.statusCode || 200);
        const durationSeconds = (Date.now() - startTime) / 1000;

        // Update existing MetricsService
        this.metrics.incrementRequests(method, route, status);
        if (cpPhase) this.metrics.incCpRequest(cpPhase, route);
        if (cpPhase) this.metrics.observeCpLatency(cpPhase, route, durationSeconds);
        if (status >= 400 && status < 500) this.metrics.incrementRequests(method, `${route}:4xx`, status);
        if (status >= 500) this.metrics.incrementRequests(method, `${route}:5xx`, status);

        // Update PrometheusService if available
        if (this.prometheusService) {
          this.prometheusService.recordHttpRequest(method, route, status, durationSeconds);
        }

        // Update tracing
        span.setAttribute('route', route);
        span.setAttribute('status', String(status));
        if (tenantId) span.setAttribute('tenant_id', tenantId);
        if (cpPhase) span.setAttribute('cp_phase', cpPhase);
        this.tracing.endSpan(span, status < 500);
      }),
    );
  }
}