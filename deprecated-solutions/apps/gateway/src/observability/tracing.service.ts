/**
 * Tracing Service for NestJS Gateway
 * Provides OpenTelemetry distributed tracing
 * Stub implementation - replace with @opentelemetry/api
 */

import { Injectable } from '@nestjs/common';
import { trace, context, Span, SpanStatusCode, Tracer } from '@opentelemetry/api';

@Injectable()
export class TracingService {
  private readonly tracer: Tracer;

  constructor() {
    this.tracer = trace.getTracer('beamline-gateway');
  }

  getTraceId(): string {
    return this.generateTraceId();
  }

  generateTraceId(): string {
    const buf = new Uint8Array(16);
    for (let i = 0; i < buf.length; i++) buf[i] = Math.floor(Math.random() * 256);
    return Array.from(buf)
      .map((b) => b.toString(16).padStart(2, '0'))
      .join('');
  }

  generateSpanId(): string {
    const buf = new Uint8Array(8);
    for (let i = 0; i < buf.length; i++) buf[i] = Math.floor(Math.random() * 256);
    return Array.from(buf)
      .map((b) => b.toString(16).padStart(2, '0'))
      .join('');
  }

  buildTraceparent(traceId: string, spanId: string): string {
    return `00-${traceId}-${spanId}-01`;
  }

  extractTraceContext(headers: Record<string, string>): string | undefined {
    // TODO: Extract from headers using OpenTelemetry
    return headers['trace_id'] || headers['x-trace-id'];
  }

  injectTraceContext(traceId: string, spanId: string): Record<string, string> {
    // TODO: Inject using OpenTelemetry
    return {
      trace_id: traceId,
      span_id: spanId,
      'X-Trace-Id': traceId,
      'X-Span-Id': spanId,
    };
  }

  startSpan(
    name: string,
    attributes?: Record<string, string>,
  ): Span {
    const span = this.tracer.startSpan(name);
    if (attributes) {
      for (const [k, v] of Object.entries(attributes)) span.setAttribute(k, v);
    }
    try {
      if (process.env.ENABLE_TRACE_TEST_COLLECTOR === 'true') {
        const g: any = global as any;
        g.__traceCollector = g.__traceCollector || { spans: [] };
        g.__traceCollector.spans.push({ name, attrs: attributes || {} });
      }
    } catch {}
    return span;
  }

  annotateSpan(span: Span, attrs: Record<string, string | number | boolean | undefined>): void {
    for (const [k, v] of Object.entries(attrs)) {
      if (v === undefined) continue;
      span.setAttribute(k, String(v));
    }
  }

  endSpan(span: Span, success: boolean = true): void {
    span.setStatus({ code: success ? SpanStatusCode.OK : SpanStatusCode.ERROR });
    span.end();
  }
}
