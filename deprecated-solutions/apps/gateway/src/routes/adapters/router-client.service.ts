import { Injectable } from '@nestjs/common';
import { IRouterClient } from './router-client.interface';
import { RouteRequestDto } from '../dto/route-request.dto';
import { RouteDecisionDto } from '../dto/route-decision.dto';
import { MockRouterClient } from '../mocks/router-client.mock';
import * as http from 'http';
import * as https from 'https';
import { URL } from 'url';
import { TracingService } from '../../observability/tracing.service';

/**
 * Router Client Service
 * Implements IRouterClient interface
 * Currently uses mock implementation, can be replaced with real gRPC/NATS client
 */
@Injectable()
export class RouterClientService implements IRouterClient {
  private client: IRouterClient;

  constructor() {
    const mode = process.env.ROUTER_CLIENT ?? 'mock';
    if (mode === 'http') {
      this.client = {
        async decide(routeRequest: RouteRequestDto): Promise<RouteDecisionDto> {
          const baseUrl = process.env.ROUTER_BASE_URL || 'http://trae-router:3081';
          const url = new URL('/api/v1/router/decide', baseUrl);
          const payload = JSON.stringify(routeRequest);
          const lib = url.protocol === 'https:' ? https : http;
          const tracing = new TracingService();
          const routeReqAny = routeRequest as unknown as Record<string, unknown>;
          const traceId = (routeReqAny['trace_id'] as string) || tracing.generateTraceId();
          const spanId = tracing.generateSpanId();
          const traceparent = tracing.buildTraceparent(traceId, spanId);
          const cpPhase = (routeReqAny['cp_phase'] as string) || 'cp2';
          const attempt = async (): Promise<RouteDecisionDto> =>
            await new Promise<RouteDecisionDto>((resolve, reject) => {
              const req = lib.request(
                {
                  method: 'POST',
                  hostname: url.hostname,
                  port: url.port,
                  path: url.pathname,
                  headers: {
                    'Content-Type': 'application/json',
                    'Content-Length': Buffer.byteLength(payload),
                    traceparent,
                    'X-Trace-Id': traceId,
                    'X-Cp-Phase': cpPhase,
                  },
                },
                (res) => {
                  let data = '';
                  res.on('data', (chunk) => (data += chunk));
                  res.on('end', () => {
                    try {
                      resolve(JSON.parse(data));
                    } catch {
                      reject(new Error('Invalid JSON from router service'));
                    }
                  });
                },
              );
              req.on('error', (err) => reject(err));
              req.write(payload);
              req.end();
            });
          const maxAttempts = parseInt(process.env.ROUTER_HTTP_RETRY_MAX ?? '3', 10);
          let delay = parseInt(process.env.ROUTER_HTTP_RETRY_DELAY_MS ?? '200', 10);
          for (let i = 0; i < maxAttempts; i++) {
            try {
              return await attempt();
            } catch {
              if (i === maxAttempts - 1) throw new Error('Router HTTP retry exhausted');
              await new Promise((r) => setTimeout(r, delay));
              delay *= 2;
              try {
                const { incHttpRetry } = await import('../../observability/internal-metrics.store');
                incHttpRetry();
              } catch {}
            }
          }
          throw new Error('Router HTTP retry exhausted');
        },
      };
    } else if (mode === 'nats') {
      this.client = {
        async decide(routeRequest: RouteRequestDto): Promise<RouteDecisionDto> {
          let nats: any;
          try {
            // eslint-disable-next-line @typescript-eslint/no-var-requires
            nats = require('nats');
          } catch {
            throw new Error('NATS client library not installed');
          }
          const subject = process.env.ROUTER_NATS_SUBJECT || 'beamline.router.v1.decide';
          const url = process.env.NATS_URL || 'nats://localhost:4222';
          const nc = await nats.connect({ servers: url });
          const tracing = new TracingService();
          const span = tracing.startSpan('nats.request', { subject });
          try {
            const payload = JSON.stringify(routeRequest);
            const opts: Record<string, unknown> = {
              timeout: parseInt(process.env.NATS_REQUEST_TIMEOUT_MS ?? '1000', 10),
            };
          if (nats.headers) {
            const hdrs = nats.headers();
            const routeReqAny2 = routeRequest as unknown as Record<string, unknown>;
            const traceId = (routeReqAny2['trace_id'] as string) || tracing.generateTraceId();
            const spanId = tracing.generateSpanId();
            const traceparent = tracing.buildTraceparent(traceId, spanId);
            if (traceId) hdrs.set('trace_id', traceId);
            const version = (routeReqAny2['version'] as string) || '1';
            hdrs.set('version', String(version));
            const tenantId = routeReqAny2['tenant_id'] as string | undefined;
            if (tenantId) hdrs.set('tenant_id', String(tenantId));
            hdrs.set('traceparent', traceparent);
            const cpPhase = (routeReqAny2['cp_phase'] as string) || 'cp2';
            hdrs.set('cp_phase', cpPhase);
            opts.headers = hdrs;
            if (tenantId) span.setAttribute('tenant_id', String(tenantId));
            span.setAttribute('cp_phase', cpPhase);
          }
            const sc = nats.StringCodec();
            const replySubject = process.env.NATS_REPLY_SUBJECT;
            const maxAttempts = parseInt(process.env.ROUTER_NATS_RETRY_MAX ?? '3', 10);
            let delay = parseInt(process.env.ROUTER_NATS_RETRY_DELAY_MS ?? '200', 10);
            for (let i = 0; i < maxAttempts; i++) {
              try {
                if (replySubject) {
                  const sub = nc.subscribe(replySubject, { max: 1 });
                  const publishOpts = { ...(opts as Record<string, unknown>), reply: replySubject };
                  nc.publish(subject, sc.encode(payload), publishOpts);
                  const timeoutMs = (opts.timeout as number) || 1000;
                  const result = await Promise.race([
                    (async () => {
                      for await (const m of sub) {
                        return JSON.parse(sc.decode(m.data));
                      }
                      throw new Error('No reply message');
                    })(),
                    new Promise((resolve, reject) =>
                      setTimeout(() => reject(new Error('NATS reply timeout')), timeoutMs as number),
                    ),
                  ]);
                  span.setAttribute('reply_subject', replySubject);
                  tracing.endSpan(span, true);
                  return result as RouteDecisionDto;
                } else {
                  const msg = await nc.request(subject, sc.encode(payload), opts);
                  const data = sc.decode(msg.data);
                  tracing.endSpan(span, true);
                  return JSON.parse(data);
                }
              } catch {
                if (i === maxAttempts - 1) {
                  tracing.endSpan(span, false);
                  throw new Error('Router NATS retry exhausted');
                }
                await new Promise((r) => setTimeout(r, delay));
                delay *= 2;
                try {
                  const { incNatsRetry } = await import(
                    '../../observability/internal-metrics.store'
                  );
                  incNatsRetry();
                } catch {}
              }
            }
            throw new Error('Router NATS retry exhausted');
          } finally {
            await nc.close();
          }
        },
      };
    } else if (mode === 'grpc') {
      this.client = {
        async decide(_routeRequest: RouteRequestDto): Promise<RouteDecisionDto> {
          throw new Error('gRPC router client not implemented');
        },
      };
    } else {
      // Use mock implementation for now
      // TODO: Replace with real gRPC/NATS client when Router is available
      this.client = new MockRouterClient();
    }
  }

  /**
   * Request route decision from Router service
   */
  async decide(routeRequest: RouteRequestDto): Promise<RouteDecisionDto> {
    return this.client.decide(routeRequest);
  }

  /**
   * Publish message to NATS (optional)
   */
  async publish(
    message:
      | import('../dto/message.dto').MessageDto
      | ({ message_id?: string } & Record<string, unknown>),
  ): Promise<{ message_id: string; ack: boolean }> {
    if (this.client.publish) {
      return this.client.publish(message);
    }
    // Mock implementation
    return {
      message_id: message.message_id || `msg_${Date.now()}`,
      ack: true,
    };
  }

  async connect(): Promise<void> {
    if (this.client.connect) {
      await this.client.connect();
    }
  }

  async disconnect(): Promise<void> {
    if (this.client.disconnect) {
      await this.client.disconnect();
    }
  }
}
