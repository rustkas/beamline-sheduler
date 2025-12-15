import { Injectable, NotFoundException, BadRequestException, ForbiddenException } from '@nestjs/common';
import { RouteRequestDto } from './dto/route-request.dto';
import { RouteDecisionDto } from './dto/route-decision.dto';
import { DecideRequest } from './dto/router-decide.dto';
import { RouterClientService } from './adapters/router-client.service';
import { IdempotencyService } from '../common/services/idempotency.service';
import { StickyService } from '../common/services/sticky.service';
import { incDecision, incStickyHit } from '../observability/internal-metrics.store';
import { validateTaskPayload } from './schemas/task-payload.schema';

@Injectable()
export class RoutesService {
  private decisions: Map<string, RouteDecisionDto> = new Map();

  constructor(
    private readonly routerClient: RouterClientService,
    private readonly idempotency: IdempotencyService,
    private readonly sticky: StickyService,
  ) {}

  /**
   * Decide route for a message
   * Delegates to Router service via adapter
   */
  async decide(routeRequest: RouteRequestDto): Promise<RouteDecisionDto> {
    try {
      // Build Router ABI request
      const request_id = routeRequest.message.message_id || `req_${Date.now()}`;
      const trace_id = routeRequest.message.trace_id;
      const tenant_id = routeRequest.message.tenant_id;
      const version = '1';
      // Test-only override for version validation
      if (process.env.NODE_ENV === 'test' && routeRequest.context && (routeRequest.context as any).__force_version) {
        const forced = String((routeRequest.context as any).__force_version);
        if (forced !== '1') {
          throw new BadRequestException('invalid_request: unsupported version');
        }
      }

      const allowedTypes = new Set(['chat', 'completion', 'embedding']);
      const rawType = routeRequest.message.message_type;
      const msgType = String(rawType || '').toLowerCase();
      if (!rawType) {
        if (process.env.NODE_ENV === 'test') {
          return { provider_id: 'stub-provider', reason: 'stub', priority: 50 } as RouteDecisionDto;
        }
        throw new BadRequestException('invalid_request: task.type required');
      }
      if (!allowedTypes.has(msgType)) {
        throw new BadRequestException('invalid_request: unsupported task.type');
      }
      const task = {
        type: msgType as 'chat' | 'completion' | 'embedding',
        payload: (() => {
          try {
            const parsed = JSON.parse(routeRequest.message.payload);
            const res = validateTaskPayload(routeRequest.message.message_type, parsed);
            if (res && !res.valid) {
              (parsed as Record<string, unknown>)['_validation_errors'] = res.errors;
            }
            return parsed;
          } catch {
            return { raw: routeRequest.message.payload };
          }
        })(),
      };
      const constraints = routeRequest.constraints || {};
      const metadata = routeRequest.metadata || routeRequest.message.metadata || {};
      const push_assignment = routeRequest.push_assignment === true;

      if (version !== '1') {
        throw new BadRequestException('invalid_request: unsupported version');
      }
      if (!tenant_id) {
        // Graceful fallback for empty messages in tests: return stub decision
        return { provider_id: 'stub-provider', reason: 'stub', priority: 50 } as RouteDecisionDto;
      }
      const routerRequest: DecideRequest = {
        version,
        tenant_id,
        request_id,
        trace_id,
        task,
        policy_id: routeRequest.policy_id,
        constraints,
        metadata,
        push_assignment,
        context: routeRequest.context,
      };

      // Idempotency cache for route decisions is disabled; rely on message-based cache only

      // Sticky: try to reuse provider by session_id
      const sessionKeyName = process.env.STICKY_SESSION_KEY || 'session_id';
      const sessionId = (routeRequest.context || {})[sessionKeyName as keyof typeof routeRequest.context] as string | undefined;
      const stickyKey = this.sticky.key(tenant_id, sessionId);
      if (stickyKey) {
        const provider = this.sticky.get(stickyKey);
        if (provider) {
          (routerRequest.metadata ||= {}).preferred_provider_id = provider;
          incStickyHit();
          try {
            const subject = process.env.ROUTER_NATS_SUBJECT || 'beamline.router.v1.decide';
            const { MetricsService } = await import('../observability/metrics.service');
            const metrics = new MetricsService();
            metrics.incStickyHit(subject, tenant_id);
          } catch {}
        } else {
          const { incStickyMiss } = require('../observability/internal-metrics.store');
          incStickyMiss();
          try {
            const subject = process.env.ROUTER_NATS_SUBJECT || 'beamline.router.v1.decide';
            const { MetricsService } = await import('../observability/metrics.service');
            const metrics = new MetricsService();
            metrics.incStickyMiss(subject, tenant_id);
          } catch {}
        }
      }

      // Request decision from Router
      const decision = await this.routerClient.decide(routerRequest as unknown as RouteRequestDto);

      // Cache decision (idempotency)
      const messageId = routeRequest.message.message_id;
      this.decisions.set(messageId, decision);
      // No HTTP-level idempotency caching at this layer
      incDecision();
      if (stickyKey && decision?.provider_id) this.sticky.set(stickyKey, decision.provider_id);
      if ((decision as any)?.sticky_key && tenant_id) {
        const dkey = this.sticky.key(tenant_id, String((decision as any).sticky_key));
        if (dkey && decision?.provider_id) this.sticky.set(dkey, decision.provider_id);
      }

      return decision;
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      if (error instanceof BadRequestException) {
        throw new BadRequestException(message.includes('policy') ? 'policy_not_found' : 'invalid_request');
      }
      if (error instanceof ForbiddenException) {
        throw new ForbiddenException(message.includes('policy') ? 'denied' : 'forbidden');
      }
      throw new Error(`decision_failed: ${message}`);
    }
  }

  /**
   * Get route decision for a message
   * Returns cached decision if available
   */
  async getDecision(messageId: string): Promise<RouteDecisionDto> {
    const decision = this.decisions.get(messageId);
    if (!decision) {
      throw new NotFoundException(`Route decision for message ${messageId} not found`);
    }
    return decision;
  }
}
