import { ExecutionContext, Injectable, Logger } from '@nestjs/common';
import { ThrottlerGuard, ThrottlerException } from '@nestjs/throttler';
import { incHits, incExceeded } from '../utils/prometheus.registry';
import { rateLimitConfig } from '../../config/rate-limit.config';

@Injectable()
export class RateLimitGuard extends ThrottlerGuard {
  private readonly logger = new Logger(RateLimitGuard.name);

  async canActivate(context: ExecutionContext): Promise<boolean> {
    const { req } = this.getRequestResponse(context) as { req: Record<string, any> };
    const tenantHeader = req.headers?.['x-tenant-id'];
    const tenant =
      (req.user?.tenant_id as string) ||
      (Array.isArray(tenantHeader) ? tenantHeader[0] : tenantHeader) ||
      'anonymous';
    const endpoint = this.canonicalEndpoint(req);

    // Record hit
    incHits(endpoint, tenant);

    // Call parent canActivate
    return super.canActivate(context);
  }

  protected async getTracker(req: Record<string, any>): Promise<string> {
    const tenantHeader = req.headers?.['x-tenant-id'];
    const tenant =
      (req.user?.tenant_id as string) ||
      (Array.isArray(tenantHeader) ? tenantHeader[0] : tenantHeader) ||
      'anonymous';
    const endpoint = this.canonicalEndpoint(req);
    return `${tenant}:${endpoint}`;
  }


  protected async throwThrottlingException(
    context: ExecutionContext,
    throttlerLimitDetail: { limit: number; ttl: number; key: string; tracker: string },
  ): Promise<void> {
    const { req, res } = this.getRequestResponse(context) as {
      req: Record<string, any>;
      res: Record<string, any>;
    };

    const ttlSeconds = Math.ceil(throttlerLimitDetail.ttl / 1000);
    const nowSeconds = Math.floor(Date.now() / 1000);
    const resetAtSeconds = nowSeconds + ttlSeconds;

    // Set headers
    res.setHeader('Retry-After', ttlSeconds.toString());
    res.setHeader('X-RateLimit-Limit', throttlerLimitDetail.limit.toString());
    res.setHeader('X-RateLimit-Remaining', '0');
    res.setHeader('X-RateLimit-Reset', resetAtSeconds.toString());

    // Extract tenant, endpoint, trace_id
      const tenantHeader = req.headers?.['x-tenant-id'];
    const tenantId =
      (req.user?.tenant_id as string) ||
      (Array.isArray(tenantHeader) ? tenantHeader[0] : tenantHeader) ||
      'anonymous';
    const endpoint = this.canonicalEndpoint(req);
    const traceId = req.headers?.['x-trace-id'] || undefined;

    // Record exceeded
    incExceeded(endpoint, tenantId);

    // Structured logging
    this.logger.warn('Rate limit exceeded', {
      tenant_id: tenantId,
      endpoint,
      trace_id: traceId,
      decision: 'exceeded',
    });

    // Unified JSON error response
    const body = {
      code: 'RATE_LIMITED',
      message: 'Rate limit exceeded',
      details: {
        tenant_id: tenantId,
        endpoint,
        limit: throttlerLimitDetail.limit,
        remaining: 0,
        window_seconds: ttlSeconds,
        reset_at: new Date(resetAtSeconds * 1000).toISOString(),
        trace_id: traceId,
      },
      retry_after_ms: throttlerLimitDetail.ttl,
      timestamp: new Date().toISOString(),
    };

    res.status(429).json(body);
    throw new ThrottlerException('Rate limit exceeded');
  }

  /**
   * Canonical endpoint path normalization
   */
  private canonicalEndpoint(req: Record<string, any>): string {
    if (req.route?.path) {
      return req.route.path.replace(/^\//, '').split('?')[0];
    }
    if (req.params && Object.keys(req.params).length > 0) {
      let pattern = req.originalUrl || req.url || '';
      Object.keys(req.params).forEach((key) => {
        pattern = pattern.replace(req.params[key], `:${key}`);
      });
      return pattern.replace(/^\//, '').split('?')[0];
    }
    return (req.originalUrl || req.url || '/unknown').replace(/^\//, '').split('?')[0];
  }
}
