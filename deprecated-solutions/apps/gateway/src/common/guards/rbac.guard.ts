import { CanActivate, ExecutionContext, Injectable, ForbiddenException, Logger, Optional } from '@nestjs/common';
import { RouterAuthService } from '../../auth/router-auth.service';

/**
 * RBAC Guard with Router Admin integration
 * Feature flag: USE_ROUTER_AUTH
 * - If enabled: Uses Router Admin gRPC for all RBAC checks
 * - If disabled: Falls back to local allowlists and roleMatrix
 */
@Injectable()
export class RBACGuard implements CanActivate {
  private readonly logger = new Logger(RBACGuard.name);
  private readonly useRouterAuth: boolean;
  private cache = new Map<string, { ts: number; allowed: boolean }>();
  private ttlMs = parseInt(process.env.RBAC_CACHE_TTL_MS ?? '60000', 10);

  constructor(@Optional() private readonly routerAuthService?: RouterAuthService) {
    // Feature flag: USE_ROUTER_AUTH
    this.useRouterAuth = process.env.USE_ROUTER_AUTH === 'true';
    this.logger.log(`RBACGuard initialized. USE_ROUTER_AUTH: ${this.useRouterAuth}`);
  }

  async canActivate(context: ExecutionContext): Promise<boolean> {
    const req = context.switchToHttp().getRequest();
    const tenant =
      (req.headers['x-tenant-id'] as string | undefined) ||
      (req.body?.message?.tenant_id as string | undefined);
    const policy = (req.body?.policy_id as string | undefined) || 'default';
    const policyFormat = /^[a-zA-Z0-9_.:-]+$/;
    if (policy && !policyFormat.test(policy)) {
      throw new ForbiddenException('Policy format invalid');
    }
    const role = (req.headers['x-role'] as string | undefined) || 'viewer';
    const resource = (req.headers['x-resource'] as string | undefined) || 'policy';
    const action = (req.headers['x-action'] as string | undefined) || 'read';
    const userId = (req.headers['x-user-id'] as string | undefined) || 'anonymous';

    // Use Router Admin if enabled and available
    if (this.useRouterAuth && this.routerAuthService?.isEnabled()) {
      return this.checkWithRouterAdmin(tenant, userId, resource, action);
    }

    // Fallback to local allowlists and roleMatrix
    return this.checkWithLocalLogic(tenant, policy, role, resource, action);
  }

  /**
   * Check RBAC using Router Admin gRPC
   */
  private async checkWithRouterAdmin(
    tenant: string | undefined,
    userId: string,
    resource: string,
    action: string,
  ): Promise<boolean> {
    if (!tenant) {
      this.logger.warn('Router Admin RBAC check requires tenant_id');
      throw new ForbiddenException('Tenant ID required for Router Admin RBAC');
    }

    try {
      // Check permission via Router Admin
      const authorized = await this.routerAuthService!.checkPermission(tenant, userId, resource, action);
      
      if (!authorized) {
        this.logger.debug(`RBAC denied: ${tenant}:${userId} -> ${resource}:${action}`);
        throw new ForbiddenException('RBAC denied');
      }

      this.logger.debug(`RBAC allowed: ${tenant}:${userId} -> ${resource}:${action}`);
      return true;
    } catch (error) {
      if (error instanceof ForbiddenException) {
        throw error;
      }
      // Fail-closed: deny if Router unavailable
      this.logger.error(`Router Admin RBAC check failed: ${error.message}`, error.stack);
      throw new ForbiddenException('RBAC check failed: Router unavailable');
    }
  }

  /**
   * Check RBAC using local allowlists and roleMatrix (legacy)
   */
  private checkWithLocalLogic(
    tenant: string | undefined,
    policy: string,
    role: string,
    resource: string,
    action: string,
  ): boolean {
    const allowlistTenants = (process.env.GATEWAY_TENANT_ALLOWLIST || '')
      .split(',')
      .filter(Boolean);
    const allowlistPolicies = (process.env.GATEWAY_POLICY_ALLOWLIST || '')
      .split(',')
      .filter(Boolean);
    const roleMatrix: Record<string, Record<string, string[]>> = {
      admin: {
        policy: ['read', 'write', 'delete', 'admin'],
        config: ['read', 'write', 'delete', 'admin'],
        metrics: ['read', 'admin'],
        usage: ['read', 'admin'],
      },
      operator: {
        policy: ['read', 'write'],
        config: ['read'],
        metrics: ['read'],
        usage: ['read'],
      },
      viewer: {
        policy: ['read'],
        config: ['read'],
        metrics: ['read'],
        usage: ['read'],
      },
    };

    // Check tenant allowlist
    if (allowlistTenants.length > 0 && tenant && !allowlistTenants.includes(tenant)) {
      throw new ForbiddenException('Tenant not allowed');
    }

    // Check policy allowlist
    if (allowlistPolicies.length > 0 && policy && !allowlistPolicies.includes(policy)) {
      throw new ForbiddenException('Policy not allowed');
    }

    // Check role matrix
    const cacheKey = `${role}|${resource}|${action}`;
    const now = Date.now();
    const cached = this.cache.get(cacheKey);
    if (cached && now - cached.ts <= this.ttlMs) {
      if (!cached.allowed) throw new ForbiddenException('RBAC denied');
      return true;
    }
    const allowedActions = roleMatrix[role]?.[resource] || [];
    const allowed = allowedActions.includes(action);
    this.cache.set(cacheKey, { ts: now, allowed });
    if (!allowed) {
      throw new ForbiddenException('RBAC denied');
    }

    return true;
  }
}
