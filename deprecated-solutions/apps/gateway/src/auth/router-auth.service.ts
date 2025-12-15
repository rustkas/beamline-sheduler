import { Injectable, Logger, UnauthorizedException, ForbiddenException } from '@nestjs/common';
import { RouterAdminClientService } from '../router-admin/router-admin-client.service';
import { UnifiedAuthContext } from './unified-auth-context.service';

/**
 * Simple in-memory cache interface
 */
interface CacheEntry<T> {
  value: T;
  expiresAt: number;
}

/**
 * Router Auth Service
 * Handles authentication and authorization through Router Admin gRPC service
 * Provides caching for performance optimization
 */
@Injectable()
export class RouterAuthService {
  private readonly logger = new Logger(RouterAuthService.name);
  private readonly useRouterAuth: boolean;

  // In-memory cache
  private readonly cache: Map<string, CacheEntry<any>> = new Map();

  // Cache TTLs
  private readonly RBAC_CACHE_TTL = (parseInt(process.env.RBAC_CACHE_TTL_SECONDS ?? '60', 10)) * 1000;
  private readonly POLICY_CACHE_TTL = (parseInt(process.env.POLICY_CACHE_TTL_SECONDS ?? '300', 10)) * 1000;
  private readonly AUTH_CACHE_TTL = (parseInt(process.env.AUTH_CACHE_TTL_SECONDS ?? '300', 10)) * 1000;

  constructor(private readonly routerAdminClient: RouterAdminClientService) {
    // Feature flag: USE_ROUTER_AUTH
    this.useRouterAuth = process.env.USE_ROUTER_AUTH === 'true';
    this.logger.log(`Router Auth Service initialized. USE_ROUTER_AUTH: ${this.useRouterAuth}`);

    // Cleanup expired cache entries every minute
    setInterval(() => this.cleanupCache(), 60 * 1000);
  }

  /**
   * Get value from cache
   */
  private getCache<T>(key: string): T | undefined {
    const entry = this.cache.get(key);
    if (!entry) {
      return undefined;
    }

    if (Date.now() > entry.expiresAt) {
      this.cache.delete(key);
      return undefined;
    }

    return entry.value as T;
  }

  /**
   * Set value in cache
   */
  private setCache<T>(key: string, value: T, ttl: number): void {
    this.cache.set(key, {
      value,
      expiresAt: Date.now() + ttl,
    });
  }

  /**
   * Cleanup expired cache entries
   */
  private cleanupCache(): void {
    const now = Date.now();
    for (const [key, entry] of this.cache.entries()) {
      if (now > entry.expiresAt) {
        this.cache.delete(key);
      }
    }
  }

  /**
   * Check if Router Auth is enabled
   */
  isEnabled(): boolean {
    return this.useRouterAuth && this.routerAdminClient.isConnected();
  }

  /**
   * Validate token and get authentication context
   * @param token JWT token, API key, or session token
   * @param tenantId Optional tenant ID
   * @returns UnifiedAuthContext or null if invalid
   */
  async validateToken(
    token: string,
    tenantId?: string,
    tokenType: 'jwt' | 'api_key' | 'session' = 'jwt',
  ): Promise<UnifiedAuthContext | null> {
    if (!this.isEnabled()) {
      this.logger.debug('Router Auth disabled, skipping token validation');
      return null;
    }

    // Check cache first
    const cacheKey = `auth:token:${tokenType}:${token.substring(0, 16)}`;
    const cached = this.getCache<UnifiedAuthContext>(cacheKey);
    if (cached) {
      this.logger.debug('Token validation cache hit');
      return cached;
    }

    try {
      // TODO: Implement actual token validation via Router Admin
      // For now, this is a placeholder that will be implemented when Router Admin
      // has authentication endpoints (currently only has policy management)
      
      // When Router Admin has auth endpoints, this would call:
      // const response = await this.routerAdminClient.authenticate({ token, tenant_id: tenantId });
      
      this.logger.warn('Token validation not yet implemented in Router Admin');
      return null;
    } catch (error) {
      this.logger.error(`Token validation failed: ${error.message}`, error.stack);
      throw new UnauthorizedException(`Token validation failed: ${error.message}`);
    }
  }

  /**
   * Check if user has permission for resource/action
   * @param tenantId Tenant ID
   * @param userId User ID
   * @param resource Resource name
   * @param action Action name
   * @returns true if authorized, false otherwise
   */
  async checkPermission(
    tenantId: string,
    userId: string,
    resource: string,
    action: string,
  ): Promise<boolean> {
    if (!this.isEnabled()) {
      this.logger.debug('Router Auth disabled, skipping permission check');
      return false; // Fail-closed: deny if Router unavailable
    }

    // Check cache first
    const cacheKey = `rbac:${tenantId}:${userId}:${resource}:${action}`;
    const cached = this.getCache<boolean>(cacheKey);
    if (cached !== undefined) {
      this.logger.debug('Permission check cache hit');
      return cached;
    }

    try {
      // TODO: Implement actual permission check via Router Admin
      // For now, this is a placeholder that will be implemented when Router Admin
      // has authorization endpoints
      
      // When Router Admin has authz endpoints, this would call:
      // const response = await this.routerAdminClient.authorize({ tenant_id: tenantId, user_id: userId, resource, action });
      // const authorized = response.authorized;
      
      this.logger.warn('Permission check not yet implemented in Router Admin');
      
      // Fail-closed: deny if Router unavailable
      this.setCache(cacheKey, false, this.RBAC_CACHE_TTL);
      return false;
    } catch (error) {
      this.logger.error(`Permission check failed: ${error.message}`, error.stack);
      // Fail-closed: deny on error
      this.setCache(cacheKey, false, this.RBAC_CACHE_TTL);
      return false;
    }
  }

  /**
   * Check if user has required role
   * @param tenantId Tenant ID
   * @param userId User ID
   * @param role Required role
   * @returns true if user has role, false otherwise
   */
  async checkRole(tenantId: string, userId: string, role: string): Promise<boolean> {
    if (!this.isEnabled()) {
      this.logger.debug('Router Auth disabled, skipping role check');
      return false; // Fail-closed
    }

    // Check cache first
    const cacheKey = `rbac:role:${tenantId}:${userId}:${role}`;
    const cached = this.getCache<boolean>(cacheKey);
    if (cached !== undefined) {
      this.logger.debug('Role check cache hit');
      return cached;
    }

    try {
      // TODO: Implement actual role check via Router Admin
      // This would get user roles from Router Admin and check if role is present
      
      this.logger.warn('Role check not yet implemented in Router Admin');
      
      // Fail-closed: deny if Router unavailable
      this.setCache(cacheKey, false, this.RBAC_CACHE_TTL);
      return false;
    } catch (error) {
      this.logger.error(`Role check failed: ${error.message}`, error.stack);
      // Fail-closed: deny on error
      this.setCache(cacheKey, false, this.RBAC_CACHE_TTL);
      return false;
    }
  }

  /**
   * Get user roles
   * @param tenantId Tenant ID
   * @param userId User ID
   * @returns Array of role names
   */
  async getUserRoles(tenantId: string, userId: string): Promise<string[]> {
    if (!this.isEnabled()) {
      this.logger.debug('Router Auth disabled, returning empty roles');
      return [];
    }

    // Check cache first
    const cacheKey = `rbac:roles:${tenantId}:${userId}`;
    const cached = this.getCache<string[]>(cacheKey);
    if (cached) {
      this.logger.debug('User roles cache hit');
      return cached;
    }

    try {
      // TODO: Implement actual role retrieval via Router Admin
      // This would get user info from Router Admin and return roles
      
      this.logger.warn('Get user roles not yet implemented in Router Admin');
      
      const roles: string[] = [];
      this.setCache(cacheKey, roles, this.RBAC_CACHE_TTL);
      return roles;
    } catch (error) {
      this.logger.error(`Get user roles failed: ${error.message}`, error.stack);
      return [];
    }
  }

  /**
   * Get user permissions
   * @param tenantId Tenant ID
   * @param userId User ID
   * @returns Array of permission strings
   */
  async getUserPermissions(tenantId: string, userId: string): Promise<string[]> {
    if (!this.isEnabled()) {
      this.logger.debug('Router Auth disabled, returning empty permissions');
      return [];
    }

    // Check cache first
    const cacheKey = `rbac:permissions:${tenantId}:${userId}`;
    const cached = this.getCache<string[]>(cacheKey);
    if (cached) {
      this.logger.debug('User permissions cache hit');
      return cached;
    }

    try {
      // TODO: Implement actual permission retrieval via Router Admin
      // This would get user info from Router Admin and return permissions
      
      this.logger.warn('Get user permissions not yet implemented in Router Admin');
      
      const permissions: string[] = [];
      this.setCache(cacheKey, permissions, this.RBAC_CACHE_TTL);
      return permissions;
    } catch (error) {
      this.logger.error(`Get user permissions failed: ${error.message}`, error.stack);
      return [];
    }
  }

  /**
   * Invalidate cache for a user
   * @param tenantId Tenant ID
   * @param userId User ID
   */
  async invalidateUserCache(tenantId: string, userId: string): Promise<void> {
    const patterns = [
      `rbac:${tenantId}:${userId}:*`,
      `rbac:role:${tenantId}:${userId}:*`,
      `rbac:roles:${tenantId}:${userId}`,
      `rbac:permissions:${tenantId}:${userId}`,
    ];

    for (const pattern of patterns) {
      // Note: cache-manager doesn't support pattern deletion by default
      // This would need a custom cache implementation or manual key tracking
      this.logger.debug(`Cache invalidation requested for pattern: ${pattern}`);
    }
  }
}

