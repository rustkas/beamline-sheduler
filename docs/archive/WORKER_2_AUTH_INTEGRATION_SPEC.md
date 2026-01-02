# Worker 2: Authentication Integration - Detailed Technical Specification

## 🎯 Mission Overview

**Worker 2** (Architecture/Tech Lead) is responsible for creating the **unified authentication system** that integrates Gateway (NestJS) with Router Admin gRPC service. This ensures a single source of truth for authentication, authorization, and RBAC across the entire BeamLine Constructor platform.

## 📋 Priority 1 Tasks Breakdown

### 1. Router Admin gRPC Integration Architecture

#### **Core Service Architecture**

```typescript
// apps/gateway/src/auth/router-auth.service.ts

import { Injectable, Logger, OnModuleInit, OnModuleDestroy } from '@nestjs/common';
import { ClientGrpc, RpcException } from '@nestjs/microservices';
import { firstValueFrom, timeout, catchError, retry } from 'rxjs/operators';
import { Observable, of, throwError } from 'rxjs';

// gRPC service interfaces (generated from proto)
interface RouterAdminAuthService {
  validateToken(request: ValidateTokenRequest): Observable<ValidateTokenResponse>;
  checkPermissions(request: CheckPermissionsRequest): Observable<CheckPermissionsResponse>;
  getUserContext(request: GetUserContextRequest): Observable<GetUserContextResponse>;
  refreshToken(request: RefreshTokenRequest): Observable<RefreshTokenResponse>;
  revokeToken(request: RevokeTokenRequest): Observable<RevokeTokenResponse>;
}

interface ValidateTokenRequest {
  token: string;
  tenant_id: string;
  service_id: string;
}

interface ValidateTokenResponse {
  valid: boolean;
  user_id: string;
  tenant_id: string;
  roles: string[];
  permissions: string[];
  expires_at: string;
  metadata: Record<string, any>;
}

interface CheckPermissionsRequest {
  user_id: string;
  tenant_id: string;
  resource: string;
  action: string;
  context?: Record<string, any>;
}

interface CheckPermissionsResponse {
  allowed: boolean;
  reason?: string;
  alternative_actions?: string[];
  quota_remaining?: {
    requests: number;
    tokens: number;
    cost: number;
  };
}

@Injectable()
export class RouterAuthService implements OnModuleInit, OnModuleDestroy {
  private readonly logger = new Logger(RouterAuthService.name);
  private routerAdminClient: RouterAdminAuthService;
  private readonly GRPC_TIMEOUT = 5000; // 5 seconds
  private readonly MAX_RETRIES = 3;
  private readonly RETRY_DELAY = 1000; // 1 second

  constructor(
    @Inject('ROUTER_ADMIN_PACKAGE') private readonly client: ClientGrpc,
    private readonly cacheService: CacheService,
    private readonly metricsService: MetricsService
  ) {}

  onModuleInit() {
    this.routerAdminClient = this.client.getService<RouterAdminAuthService>('RouterAdminAuthService');
    this.logger.log('Router Admin gRPC client initialized');
  }

  onModuleDestroy() {
    this.logger.log('Router Auth service shutting down');
  }

  /**
   * Validate authentication token through Router Admin
   * Central authentication point for all Gateway requests
   */
  async validateToken(
    token: string,
    tenantId: string,
    serviceId: string = 'gateway'
  ): Promise<AuthValidationResult> {
    const cacheKey = `auth:${tenantId}:${this.hashToken(token)}`;
    
    // Check cache first
    const cachedResult = await this.cacheService.get<AuthValidationResult>(cacheKey);
    if (cachedResult) {
      this.metricsService.incrementAuthCacheHit();
      this.logger.debug('Auth validation cache hit', { tenantId });
      return cachedResult;
    }

    this.metricsService.incrementAuthCacheMiss();
    
    try {
      const request: ValidateTokenRequest = {
        token: this.sanitizeToken(token),
        tenant_id: tenantId,
        service_id: serviceId
      };

      this.logger.debug('Validating token with Router Admin', { 
        tenantId, 
        serviceId,
        tokenHash: this.hashToken(token) 
      });

      const response = await firstValueFrom(
        this.routerAdminClient.validateToken(request).pipe(
          timeout(this.GRPC_TIMEOUT),
          retry({
            count: this.MAX_RETRIES,
            delay: this.RETRY_DELAY,
            resetOnSuccess: true
          }),
          catchError((error: RpcException) => {
            this.logger.error('Token validation failed', {
              tenantId,
              serviceId,
              error: error.message,
              code: error.getError()?.code
            });
            
            this.metricsService.incrementAuthFailure();
            return throwError(() => this.mapGrpcError(error));
          })
        )
      );

      const result: AuthValidationResult = {
        valid: response.valid,
        userId: response.user_id,
        tenantId: response.tenant_id,
        roles: response.roles || [],
        permissions: response.permissions || [],
        expiresAt: new Date(response.expires_at),
        metadata: response.metadata || {},
        cached: false
      };

      // Cache successful validations
      if (response.valid) {
        const ttl = this.calculateCacheTTL(result.expiresAt);
        await this.cacheService.set(cacheKey, result, ttl);
        this.metricsService.incrementAuthSuccess();
      }

      return result;
    } catch (error) {
      this.logger.error('Unexpected error during token validation', {
        tenantId,
        serviceId,
        error: error.message
      });
      throw new UnauthorizedException('Authentication service unavailable');
    }
  }

  /**
   * Check user permissions for specific resource/action
   * RBAC integration with Router Admin
   */
  async checkPermissions(
    userId: string,
    tenantId: string,
    resource: string,
    action: string,
    context?: Record<string, any>
  ): Promise<PermissionCheckResult> {
    const cacheKey = `perms:${tenantId}:${userId}:${resource}:${action}:${this.hashContext(context)}`;
    
    // Check cache first
    const cachedResult = await this.cacheService.get<PermissionCheckResult>(cacheKey);
    if (cachedResult) {
      this.metricsService.incrementPermissionCacheHit();
      return cachedResult;
    }

    try {
      const request: CheckPermissionsRequest = {
        user_id: userId,
        tenant_id: tenantId,
        resource: resource,
        action: action,
        context: context || {}
      };

      this.logger.debug('Checking permissions with Router Admin', {
        userId,
        tenantId,
        resource,
        action
      });

      const response = await firstValueFrom(
        this.routerAdminClient.checkPermissions(request).pipe(
          timeout(this.GRPC_TIMEOUT),
          retry({
            count: this.MAX_RETRIES,
            delay: this.RETRY_DELAY
          }),
          catchError((error: RpcException) => {
            this.logger.error('Permission check failed', {
              userId,
              tenantId,
              resource,
              action,
              error: error.message
            });
            return throwError(() => this.mapGrpcError(error));
          })
        )
      );

      const result: PermissionCheckResult = {
        allowed: response.allowed,
        reason: response.reason,
        alternativeActions: response.alternative_actions || [],
        quotaRemaining: response.quota_remaining,
        cached: false
      };

      // Cache permission checks (shorter TTL than auth)
      if (result.allowed !== undefined) {
        await this.cacheService.set(cacheKey, result, 60); // 1 minute TTL
      }

      return result;
    } catch (error) {
      this.logger.error('Unexpected error during permission check', {
        userId,
        tenantId,
        resource,
        action,
        error: error.message
      });
      
      // Fail closed - deny access on service error
      return {
        allowed: false,
        reason: 'Permission service unavailable',
        cached: false
      };
    }
  }

  /**
   * Get comprehensive user context from Router Admin
   * Includes roles, permissions, quotas, and rate limits
   */
  async getUserContext(
    userId: string,
    tenantId: string
  ): Promise<UserContext> {
    const cacheKey = `context:${tenantId}:${userId}`;
    
    // Check cache first
    const cachedContext = await this.cacheService.get<UserContext>(cacheKey);
    if (cachedContext) {
      return cachedContext;
    }

    try {
      const request: GetUserContextRequest = {
        user_id: userId,
        tenant_id: tenantId
      };

      const response = await firstValueFrom(
        this.routerAdminClient.getUserContext(request).pipe(
          timeout(this.GRPC_TIMEOUT),
          retry({
            count: this.MAX_RETRIES,
            delay: this.RETRY_DELAY
          })
        )
      );

      const context: UserContext = {
        userId: response.user_id,
        tenantId: response.tenant_id,
        roles: response.roles || [],
        permissions: response.permissions || [],
        rateLimits: response.rate_limits || {},
        quotas: response.quotas || {},
        metadata: response.metadata || {},
        cached: false
      };

      // Cache user context
      await this.cacheService.set(cacheKey, context, 300); // 5 minutes TTL
      
      return context;
    } catch (error) {
      this.logger.error('Failed to get user context', {
        userId,
        tenantId,
        error: error.message
      });
      throw new ServiceUnavailableException('User context service unavailable');
    }
  }

  /**
   * Refresh authentication token
   */
  async refreshToken(
    refreshToken: string,
    tenantId: string
  ): Promise<TokenRefreshResult> {
    try {
      const request: RefreshTokenRequest = {
        refresh_token: refreshToken,
        tenant_id: tenantId
      };

      const response = await firstValueFrom(
        this.routerAdminClient.refreshToken(request).pipe(
          timeout(this.GRPC_TIMEOUT),
          retry({
            count: this.MAX_RETRIES,
            delay: this.RETRY_DELAY
          })
        )
      );

      return {
        accessToken: response.access_token,
        refreshToken: response.refresh_token,
        expiresAt: new Date(response.expires_at),
        tokenType: response.token_type || 'Bearer'
      };
    } catch (error) {
      this.logger.error('Token refresh failed', {
        tenantId,
        error: error.message
      });
      throw new UnauthorizedException('Token refresh failed');
    }
  }

  /**
   * Revoke authentication token
   */
  async revokeToken(
    token: string,
    tenantId: string
  ): Promise<void> {
    try {
      const request: RevokeTokenRequest = {
        token: token,
        tenant_id: tenantId
      };

      await firstValueFrom(
        this.routerAdminClient.revokeToken(request).pipe(
          timeout(this.GRPC_TIMEOUT)
        )
      );

      // Clear cache entries for revoked token
      const cacheKey = `auth:${tenantId}:${this.hashToken(token)}`;
      await this.cacheService.del(cacheKey);
      
      this.logger.log('Token revoked successfully', { tenantId });
    } catch (error) {
      this.logger.error('Token revocation failed', {
        tenantId,
        error: error.message
      });
      // Don't throw - revocation is best effort
    }
  }

  /**
   * Health check for Router Admin connection
   */
  async healthCheck(): Promise<HealthStatus> {
    try {
      // Simple ping to Router Admin
      await firstValueFrom(
        this.routerAdminClient.validateToken({
          token: 'health-check',
          tenant_id: 'system',
          service_id: 'gateway'
        }).pipe(
          timeout(2000),
          catchError(() => of({ valid: false } as ValidateTokenResponse))
        )
      );

      return {
        status: 'healthy',
        timestamp: new Date().toISOString(),
        details: {
          service: 'Router Admin gRPC',
          reachable: true
        }
      };
    } catch (error) {
      return {
        status: 'unhealthy',
        timestamp: new Date().toISOString(),
        details: {
          service: 'Router Admin gRPC',
          reachable: false,
          error: error.message
        }
      };
    }
  }

  /**
   * Utility methods
   */
  private hashToken(token: string): string {
    // Use crypto to create a hash for cache keys
    return crypto.createHash('sha256').update(token).digest('hex').substring(0, 16);
  }

  private sanitizeToken(token: string): string {
    // Remove Bearer prefix if present
    return token.replace(/^Bearer\s+/i, '');
  }

  private hashContext(context?: Record<string, any>): string {
    if (!context) return 'empty';
    return crypto.createHash('sha256').update(JSON.stringify(context)).digest('hex').substring(0, 8);
  }

  private calculateCacheTTL(expiresAt: Date): number {
    const now = new Date();
    const diffMs = expiresAt.getTime() - now.getTime();
    // Cache until 5 minutes before expiration, minimum 1 minute
    return Math.max(Math.floor((diffMs - 300000) / 1000), 60);
  }

  private mapGrpcError(error: RpcException): HttpException {
    const errorCode = error.getError()?.code;
    const message = error.message || 'Authentication service error';

    switch (errorCode) {
      case 'UNAUTHENTICATED':
      case 'UNAUTHORIZED':
        return new UnauthorizedException(message);
      case 'PERMISSION_DENIED':
        return new ForbiddenException(message);
      case 'NOT_FOUND':
        return new NotFoundException(message);
      case 'INVALID_ARGUMENT':
        return new BadRequestException(message);
      case 'RESOURCE_EXHAUSTED':
        return new TooManyRequestsException(message);
      case 'UNAVAILABLE':
      case 'DEADLINE_EXCEEDED':
        return new ServiceUnavailableException('Authentication service unavailable');
      default:
        return new InternalServerErrorException('Authentication service error');
    }
  }
}
```

### 2. Unified Authentication Context System

#### **Context Management Architecture**

```typescript
// apps/gateway/src/auth/auth-context.service.ts

import { Injectable, Logger, Scope, Inject } from '@nestjs/common';
import { REQUEST } from '@nestjs/core';
import { Request } from 'express';

/**
 * Unified authentication context that follows the request lifecycle
 * Provides consistent auth context across all Gateway operations
 */
@Injectable({ scope: Scope.REQUEST })
export class AuthContextService {
  private readonly logger = new Logger(AuthContextService.name);
  private context: AuthContext;
  private initialized = false;

  constructor(
    @Inject(REQUEST) private readonly request: Request,
    private readonly routerAuthService: RouterAuthService,
    private readonly cacheService: CacheService
  ) {}

  /**
   * Initialize authentication context for the current request
   * Called by AuthGuard on request start
   */
  async initialize(): Promise<void> {
    if (this.initialized) {
      return;
    }

    const startTime = Date.now();
    
    try {
      // Extract auth data from request
      const token = this.extractTokenFromRequest();
      const tenantId = this.extractTenantIdFromRequest();
      
      if (!token || !tenantId) {
        throw new UnauthorizedException('Missing authentication credentials');
      }

      // Validate token with Router Admin
      const validationResult = await this.routerAuthService.validateToken(token, tenantId);
      
      if (!validationResult.valid) {
        throw new UnauthorizedException('Invalid authentication token');
      }

      // Build unified context
      this.context = {
        requestId: this.request.id || this.generateRequestId(),
        tenantId: validationResult.tenantId,
        userId: validationResult.userId,
        token: token,
        roles: validationResult.roles,
        permissions: validationResult.permissions,
        expiresAt: validationResult.expiresAt,
        metadata: validationResult.metadata,
        validatedAt: new Date(),
        cached: validationResult.cached
      };

      // Store context in request for access by other services
      this.request.authContext = this.context;
      this.initialized = true;

      const duration = Date.now() - startTime;
      this.logger.debug('Authentication context initialized', {
        requestId: this.context.requestId,
        tenantId: this.context.tenantId,
        userId: this.context.userId,
        duration: `${duration}ms`,
        cached: this.context.cached
      });

    } catch (error) {
      this.logger.error('Failed to initialize authentication context', {
        requestId: this.request.id,
        error: error.message
      });
      throw error;
    }
  }

  /**
   * Get current authentication context
   */
  getContext(): AuthContext {
    if (!this.initialized) {
      throw new Error('Authentication context not initialized');
    }
    return this.context;
  }

  /**
   * Check if user has specific permission
   */
  hasPermission(permission: string): boolean {
    if (!this.initialized) {
      return false;
    }
    return this.context.permissions.includes(permission);
  }

  /**
   * Check if user has any of the specified permissions
   */
  hasAnyPermission(permissions: string[]): boolean {
    if (!this.initialized) {
      return false;
    }
    return permissions.some(permission => this.context.permissions.includes(permission));
  }

  /**
   * Check if user has all of the specified permissions
   */
  hasAllPermissions(permissions: string[]): boolean {
    if (!this.initialized) {
      return false;
    }
    return permissions.every(permission => this.context.permissions.includes(permission));
  }

  /**
   * Check if user has specific role
   */
  hasRole(role: string): boolean {
    if (!this.initialized) {
      return false;
    }
    return this.context.roles.includes(role);
  }

  /**
   * Check if user has any of the specified roles
   */
  hasAnyRole(roles: string[]): boolean {
    if (!this.initialized) {
      return false;
    }
    return roles.some(role => this.context.roles.includes(role));
  }

  /**
   * Check if authentication is still valid (not expired)
   */
  isValid(): boolean {
    if (!this.initialized) {
      return false;
    }
    return new Date() < this.context.expiresAt;
  }

  /**
   * Refresh authentication context if needed
   */
  async refreshIfNeeded(): Promise<void> {
    if (!this.initialized) {
      return;
    }

    const now = new Date();
    const expiresAt = this.context.expiresAt;
    const timeUntilExpiry = expiresAt.getTime() - now.getTime();
    const refreshThreshold = 5 * 60 * 1000; // 5 minutes

    if (timeUntilExpiry < refreshThreshold) {
      this.logger.debug('Refreshing authentication context', {
        requestId: this.context.requestId,
        userId: this.context.userId,
        expiresAt: expiresAt.toISOString()
      });

      // Re-initialize with current token
      await this.initialize();
    }
  }

  /**
   * Get context for logging/auditing
   */
  getAuditContext(): AuditContext {
    if (!this.initialized) {
      return {
        requestId: this.request.id || 'unknown',
        authenticated: false
      };
    }

    return {
      requestId: this.context.requestId,
      tenantId: this.context.tenantId,
      userId: this.context.userId,
      roles: this.context.roles,
      permissions: this.context.permissions,
      authenticated: true,
      validatedAt: this.context.validatedAt
    };
  }

  /**
   * Extract token from request headers
   */
  private extractTokenFromRequest(): string | null {
    const authHeader = this.request.headers.authorization;
    if (!authHeader) {
      return null;
    }

    const parts = authHeader.split(' ');
    if (parts.length !== 2 || parts[0].toLowerCase() !== 'bearer') {
      return null;
    }

    return parts[1];
  }

  /**
   * Extract tenant ID from request
   */
  private extractTenantIdFromRequest(): string | null {
    // Priority order for tenant ID extraction:
    // 1. X-Tenant-ID header
    // 2. subdomain (e.g., tenant1.api.beamline.com)
    // 3. Default tenant from config

    const tenantHeader = this.request.headers['x-tenant-id'] as string;
    if (tenantHeader) {
      return tenantHeader;
    }

    // Extract from subdomain
    const host = this.request.headers.host;
    if (host) {
      const subdomain = host.split('.')[0];
      if (subdomain && subdomain !== 'api' && subdomain !== 'gateway') {
        return subdomain;
      }
    }

    return process.env.DEFAULT_TENANT_ID || 'default';
  }

  private generateRequestId(): string {
    return `req_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
  }
}

/**
 * Authentication context interfaces
 */
export interface AuthContext {
  requestId: string;
  tenantId: string;
  userId: string;
  token: string;
  roles: string[];
  permissions: string[];
  expiresAt: Date;
  metadata: Record<string, any>;
  validatedAt: Date;
  cached: boolean;
}

export interface AuditContext {
  requestId: string;
  tenantId?: string;
  userId?: string;
  roles?: string[];
  permissions?: string[];
  authenticated: boolean;
  validatedAt?: Date;
}

export interface AuthValidationResult {
  valid: boolean;
  userId: string;
  tenantId: string;
  roles: string[];
  permissions: string[];
  expiresAt: Date;
  metadata: Record<string, any>;
  cached: boolean;
}

export interface PermissionCheckResult {
  allowed: boolean;
  reason?: string;
  alternativeActions?: string[];
  quotaRemaining?: {
    requests: number;
    tokens: number;
    cost: number;
  };
  cached: boolean;
}
```

### 3. RBAC Mapping Between Router and Gateway

#### **Role-Based Access Control Integration**

```typescript
// apps/gateway/src/auth/rbac-mapping.service.ts

import { Injectable, Logger } from '@nestjs/common';

/**
 * Maps Router RBAC system to Gateway permissions
 * Provides unified permission model across the platform
 */
@Injectable()
export class RBACMappingService {
  private readonly logger = new Logger(RBACMappingService.name);
  
  /**
   * Router role to Gateway permissions mapping
   * Defines how Router roles translate to Gateway-specific permissions
   */
  private readonly rolePermissionMapping: Record<string, string[]> = {
    // Platform roles
    'platform:admin': [
      '*', // All permissions
      'system:*',
      'tenant:*',
      'user:*',
      'model:*',
      'provider:*',
      'policy:*',
      'audit:*'
    ],
    'platform:operator': [
      'system:read',
      'tenant:read',
      'user:read',
      'model:read',
      'provider:read',
      'policy:read',
      'audit:read'
    ],
    
    // Tenant roles
    'tenant:admin': [
      'tenant:manage',
      'user:manage',
      'model:manage',
      'provider:manage',
      'policy:manage',
      'audit:read',
      'usage:read'
    ],
    'tenant:developer': [
      'user:read',
      'model:use',
      'provider:use',
      'policy:read',
      'usage:read',
      'api:use'
    ],
    'tenant:user': [
      'model:use',
      'provider:use',
      'usage:read',
      'api:use'
    ],
    
    // Service-specific roles
    'service:gateway': [
      'auth:validate',
      'auth:refresh',
      'user:read',
      'tenant:read',
      'model:use',
      'provider:use',
      'policy:read',
      'usage:report'
    ],
    'service:worker': [
      'task:execute',
      'model:use',
      'provider:use',
      'usage:report'
    ],
    'service:monitor': [
      'system:read',
      'metrics:read',
      'logs:read',
      'health:read'
    ]
  };

  /**
   * Resource-specific permission mappings
   */
  private readonly resourcePermissionMapping: Record<string, Record<string, string[]>> = {
    'chat.completion': {
      'create': ['api:use', 'model:use'],
      'read': ['api:read', 'usage:read'],
      'stream': ['api:use', 'model:use']
    },
    'completions': {
      'create': ['api:use', 'model:use'],
      'read': ['api:read', 'usage:read']
    },
    'embeddings': {
      'create': ['api:use', 'model:use'],
      'read': ['api:read', 'usage:read']
    },
    'images': {
      'generate': ['api:use', 'model:use'],
      'edit': ['api:use', 'model:use'],
      'read': ['api:read', 'usage:read']
    },
    'audio': {
      'transcribe': ['api:use', 'model:use'],
      'translate': ['api:use', 'model:use'],
      'synthesize': ['api:use', 'model:use']
    },
    'files': {
      'upload': ['storage:write'],
      'download': ['storage:read'],
      'delete': ['storage:write'],
      'list': ['storage:read']
    },
    'models': {
      'list': ['model:read'],
      'retrieve': ['model:read'],
      'use': ['model:use']
    },
    'usage': {
      'read': ['usage:read'],
      'report': ['usage:report']
    },
    'policies': {
      'read': ['policy:read'],
      'create': ['policy:write'],
      'update': ['policy:write'],
      'delete': ['policy:write']
    }
  };

  /**
   * Convert Router roles to Gateway permissions
   */
  mapRouterRolesToGatewayPermissions(routerRoles: string[]): string[] {
    const gatewayPermissions = new Set<string>();
    
    for (const routerRole of routerRoles) {
      const permissions = this.rolePermissionMapping[routerRole];
      if (permissions) {
        permissions.forEach(permission => gatewayPermissions.add(permission));
      } else {
        this.logger.warn('Unknown Router role', { role: routerRole });
      }
    }

    // Handle wildcard permissions
    if (gatewayPermissions.has('*')) {
      return ['*'];
    }

    return Array.from(gatewayPermissions);
  }

  /**
   * Check if user has permission for specific resource and action
   */
  checkResourcePermission(
    userPermissions: string[],
    resource: string,
    action: string
  ): PermissionCheck {
    // Admin wildcard check
    if (userPermissions.includes('*')) {
      return {
        allowed: true,
        reason: 'Admin wildcard permission'
      };
    }

    // Check specific resource permissions
    const resourceMapping = this.resourcePermissionMapping[resource];
    if (!resourceMapping) {
      return {
        allowed: false,
        reason: `Unknown resource: ${resource}`
      };
    }

    const requiredPermissions = resourceMapping[action];
    if (!requiredPermissions) {
      return {
        allowed: false,
        reason: `Unknown action: ${action} for resource: ${resource}`
      };
    }

    // Check if user has any of the required permissions
    const hasPermission = requiredPermissions.some(permission => 
      userPermissions.includes(permission)
    );

    if (hasPermission) {
      return {
        allowed: true,
        reason: `Has required permission for ${resource}:${action}`
      };
    }

    return {
      allowed: false,
      reason: `Missing required permission for ${resource}:${action}`,
      requiredPermissions: requiredPermissions,
      userPermissions: userPermissions
    };
  }

  /**
   * Get hierarchical permissions for user
   */
  getHierarchicalPermissions(userRoles: string[]): HierarchicalPermissions {
    const permissions = this.mapRouterRolesToGatewayPermissions(userRoles);
    
    return {
      direct: permissions.filter(p => !p.includes('*')),
      wildcard: permissions.filter(p => p.includes('*')),
      inherited: this.getInheritedPermissions(permissions),
      effective: this.calculateEffectivePermissions(permissions)
    };
  }

  /**
   * Validate permission assignment
   */
  validatePermissionAssignment(
    userRoles: string[],
    targetPermission: string
  ): ValidationResult {
    const userPermissions = this.mapRouterRolesToGatewayPermissions(userRoles);
    
    // Check if user can assign this permission
    const canAssign = this.canAssignPermission(userPermissions, targetPermission);
    
    return {
      valid: canAssign,
      reason: canAssign 
        ? `User can assign permission: ${targetPermission}`
        : `User lacks permission to assign: ${targetPermission}`,
      userPermissions: userPermissions,
      targetPermission: targetPermission
    };
  }

  /**
   * Get permission dependencies
   */
  getPermissionDependencies(permission: string): string[] {
    const dependencies: Record<string, string[]> = {
      'tenant:manage': ['tenant:read', 'user:manage', 'policy:manage'],
      'user:manage': ['user:read', 'user:create', 'user:update', 'user:delete'],
      'model:manage': ['model:read', 'model:create', 'model:update', 'model:delete'],
      'provider:manage': ['provider:read', 'provider:create', 'provider:update', 'provider:delete'],
      'policy:manage': ['policy:read', 'policy:create', 'policy:update', 'policy:delete'],
      'api:use': ['auth:validate', 'model:use'],
      'storage:write': ['storage:read']
    };

    return dependencies[permission] || [];
  }

  /**
   * Private helper methods
   */
  private getInheritedPermissions(directPermissions: string[]): string[] {
    const inherited = new Set<string>();
    
    for (const permission of directPermissions) {
      const dependencies = this.getPermissionDependencies(permission);
      dependencies.forEach(dep => inherited.add(dep));
    }

    return Array.from(inherited);
  }

  private calculateEffectivePermissions(permissions: string[]): string[] {
    if (permissions.includes('*')) {
      return ['*'];
    }

    const effective = new Set<string>(permissions);
    
    // Add inherited permissions
    for (const permission of permissions) {
      const dependencies = this.getPermissionDependencies(permission);
      dependencies.forEach(dep => effective.add(dep));
    }

    return Array.from(effective);
  }

  private canAssignPermission(userPermissions: string[], targetPermission: string): boolean {
    // Admin can assign any permission
    if (userPermissions.includes('*')) {
      return true;
    }

    // Check for specific assignment permissions
    const assignmentPermissions = userPermissions.filter(p => p.includes(':assign'));
    
    // Check if user has permission to assign the target permission
    return assignmentPermissions.some(permission => {
      const [, assignablePermission] = permission.split(':');
      return assignablePermission === targetPermission || assignablePermission === '*';
    });
  }
}

/**
 * RBAC interfaces and types
 */
export interface PermissionCheck {
  allowed: boolean;
  reason: string;
  requiredPermissions?: string[];
  userPermissions?: string[];
}

export interface HierarchicalPermissions {
  direct: string[];
  wildcard: string[];
  inherited: string[];
  effective: string[];
}

export interface ValidationResult {
  valid: boolean;
  reason: string;
  userPermissions: string[];
  targetPermission: string;
}
```

### 4. Removal of Local Allowlists

#### **Migration from Local to Centralized Authorization**

```typescript
// apps/gateway/src/auth/migration/local-allowlist-removal.service.ts

import { Injectable, Logger } from '@nestjs/common';

/**
 * Service to migrate from local allowlists to Router Admin RBAC
 * Ensures smooth transition without breaking existing functionality
 */
@Injectable()
export class LocalAllowlistRemovalService {
  private readonly logger = new Logger(LocalAllowlistRemovalService.name);

  constructor(
    private readonly routerAuthService: RouterAuthService,
    private readonly rbacMappingService: RBACMappingService,
    private readonly configService: ConfigService
  ) {}

  /**
   * Migrate local allowlist entries to Router Admin policies
   * Phase 1: Read existing allowlists and create Router policies
   */
  async migrateLocalAllowlists(): Promise<MigrationResult> {
    this.logger.log('Starting local allowlist migration to Router Admin');

    const results: MigrationResult = {
      totalProcessed: 0,
      successful: 0,
      failed: 0,
      warnings: [],
      errors: []
    };

    try {
      // 1. Read existing local allowlists
      const localAllowlists = await this.readLocalAllowlists();
      
      this.logger.log(`Found ${localAllowlists.length} local allowlist entries`);

      // 2. Convert to Router Admin policies
      for (const allowlist of localAllowlists) {
        try {
          const policy = this.convertAllowlistToPolicy(allowlist);
          await this.createRouterPolicy(policy);
          results.successful++;
          
          this.logger.debug(`Migrated allowlist entry: ${allowlist.id}`);
        } catch (error) {
          results.failed++;
          results.errors.push({
            id: allowlist.id,
            error: error.message
          });
          
          this.logger.error(`Failed to migrate allowlist entry: ${allowlist.id}`, {
            error: error.message
          });
        }
        
        results.totalProcessed++;
      }

      // 3. Validate migration
      await this.validateMigration(results);

      this.logger.log('Local allowlist migration completed', {
        total: results.totalProcessed,
        successful: results.successful,
        failed: results.failed
      });

      return results;
    } catch (error) {
      this.logger.error('Migration process failed', {
        error: error.message
      });
      throw error;
    }
  }

  /**
   * Read existing local allowlist configurations
   */
  private async readLocalAllowlists(): Promise<LocalAllowlistEntry[]> {
    const allowlists: LocalAllowlistEntry[] = [];

    // Read from environment variables
    const envAllowlists = this.configService.get<string>('AUTH_LOCAL_ALLOWLISTS');
    if (envAllowlists) {
      try {
        const parsed = JSON.parse(envAllowlists);
        allowlists.push(...parsed);
      } catch (error) {
        this.logger.warn('Failed to parse environment allowlists', { error: error.message });
      }
    }

    // Read from file-based configuration
    const configPath = this.configService.get<string>('AUTH_ALLOWLIST_CONFIG_PATH');
    if (configPath) {
      try {
        const fileContent = await fs.readFile(configPath, 'utf8');
        const fileAllowlists = JSON.parse(fileContent);
        allowlists.push(...fileAllowlists);
      } catch (error) {
        this.logger.warn('Failed to read file-based allowlists', { 
          path: configPath,
          error: error.message 
        });
      }
    }

    // Read from database (if applicable)
    try {
      const dbAllowlists = await this.readDatabaseAllowlists();
      allowlists.push(...dbAllowlists);
    } catch (error) {
      this.logger.warn('Failed to read database allowlists', { error: error.message });
    }

    return allowlists;
  }

  /**
   * Convert local allowlist entry to Router Admin policy
   */
  private convertAllowlistToPolicy(allowlist: LocalAllowlistEntry): RouterPolicy {
    const policy: RouterPolicy = {
      id: `migrated_${allowlist.id}`,
      name: `Migrated: ${allowlist.name || allowlist.id}`,
      description: `Migrated from local allowlist: ${allowlist.description || 'No description'}`,
      tenant_id: allowlist.tenant_id || 'default',
      version: '1.0',
      rules: this.buildPolicyRules(allowlist),
      metadata: {
        migrated_from: 'local_allowlist',
        original_id: allowlist.id,
        migrated_at: new Date().toISOString(),
        migration_version: '1.0'
      },
      created_at: new Date().toISOString(),
      updated_at: new Date().toISOString()
    };

    return policy;
  }

  /**
   * Build policy rules from allowlist entry
   */
  private buildPolicyRules(allowlist: LocalAllowlistEntry): PolicyRule[] {
    const rules: PolicyRule[] = [];

    // User-based allowlist
    if (allowlist.user_ids && allowlist.user_ids.length > 0) {
      rules.push({
        id: 'user_allowlist',
        type: 'user_selection',
        priority: 100,
        conditions: [
          {
            field: 'user_id',
            operator: 'in',
            value: allowlist.user_ids
          }
        ],
        actions: [
          {
            type: 'allow',
            parameters: {
              reason: 'User in allowlist',
              allowlist_type: 'user'
            }
          }
        ]
      });
    }

    // IP-based allowlist
    if (allowlist.ip_ranges && allowlist.ip_ranges.length > 0) {
      rules.push({
        id: 'ip_allowlist',
        type: 'ip_filtering',
        priority: 90,
        conditions: [
          {
            field: 'client_ip',
            operator: 'in_range',
            value: allowlist.ip_ranges
          }
        ],
        actions: [
          {
            type: 'allow',
            parameters: {
              reason: 'IP in allowlist',
              allowlist_type: 'ip'
            }
          }
        ]
      });
    }

    // API key-based allowlist
    if (allowlist.api_key_patterns && allowlist.api_key_patterns.length > 0) {
      rules.push({
        id: 'api_key_allowlist',
        type: 'api_key_validation',
        priority: 80,
        conditions: [
          {
            field: 'api_key_pattern',
            operator: 'matches',
            value: allowlist.api_key_patterns
          }
        ],
        actions: [
          {
            type: 'allow',
            parameters: {
              reason: 'API key pattern matches allowlist',
              allowlist_type: 'api_key'
            }
          }
        ]
      });
    }

    // Time-based allowlist
    if (allowlist.time_restrictions) {
      rules.push({
        id: 'time_allowlist',
        type: 'time_based',
        priority: 70,
        conditions: this.buildTimeConditions(allowlist.time_restrictions),
        actions: [
          {
            type: 'allow',
            parameters: {
              reason: 'Within allowed time window',
              allowlist_type: 'time'
            }
          }
        ]
      });
    }

    return rules;
  }

  /**
   * Build time-based conditions
   */
  private buildTimeConditions(restrictions: TimeRestriction[]): Condition[] {
    return restrictions.map(restriction => ({
      field: 'current_time',
      operator: 'between',
      value: {
        start: restriction.start_time,
        end: restriction.end_time,
        timezone: restriction.timezone || 'UTC',
        days: restriction.days || ['monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday']
      }
    }));
  }

  /**
   * Create Router policy through gRPC
   */
  private async createRouterPolicy(policy: RouterPolicy): Promise<void> {
    try {
      await this.routerAuthService.createPolicy(policy);
      
      this.logger.info('Created Router policy from allowlist', {
        policyId: policy.id,
        tenantId: policy.tenant_id
      });
    } catch (error) {
      this.logger.error('Failed to create Router policy', {
        policyId: policy.id,
        error: error.message
      });
      throw error;
    }
  }

  /**
   * Validate migration results
   */
  private async validateMigration(results: MigrationResult): Promise<void> {
    this.logger.log('Validating migration results');

    // Check for critical failures
    if (results.failed > results.successful) {
      results.warnings.push('More failures than successes - manual review required');
    }

    // Verify sample policies were created
    try {
      const samplePolicy = await this.routerAuthService.getPolicy('migrated_sample');
      if (!samplePolicy) {
        results.warnings.push('Sample policy not found - migration may have issues');
      }
    } catch (error) {
      results.warnings.push('Could not verify sample policy creation');
    }

    // Generate migration report
    await this.generateMigrationReport(results);
  }

  /**
   * Generate detailed migration report
   */
  private async generateMigrationReport(results: MigrationResult): Promise<void> {
    const report = {
      timestamp: new Date().toISOString(),
      summary: {
        total: results.totalProcessed,
        successful: results.successful,
        failed: results.failed,
        successRate: results.totalProcessed > 0 
          ? (results.successful / results.totalProcessed * 100).toFixed(2) + '%'
          : '0%'
      },
      warnings: results.warnings,
      errors: results.errors,
      recommendations: this.generateRecommendations(results)
    };

    // Save report to file
    const reportPath = `migration_report_${Date.now()}.json`;
    await fs.writeFile(reportPath, JSON.stringify(report, null, 2));
    
    this.logger.log(`Migration report saved to ${reportPath}`);
  }

  /**
   * Generate migration recommendations
   */
  private generateRecommendations(results: MigrationResult): string[] {
    const recommendations: string[] = [];

    if (results.failed > 0) {
      recommendations.push('Review failed migrations and manually create equivalent policies');
      recommendations.push('Test migrated policies before removing local allowlists');
    }

    if (results.warnings.length > 0) {
      recommendations.push('Address migration warnings before proceeding');
    }

    if (results.successful === results.totalProcessed) {
      recommendations.push('All migrations successful - proceed with allowlist removal');
      recommendations.push('Monitor Router Admin policies for performance');
    }

    return recommendations;
  }

  /**
   * Phase 2: Remove local allowlists after successful migration
   */
  async removeLocalAllowlists(): Promise<void> {
    this.logger.log('Removing local allowlists after successful migration');

    try {
      // Remove from environment variables
      delete process.env.AUTH_LOCAL_ALLOWLISTS;
      
      // Remove from configuration files
      const configPath = this.configService.get<string>('AUTH_ALLOWLIST_CONFIG_PATH');
      if (configPath && fs.existsSync(configPath)) {
        await fs.unlink(configPath);
        this.logger.log(`Removed allowlist config file: ${configPath}`);
      }

      // Remove from database (if applicable)
      await this.removeDatabaseAllowlists();

      this.logger.log('Local allowlists removed successfully');
    } catch (error) {
      this.logger.error('Failed to remove local allowlists', {
        error: error.message
      });
      throw error;
    }
  }

  /**
   * Read database allowlists (placeholder implementation)
   */
  private async readDatabaseAllowlists(): Promise<LocalAllowlistEntry[]> {
    // Implementation depends on database schema
    this.logger.debug('Reading database allowlists');
    return [];
  }

  /**
   * Remove database allowlists (placeholder implementation)
   */
  private async removeDatabaseAllowlists(): Promise<void> {
    // Implementation depends on database schema
    this.logger.debug('Removing database allowlists');
  }
}

/**
 * Migration interfaces
 */
export interface MigrationResult {
  totalProcessed: number;
  successful: number;
  failed: number;
  warnings: string[];
  errors: MigrationError[];
}

export interface MigrationError {
  id: string;
  error: string;
}

export interface LocalAllowlistEntry {
  id: string;
  name?: string;
  description?: string;
  tenant_id?: string;
  user_ids?: string[];
  ip_ranges?: string[];
  api_key_patterns?: string[];
  time_restrictions?: TimeRestriction[];
  created_at?: string;
  updated_at?: string;
}

export interface TimeRestriction {
  start_time: string;
  end_time: string;
  timezone?: string;
  days?: string[];
}

export interface RouterPolicy {
  id: string;
  name: string;
  description?: string;
  tenant_id: string;
  version: string;
  rules: PolicyRule[];
  metadata?: Record<string, any>;
  created_at: string;
  updated_at: string;
}

export interface PolicyRule {
  id: string;
  type: string;
  priority: number;
  conditions: Condition[];
  actions: Action[];
}

export interface Condition {
  field: string;
  operator: string;
  value: any;
}

export interface Action {
  type: string;
  parameters?: Record<string, any>;
}
```

## 📊 Performance Requirements

### Authentication Performance Targets
- **Token validation**: < 50ms (cached), < 200ms (uncached)
- **Permission checks**: < 30ms (cached), < 100ms (uncached)
- **Context retrieval**: < 100ms (cached), < 300ms (uncached)
- **gRPC connection pool**: 10-50 connections
- **Cache hit rate**: > 90% for auth, > 85% for permissions

### Scalability Requirements
- **Concurrent authentications**: 10,000+ per second
- **Active sessions**: 1,000,000+ per Gateway instance
- **Permission checks**: 50,000+ per second
- **Token refresh rate**: < 1% of total requests

### Security Requirements
- **Token validation**: Zero tolerance for invalid tokens
- **Permission elevation**: Strictly controlled through RBAC
- **Audit logging**: 100% of auth events logged
- **Rate limiting**: Per-user and per-tenant limits enforced

## 🔧 Implementation Guidelines

### Code Quality Standards
1. **Type Safety**: Strict TypeScript with comprehensive interfaces
2. **Error Handling**: Graceful degradation with proper fallbacks
3. **Logging**: Structured logging with correlation IDs
4. **Metrics**: Comprehensive auth metrics collection
5. **Testing**: 100% coverage for auth critical paths

### Migration Strategy
1. **Phase 1**: Deploy Router Admin auth service alongside existing auth
2. **Phase 2**: Dual-write to both systems for validation
3. **Phase 3**: Gradual traffic migration with feature flags
4. **Phase 4**: Complete cutover and removal of local auth
5. **Phase 5**: Cleanup and optimization

### Monitoring Requirements
- **Authentication success rate**: > 99.9%
- **Permission check success rate**: > 99.95%
- **gRPC error rate**: < 0.1%
- **Cache hit rate**: > 90%
- **Token refresh rate**: < 1%

This comprehensive specification provides Worker 2 with everything needed to implement a production-ready unified authentication system that ensures perfect synchronization between Router and Gateway components.