import { Injectable, CanActivate, ExecutionContext, UnauthorizedException, ForbiddenException, Logger } from '@nestjs/common';
import { Reflector } from '@nestjs/core';
import { Request } from 'express';
import { Observable } from 'rxjs';
import { RouterAdminGrpcService } from './router-admin-grpc.service';
import { RouterAuthService } from './router-auth.service';
import { TracingService } from '../observability/tracing.service';

export interface UnifiedAuthContext {
  tenant: {
    id: string;
    name: string;
    status: 'active' | 'suspended' | 'inactive';
    settings?: Record<string, any>;
  };
  user: {
    id: string;
    email: string;
    name: string;
    roles: string[];
    permissions: string[];
    status: 'active' | 'suspended' | 'inactive';
    metadata?: Record<string, any>;
  };
  quota: {
    requests_per_minute: number;
    requests_per_hour: number;
    requests_per_day: number;
    tokens_per_minute: number;
    tokens_per_hour: number;
    tokens_per_day: number;
    current_usage?: {
      requests_this_minute: number;
      requests_this_hour: number;
      requests_today: number;
      tokens_this_minute: number;
      tokens_this_hour: number;
      tokens_today: number;
    };
  };
  api_key?: {
    id: string;
    name: string;
    scopes: string[];
    expires_at?: string;
  };
  session?: {
    id: string;
    created_at: string;
    expires_at: string;
    metadata?: Record<string, any>;
  };
  trace_id: string;
  authenticated_at: string;
  authentication_method: 'jwt' | 'api_key' | 'session' | 'anonymous';
}

export interface AuthRequirements {
  roles?: string[];
  permissions?: string[];
  resources?: string[];
  actions?: string[];
  requireTenant?: boolean;
  requireUser?: boolean;
  allowApiKey?: boolean;
  allowAnonymous?: boolean;
}

@Injectable()
export class UnifiedAuthContextService {
  private readonly logger = new Logger(UnifiedAuthContextService.name);
  private readonly useRouterAuth: boolean;

  constructor(
    private readonly routerAdminService: RouterAdminGrpcService,
    private readonly routerAuthService: RouterAuthService,
    private readonly reflector: Reflector,
    private readonly telemetry: TracingService,
  ) {
    // Feature flag: USE_ROUTER_AUTH
    this.useRouterAuth = process.env.USE_ROUTER_AUTH === 'true';
    this.logger.log(`UnifiedAuthContextService initialized. USE_ROUTER_AUTH: ${this.useRouterAuth}`);
  }

  async createContext(request: Request): Promise<UnifiedAuthContext> {
    const traceId = this.telemetry.getTraceId();
    const span = this.telemetry.startSpan('auth.create_context', { traceId });
    
    try {
      // Extract authentication credentials
      const authHeader = request.headers.authorization;
      const apiKey = request.headers['x-api-key'] as string;
      const sessionCookie = request.cookies?.session_id;
      
      let context: UnifiedAuthContext;
      
      if (authHeader?.startsWith('Bearer ')) {
        context = await this.createJwtContext(authHeader.substring(7), traceId);
      } else if (apiKey) {
        context = await this.createApiKeyContext(apiKey, traceId);
      } else if (sessionCookie) {
        context = await this.createSessionContext(sessionCookie, traceId);
      } else {
        context = await this.createAnonymousContext(traceId);
      }
      
      // Store context in request for later use
      (request as any).authContext = context;
      
      span.setAttributes({
        'auth.tenant_id': context.tenant.id,
        'auth.user_id': context.user?.id || 'anonymous',
        'auth.method': context.authentication_method,
        'auth.authenticated': context.user !== undefined
      });
      
      return context;
    } catch (error) {
      span.recordException(error);
      throw error;
    } finally {
      span.end();
    }
  }

  private async createJwtContext(token: string, traceId: string): Promise<UnifiedAuthContext> {
    try {
      // Decode JWT to get tenant and user info
      const payload = this.decodeJwt(token);
      const tenantId = payload.tenant_id;
      const userId = payload.sub;

      // Use Router Auth if enabled
      if (this.useRouterAuth && this.routerAuthService.isEnabled()) {
        const authContext = await this.routerAuthService.validateToken(token, tenantId, 'jwt');
        if (authContext) {
          return authContext;
        }
        // If Router Auth returns null, fall through to legacy validation
      }

      // Legacy validation with RouterAdminGrpcService (for backward compatibility)
      try {
        const authResponse = await this.routerAdminService.authenticate(tenantId, userId, token, { jwt: true });

        if (!authResponse.authenticated) {
          throw new UnauthorizedException('Invalid authentication token');
        }

        return {
          tenant: {
            id: tenantId,
            name: authResponse.user?.name || tenantId,
            status: 'active',
            settings: {},
          },
          user: {
            id: userId,
            email: authResponse.user?.email || '',
            name: authResponse.user?.name || '',
            roles: authResponse.roles || [],
            permissions: authResponse.permissions || [],
            status: authResponse.user?.status || 'active',
            metadata: authResponse.user?.metadata,
          },
          quota: authResponse.quota || this.getDefaultQuota(),
          trace_id: traceId,
          authenticated_at: new Date().toISOString(),
          authentication_method: 'jwt',
        };
      } catch (legacyError) {
        // If legacy service fails and Router Auth is enabled, fail-closed
        if (this.useRouterAuth) {
          throw new UnauthorizedException('JWT authentication failed: Router unavailable');
        }
        throw legacyError;
      }
    } catch (error) {
      throw new UnauthorizedException(`JWT authentication failed: ${error.message}`);
    }
  }

  private async createApiKeyContext(apiKey: string, traceId: string): Promise<UnifiedAuthContext> {
    try {
      // Use Router Auth if enabled
      if (this.useRouterAuth && this.routerAuthService.isEnabled()) {
        const authContext = await this.routerAuthService.validateToken(apiKey, undefined, 'api_key');
        if (authContext) {
          return authContext;
        }
        // If Router Auth returns null, fall through to legacy validation
      }

      // Legacy validation with RouterAdminGrpcService (for backward compatibility)
      try {
        const validationResponse = await this.routerAdminService.validateApiKey(apiKey);

        if (!validationResponse.valid) {
          throw new UnauthorizedException('Invalid API key');
        }

        const tenantId = validationResponse.tenant_id;
        const userId = validationResponse.user_id;

        // Get user details if available
        let userInfo;
        if (userId) {
          const authResponse = await this.routerAdminService.authenticate(tenantId, userId, apiKey, {
            api_key: true,
          });
          userInfo = authResponse.user;
        }

        // Get roles and permissions from Router Auth if available
        let roles: string[] = validationResponse.scopes || [];
        let permissions: string[] = [];
        if (this.useRouterAuth && this.routerAuthService.isEnabled() && tenantId && userId) {
          const routerRoles = await this.routerAuthService.getUserRoles(tenantId, userId);
          const routerPermissions = await this.routerAuthService.getUserPermissions(tenantId, userId);
          roles = routerRoles.length > 0 ? routerRoles : roles;
          permissions = routerPermissions;
        }

        return {
          tenant: {
            id: tenantId,
            name: validationResponse.tenant_name || tenantId,
            status: 'active',
            settings: {},
          },
          user: userInfo
            ? {
                id: userId!,
                email: userInfo.email || '',
                name: userInfo.name || '',
                roles: roles,
                permissions: permissions,
                status: userInfo.status || 'active',
                metadata: userInfo.metadata,
              }
            : {
                id: 'api_key_user',
                email: 'api-key@system',
                name: 'API Key User',
                roles: roles,
                permissions: permissions,
                status: 'active',
              },
          quota: validationResponse.quota || this.getDefaultQuota(),
          api_key: {
            id: validationResponse.key_id || 'unknown',
            name: validationResponse.key_name || 'API Key',
            scopes: validationResponse.scopes || [],
            expires_at: validationResponse.expires_at,
          },
          trace_id: traceId,
          authenticated_at: new Date().toISOString(),
          authentication_method: 'api_key',
        };
      } catch (legacyError) {
        // If legacy service fails and Router Auth is enabled, fail-closed
        if (this.useRouterAuth) {
          throw new UnauthorizedException('API key authentication failed: Router unavailable');
        }
        throw legacyError;
      }
    } catch (error) {
      throw new UnauthorizedException(`API key authentication failed: ${error.message}`);
    }
  }

  private async createSessionContext(sessionId: string, traceId: string): Promise<UnifiedAuthContext> {
    try {
      // Use Router Auth if enabled
      if (this.useRouterAuth && this.routerAuthService.isEnabled()) {
        const authContext = await this.routerAuthService.validateToken(sessionId, undefined, 'session');
        if (authContext) {
          return authContext;
        }
        // If Router Auth returns null, fall through to legacy validation
      }

      // Legacy session validation (placeholder - would need session store integration)
      // For now, throw error as session auth is not fully implemented
      throw new UnauthorizedException('Session authentication not implemented yet');
    } catch (error) {
      if (error instanceof UnauthorizedException) {
        throw error;
      }
      throw new UnauthorizedException(`Session authentication failed: ${error.message}`);
    }
  }

  private async createAnonymousContext(traceId: string): Promise<UnifiedAuthContext> {
    return {
      tenant: {
        id: 'anonymous',
        name: 'Anonymous',
        status: 'active',
        settings: {}
      },
      user: {
        id: 'anonymous',
        email: 'anonymous@system',
        name: 'Anonymous User',
        roles: ['anonymous'],
        permissions: ['read:public'],
        status: 'active'
      },
      quota: this.getAnonymousQuota(),
      trace_id: traceId,
      authenticated_at: new Date().toISOString(),
      authentication_method: 'anonymous'
    };
  }

  private decodeJwt(token: string): any {
    // JWT decoding logic would be implemented here
    // This is a simplified version
    const parts = token.split('.');
    if (parts.length !== 3) {
      throw new Error('Invalid JWT format');
    }
    
    try {
      const payload = JSON.parse(Buffer.from(parts[1], 'base64').toString());
      return payload;
    } catch (error) {
      throw new Error('Invalid JWT payload');
    }
  }

  private getDefaultQuota(): UnifiedAuthContext['quota'] {
    return {
      requests_per_minute: 60,
      requests_per_hour: 1000,
      requests_per_day: 10000,
      tokens_per_minute: 10000,
      tokens_per_hour: 100000,
      tokens_per_day: 1000000
    };
  }

  private getAnonymousQuota(): UnifiedAuthContext['quota'] {
    return {
      requests_per_minute: 10,
      requests_per_hour: 100,
      requests_per_day: 1000,
      tokens_per_minute: 1000,
      tokens_per_hour: 10000,
      tokens_per_day: 100000
    };
  }
}

@Injectable()
export class UnifiedAuthGuard implements CanActivate {
  constructor(
    private readonly authContextService: UnifiedAuthContextService,
    private readonly reflector: Reflector
  ) {}

  async canActivate(context: ExecutionContext): Promise<boolean> {
    const request = context.switchToHttp().getRequest();
    const requirements = this.reflector.get<AuthRequirements>('auth', context.getHandler()) || {};
    
    try {
      // Create auth context
      const authContext = await this.authContextService.createContext(request);
      
      // Check requirements
      if (requirements.requireTenant && authContext.tenant.id === 'anonymous') {
        throw new UnauthorizedException('Tenant authentication required');
      }
      
      if (requirements.requireUser && authContext.user.id === 'anonymous') {
        throw new UnauthorizedException('User authentication required');
      }
      
      if (requirements.roles && requirements.roles.length > 0) {
        const hasRequiredRole = requirements.roles.some(role => 
          authContext.user.roles.includes(role)
        );
        if (!hasRequiredRole) {
          throw new ForbiddenException(`Required role not found: ${requirements.roles.join(', ')}`);
        }
      }
      
      if (requirements.permissions && requirements.permissions.length > 0) {
        const hasRequiredPermission = requirements.permissions.some(permission =>
          authContext.user.permissions.includes(permission)
        );
        if (!hasRequiredPermission) {
          throw new ForbiddenException(`Required permission not found: ${requirements.permissions.join(', ')}`);
        }
      }
      
      // Check resource/action authorization if specified
      if (requirements.resources && requirements.actions) {
        const routerAdminService = request.injector.get(RouterAdminGrpcService);
        
        for (let i = 0; i < requirements.resources.length; i++) {
          const resource = requirements.resources[i];
          const action = requirements.actions[i] || requirements.actions[0];
          
          const authzResponse = await routerAdminService.authorize(
            authContext.tenant.id,
            authContext.user.id,
            resource,
            action
          );
          
          if (!authzResponse.authorized) {
            throw new ForbiddenException(`Access denied to ${resource}:${action} - ${authzResponse.reason}`);
          }
        }
      }
      
      return true;
    } catch (error) {
      if (error instanceof UnauthorizedException || error instanceof ForbiddenException) {
        throw error;
      }
      throw new UnauthorizedException(`Authentication failed: ${error.message}`);
    }
  }
}