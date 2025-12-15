import { Injectable, OnModuleInit, Optional } from '@nestjs/common';
let NestMicro: any;
try {
  // eslint-disable-next-line @typescript-eslint/no-var-requires
  NestMicro = require('@nestjs/microservices');
} catch {}
type ClientGrpc = any;
import { Observable } from 'rxjs';
import { lastValueFrom } from 'rxjs';

// Router Admin gRPC service interfaces
interface RouterAdminService {
  authenticate(request: any): Observable<any>;
  authorize(request: any): Observable<any>;
  getUser(request: any): Observable<any>;
  getTenant(request: any): Observable<any>;
  validateApiKey(request: any): Observable<any>;
  getQuota(request: any): Observable<any>;
  checkRateLimit(request: any): Observable<any>;
  getRoles(request: any): Observable<any>;
  getPermissions(request: any): Observable<any>;
}

interface AuthRequest {
  tenant_id: string;
  user_id: string;
  token: string;
  context?: Record<string, any>;
}

interface AuthResponse {
  authenticated: boolean;
  user?: UserInfo;
  roles: string[];
  permissions: string[];
  quota?: QuotaInfo;
  trace_id: string;
}

interface AuthzRequest {
  tenant_id: string;
  user_id: string;
  resource: string;
  action: string;
  context?: Record<string, any>;
}

interface AuthzResponse {
  authorized: boolean;
  reason?: string;
  policy_id?: string;
  trace_id: string;
}

interface UserInfo {
  id: string;
  email: string;
  name: string;
  roles: string[];
  tenant_id: string;
  status: 'active' | 'suspended' | 'inactive';
  metadata?: Record<string, any>;
}

interface QuotaInfo {
  requests_per_minute: number;
  requests_per_hour: number;
  requests_per_day: number;
  tokens_per_minute: number;
  tokens_per_hour: number;
  tokens_per_day: number;
  current_usage?: UsageInfo;
}

interface UsageInfo {
  requests_this_minute: number;
  requests_this_hour: number;
  requests_today: number;
  tokens_this_minute: number;
  tokens_this_hour: number;
  tokens_today: number;
}

@Injectable()
export class RouterAdminGrpcService implements OnModuleInit {
  private routerAdminService: RouterAdminService;

  constructor(@Optional() private readonly client?: ClientGrpc) {}

  onModuleInit() {
    const service = (this.client as any)?.getService?.('RouterAdmin');
    if (service) {
      this.routerAdminService = service;
    } else {
      this.routerAdminService = {
        authenticate: (request: any) =>
          require('rxjs').of({ authenticated: false, roles: [], permissions: [], trace_id: request?.context?.trace_id || 'stub' }),
        authorize: (request: any) => require('rxjs').of({ authorized: false, reason: 'stub', trace_id: request?.context?.trace_id || 'stub' }),
        getUser: () => require('rxjs').of({}),
        getTenant: () => require('rxjs').of({}),
        validateApiKey: () => require('rxjs').of({ valid: false }),
        getQuota: () => require('rxjs').of({}),
        checkRateLimit: () => require('rxjs').of({ allowed: true }),
        getRoles: () => require('rxjs').of([]),
        getPermissions: () => require('rxjs').of([]),
      } as RouterAdminService;
    }
  }

  async authenticate(tenantId: string, userId: string, token: string, context?: Record<string, any>): Promise<AuthResponse> {
    const request: AuthRequest = {
      tenant_id: tenantId,
      user_id: userId,
      token,
      context
    };

    try {
      const response = await lastValueFrom(this.routerAdminService.authenticate(request));
      return response;
    } catch (error) {
      throw new Error(`Router authentication failed: ${error.message}`);
    }
  }

  async authorize(tenantId: string, userId: string, resource: string, action: string, context?: Record<string, any>): Promise<AuthzResponse> {
    const request: AuthzRequest = {
      tenant_id: tenantId,
      user_id: userId,
      resource,
      action,
      context
    };

    try {
      const response = await lastValueFrom(this.routerAdminService.authorize(request));
      return response;
    } catch (error) {
      throw new Error(`Router authorization failed: ${error.message}`);
    }
  }

  async validateApiKey(apiKey: string, tenantId?: string): Promise<any> {
    const request = {
      api_key: apiKey,
      tenant_id: tenantId
    };

    try {
      const response = await lastValueFrom(this.routerAdminService.validateApiKey(request));
      return response;
    } catch (error) {
      throw new Error(`API key validation failed: ${error.message}`);
    }
  }

  async getQuota(tenantId: string, userId?: string): Promise<any> {
    const request = {
      tenant_id: tenantId,
      user_id: userId
    };

    try {
      const response = await lastValueFrom(this.routerAdminService.getQuota(request));
      return response;
    } catch (error) {
      throw new Error(`Quota retrieval failed: ${error.message}`);
    }
  }

  async checkRateLimit(tenantId: string, userId: string, resource: string, requested: number): Promise<any> {
    const request = {
      tenant_id: tenantId,
      user_id: userId,
      resource,
      requested
    };

    try {
      const response = await lastValueFrom(this.routerAdminService.checkRateLimit(request));
      return response;
    } catch (error) {
      throw new Error(`Rate limit check failed: ${error.message}`);
    }
  }
}
