---
version: 1.0
authors:
  - WORKER wrk-2: Architecture/Tech Lead
last_update: 2025-01-27T12:00:00Z
status: draft
rule_version: v10
message_protocol: v1
---

# Router ↔ Gateway Integration Architecture

## Purpose

This document defines the architectural specification for unified integration between Router (Erlang/OTP) and Gateway (NestJS) components, ensuring perfect synchronization and eliminating duplicate logic.

## Current State Analysis

### Gateway Current Implementation

**Authentication/Authorization** (`apps/gateway/src/common/guards/rbac.guard.ts`):
- ❌ Local allowlist via `GATEWAY_TENANT_ALLOWLIST` env variable
- ❌ Local allowlist via `GATEWAY_POLICY_ALLOWLIST` env variable
- ❌ Hardcoded `roleMatrix` for RBAC checks
- ✅ Basic tenant/policy format validation

**Policy Management** (`apps/gateway/src/policies/policies.service.ts`):
- ❌ Local in-memory `Map<string, PolicyDto>` storage
- ❌ No synchronization with Router
- ✅ Basic CRUD operations (stub implementation)

**Router Communication** (`apps/gateway/src/routes/adapters/router-client.service.ts`):
- ✅ Supports HTTP, NATS, Mock modes
- ❌ gRPC mode not implemented (throws error)
- ✅ Basic retry logic for HTTP/NATS
- ✅ Tracing support

### Router Current Implementation

**RouterAdmin Service** (`proto/beamline/flow/v1/flow.proto`):
- ✅ `UpsertPolicy` - Create/update policy
- ✅ `DeletePolicy` - Delete policy
- ✅ `GetPolicy` - Get policy by ID
- ✅ `ListPolicies` - List all policies for tenant

**Router Service** (`proto/beamline/flow/v1/flow.proto`):
- ✅ `Decide` - Make routing decision

## Target Architecture

### Integration Principles

1. **Single Source of Truth**: Router is the authoritative source for:
   - RBAC permissions and roles
   - Routing policies
   - Tenant quotas and rate limits
   - Authentication context

2. **Gateway as Adapter**: Gateway adapts HTTP/REST requests to Router protocols:
   - No business logic in Gateway
   - All decisions delegated to Router
   - Gateway only handles HTTP ↔ Router translation

3. **Protocol Flexibility**: Support both gRPC and NATS:
   - gRPC for reliable, structured communication
   - NATS for high-throughput, pub/sub scenarios
   - Auto-selection based on payload size and latency requirements

4. **Backward Compatibility**: Migration must be gradual:
   - Feature flags for gradual rollout
   - Fallback to local logic if Router unavailable
   - No breaking changes to existing API contracts

## Architecture Components

### Component 1: Router Admin gRPC Client

**Purpose**: Communicate with Router Admin service for policy and RBAC management.

**Interface** (`apps/gateway/src/router-admin/router-admin-client.interface.ts`):

```typescript
/**
 * Router Admin gRPC Client Interface
 * Defines contract for Router Admin service communication
 */
export interface IRouterAdminClient {
  /**
   * Check RBAC permission
   */
  checkPermission(request: CheckPermissionRequest): Promise<CheckPermissionResponse>;

  /**
   * Get tenant quota
   */
  getTenantQuota(request: GetTenantQuotaRequest): Promise<GetTenantQuotaResponse>;

  /**
   * Check rate limit
   */
  checkRateLimit(request: CheckRateLimitRequest): Promise<CheckRateLimitResponse>;

  /**
   * Get policy
   */
  getPolicy(request: GetPolicyRequest): Promise<GetPolicyResponse>;

  /**
   * List policies
   */
  listPolicies(request: ListPoliciesRequest): Promise<ListPoliciesResponse>;

  /**
   * Upsert policy
   */
  upsertPolicy(request: UpsertPolicyRequest): Promise<UpsertPolicyResponse>;

  /**
   * Delete policy
   */
  deletePolicy(request: DeletePolicyRequest): Promise<DeletePolicyResponse>;
}
```

**Implementation Requirements**:
- Use `@grpc/grpc-js` for gRPC client
- Generate TypeScript types from `proto/beamline/flow/v1/flow.proto`
- Support connection pooling and retries
- Handle gRPC errors and map to HTTP status codes
- Support TLS for production

**Configuration**:
```typescript
interface RouterAdminConfig {
  endpoint: string;        // e.g., "router:9000"
  timeout: number;         // milliseconds
  maxRetries: number;
  tls?: {
    enabled: boolean;
    certFile?: string;
    keyFile?: string;
    caFile?: string;
  };
}
```

### Component 2: Router Auth Service

**Purpose**: Unified authentication and authorization via Router Admin.

**Interface** (`apps/gateway/src/auth/router-auth.service.interface.ts`):

```typescript
export interface IRouterAuthService {
  /**
   * Validate access (RBAC check)
   */
  validateAccess(
    tenantId: string,
    userId: string,
    role: string,
    resource: string,
    action: string,
    context?: Record<string, any>
  ): Promise<AccessValidationResult>;

  /**
   * Get tenant quota
   */
  getTenantQuota(tenantId: string): Promise<TenantQuota>;

  /**
   * Check rate limit
   */
  checkRateLimit(
    tenantId: string,
    userId: string,
    endpoint: string,
    requestWeight?: number
  ): Promise<RateLimitResult>;
}
```

**Implementation** (`apps/gateway/src/auth/router-auth.service.ts`):

```typescript
@Injectable()
export class RouterAuthService implements IRouterAuthService {
  constructor(
    private readonly routerAdminClient: IRouterAdminClient,
    private readonly cacheService: CacheService,
    private readonly logger: Logger,
  ) {}

  async validateAccess(
    tenantId: string,
    userId: string,
    role: string,
    resource: string,
    action: string,
    context?: Record<string, any>
  ): Promise<AccessValidationResult> {
    // 1. Check cache
    const cacheKey = `rbac:${tenantId}:${userId}:${role}:${resource}:${action}`;
    const cached = await this.cacheService.get<AccessValidationResult>(cacheKey);
    if (cached) {
      return cached;
    }

    // 2. Call Router Admin
    const request: CheckPermissionRequest = {
      tenant_id: tenantId,
      user_id: userId,
      role: role,
      resource: resource,
      action: action,
      context: context || {},
    };

    try {
      const response = await this.routerAdminClient.checkPermission(request);
      
      const result: AccessValidationResult = {
        allowed: response.allowed,
        quota_remaining: response.quota_remaining,
        rate_limit: response.rate_limit,
      };

      // 3. Cache result (TTL: 60 seconds)
      await this.cacheService.set(cacheKey, result, 60);

      return result;
    } catch (error) {
      // Fail-closed: if Router unavailable, deny access
      this.logger.error('RBAC check failed', { error, request });
      return {
        allowed: false,
        error: 'Router unavailable',
      };
    }
  }

  // ... other methods
}
```

**Migration Strategy**:
1. Add feature flag: `USE_ROUTER_AUTH=true`
2. If flag enabled → use RouterAuthService
3. If flag disabled → use existing RBACGuard logic
4. Gradual rollout per tenant

### Component 3: Router Policies Service

**Purpose**: Centralized policy management via Router Admin.

**Interface** (`apps/gateway/src/policies/router-policies.service.interface.ts`):

```typescript
export interface IRouterPoliciesService {
  getPolicy(tenantId: string, policyId: string): Promise<PolicyDto>;
  listPolicies(tenantId: string): Promise<PolicyDto[]>;
  upsertPolicy(tenantId: string, policy: PolicyDto): Promise<void>;
  deletePolicy(tenantId: string, policyId: string): Promise<void>;
}
```

**Implementation** (`apps/gateway/src/policies/router-policies.service.ts`):

```typescript
@Injectable()
export class RouterPoliciesService implements IRouterPoliciesService {
  constructor(
    private readonly routerAdminClient: IRouterAdminClient,
    private readonly cacheService: CacheService,
    private readonly logger: Logger,
  ) {}

  async getPolicy(tenantId: string, policyId: string): Promise<PolicyDto> {
    // 1. Check cache
    const cacheKey = `policy:${tenantId}:${policyId}`;
    const cached = await this.cacheService.get<PolicyDto>(cacheKey);
    if (cached) {
      return cached;
    }

    // 2. Call Router Admin
    const request: GetPolicyRequest = {
      tenant_id: tenantId,
      policy_id: policyId,
    };

    try {
      const response = await this.routerAdminClient.getPolicy(request);
      
      // 3. Map Router format → Gateway format
      const policy = this.mapRouterPolicyToGateway(response.policy);
      
      // 4. Cache (TTL: 5 minutes)
      await this.cacheService.set(cacheKey, policy, 300);
      
      return policy;
    } catch (error) {
      if (error.code === grpc.status.NOT_FOUND) {
        throw new NotFoundException(`Policy ${policyId} not found`);
      }
      throw error;
    }
  }

  private mapRouterPolicyToGateway(routerPolicy: AdminPolicy): PolicyDto {
    return {
      tenant_id: routerPolicy.tenant_id,
      policy_id: routerPolicy.policy_id,
      name: routerPolicy.name,
      version: routerPolicy.version,
      providers: routerPolicy.providers.map(p => ({
        id: p.id,
        weight: p.weight,
        priority: p.priority,
        enabled: p.enabled,
      })),
      rules: routerPolicy.rules.map(r => ({
        match: r.match,
        prefer: r.prefer,
        fallback: r.fallback,
      })),
      sticky: routerPolicy.sticky,
      enabled: routerPolicy.enabled,
    };
  }

  // ... other methods
}
```

**Migration Strategy**:
1. Export existing Gateway policies to JSON
2. Import to Router via Admin API
3. Update Gateway to use RouterPoliciesService
4. Remove local policy storage

### Component 4: Router Context Interceptor

**Purpose**: Unified context extraction and validation for all requests.

**Implementation** (`apps/gateway/src/common/interceptors/router-context.interceptor.ts`):

```typescript
@Injectable()
export class RouterContextInterceptor implements NestInterceptor {
  constructor(
    private readonly routerAuthService: IRouterAuthService,
    private readonly tracingService: TracingService,
  ) {}

  async intercept(context: ExecutionContext, next: CallHandler): Promise<Observable<any>> {
    const request = context.switchToHttp().getRequest();
    const response = context.switchToHttp().getResponse();

    // 1. Extract context
    const routerContext = this.extractContext(request);
    
    // 2. Validate RBAC
    const hasAccess = await this.routerAuthService.validateAccess(
      routerContext.tenantId,
      routerContext.userId,
      routerContext.role,
      this.getResource(context),
      this.getAction(context),
      routerContext.metadata,
    );

    if (!hasAccess.allowed) {
      throw new ForbiddenException('Access denied by RBAC policy');
    }

    // 3. Check rate limits
    const rateLimit = await this.routerAuthService.checkRateLimit(
      routerContext.tenantId,
      routerContext.userId,
      request.route.path,
      1,
    );

    if (!rateLimit.allowed) {
      response.setHeader('Retry-After', rateLimit.retry_after.toString());
      throw new TooManyRequestsException({
        message: 'Rate limit exceeded',
        retry_after: rateLimit.retry_after,
      });
    }

    // 4. Attach context to request
    request.routerContext = {
      ...routerContext,
      rateLimitInfo: rateLimit,
    };

    return next.handle();
  }

  private extractContext(request: any): RouterContext {
    const headers = request.headers;
    
    return {
      tenantId: this.validateTenantId(headers['x-tenant-id']),
      userId: headers['x-user-id'] || 'anonymous',
      role: this.validateRole(headers['x-role']),
      traceId: headers['x-trace-id'] || this.tracingService.generateTraceId(),
      metadata: {
        ip: request.ip,
        userAgent: headers['user-agent'],
        requestId: headers['x-request-id'],
      },
    };
  }
}
```

### Component 5: Protocol Unification Service

**Purpose**: Automatic gRPC/NATS selection based on payload and requirements.

**Interface** (`apps/gateway/src/router-protocol/router-protocol.service.interface.ts`):

```typescript
export interface IRouterProtocolService {
  sendToRouter(
    request: UnifiedRouteRequest,
    options?: RouterProtocolOptions
  ): Promise<UnifiedRouteResponse>;
}
```

**Protocol Selection Logic**:
- **gRPC preferred** when:
  - Payload size > 1MB
  - High reliability required
  - TLS encryption required
  - Streaming needed

- **NATS preferred** when:
  - Payload size < 1MB
  - Low latency required (<50ms)
  - Broadcast needed
  - High throughput needed

**Implementation** (`apps/gateway/src/router-protocol/router-protocol.service.ts`):

```typescript
@Injectable()
export class RouterProtocolService implements IRouterProtocolService {
  private readonly GRPC_THRESHOLD_BYTES = 1024 * 1024; // 1MB
  private readonly NATS_LATENCY_THRESHOLD_MS = 50;

  constructor(
    private readonly grpcClient: IRouterClient,
    private readonly natsClient: INatsClient,
    private readonly tracingService: TracingService,
  ) {}

  async sendToRouter(
    request: UnifiedRouteRequest,
    options: RouterProtocolOptions = {}
  ): Promise<UnifiedRouteResponse> {
    const protocol = this.determineProtocol(request, options);
    
    switch (protocol) {
      case 'grpc':
        return this.sendViaGrpc(request, options);
      case 'nats':
        return this.sendViaNats(request, options);
      default:
        throw new Error(`Unsupported protocol: ${protocol}`);
    }
  }

  private determineProtocol(
    request: UnifiedRouteRequest,
    options: RouterProtocolOptions
  ): 'grpc' | 'nats' {
    // Explicit protocol
    if (options.protocol) {
      return options.protocol;
    }

    // Payload size
    const payloadSize = JSON.stringify(request.message.payload).length;
    if (payloadSize > this.GRPC_THRESHOLD_BYTES) {
      return 'grpc';
    }

    // Latency requirements
    if (options.requireLowLatency && payloadSize < 65536) {
      return 'nats';
    }

    // Default: gRPC
    return 'grpc';
  }
}
```

## Data Contracts

### Request Contracts

**UnifiedRouteRequest** (matches `RouteRequest` from proto):
```typescript
interface UnifiedRouteRequest {
  message: {
    message_id: string;
    tenant_id: string;
    trace_id?: string;
    message_type: 'chat' | 'completion' | 'embedding' | 'audio';
    payload: any; // JSON-serializable
    metadata?: Record<string, string>;
    timestamp_ms?: number;
    // CP2+ optional fields
    run_id?: string;
    flow_id?: string;
    step_id?: string;
    idempotency_key?: string;
    span_id?: string;
  };
  policy_id?: string;
  context?: Record<string, string>;
  idempotency_key?: string; // CP2+ optional
}
```

### Response Contracts

**UnifiedRouteResponse** (matches `RouteDecision` from proto):
```typescript
interface UnifiedRouteResponse {
  provider_id: string;
  reason: 'weighted' | 'sticky' | 'fallback' | 'priority' | 'random';
  priority: number; // 0-100
  expected_latency_ms: number;
  expected_cost: number;
  metadata?: Record<string, string>;
}
```

### Error Contracts

**UnifiedRouterError**:
```typescript
interface UnifiedRouterError {
  error: {
    code: RouterErrorCode;
    message: string;
    details?: any;
    trace_id?: string;
    retryable: boolean;
    retry_after?: number;
  };
}

enum RouterErrorCode {
  // Client errors (4xx)
  INVALID_REQUEST = 'INVALID_REQUEST',
  UNAUTHORIZED = 'UNAUTHORIZED',
  FORBIDDEN = 'FORBIDDEN',
  NOT_FOUND = 'NOT_FOUND',
  RATE_LIMITED = 'RATE_LIMITED',
  
  // Server errors (5xx)
  INTERNAL_ERROR = 'INTERNAL_ERROR',
  SERVICE_UNAVAILABLE = 'SERVICE_UNAVAILABLE',
  TIMEOUT = 'TIMEOUT',
  
  // Business logic errors
  POLICY_NOT_FOUND = 'POLICY_NOT_FOUND',
  PROVIDER_UNAVAILABLE = 'PROVIDER_UNAVAILABLE',
  QUOTA_EXCEEDED = 'QUOTA_EXCEEDED',
}
```

## Migration Plan

### Phase 1: Infrastructure Setup (Week 1)

**Tasks**:
1. Generate TypeScript types from `proto/beamline/flow/v1/flow.proto`
2. Create Router Admin gRPC client interface
3. Set up gRPC client library (`@grpc/grpc-js`)
4. Configure Router Admin endpoint (port 9000)

**Deliverables**:
- `apps/gateway/src/router-admin/router-admin-client.interface.ts`
- Generated TypeScript types from proto
- Basic gRPC client implementation

### Phase 2: Auth Integration (Week 1-2)

**Tasks**:
1. Implement `RouterAuthService` with Router Admin calls
2. Add caching layer for RBAC checks
3. Update `RBACGuard` to use `RouterAuthService` (feature flag)
4. Remove local allowlist logic (after validation)

**Deliverables**:
- `apps/gateway/src/auth/router-auth.service.ts`
- Updated `RBACGuard` with feature flag
- Migration guide for removing allowlists

### Phase 3: Policy Integration (Week 2)

**Tasks**:
1. Implement `RouterPoliciesService` with Router Admin calls
2. Add caching layer for policies
3. Update `PoliciesService` to use `RouterPoliciesService` (feature flag)
4. Export/import tool for existing policies

**Deliverables**:
- `apps/gateway/src/policies/router-policies.service.ts`
- Policy export/import script
- Migration guide for policies

### Phase 4: Protocol Unification (Week 2-3)

**Tasks**:
1. Implement `RouterProtocolService` with auto-selection
2. Complete gRPC client implementation in `RouterClientService`
3. Add protocol metrics and monitoring
4. Update `RoutesService` to use `RouterProtocolService`

**Deliverables**:
- `apps/gateway/src/router-protocol/router-protocol.service.ts`
- Updated `RouterClientService` with gRPC support
- Protocol selection metrics

### Phase 5: Context Unification (Week 3)

**Tasks**:
1. Implement `RouterContextInterceptor`
2. Add unified error handling and mapping
3. Update all controllers to use interceptor
4. Remove duplicate context extraction logic

**Deliverables**:
- `apps/gateway/src/common/interceptors/router-context.interceptor.ts`
- Error mapping service
- Updated controllers

### Phase 6: Validation & Rollout (Week 3-4)

**Tasks**:
1. Integration tests for all components
2. Load testing with Router integration
3. Gradual rollout with feature flags
4. Monitor metrics and errors
5. Remove feature flags after validation

**Deliverables**:
- Integration test suite
- Load test results
- Rollout plan
- Monitoring dashboards

## Configuration

### Environment Variables

```bash
# Router Admin gRPC
ROUTER_ADMIN_GRPC_ENDPOINT=router:9000
ROUTER_ADMIN_GRPC_TIMEOUT=5000
ROUTER_ADMIN_GRPC_MAX_RETRIES=3
ROUTER_ADMIN_GRPC_TLS_ENABLED=false

# Feature Flags
USE_ROUTER_AUTH=false  # Enable Router Auth integration
USE_ROUTER_POLICIES=false  # Enable Router Policies integration
USE_ROUTER_PROTOCOL_AUTO=false  # Enable auto protocol selection

# Cache Settings
RBAC_CACHE_TTL_SECONDS=60
POLICY_CACHE_TTL_SECONDS=300
RATE_LIMIT_CACHE_TTL_SECONDS=30

# Protocol Selection
ROUTER_PROTOCOL_GRPC_THRESHOLD_BYTES=1048576  # 1MB
ROUTER_PROTOCOL_NATS_LATENCY_THRESHOLD_MS=50
ROUTER_PROTOCOL_DEFAULT=grpc
```

## Testing Strategy

### Unit Tests

- Router Admin client mock
- Router Auth Service with mocked client
- Router Policies Service with mocked client
- Protocol selection logic
- Error mapping

### Integration Tests

- End-to-end Router Admin calls
- RBAC validation flow
- Policy synchronization
- Protocol switching
- Error handling

### Load Tests

- Target: 1000 req/s per Gateway instance
- P95 latency: <50ms
- Availability: 99.9%

## Rollback Plan

### Emergency Rollback

1. Set feature flags to `false`:
   ```bash
   USE_ROUTER_AUTH=false
   USE_ROUTER_POLICIES=false
   USE_ROUTER_PROTOCOL_AUTO=false
   ```

2. Restart Gateway:
   ```bash
   kubectl rollout restart deployment/gateway
   ```

3. Verify rollback:
   ```bash
   kubectl get pods -l app=gateway
   kubectl logs -f gateway-pod | grep "Router integration"
   ```

## Success Criteria

### Functional

- ✅ All RBAC checks go through Router Admin
- ✅ All policies managed via Router Admin
- ✅ Protocol auto-selection works correctly
- ✅ Error handling unified and consistent
- ✅ No local allowlists or policy storage

### Performance

- ✅ P95 latency <50ms for routing decisions
- ✅ Cache hit rate >80% for RBAC checks
- ✅ Cache hit rate >90% for policy lookups
- ✅ Zero downtime during migration

### Reliability

- ✅ Circuit breaker prevents cascading failures
- ✅ Graceful degradation if Router unavailable
- ✅ Health checks for Router Admin connection
- ✅ Comprehensive error logging and monitoring

## References

- `proto/beamline/flow/v1/flow.proto` - Router and RouterAdmin service definitions
- `docs/ROUTER_GATEWAY_INTEGRATION_SPEC.md` - Detailed technical specification
- `docs/archive/dev/CP1_BOUNDARIES_AND_CONTRACTS.md` - CP1 module boundaries
- `docs/archive/dev/CP1_ARCHITECTURE_CHECKLIST.md` - CP1 architecture checklist

