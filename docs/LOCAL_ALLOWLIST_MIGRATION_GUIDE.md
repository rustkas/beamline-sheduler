# Local Allowlist Migration Guide

## Overview
This guide details the migration from local Gateway allowlists to centralized Router-based authentication and authorization.

## Background
Currently, the Gateway component maintains local allowlists for:
- API key validation
- User authentication
- Role-based access control
- Rate limiting and quotas
- Provider access control

These local allowlists create synchronization issues and prevent unified policy enforcement across the BeamLine platform.

## Migration Strategy

### Phase 1: Router Admin gRPC Service Integration
**Priority: HIGH**
**Timeline: Week 1-2**

1. **Implement Router Admin gRPC Client**
   - Create `RouterAdminGrpcService` with full CRUD operations
   - Implement authentication, authorization, and validation methods
   - Add comprehensive error handling and retry logic
   - Configure connection pooling and circuit breakers

2. **Create Authentication Context System**
   - Build `UnifiedAuthContextService` for centralized auth context
   - Support JWT, API key, session, and anonymous authentication
   - Implement unified auth guard with declarative requirements
   - Add OpenTelemetry tracing for all auth operations

3. **Develop RBAC Mapping System**
   - Create comprehensive role-to-permission mappings
   - Implement resource and action mapping between Router and Gateway
   - Build permission validation and authorization checks
   - Add role hierarchy and inheritance support

### Phase 2: Local Allowlist Removal
**Priority: HIGH**
**Timeline: Week 2-3**

1. **Remove Local API Key Storage**
   - Delete local API key validation logic
   - Remove API key database tables/collections
   - Migrate all API key validation to Router Admin service
   - Update API key generation and rotation processes

2. **Eliminate Local User Storage**
   - Remove local user authentication and storage
   - Migrate user profiles to Router-managed storage
   - Update user registration and profile management
   - Implement user session management via Router

3. **Replace Local Role Management**
   - Remove local role definitions and assignments
   - Migrate role management to Router Admin service
   - Update role-based access control checks
   - Implement dynamic role resolution

4. **Remove Local Quota Management**
   - Delete local quota tracking and enforcement
   - Migrate quota definitions to Router service
   - Implement quota checking via Router Admin API
   - Add quota usage reporting and analytics

### Phase 3: Policy Integration
**Priority: MEDIUM**
**Timeline: Week 3-4**

1. **Router Policy Integration**
   - Replace local policy storage with Router policies
   - Implement policy evaluation via Router Admin service
   - Add policy caching with TTL and invalidation
   - Support dynamic policy updates and propagation

2. **Rate Limiting Migration**
   - Remove local rate limiting logic
   - Implement Router-based rate limiting
   - Add distributed rate limiting coordination
   - Support per-tenant and per-user rate limits

3. **Provider Access Control**
   - Migrate provider allowlists to Router-managed lists
   - Implement provider access validation via Router
   - Add provider quota and usage tracking
   - Support provider-specific policies and constraints

## Implementation Details

### Files to Remove/Modify

#### Files to Remove:
```
apps/gateway/src/auth/local-auth.service.ts
apps/gateway/src/auth/api-key.service.ts
apps/gateway/src/auth/user.service.ts
apps/gateway/src/auth/role.service.ts
apps/gateway/src/policies/local-policy.service.ts
apps/gateway/src/quotas/local-quota.service.ts
apps/gateway/src/providers/local-provider-access.service.ts
```

#### Files to Modify:
```
apps/gateway/src/auth/auth.module.ts - Remove local service imports
apps/gateway/src/app.module.ts - Update auth module configuration
apps/gateway/src/openai/openai.controller.ts - Update auth decorators
apps/gateway/src/openai/openai-adapter.service.ts - Remove local auth calls
```

### Database Changes

#### Tables to Remove:
```sql
-- API Keys table
DROP TABLE IF EXISTS gateway_api_keys;

-- Local users table
DROP TABLE IF EXISTS gateway_users;

-- Local roles table
DROP TABLE IF EXISTS gateway_roles;

-- Local permissions table
DROP TABLE IF EXISTS gateway_permissions;

-- Local quotas table
DROP TABLE IF EXISTS gateway_quotas;

-- Local policies table
DROP TABLE IF EXISTS gateway_policies;

-- Provider allowlists table
DROP TABLE IF EXISTS gateway_provider_allowlists;
```

#### Migration Script:
```sql
-- Create migration tracking table
CREATE TABLE gateway_migration_log (
    id SERIAL PRIMARY KEY,
    migration_name VARCHAR(255) NOT NULL,
    executed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    status VARCHAR(50) NOT NULL,
    details JSONB
);

-- Log migration start
INSERT INTO gateway_migration_log (migration_name, status, details)
VALUES ('local_allowlist_removal', 'started', 
        '{"phase": "removal", "timestamp": "' || CURRENT_TIMESTAMP || '"}');
```

### Configuration Updates

#### Environment Variables to Remove:
```bash
# Local auth configuration
GATEWAY_LOCAL_AUTH_ENABLED=false
GATEWAY_LOCAL_USER_STORE_PATH=
GATEWAY_LOCAL_API_KEY_STORE_PATH=
GATEWAY_LOCAL_ROLE_STORE_PATH=

# Local quota configuration
GATEWAY_LOCAL_QUOTA_ENABLED=false
GATEWAY_LOCAL_QUOTA_STORE_PATH=

# Local policy configuration
GATEWAY_LOCAL_POLICY_ENABLED=false
GATEWAY_LOCAL_POLICY_STORE_PATH=
```

#### Environment Variables to Add:
```bash
# Router Admin gRPC configuration
ROUTER_ADMIN_GRPC_URL=router-admin:50051
ROUTER_ADMIN_GRPC_TIMEOUT=30000
ROUTER_ADMIN_GRPC_MAX_RETRIES=3
ROUTER_ADMIN_GRPC_RETRY_DELAY=1000

# Router Admin connection pooling
ROUTER_ADMIN_GRPC_MAX_CONNECTIONS=10
ROUTER_ADMIN_GRPC_KEEPALIVE_TIME=10000
ROUTER_ADMIN_GRPC_KEEPALIVE_TIMEOUT=5000

# Circuit breaker configuration
ROUTER_ADMIN_CB_FAILURE_THRESHOLD=5
ROUTER_ADMIN_CB_RESET_TIMEOUT=30000
ROUTER_ADMIN_CB_MONITORING_PERIOD=60000
```

### Code Migration Examples

#### Before (Local Authentication):
```typescript
@Injectable()
export class LocalAuthService {
  async validateApiKey(apiKey: string): Promise<boolean> {
    const key = await this.localApiKeyStore.get(apiKey);
    return key && !key.expired && key.active;
  }

  async getUserRoles(userId: string): Promise<string[]> {
    return await this.localRoleStore.getUserRoles(userId);
  }
}
```

#### After (Router-Based Authentication):
```typescript
@Injectable()
export class UnifiedAuthService {
  constructor(
    private readonly routerAdminService: RouterAdminGrpcService,
    private readonly authContextService: UnifiedAuthContextService
  ) {}

  async validateApiKey(apiKey: string): Promise<UnifiedAuthContext> {
    return await this.authContextService.createApiKeyContext(apiKey);
  }

  async getUserRoles(userId: string, tenantId: string): Promise<string[]> {
    const authResponse = await this.routerAdminService.authenticate(tenantId, userId, '');
    return authResponse.roles;
  }
}
```

#### Before (Local Policy Check):
```typescript
@Injectable()
export class LocalPolicyService {
  async checkPolicy(policyId: string, context: any): Promise<boolean> {
    const policy = await this.localPolicyStore.get(policyId);
    return this.evaluatePolicy(policy, context);
  }
}
```

#### After (Router Policy Check):
```typescriptn@Injectable()
export class RouterPolicyService {
  constructor(private readonly routerAdminService: RouterAdminGrpcService) {}

  async checkPolicy(policyId: string, context: any): Promise<boolean> {
    const authzResponse = await this.routerAdminService.authorize(
      context.tenantId,
      context.userId,
      context.resource,
      context.action
    );
    return authzResponse.authorized;
  }
}
```

## Testing Strategy

### Unit Tests
1. **Router Admin gRPC Client Tests**
   - Test all authentication methods
   - Test error handling and retry logic
   - Test circuit breaker functionality
   - Test connection pooling

2. **Unified Auth Context Tests**
   - Test JWT authentication flow
   - Test API key authentication flow
   - Test anonymous authentication flow
   - Test auth guard functionality

3. **RBAC Mapping Tests**
   - Test role-to-permission mapping
   - Test resource mapping
   - Test permission validation
   - Test role hierarchy

### Integration Tests
1. **End-to-End Authentication Flow**
   - Test complete authentication flow from Gateway to Router
   - Test authorization with different roles and permissions
   - Test quota enforcement
   - Test rate limiting

2. **Policy Migration Tests**
   - Test policy evaluation via Router Admin
   - Test policy caching and invalidation
   - Test dynamic policy updates
   - Test policy precedence and conflict resolution

### Performance Tests
1. **Authentication Performance**
   - Measure authentication latency
   - Test concurrent authentication requests
   - Test authentication under load
   - Measure auth context creation time

2. **Authorization Performance**
   - Measure authorization check latency
   - Test concurrent authorization requests
   - Test RBAC mapping performance
   - Measure permission validation time

## Rollback Plan

### Emergency Rollback
1. **Immediate Rollback**
   - Restore local allowlist services
   - Disable Router Admin integration
   - Revert to local authentication
   - Notify operations team

2. **Gradual Rollback**
   - Implement feature flags for Router integration
   - Enable fallback to local services
   - Monitor error rates and performance
   - Gradually migrate back to local services

### Rollback Triggers
- Authentication failure rate > 5%
- Authorization latency > 500ms
- Router Admin service unavailable > 30 seconds
- Error rate increase > 10%
- Performance degradation > 50%

## Monitoring and Alerting

### Key Metrics
1. **Authentication Metrics**
   - Authentication success/failure rate
   - Authentication latency (p50, p95, p99)
   - Auth context creation time
   - JWT validation time

2. **Authorization Metrics**
   - Authorization success/failure rate
   - Authorization latency (p50, p95, p99)
   - RBAC mapping time
   - Permission validation time

3. **Router Admin Integration Metrics**
   - gRPC request success/failure rate
   - gRPC request latency
   - Circuit breaker state changes
   - Connection pool utilization

### Alerting Rules
```yaml
# Authentication alerts
- alert: HighAuthenticationFailureRate
  expr: rate(gateway_auth_failures_total[5m]) > 0.05
  for: 2m
  annotations:
    summary: "High authentication failure rate"
    description: "Authentication failure rate is {{ $value }}%"

# Authorization alerts
- alert: HighAuthorizationLatency
  expr: histogram_quantile(0.95, gateway_authz_duration_seconds) > 0.5
  for: 5m
  annotations:
    summary: "High authorization latency"
    description: "95th percentile authorization latency is {{ $value }}s"

# Router Admin integration alerts
- alert: RouterAdminUnavailable
  expr: up{job="router-admin-grpc"} == 0
  for: 30s
  annotations:
    summary: "Router Admin service unavailable"
    description: "Router Admin gRPC service is down"
```

## Success Criteria

### Functional Criteria
- ✅ All local allowlists removed
- ✅ Router Admin integration working
- ✅ Authentication via Router Admin
- ✅ Authorization via Router Admin
- ✅ RBAC mapping functional
- ✅ Policy evaluation via Router
- ✅ Quota management via Router

### Performance Criteria
- ✅ Authentication latency < 100ms (p95)
- ✅ Authorization latency < 50ms (p95)
- ✅ Router Admin availability > 99.9%
- ✅ Authentication success rate > 99.5%
- ✅ Authorization success rate > 99.5%

### Operational Criteria
- ✅ Monitoring and alerting configured
- ✅ Rollback plan tested and ready
- ✅ Documentation updated
- ✅ Team training completed
- ✅ Incident response procedures updated

## Post-Migration Tasks

1. **Cleanup**
   - Remove deprecated code and configuration
   - Archive old database tables
   - Update deployment scripts
   - Clean up CI/CD pipelines

2. **Optimization**
   - Optimize Router Admin integration performance
   - Implement advanced caching strategies
   - Add batch operations for bulk requests
   - Implement connection optimization

3. **Enhancement**
   - Add advanced RBAC features
   - Implement dynamic policy updates
   - Add multi-tenant isolation
   - Implement advanced audit logging

## Conclusion

This migration eliminates local allowlists and creates a unified, centralized authentication and authorization system managed by the Router Admin service. This ensures consistency across the BeamLine platform and enables advanced features like dynamic policy management, centralized quota enforcement, and unified audit logging.