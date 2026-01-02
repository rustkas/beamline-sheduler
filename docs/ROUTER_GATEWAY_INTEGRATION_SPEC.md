# Router-Gateway Integration Specification (LEGACY)

**Status**: LEGACY (historical)

This document describes the old integration between Router (Erlang/OTP) and the legacy NestJS Gateway (`apps/gateway`).

**Current stack** uses **C-Gateway** (`apps/c-gateway`) as the HTTP entrypoint.

Use instead:
- `docs/CGATEWAY_ROUTER_INTEGRATION_ASIS.md`
- `docs/NATS_SUBJECTS.md`

---

## Overview

This document specifies the complete integration between the Router (Erlang/OTP) and Gateway (NestJS) components for CP1-LC checkpoint. The integration ensures perfect synchronization of authentication, policies, routing decisions, and error handling across the distributed system.

## Architecture Integration Scope

### 1. Authentication System Unification

**Goal**: Replace Gateway's local authentication with Router Admin gRPC integration

#### Router Admin gRPC Service Integration

**Service**: `RouterAdminAuthService`
- **Location**: `/apps/gateway/src/auth/router-admin-grpc.service.ts`
- **Purpose**: Centralized authentication and authorization through Router
- **Key Methods**:
  - `validateAccess(tenantId, userId, role, resource, action)`
  - `getUserContext(userId, tenantId)`
  - `checkRateLimit(tenantId, userId, endpoint)`
  - `validateApiKey(apiKey, tenantId)`

#### Authentication Context System

**Unified Context Interface**:
```typescript
interface AuthContext {
  tenantId: string;
  userId: string;
  role: 'admin' | 'developer' | 'user' | 'anonymous';
  permissions: string[];
  quota: {
    requestsRemaining: number;
    resetTime: Date;
  };
  session?: {
    id: string;
    createdAt: Date;
    expiresAt: Date;
  };
}
```

**Authentication Methods**:
1. **JWT Token**: Bearer token validation via Router
2. **API Key**: Key validation with tenant scoping
3. **Session**: Session-based authentication
4. **Anonymous**: Limited access with rate limiting

#### RBAC Mapping Standards

**Role Hierarchy**:
```
admin (full access)
  └── developer (project management)
      └── user (basic operations)
          └── anonymous (read-only)
```

**Permission Matrix**:
| Role | Flow Management | Provider Config | Policy Management | System Admin |
|------|----------------|------------------|-------------------|---------------|
| admin | full | full | full | full |
| developer | project | read | read | none |
| user | basic | none | none | none |
| anonymous | read | none | none | none |

### 2. Policy Management Migration

**Goal**: Replace Gateway's local policy storage with Router Admin integration

#### Router Policies Service

**Service**: `RouterPoliciesService`
- **Location**: `/apps/gateway/src/policies/router-policies.service.ts`
- **Purpose**: Centralized policy management through Router
- **Key Methods**:
  - `getPolicy(tenantId, policyId)`
  - `upsertPolicy(tenantId, policyInput)`
  - `deletePolicy(tenantId, policyId)`
  - `listPolicies(tenantId, filters)`
  - `validatePolicy(policy)`

#### Policy Caching Strategy

**Cache Implementation**:
- **TTL**: 5 minutes for active policies
- **Invalidation**: Webhook-based from Router
- **Storage**: Redis with tenant isolation
- **Keys**: `policy:{tenantId}:{policyId}`

**Cache Operations**:
```typescript
async getPolicy(tenantId: string, policyId: string): Promise<Policy> {
  const cacheKey = `policy:${tenantId}:${policyId}`;
  const cached = await this.redis.get(cacheKey);
  
  if (cached) {
    return JSON.parse(cached);
  }
  
  const policy = await this.routerAdmin.getPolicy(tenantId, policyId);
  await this.redis.setex(cacheKey, 300, JSON.stringify(policy));
  return policy;
}
```

### 3. Protocol Unification

**Goal**: Support both gRPC and NATS protocols with automatic selection

#### Router Protocol Service

**Service**: `RouterProtocolService`
- **Location**: `/apps/gateway/src/common/protocols/router-protocol.service.ts`
- **Purpose**: Unified protocol handling with auto-selection
- **Supported Protocols**: gRPC, NATS
- **Selection Criteria**: Payload size, latency requirements, availability

#### Protocol Selection Logic

**Auto-Selection Algorithm**:
```typescript
private determineProtocol(options: RouterProtocolOptions): 'grpc' | 'nats' {
  // Large payloads → gRPC
  if (options.payloadSize > 1024 * 1024) { // 1MB
    return 'grpc';
  }
  
  // Low latency requirements → NATS
  if (options.maxLatency < 50) { // 50ms
    return this.nats.isHealthy() ? 'nats' : 'grpc';
  }
  
  // Default based on health
  if (this.nats.isHealthy() && !this.grpc.isHealthy()) {
    return 'nats';
  }
  
  return 'grpc';
}
```

**Protocol Comparison**:
| Protocol | Best For | Latency | Throughput | Reliability |
|----------|----------|---------|------------|-------------|
| gRPC | Large payloads, complex queries | Medium | High | High |
| NATS | Small messages, high frequency | Low | Very High | High |

### 4. Contract and Protocol Unification

**Goal**: Establish unified contracts between Gateway and Router components

#### Unified Contract Interface

**File**: `/apps/gateway/src/common/interfaces/router-contracts.interface.ts`

**Core Interfaces**:
```typescript
interface UnifiedRouteRequest {
  message: {
    id: string;
    tenantId: string;
    userId: string;
    content: any;
    metadata?: Record<string, any>;
  };
  policy?: {
    id: string;
    rules: PolicyRule[];
  };
  context?: {
    traceId: string;
    spanId: string;
    timestamp: Date;
    clientInfo?: ClientInfo;
  };
}

interface UnifiedRouteResponse {
  decision: {
    action: 'allow' | 'deny' | 'modify';
    reason?: string;
    modifications?: Record<string, any>;
    route?: string;
  };
  trace: {
    id: string;
    duration: number;
    services: ServiceTrace[];
  };
}
```

#### Request/Response Mapping

**Gateway → Router Request Mapping**:
```typescript
mapToRouterRequest(gatewayRequest: any): UnifiedRouteRequest {
  return {
    message: {
      id: gatewayRequest.id || uuid(),
      tenantId: gatewayRequest.tenantId,
      userId: gatewayRequest.userId,
      content: gatewayRequest.content,
      metadata: gatewayRequest.metadata
    },
    policy: gatewayRequest.policy,
    context: {
      traceId: this.tracing.getTraceId(),
      spanId: this.tracing.getSpanId(),
      timestamp: new Date(),
      clientInfo: this.getClientInfo(gatewayRequest)
    }
  };
}
```

**Router → Gateway Response Mapping**:
```typescript
mapFromRouterResponse(routerResponse: UnifiedRouteResponse): any {
  return {
    status: routerResponse.decision.action === 'allow' ? 'success' : 'blocked',
    data: routerResponse.decision.modifications || routerResponse,
    trace: routerResponse.trace,
    reason: routerResponse.decision.reason
  };
}
```

### 5. Error Handling and Mapping Standards

**Goal**: Standardize error handling across Gateway and Router components

#### Unified Error Structure

**Error Interface**:
```typescript
interface UnifiedError {
  code: string;
  message: string;
  details?: any;
  traceId: string;
  timestamp: Date;
  source: 'gateway' | 'router' | 'provider';
  severity: 'error' | 'warning' | 'info';
}
```

#### Error Mapping Standards

**Router Error Codes**:
- `ROUTER_AUTH_FAILED`: Authentication failed
- `ROUTER_POLICY_VIOLATION`: Policy violation
- `ROUTER_RATE_LIMITED`: Rate limit exceeded
- `ROUTER_UNAVAILABLE`: Router service unavailable
- `ROUTER_TIMEOUT`: Request timeout

**Gateway Error Codes**:
- `GATEWAY_VALIDATION_ERROR`: Request validation failed
- `GATEWAY_PROVIDER_ERROR`: Provider communication error
- `GATEWAY_INTERNAL_ERROR`: Internal server error
- `GATEWAY_TIMEOUT`: Gateway timeout

**Error Mapping Logic**:
```typescript
mapRouterError(routerError: any): UnifiedError {
  const errorMap = {
    'auth_failed': 'ROUTER_AUTH_FAILED',
    'policy_denied': 'ROUTER_POLICY_VIOLATION',
    'rate_limited': 'ROUTER_RATE_LIMITED',
    'service_unavailable': 'ROUTER_UNAVAILABLE',
    'timeout': 'ROUTER_TIMEOUT'
  };
  
  return {
    code: errorMap[routerError.code] || 'ROUTER_UNKNOWN_ERROR',
    message: routerError.message,
    details: routerError.details,
    traceId: this.tracing.getTraceId(),
    timestamp: new Date(),
    source: 'router',
    severity: 'error'
  };
}
```

### 6. Configuration Management

**Goal**: Centralized configuration with environment-specific overrides

#### Configuration Structure

**Base Configuration** (`/apps/gateway/config/router-integration.yml`):
```yaml
router:
  connection:
    host: ${ROUTER_HOST:localhost}
    port: ${ROUTER_PORT:9090}
    timeout: ${ROUTER_TIMEOUT:30000}
    retries: ${ROUTER_RETRIES:3}
  
  protocols:
    grpc:
      enabled: true
      max_payload_size: 10485760  # 10MB
      compression: gzip
    nats:
      enabled: true
      servers: ${NATS_SERVERS:nats://localhost:4222}
      max_reconnect_attempts: 10
  
  auth:
    cache_ttl: 300  # 5 minutes
    rate_limit_window: 60000  # 1 minute
    max_requests_per_window: 1000
  
  policies:
    cache_ttl: 300
    refresh_interval: 60
  
  tracing:
    enabled: true
    sample_rate: 0.1  # 10%
    export_interval: 5000
```

#### Environment Overrides

**Development** (`router-integration.dev.yml`):
```yaml
router:
  connection:
    host: localhost
    port: 9090
  protocols:
    nats:
      servers: nats://localhost:4222
  tracing:
    enabled: true
    sample_rate: 1.0  # 100% for debugging
```

**Production** (`router-integration.prod.yml`):
```yaml
router:
  connection:
    host: ${ROUTER_SERVICE_HOST}
    port: ${ROUTER_SERVICE_PORT}
    timeout: 10000
    retries: 5
  protocols:
    grpc:
      max_payload_size: 52428800  # 50MB
    nats:
      servers: ${NATS_CLUSTER_SERVERS}
      max_reconnect_attempts: 50
  auth:
    cache_ttl: 900  # 15 minutes
    rate_limit_window: 60000
    max_requests_per_window: 5000
  tracing:
    enabled: true
    sample_rate: 0.01  # 1% for performance
```

### 7. Health Checks and Monitoring

**Goal**: Comprehensive health monitoring and alerting

#### Health Check Endpoints

**Gateway Health** (`/health/gateway`):
```json
{
  "status": "healthy",
  "timestamp": "2024-01-15T10:30:00Z",
  "services": {
    "router_connection": "healthy",
    "authentication": "healthy",
    "policies": "healthy",
    "nats": "healthy",
    "grpc": "healthy"
  },
  "metrics": {
    "response_time_p95": 45,
    "error_rate": 0.001,
    "throughput_rps": 150
  }
}
```

**Router Health** (`/health/router`):
```json
{
  "status": "healthy",
  "timestamp": "2024-01-15T10:30:00Z",
  "services": {
    "mnesia": "healthy",
    "policy_engine": "healthy",
    "auth_service": "healthy",
    "flow_runtime": "healthy"
  },
  "metrics": {
    "decision_time_p95": 25,
    "cache_hit_rate": 0.85,
    "active_flows": 42
  }
}
```

#### Monitoring Metrics

**Key Metrics**:
- **Request Latency**: P50, P95, P99 response times
- **Error Rates**: 4xx and 5xx error percentages
- **Throughput**: Requests per second by endpoint
- **Protocol Distribution**: gRPC vs NATS usage ratios
- **Cache Performance**: Hit rates and eviction counts
- **Authentication Success**: Login success/failure rates
- **Policy Evaluation**: Decision time and cache effectiveness

**Alerting Rules**:
- **High Error Rate**: > 5% error rate for 5 minutes
- **High Latency**: P95 > 100ms for 10 minutes
- **Service Unavailable**: Any health check failing for 2 minutes
- **Authentication Failures**: > 10% failure rate for 5 minutes
- **Cache Degradation**: Hit rate < 70% for 15 minutes

### 8. Testing Strategy

**Goal**: Comprehensive testing coverage for all integration points

#### Unit Tests

**Authentication Service Tests**:
```typescript
describe('RouterAdminAuthService', () => {
  describe('validateAccess', () => {
    it('should allow access for valid permissions', async () => {
      const result = await service.validateAccess('tenant1', 'user1', 'admin', 'flows', 'create');
      expect(result.allowed).toBe(true);
    });
    
    it('should deny access for insufficient permissions', async () => {
      const result = await service.validateAccess('tenant1', 'user1', 'user', 'flows', 'delete');
      expect(result.allowed).toBe(false);
    });
  });
});
```

#### Integration Tests

**Router-Gateway Integration Tests**:
```typescript
describe('Router Gateway Integration', () => {
  describe('Protocol Selection', () => {
    it('should use NATS for small payloads', async () => {
      const response = await request(app.getHttpServer())
        .post('/v1/chat/completions')
        .send(smallPayload)
        .expect(200);
      
      expect(response.headers['x-protocol']).toBe('nats');
    });
    
    it('should use gRPC for large payloads', async () => {
      const response = await request(app.getHttpServer())
        .post('/v1/chat/completions')
        .send(largePayload)
        .expect(200);
      
      expect(response.headers['x-protocol']).toBe('grpc');
    });
  });
});
```

#### End-to-End Tests

**Complete Flow Tests**:
```typescript
describe('End-to-End Flow Integration', () => {
  it('should complete full authentication and routing flow', async () => {
    // 1. Authenticate user
    const authResponse = await authenticateUser('testuser', 'password');
    const token = authResponse.token;
    
    // 2. Make authenticated request
    const chatResponse = await request(app.getHttpServer())
      .post('/v1/chat/completions')
      .set('Authorization', `Bearer ${token}`)
      .send({
        model: 'gpt-4',
        messages: [{ role: 'user', content: 'Hello' }]
      })
      .expect(200);
    
    // 3. Verify response structure
    expect(chatResponse.body).toHaveProperty('choices');
    expect(chatResponse.body).toHaveProperty('usage');
    
    // 4. Verify tracing
    expect(chatResponse.headers['x-trace-id']).toBeDefined();
  });
});
```

### 9. Migration Strategy

**Goal**: Zero-downtime migration from local Gateway authentication to Router integration

#### Migration Phases

**Phase 1: Dual Mode (Week 1-2)**
- Deploy Router Admin gRPC service alongside existing auth
- Implement feature flag: `USE_ROUTER_AUTH=false`
- Run both systems in parallel
- Compare authentication results

**Phase 2: Gradual Rollout (Week 3-4)**
- Enable Router auth for 10% of traffic
- Monitor error rates and latency
- Gradually increase to 50%, then 100%
- Maintain fallback to local auth

**Phase 3: Full Migration (Week 5)**
- Remove local authentication code
- Update all tests
- Remove feature flag
- Archive local auth modules

#### Rollback Plan

**Emergency Rollback**:
1. Set feature flag: `USE_ROUTER_AUTH=false`
2. Restart Gateway services
3. Verify local authentication works
4. Investigate Router issues
5. Fix and retry migration

**Data Migration**:
- Export local user data to Router
- Verify data integrity
- Update user references
- Maintain audit trail

### 10. Performance Optimization

**Goal**: Optimize integration performance for high-throughput scenarios

#### Optimization Strategies

**Connection Pooling**:
- gRPC connection pools (min: 5, max: 50)
- NATS connection multiplexing
- Redis connection pooling
- Database connection optimization

**Caching Layers**:
- Authentication result caching (5 min TTL)
- Policy caching with invalidation
- Response caching for idempotent requests
- CDN caching for static responses

**Circuit Breaker Pattern**:
```typescript
const circuitBreaker = new CircuitBreaker({
  failureThreshold: 5,
  resetTimeout: 30000,
  monitoringPeriod: 60000,
  onCircuitOpen: () => {
    logger.error('Router circuit breaker opened');
    alertOpsTeam('Router unavailable');
  }
});
```

**Load Balancing**:
- Round-robin for gRPC connections
- Least-connections for NATS
- Geographic routing for global deployment
- Health-based routing

### 11. Security Considerations

**Goal**: Ensure secure communication and data protection

#### Security Measures

**Transport Security**:
- mTLS for all gRPC connections
- TLS 1.3 for NATS connections
- Certificate rotation (90-day lifecycle)
- Certificate pinning for critical services

**Data Protection**:
- Encryption at rest for cached data
- Field-level encryption for sensitive data
- Audit logging for all access attempts
- Data retention policies (30-day default)

**Access Control**:
- Principle of least privilege
- Regular access reviews
- Automated privilege revocation
- Break-glass procedures

**Security Monitoring**:
- Real-time threat detection
- Anomaly detection for access patterns
- Automated incident response
- Regular security audits

### 12. Deployment Configuration

**Goal**: Production-ready deployment with high availability

#### Kubernetes Deployment

**Gateway Deployment** (`gateway-deployment.yaml`):
```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: gateway
spec:
  replicas: 3
  strategy:
    type: RollingUpdate
    rollingUpdate:
      maxUnavailable: 1
      maxSurge: 1
  template:
    spec:
      containers:
      - name: gateway
        image: ghcr.io/beamline/gateway:v1.0.0
        ports:
        - containerPort: 3000
        - containerPort: 9090
        env:
        - name: ROUTER_SERVICE_HOST
          value: "router-service"
        - name: ROUTER_SERVICE_PORT
          value: "9090"
        - name: NATS_SERVERS
          value: "nats://nats-cluster:4222"
        resources:
          requests:
            memory: "512Mi"
            cpu: "500m"
          limits:
            memory: "2Gi"
            cpu: "2000m"
        livenessProbe:
          httpGet:
            path: /health/gateway
            port: 3000
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /health/gateway
            port: 3000
          initialDelaySeconds: 5
          periodSeconds: 5
```

**Service Configuration** (`gateway-service.yaml`):
```yaml
apiVersion: v1
kind: Service
metadata:
  name: gateway-service
spec:
  type: LoadBalancer
  ports:
  - port: 80
    targetPort: 3000
    name: http
  - port: 9090
    targetPort: 9090
    name: grpc
  selector:
    app: gateway
```

#### Environment Configuration

**Development Environment**:
```bash
# .env.development
ROUTER_SERVICE_HOST=localhost
ROUTER_SERVICE_PORT=9090
NATS_SERVERS=nats://localhost:4222
REDIS_URL=redis://localhost:6379
LOG_LEVEL=debug
USE_ROUTER_AUTH=true
```

**Production Environment**:
```bash
# .env.production
ROUTER_SERVICE_HOST=router-service.production.svc.cluster.local
ROUTER_SERVICE_PORT=9090
NATS_SERVERS=nats://nats-cluster.production.svc.cluster.local:4222
REDIS_URL=redis://redis-cluster.production.svc.cluster.local:6379
LOG_LEVEL=info
USE_ROUTER_AUTH=true
TRACING_ENABLED=true
METRICS_ENABLED=true
```

## Implementation Checklist

### Phase 1: Foundation (Week 1)
- [ ] Set up Router Admin gRPC client
- [ ] Implement authentication service integration
- [ ] Create unified error handling
- [ ] Add health check endpoints
- [ ] Write unit tests for core services

### Phase 2: Integration (Week 2)
- [ ] Implement policy management migration
- [ ] Add protocol selection logic
- [ ] Create contract mapping services
- [ ] Set up caching layers
- [ ] Write integration tests

### Phase 3: Optimization (Week 3)
- [ ] Implement circuit breaker patterns
- [ ] Add performance monitoring
- [ ] Optimize connection pooling
- [ ] Fine-tune caching strategies
- [ ] Load testing and optimization

### Phase 4: Production (Week 4)
- [ ] Complete end-to-end testing
- [ ] Set up production monitoring
- [ ] Create deployment configurations
- [ ] Write operational runbooks
- [ ] Conduct security review

### Phase 5: Migration (Week 5)
- [ ] Execute zero-downtime migration
- [ ] Monitor system stability
- [ ] Remove legacy code
- [ ] Update documentation
- [ ] Conduct post-migration review

## Success Criteria

### Functional Requirements
- ✅ All authentication goes through Router Admin gRPC
- ✅ Policy management is centralized in Router
- ✅ Protocol selection works automatically
- ✅ Error handling is unified across components
- ✅ Health monitoring covers all integration points

### Performance Requirements
- ✅ Authentication latency < 50ms P95
- ✅ Policy evaluation < 25ms P95
- ✅ Protocol selection overhead < 5ms
- ✅ Overall request latency < 100ms P95
- ✅ System supports 1000+ RPS per instance

### Reliability Requirements
- ✅ 99.9% uptime for authentication service
- ✅ 99.9% uptime for policy management
- ✅ Graceful degradation on Router unavailability
- ✅ Automatic failover between protocols
- ✅ Zero-downtime deployment capability

### Security Requirements
- ✅ All communications encrypted with mTLS
- ✅ Authentication audit trail complete
- ✅ Access control enforced at all levels
- ✅ No sensitive data in logs or cache
- ✅ Regular security scans pass

This specification provides the complete technical foundation for Worker 2 to implement the Router-Gateway integration, ensuring perfect synchronization and seamless operation across the distributed BeamLine Constructor platform.