# Gateway Rate Limiting Operations Runbook

**Version**: CP2-LC  
**Date**: 2025-11-26  
**Status**: Production Ready  
**Target Audience**: SRE/Ops teams

---

## Overview

This runbook provides operational procedures for managing Gateway rate limiting in production environments. It covers configuration, troubleshooting, and remediation for rate limiting issues including mass 429 responses and distinguishing between real abuse and overly strict limits.

**Key Components**:
- Rate Limiting Implementation (`apps/c-gateway/src/http_server.c`)
- Fixed-Window Algorithm (CP1) / Distributed Rate Limiting (CP2+ PoC)
- Per-Endpoint Limits (routes/decide, messages, registry/blocks)
- HTTP 429 Responses with Standard Headers

---

## Table of Contents

1. [Health Checks](#health-checks)
2. [Configuration](#configuration)
3. [Common Symptoms](#common-symptoms)
4. [Diagnosis Procedures](#diagnosis-procedures)
5. [Remediation Steps](#remediation-steps)
6. [Distinguishing Abuse vs Strict Limits](#distinguishing-abuse-vs-strict-limits)
7. [Emergency Procedures](#emergency-procedures)
8. [Monitoring & Alerts](#monitoring--alerts)
9. [Troubleshooting](#troubleshooting)

---

## Health Checks

### 1. Rate Limiting Status

#### Check Rate Limiting Metrics

**Via Prometheus/Grafana**:
```promql
# Rate limit hits
rate(gateway_rate_limit_hits_total[5m]) by (endpoint)

# Rate limit allowed
rate(gateway_rate_limit_allowed_total[5m]) by (endpoint)

# Rate limit exceeded
rate(gateway_rate_limit_exceeded_total[5m]) by (endpoint)

# Rate limit hit ratio
rate(gateway_rate_limit_exceeded_total[5m]) / rate(gateway_rate_limit_hits_total[5m])
```

**Expected Values**:
- `gateway_rate_limit_hits_total` > 0 (rate limiting active)
- `gateway_rate_limit_exceeded_total` ≈ 0 (no excessive 429s)
- Hit ratio < 0.1 (less than 10% of requests rate limited)

#### Check HTTP Status Codes

**Via Prometheus/Grafana**:
```promql
# HTTP 429 responses
rate(gateway_http_requests_total{status="429"}[5m]) by (path)

# HTTP 429 vs other statuses
rate(gateway_http_requests_total[5m]) by (status)
```

**Expected Values**:
- `status="429"` < 5% of total requests
- `status="200"` > 90% of total requests

#### Check Rate Limiting Configuration

**Via Environment Variables**:
```bash
# Check current limits
echo $GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT
echo $GATEWAY_RATE_LIMIT_MESSAGES
echo $GATEWAY_RATE_LIMIT_TTL_SECONDS

# Check distributed rate limiting
echo $GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED
echo $GATEWAY_RATE_LIMIT_MODE
echo $GATEWAY_RATE_LIMIT_BACKEND
echo $GATEWAY_RATE_LIMIT_REDIS_URI
echo $GATEWAY_RATE_LIMIT_FALLBACK_TO_LOCAL
```

**Expected Values**:
- Limits configured (default: 50/100/200 req/min)
- TTL configured (default: 60 seconds)
- Mode configured (`local` | `redis` | `hybrid`)
- Redis URI configured (if mode is `redis` or `hybrid`)
- Fallback enabled (default: `true`)

---

## Configuration

### Distributed Rate Limiting (CP2+)

**Feature Flags**:
- `GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED`: Enable distributed rate limiting (default: `false`)
- `GATEWAY_RATE_LIMIT_MODE`: Rate limiting mode (`local` | `redis` | `hybrid`, default: `local`)
- `GATEWAY_RATE_LIMIT_REDIS_URI`: Redis connection URI (recommended: `redis://host:port`)
- `GATEWAY_RATE_LIMIT_FALLBACK_TO_LOCAL`: Enable fallback to local mode (default: `true`)

**Configuration Examples**:

**Staging Mode**:
```bash
export GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED=true
export GATEWAY_RATE_LIMIT_MODE=redis
export GATEWAY_RATE_LIMIT_REDIS_URI=redis://redis-staging:6379
export GATEWAY_RATE_LIMIT_REDIS_TIMEOUT_MS=2000
export GATEWAY_RATE_LIMIT_FALLBACK_TO_LOCAL=true
```

**Production Mode**:
```bash
export GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED=true
export GATEWAY_RATE_LIMIT_MODE=redis
export GATEWAY_RATE_LIMIT_REDIS_URI=redis://redis-prod:6379
export GATEWAY_RATE_LIMIT_REDIS_TIMEOUT_MS=1000
export GATEWAY_RATE_LIMIT_FALLBACK_TO_LOCAL=true
```

**Strict Mode (No Fallback)**:
```bash
export GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED=true
export GATEWAY_RATE_LIMIT_MODE=redis
export GATEWAY_RATE_LIMIT_REDIS_URI=redis://redis-prod:6379
export GATEWAY_RATE_LIMIT_FALLBACK_TO_LOCAL=false
```

**Mode Logging**:
- Check Gateway logs for rate limiter initialization:
  ```
  INFO: Rate limiter initialized in redis mode
  INFO: Redis backend: redis-staging:6379 (timeout: 2000ms)
  ```
- Check for fallback events:
  ```
  WARNING: Rate limiter error (mode: redis), checking fallback
  INFO: Fallback to local mode enabled, allowing request
  INFO: Rate limiter mode changed to fallback due to errors
  ```

**For detailed staging rollout plan, see**: `docs/archive/dev/GATEWAY_DISTRIBUTED_RATE_LIMITING_STAGING_ROLLOUT.md`

### Changing Rate Limits

#### Method 1: Environment Variables (Runtime)

**For Single Instance**:
```bash
# Set new limits
export GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT=100  # Increase from 50 to 100
export GATEWAY_RATE_LIMIT_MESSAGES=200  # Increase from 100 to 200
export GATEWAY_RATE_LIMIT_TTL_SECONDS=60  # Keep window at 60 seconds

# Restart Gateway
systemctl restart c-gateway
```

**For Multiple Instances** (if using distributed rate limiting):
```bash
# Set limits for all instances
for instance in gateway-1 gateway-2 gateway-3; do
    ssh $instance "export GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT=100 && systemctl restart c-gateway"
done
```

#### Method 2: Configuration File (Persistent)

**Edit Configuration File**:
```bash
# Edit Gateway configuration
vim /etc/c-gateway/config.env

# Add/update limits
GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT=100
GATEWAY_RATE_LIMIT_MESSAGES=200
GATEWAY_RATE_LIMIT_TTL_SECONDS=60

# Restart Gateway
systemctl restart c-gateway
```

#### Method 3: Kubernetes ConfigMap (If Using K8s)

**Update ConfigMap**:
```bash
# Edit ConfigMap
kubectl edit configmap gateway-config

# Update limits
GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT=100
GATEWAY_RATE_LIMIT_MESSAGES=200

# Restart Gateway pods
kubectl rollout restart deployment/gateway
```

### Verifying Rate Limit Changes

**Via Metrics**:
```promql
# Check if limits changed (observe behavior)
rate(gateway_rate_limit_exceeded_total[5m]) by (endpoint)

# Should decrease if limits increased
```

**Via HTTP Headers**:
```bash
# Check rate limit headers
curl -I http://gateway.example.com/api/v1/routes/decide \
  -H "X-Tenant-ID: test-tenant"

# Look for:
# X-RateLimit-Limit: 100
# X-RateLimit-Remaining: 99
# X-RateLimit-Reset: 1234567890
```

---

## Common Symptoms

### Symptom 1: Mass 429 Responses

**Indicators**:
- `gateway_rate_limit_exceeded_total` increasing rapidly
- `gateway_http_requests_total{status="429"}` > 10% of total requests
- Clients reporting "Too Many Requests" errors
- Alerts: `GatewayRateLimitExceededHigh` (if configured)

**Possible Causes**:
1. **Legitimate Traffic Spike**: Sudden increase in legitimate traffic
2. **Too Strict Limits**: Limits set too low for normal traffic
3. **Client Retry Storms**: Clients retrying failed requests excessively
4. **Distributed Rate Limiting Issues**: Inconsistent limits across instances (if using distributed mode)

**Diagnosis**: See [Diagnosis: Mass 429](#diagnosis-mass-429)

### Symptom 2: Inconsistent Rate Limiting

**Indicators**:
- Different Gateway instances showing different rate limit behavior
- Some instances allowing requests while others reject
- Metrics showing inconsistent patterns across instances

**Possible Causes**:
1. **In-Memory Mode (CP1)**: Each instance has independent counters
2. **Distributed Mode Issues**: Redis unavailable or inconsistent state
3. **Configuration Mismatch**: Different limits configured per instance

**Diagnosis**: See [Diagnosis: Inconsistent Limits](#diagnosis-inconsistent-limits)

### Symptom 3: Rate Limiting Not Working

**Indicators**:
- `gateway_rate_limit_exceeded_total` = 0 despite high traffic
- No 429 responses even when limits should be exceeded
- Rate limiting appears disabled

**Possible Causes**:
1. **Rate Limiting Disabled**: Configuration disabled rate limiting
2. **Limit Set Too High**: Limits exceed actual traffic
3. **Implementation Issue**: Rate limiting code not executing

**Diagnosis**: See [Diagnosis: Rate Limiting Not Working](#diagnosis-rate-limiting-not-working)

---

## Diagnosis Procedures

### Diagnosis: Mass 429

#### Step 1: Check Rate Limit Metrics

**Via Prometheus/Grafana**:
```promql
# Rate limit exceeded rate
rate(gateway_rate_limit_exceeded_total[5m]) by (endpoint, tenant_id)

# Rate limit hit ratio
rate(gateway_rate_limit_exceeded_total[5m]) / rate(gateway_rate_limit_hits_total[5m])

# Top tenants hitting limits
topk(10, rate(gateway_rate_limit_exceeded_total[5m]) by (tenant_id))
```

**What to Look For**:
- Which endpoints are most affected?
- Which tenants are hitting limits?
- What is the hit ratio? (> 0.5 = too strict)

#### Step 2: Check Traffic Patterns

**Via Prometheus/Grafana**:
```promql
# Total requests
rate(gateway_http_requests_total[5m]) by (path)

# Requests by tenant
rate(gateway_http_requests_total[5m]) by (tenant_id)

# Request rate over time
rate(gateway_http_requests_total[1h])
```

**What to Look For**:
- Sudden traffic spike?
- Specific tenant causing issues?
- Traffic pattern (burst vs steady)

#### Step 3: Check Current Limits

**Via Configuration**:
```bash
# Check current limits
echo $GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT
echo $GATEWAY_RATE_LIMIT_MESSAGES
echo $GATEWAY_RATE_LIMIT_TTL_SECONDS

# Check if limits are reasonable for traffic
# Compare: limit vs actual request rate
```

**What to Look For**:
- Limits too low for normal traffic?
- TTL too short (causing premature resets)?

#### Step 4: Check Client Behavior

**Via Logs**:
```bash
# Check for retry storms
grep "429" /var/log/gateway/gateway.log | awk '{print $NF}' | sort | uniq -c | sort -rn | head -10

# Check request patterns
grep "POST /api/v1/routes/decide" /var/log/gateway/gateway.log | tail -100
```

**What to Look For**:
- Clients retrying excessively?
- Burst patterns (many requests in short time)?
- Specific clients causing issues?

### Diagnosis: Inconsistent Limits

#### Step 1: Check Per-Instance Metrics

**Via Prometheus/Grafana**:
```promql
# Rate limit exceeded by instance
rate(gateway_rate_limit_exceeded_total[5m]) by (instance, endpoint)

# Compare instances
sum(rate(gateway_rate_limit_exceeded_total[5m]) by (instance))
```

**What to Look For**:
- Different instances showing different behavior?
- Some instances allowing more requests?

#### Step 2: Check Distributed Rate Limiting Status

**Via Configuration**:
```bash
# Check if distributed mode enabled
echo $GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED
echo $GATEWAY_RATE_LIMIT_BACKEND

# Check Redis connection (if using distributed mode)
redis-cli -h $GATEWAY_RATE_LIMIT_REDIS_HOST -p $GATEWAY_RATE_LIMIT_REDIS_PORT ping
```

**What to Look For**:
- Distributed mode enabled but Redis unavailable?
- Fallback to memory mode causing inconsistencies?

#### Step 3: Check Configuration Consistency

**Via Configuration**:
```bash
# Check limits on all instances
for instance in gateway-1 gateway-2 gateway-3; do
    echo "=== $instance ==="
    ssh $instance "echo \$GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT"
done
```

**What to Look For**:
- Different limits configured per instance?
- Configuration drift?

### Diagnosis: Rate Limiting Not Working

#### Step 1: Check Rate Limiting Enabled

**Via Configuration**:
```bash
# Check if rate limiting is enabled
echo $GATEWAY_RATE_LIMIT_ENABLED  # Should be "true" or unset (default: enabled)

# Check Gateway code (if accessible)
grep -r "rate_limit_check" /path/to/c-gateway/src/
```

**What to Look For**:
- Rate limiting disabled?
- Code not calling rate limit checks?

#### Step 2: Check Limits vs Traffic

**Via Metrics**:
```promql
# Request rate vs limit
rate(gateway_http_requests_total[5m]) by (path)
# Compare with configured limits

# If request rate < limit, rate limiting won't trigger
```

**What to Look For**:
- Limits much higher than actual traffic?
- Rate limiting not needed (traffic below limits)?

#### Step 3: Check Implementation

**Via Logs**:
```bash
# Check for rate limit logs
grep "rate_limit" /var/log/gateway/gateway.log | tail -20

# Check for rate limit errors
grep "rate_limit.*error\|rate_limit.*fail" /var/log/gateway/gateway.log
```

**What to Look For**:
- Rate limiting code executing?
- Any errors in rate limiting logic?

---

## Remediation Steps

### Remediation: Mass 429 Responses

#### Step 1: Determine Root Cause

**Decision Tree**:
- **Legitimate Traffic Spike** → Increase limits temporarily
- **Too Strict Limits** → Increase limits permanently
- **Client Retry Storms** → Fix client retry logic
- **Distributed Mode Issues** → Fix Redis or fallback to memory mode

#### Step 2: Increase Limits (Temporary)

**Actions**:
1. Increase limits by 2x (temporary relief)
2. Monitor rate limit hit ratio
3. Adjust based on traffic patterns

**Via Environment Variables**:
```bash
# Double the limits (temporary)
export GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT=100  # Was 50
export GATEWAY_RATE_LIMIT_MESSAGES=200  # Was 100

# Restart Gateway
systemctl restart c-gateway
```

**Monitor**:
```promql
# Watch rate limit hit ratio (should decrease)
rate(gateway_rate_limit_exceeded_total[5m]) / rate(gateway_rate_limit_hits_total[5m])
```

#### Step 3: Increase Limits (Permanent)

**Actions**:
1. Calculate appropriate limits based on traffic patterns
2. Update configuration files
3. Deploy to all instances

**Calculation**:
```bash
# Calculate limit based on traffic
# Example: P95 request rate = 80 req/min
# Set limit = P95 * 1.5 = 120 req/min (safety margin)

# Update configuration
GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT=120
```

#### Step 4: Fix Client Retry Storms

**Actions**:
1. Identify clients with excessive retries
2. Coordinate with client teams
3. Implement exponential backoff

**Client Fix**:
```python
# Example: Exponential backoff for 429
import time

def retry_with_backoff(func, max_retries=3):
    for attempt in range(max_retries):
        try:
            return func()
        except HTTPError as e:
            if e.status == 429:
                retry_after = int(e.headers.get('Retry-After', 60))
                wait_time = min(retry_after * (2 ** attempt), 300)  # Max 5 minutes
                time.sleep(wait_time)
            else:
                raise
    raise MaxRetriesExceeded()
```

### Remediation: Inconsistent Limits

#### Step 1: Enable Distributed Rate Limiting

**Actions**:
1. Enable distributed mode (if not already)
2. Configure Redis backend
3. Verify Redis connectivity

**Via Configuration**:
```bash
# Enable distributed rate limiting
export GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED=true
export GATEWAY_RATE_LIMIT_BACKEND=redis
export GATEWAY_RATE_LIMIT_REDIS_HOST=redis.example.com
export GATEWAY_RATE_LIMIT_REDIS_PORT=6379

# Restart Gateway
systemctl restart c-gateway
```

#### Step 2: Fix Redis Issues

**Actions**:
1. Check Redis health
2. Fix Redis connectivity
3. Verify Redis HA (if using)

**Via Command Line**:
```bash
# Check Redis health
redis-cli -h redis.example.com -p 6379 ping

# Check Redis info
redis-cli -h redis.example.com -p 6379 info

# Check Redis memory
redis-cli -h redis.example.com -p 6379 info memory
```

#### Step 3: Standardize Configuration

**Actions**:
1. Ensure all instances have same limits
2. Use configuration management (ConfigMap, etc.)
3. Verify configuration consistency

**Via Configuration Management**:
```bash
# Update all instances via ConfigMap (Kubernetes)
kubectl patch configmap gateway-config -p '{"data":{"GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT":"100"}}'

# Restart all pods
kubectl rollout restart deployment/gateway
```

### Remediation: Rate Limiting Not Working

#### Step 1: Verify Rate Limiting Enabled

**Actions**:
1. Check configuration
2. Verify code execution
3. Test rate limiting manually

**Via Configuration**:
```bash
# Ensure rate limiting is enabled (default: enabled)
# If disabled, enable it:
export GATEWAY_RATE_LIMIT_ENABLED=true

# Restart Gateway
systemctl restart c-gateway
```

#### Step 2: Test Rate Limiting

**Via HTTP Requests**:
```bash
# Send requests to test rate limiting
for i in {1..60}; do
    curl -X POST http://gateway.example.com/api/v1/routes/decide \
        -H "Content-Type: application/json" \
        -H "X-Tenant-ID: test-tenant" \
        -d '{"version":"1","tenant_id":"test-tenant","request_id":"req-'$i'","task":{"type":"text.generate","payload":{}}}'
    echo ""
done

# Should see 429 after limit exceeded
```

#### Step 3: Check Implementation

**Actions**:
1. Verify rate limiting code is called
2. Check for errors in rate limiting logic
3. Review Gateway logs

**Via Logs**:
```bash
# Check for rate limiting logs
grep "rate_limit" /var/log/gateway/gateway.log | tail -20

# Check for errors
grep "rate_limit.*error" /var/log/gateway/gateway.log
```

---

## Distinguishing Abuse vs Strict Limits

### Indicators of Real Abuse

**Patterns**:
1. **Excessive Request Rate**: Single tenant/IP sending 1000+ req/min
2. **Burst Patterns**: Sudden spikes (100+ requests in 1 second)
3. **Retry Storms**: Continuous retries without backoff
4. **Distributed Attacks**: Multiple IPs/tenants coordinated
5. **Unusual Patterns**: Requests outside normal business hours

**Metrics to Check**:
```promql
# Request rate by tenant
rate(gateway_http_requests_total[5m]) by (tenant_id)

# Request rate by IP
rate(gateway_http_requests_total[5m]) by (client_ip)

# Burst detection
max_over_time(rate(gateway_http_requests_total[1m])[5m:1m])
```

### Indicators of Too Strict Limits

**Patterns**:
1. **Legitimate Traffic**: Normal business traffic hitting limits
2. **Gradual Increase**: Traffic growing over time, limits not adjusted
3. **Multiple Tenants**: Many tenants hitting limits simultaneously
4. **Consistent Pattern**: Limits hit at same time every day (business hours)
5. **Low Hit Ratio**: < 5% of requests hitting limits (limits too low)

**Metrics to Check**:
```promql
# Rate limit hit ratio
rate(gateway_rate_limit_exceeded_total[5m]) / rate(gateway_rate_limit_hits_total[5m])

# If < 0.05 (5%), limits may be too strict

# Traffic distribution
histogram_quantile(0.95, rate(gateway_http_requests_total[5m]))
# Compare with configured limits
```

### Decision Matrix

| Indicator | Abuse | Too Strict |
|-----------|-------|------------|
| **Request Rate** | 1000+ req/min per tenant | 50-100 req/min per tenant |
| **Pattern** | Burst, random | Steady, business hours |
| **Tenants Affected** | 1-2 tenants | Many tenants |
| **Hit Ratio** | > 0.5 (50%) | < 0.05 (5%) |
| **Time Pattern** | Random times | Business hours |
| **IP Distribution** | Single/few IPs | Many IPs |

### Actions Based on Diagnosis

**If Abuse**:
1. **Block Abusive Tenant/IP**: Update firewall/ACL rules
2. **Contact Client**: Coordinate with client team
3. **Increase Limits Temporarily**: If legitimate traffic also affected
4. **Implement WAF**: Web Application Firewall for advanced protection

**If Too Strict**:
1. **Increase Limits**: Adjust limits based on traffic patterns
2. **Review Traffic Patterns**: Understand normal traffic levels
3. **Set Appropriate Limits**: P95 traffic * 1.5 safety margin
4. **Monitor and Adjust**: Continuously monitor and adjust

---

## Emergency Procedures

### Emergency: Mass 429 Responses

**Symptoms**:
- All clients getting 429 responses
- System effectively unavailable
- High error rate

**Immediate Actions**:
1. **Temporarily Increase Limits**:
   ```bash
   # Increase limits by 10x (emergency)
   export GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT=500  # Was 50
   export GATEWAY_RATE_LIMIT_MESSAGES=1000  # Was 100
   
   # Restart Gateway
   systemctl restart c-gateway
   ```

2. **Disable Rate Limiting** (LAST RESORT):
   ```bash
   # Disable rate limiting (emergency only)
   export GATEWAY_RATE_LIMIT_ENABLED=false
   
   # Restart Gateway
   systemctl restart c-gateway
   ```

3. **Investigate Root Cause**:
   - Check traffic patterns
   - Check for abuse
   - Check distributed rate limiting issues

### Emergency: Distributed Rate Limiting Failure

**Symptoms**:
- Redis unavailable
- Inconsistent limits across instances
- Fallback to memory mode not working
- Gateway logs show: "WARNING: Rate limiter error (mode: redis), checking fallback"
- Gateway logs show: "INFO: Rate limiter mode changed to fallback due to errors"

**Immediate Actions**:
1. **Verify Fallback is Working**:
   ```bash
   # Check Gateway logs for fallback events
   grep "fallback" /var/log/gateway/gateway.log | tail -20
   
   # Check if fallback is enabled
   echo $GATEWAY_RATE_LIMIT_FALLBACK_TO_LOCAL  # Should be "true"
   
   # If fallback is working, Gateway should continue operating in memory mode
   # No immediate action needed, but investigate Redis issue
   ```

2. **Fallback to Memory Mode** (if fallback not working):
   ```bash
   # Disable distributed mode
   export GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED=false
   export GATEWAY_RATE_LIMIT_MODE=local
   # Or use legacy flag:
   export GATEWAY_RATE_LIMIT_BACKEND=memory
   
   # Restart Gateway
   systemctl restart c-gateway
   ```

3. **Fix Redis**:
   ```bash
   # Check Redis health (if using URI)
   redis-cli -u redis://redis.example.com:6379 ping
   
   # Or if using HOST:PORT
   redis-cli -h redis.example.com -p 6379 ping
   
   # Check Redis connection from Gateway
   curl http://gateway.example.com/_health
   # Look for rate limiter status in response
   
   # Restart Redis if needed
   systemctl restart redis
   # Or if using Docker:
   docker restart redis-staging
   ```

4. **Re-enable Distributed Mode** (after Redis fixed):
   ```bash
   # Re-enable after Redis fixed
   export GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED=true
   export GATEWAY_RATE_LIMIT_MODE=redis
   export GATEWAY_RATE_LIMIT_REDIS_URI=redis://redis.example.com:6379
   
   # Restart Gateway
   systemctl restart c-gateway
   
   # Verify mode restored
   grep "Rate limiter initialized in redis mode" /var/log/gateway/gateway.log
   ```

**Monitoring Fallback Events**:
```promql
# Check fallback count (if metric available)
gateway_rate_limit_fallback_count

# Check rate limiter mode (if metric available)
gateway_rate_limit_mode  # 0=memory, 1=redis, 2=fallback

# Check Redis errors
rate(gateway_redis_errors_total[5m])
```

---

## Monitoring & Alerts

### Key Metrics to Monitor

**Rate Limiting**:
- `gateway_rate_limit_hits_total` - Total rate limit checks
- `gateway_rate_limit_allowed_total` - Requests allowed
- `gateway_rate_limit_exceeded_total` - Requests rate limited

**HTTP Status**:
- `gateway_http_requests_total{status="429"}` - 429 responses
- `gateway_http_requests_total{status="200"}` - Successful responses

**Rate Limit Headers**:
- Check `X-RateLimit-Limit`, `X-RateLimit-Remaining`, `X-RateLimit-Reset` headers

### Abuse Detection Metrics

**Gateway Abuse Metrics**:
- `gateway_abuse_empty_payload_total`: Counter for empty payload abuse events
- `gateway_abuse_targeted_tenant_total`: Counter for targeted tenant attacks
- `gateway_abuse_rate_limit_evasion_total`: Counter for rate limit evasion attempts
- `gateway_abuse_heavy_payload_total`: Counter for heavy payload abuse events
- `gateway_abuse_multi_tenant_flood_total`: Counter for multi-tenant flood attacks
- `gateway_abuse_blocked_tenants`: Gauge for number of currently blocked tenants

**Interpreting Abuse Metrics**:
- **Normal**: All abuse metrics ≈ 0, no alerts firing
- **Warning**: Occasional abuse events (< threshold), alerts firing but not critical
- **Critical**: High rate of abuse events (> threshold), multiple alerts firing, requires immediate action

**Query Examples**:
```promql
# Empty payload abuse rate
rate(gateway_abuse_empty_payload_total[5m])

# Targeted tenant abuse rate
rate(gateway_abuse_targeted_tenant_total[5m])

# Rate limit evasion rate
rate(gateway_abuse_rate_limit_evasion_total[5m])

# Multi-tenant flood rate
rate(gateway_abuse_multi_tenant_flood_total[5m])

# Blocked tenants count
gateway_abuse_blocked_tenants
```

### Alert Rules

**See**: `apps/otp/router/docs/PROMETHEUS_ALERTS.md` for complete alert definitions.

**Rate Limiting Alerts**:
- `GatewayRateLimitExceededHigh` - Rate limit exceeded > 10% of requests
- `GatewayRateLimitInconsistent` - Inconsistent limits across instances (if distributed mode)

**Abuse Detection Alerts**:

1. **GatewayAbuseEventDetected**:
   - **Trigger**: Any abuse event detected (> 0 events/min)
   - **Severity**: Warning
   - **Action**: Check specific abuse metrics for details

2. **GatewayAbuseEmptyPayloadHigh**:
   - **Trigger**: High rate of empty payload abuse (> 20 events/min)
   - **Severity**: Warning
   - **Action**: Check logs for tenant_id, review payload validation thresholds
   - **Remediation**: Block offending tenant if confirmed abuse, adjust min_payload_size if false positive

3. **GatewayAbuseTargetedTenantHigh**:
   - **Trigger**: High rate of targeted tenant attacks (> 5 events/min)
   - **Severity**: Critical
   - **Action**: **CRITICAL** - Review tenant logs immediately, check for DDoS
   - **Remediation**: Temporarily block tenant, contact tenant owner, review rate limits

4. **GatewayAbuseRateLimitEvasionHigh**:
   - **Trigger**: High rate of rate limit evasion attempts (> 3 events/min)
   - **Severity**: Warning
   - **Action**: Review tenant's API key usage, check for compromised keys
   - **Remediation**: Revoke suspicious API keys, temporarily block tenant

5. **GatewayAbuseHeavyPayloadHigh**:
   - **Trigger**: High rate of heavy payload abuse (> 10 events/min)
   - **Severity**: Warning
   - **Action**: Check payload size distribution, review tenant behavior
   - **Remediation**: Block tenant if confirmed abuse, adjust large_payload_threshold if false positive

6. **GatewayAbuseMultiTenantFloodHigh**:
   - **Trigger**: High rate of multi-tenant flood attacks (> 5 events/min)
   - **Severity**: Critical
   - **Action**: **CRITICAL** - Review global rate limits, check for coordinated attack
   - **Remediation**: Increase global rate limits, block suspicious IPs, contact security team

7. **GatewayAbuseBlockedTenantsHigh**:
   - **Trigger**: High number of blocked tenants (> 100)
   - **Severity**: Warning
   - **Action**: Review abuse detection thresholds, check for false positives
   - **Remediation**: Adjust thresholds if too strict, review blocked tenant list

**Steps When Abuse Alert Fires**:
1. **Check Metrics**: Review abuse metrics in Prometheus/Grafana
2. **Check Logs**: Review structured logs for abuse events (tenant_id, request_id, trace_id)
3. **Identify Pattern**: Determine abuse type (empty payload, heavy payload, targeted tenant, rate limit evasion, multi-tenant flood)
4. **Verify Legitimacy**: Check if false positive (legitimate use case vs actual abuse)
5. **Take Action**: Block tenant, adjust thresholds, revoke API keys, or contact tenant owner
6. **Monitor**: Continue monitoring metrics and logs after remediation

---

## Troubleshooting

### Issue: Mass 429 Responses

**Symptoms**: Many clients getting 429 responses

**Diagnosis**:
1. Check rate limit hit ratio
2. Check traffic patterns
3. Check current limits

**Resolution**:
- Increase limits if too strict
- Fix client retry logic if retry storms
- Investigate abuse if suspicious patterns

**Reference**: [Remediation: Mass 429](#remediation-mass-429-responses)

### Issue: Inconsistent Limits

**Symptoms**: Different instances showing different behavior

**Diagnosis**:
1. Check distributed rate limiting status
2. Check Redis connectivity
3. Check configuration consistency

**Resolution**:
- Enable distributed rate limiting
- Fix Redis issues
- Standardize configuration

**Reference**: [Remediation: Inconsistent Limits](#remediation-inconsistent-limits)

### Issue: Rate Limiting Not Working

**Symptoms**: No 429 responses despite high traffic

**Diagnosis**:
1. Check rate limiting enabled
2. Check limits vs traffic
3. Check implementation

**Resolution**:
- Enable rate limiting if disabled
- Adjust limits if too high
- Fix implementation issues

**Reference**: [Remediation: Rate Limiting Not Working](#remediation-rate-limiting-not-working)

---

## Distributed Rate Limiting (CP2+)

### Configuration

**Feature Flags**:
- `GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED` (default: `false`)
  - Enable distributed rate limiting
  - Values: `true` | `false` | `1` | `0`

- `GATEWAY_RATE_LIMIT_BACKEND` (default: `memory`)
  - Backend selection
  - Values: `memory` | `redis`

**Redis Configuration**:
- `GATEWAY_RATE_LIMIT_REDIS_HOST` (default: `localhost`)
- `GATEWAY_RATE_LIMIT_REDIS_PORT` (default: `6379`)
- `GATEWAY_RATE_LIMIT_REDIS_URI` (optional, overrides host/port)
- `GATEWAY_RATE_LIMIT_REDIS_TIMEOUT_MS` (default: `1000`)

**Fallback Configuration**:
- `GATEWAY_RATE_LIMIT_FALLBACK_TO_LOCAL` (default: `true`)
  - Enable fallback to in-memory mode when Redis unavailable
  - **Note**: If `false`, requests will return 503 when Redis unavailable

### Health Checks

**Check Rate Limiter Mode**:
```bash
# Check Gateway logs for mode
grep "Rate limiter initialized" /var/log/c-gateway.log
# Expected: "INFO: Rate limiter initialized in {memory|redis} mode"
```

**Check Redis Connectivity**:
```bash
# Test Redis connection
redis-cli -h $GATEWAY_RATE_LIMIT_REDIS_HOST -p $GATEWAY_RATE_LIMIT_REDIS_PORT ping
# Expected: PONG
```

**Check Fallback Usage**:
```bash
# Check logs for fallback events
grep "fallback" /var/log/c-gateway.log
# Expected: "WARNING: Rate limiter error, fallback to local mode" (if Redis fails)
```

### Common Issues

**Issue: Redis Connection Failures**

**Symptoms**:
- Logs show "Rate limiter error, fallback to local mode"
- Fallback mode active
- Rate limits inconsistent across instances

**Diagnosis**:
1. Check Redis connectivity: `redis-cli -h $GATEWAY_RATE_LIMIT_REDIS_HOST -p $GATEWAY_RATE_LIMIT_REDIS_PORT ping`
2. Check Redis logs: `tail -f /var/log/redis/redis.log`
3. Check Gateway logs: `grep "Rate limiter" /var/log/c-gateway.log`

**Resolution**:
1. Fix Redis connectivity issues
2. Verify Redis configuration (host, port, timeout)
3. Restart Gateway after Redis fixed
4. Verify: Logs show "Rate limiter initialized in redis mode"

**Issue: High Redis Latency**

**Symptoms**:
- High latency for rate limit checks
- Requests timing out
- Performance degradation

**Diagnosis**:
1. Check Redis latency: `redis-cli --latency -h $GATEWAY_RATE_LIMIT_REDIS_HOST -p $GATEWAY_RATE_LIMIT_REDIS_PORT`
2. Check Redis metrics: `redis-cli INFO stats`
3. Check Gateway metrics: `gateway_rate_limiter_redis_latency_seconds`

**Resolution**:
1. Optimize Redis configuration
2. Increase `GATEWAY_RATE_LIMIT_REDIS_TIMEOUT_MS` if needed
3. Enable local cache: `GATEWAY_RATE_LIMIT_LOCAL_CACHE_TTL_SECONDS=10`
4. Consider Redis cluster for high availability

**Issue: Fallback Not Working**

**Symptoms**:
- Requests return 503 when Redis unavailable
- Logs show "Rate limiter error and fallback disabled"

**Diagnosis**:
1. Check fallback configuration: `echo $GATEWAY_RATE_LIMIT_FALLBACK_TO_LOCAL`
2. Check Gateway logs: `grep "fallback" /var/log/c-gateway.log`

**Resolution**:
1. Enable fallback: `export GATEWAY_RATE_LIMIT_FALLBACK_TO_LOCAL=true`
2. Restart Gateway
3. Verify: Requests succeed even when Redis unavailable

### Emergency Procedures

**Disable Distributed Rate Limiting**:
```bash
# Set environment variables
export GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED=false
export GATEWAY_RATE_LIMIT_BACKEND=memory

# Restart Gateway
systemctl restart c-gateway

# Verify: Logs show "Rate limiter initialized in memory mode"
```

**Enable Fallback (if disabled)**:
```bash
# Set environment variable
export GATEWAY_RATE_LIMIT_FALLBACK_TO_LOCAL=true

# Restart Gateway
systemctl restart c-gateway

# Verify: Requests succeed even when Redis unavailable
```

**For detailed staging rollout plan, see**:
- **`docs/archive/dev/GATEWAY_DISTRIBUTED_RATE_LIMITING_STAGING_ROLLOUT.md`** - Complete staging rollout plan

## References

- **Rate Limiting Spec**: `docs/GATEWAY_RATE_LIMITING.md`
- **Gateway Routes**: `docs/GATEWAY_ROUTES_SPEC.md`
- **Distributed Rate Limiting**: `docs/ARCHITECTURE/gateway-distributed-rate-limiting.md`
- **Staging Rollout Plan**: `docs/archive/dev/GATEWAY_DISTRIBUTED_RATE_LIMITING_STAGING_ROLLOUT.md`
- **Error Handling Priority**: `docs/GATEWAY_ROUTER_ERROR_HANDLING_PRIORITY.md`
- **Metrics Guide**: `docs/OBSERVABILITY_METRICS_MONITORING_GUIDE.md`

