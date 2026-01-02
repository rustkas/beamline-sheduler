# Extensions Developer Guide

**Version**: 1.0  
**Date**: 2025-01-27  
**Status**: ✅ Approved  
**Target Audience**: Extension developers

---

## Purpose

This guide helps developers create, test, and deploy BeamLine extensions quickly. You should be able to create your first extension in 1-2 hours following this guide.

---

## Quick Start

### Step 1: Generate Extension Boilerplate

```bash
# Generate extension template
./scripts/generate_extension.sh my_extension pre \
  --description "My pre-processor extension" \
  --timeout 1000 \
  --retry 0

# This creates:
# - tools/extensions/my_extension/
#   - src/my_extension.js (handler)
#   - test/my_extension.test.js (unit tests)
#   - test/my_extension.e2e.js (E2E tests)
#   - package.json
#   - my_extension.registry.json (registry config)
#   - README.md
```

### Step 2: Implement Your Logic

Edit `tools/extensions/my_extension/src/my_extension.js`:

```javascript
// In processRequest() function, implement your logic:
async function processRequest(request) {
    const payload = request.payload || {};
    const messagePayload = payload.payload || '';
    
    // Your logic here
    const processed = messagePayload.toUpperCase(); // Example
    
    return {
        payload: {
            ...payload,
            payload: processed
        },
        metadata: {
            ...request.metadata,
            processed_by: 'my_extension'
        }
    };
}
```

### Step 3: Test Locally

```bash
# Start NATS (if not running)
docker-compose up -d nats

# Run extension
cd tools/extensions/my_extension
npm install
npm start
```

### Step 4: Register in Router

```bash
# Copy registry config to Router fixtures
cp my_extension.registry.json ../../apps/otp/router/priv/fixtures/extensions/

# Restart Router (or reload registry)
```

### Step 5: Run E2E Tests

```bash
# From project root
./scripts/run_extension_e2e.sh my_extension
```

---

## Extension Types

### Pre-processor

**Purpose**: Modify/enrich incoming message before routing

**Response Format**:
```json
{
  "payload": {
    "message_id": "m-1",
    "payload": "modified content",
    "metadata": {}
  },
  "metadata": {
    "enriched_field": "value"
  }
}
```

**Example**: Text normalization, language detection, content enrichment

### Validator

**Purpose**: Decide whether processing can continue

**Response Format**:
```json
{
  "status": "ok" | "reject",
  "reason": "optional reason",
  "details": {
    "field": "payload",
    "message": "Validation details"
  }
}
```

**Example**: PII detection, content filtering, rate limiting

### Post-processor

**Purpose**: Modify provider response after routing

**Response Format**:
```json
{
  "payload": {
    "message_id": "m-1",
    "payload": "modified response",
    "metadata": {}
  },
  "metadata": {
    "processed_at": "2025-01-27T12:00:00Z"
  }
}
```

**Example**: PII masking, response formatting, logging

### Custom Provider

**Purpose**: Act as another provider (LLM, RAG, CRM, etc.)

**Response Format** (CP2-style):
```json
{
  "status": "success" | "error",
  "provider_id": "my_provider",
  "response": {
    "message_id": "m-1",
    "message_type": "chat",
    "payload": "Provider response",
    "metadata": {}
  },
  "metadata": {
    "provider_latency_ms": 150
  }
}
```

**Example**: OpenAI integration, Anthropic integration, custom LLM

---

## Extension Template Structure

```
my_extension/
├── src/
│   └── my_extension.js          # Main handler
├── test/
│   ├── my_extension.test.js     # Unit tests
│   └── my_extension.e2e.js      # E2E tests
├── package.json                 # Node.js dependencies
├── my_extension.registry.json   # Registry configuration
├── README.md                    # Extension documentation
└── .gitignore                   # Git ignore rules
```

---

## Implementation Requirements

### 1. Structured Logging

**Format**: JSON logs with required fields

```javascript
console.log(JSON.stringify({
    timestamp: new Date().toISOString(),
    level: 'INFO',
    component: 'my_extension',
    message: 'Processing request',
    trace_id: request.trace_id,
    tenant_id: request.tenant_id,
    latency_ms: latencyMs
}));
```

**Required Fields**:
- `timestamp`: ISO 8601 format (UTC)
- `level`: ERROR, WARN, INFO, DEBUG
- `component`: Extension ID
- `message`: Human-readable message

**Optional Fields**:
- `trace_id`: Trace identifier
- `tenant_id`: Tenant identifier
- `latency_ms`: Processing latency
- `context`: Additional context

**PII Filtering**: Never log sensitive data (passwords, API keys, credit cards, etc.)

### 2. Timeout Handling

**Requirement**: Process requests within configured timeout

**Implementation**:
```javascript
const TIMEOUT_MS = parseInt(process.env.EXTENSION_TIMEOUT_MS || '1000', 10);

// In message handler:
const timeoutPromise = new Promise((_, reject) => {
    setTimeout(() => reject(new Error('Request timeout')), TIMEOUT_MS);
});

const response = await Promise.race([
    processRequest(request),
    timeoutPromise
]);
```

### 3. Error Handling

**Requirement**: Return structured error responses

**Pre/Post Processors**:
```javascript
catch (error) {
    return {
        error: {
            code: 'PROCESSING_ERROR',
            message: error.message
        }
    };
}
```

**Validators**:
```javascript
catch (error) {
    return {
        status: 'reject',
        reason: 'processing_error',
        details: {
            error: error.message
        }
    };
}
```

**Providers**:
```javascript
catch (error) {
    return {
        status: 'error',
        error: {
            code: 'PROCESSING_ERROR',
            message: error.message
        }
    };
}
```

### 4. Graceful Shutdown

**Requirement**: Handle SIGINT/SIGTERM signals

**Implementation**:
```javascript
const shutdown = async (signal) => {
    console.log(JSON.stringify({
        timestamp: new Date().toISOString(),
        level: 'INFO',
        component: 'my_extension',
        message: 'Shutting down',
        signal: signal
    }));
    await nc.close();
    process.exit(0);
};

process.on('SIGINT', () => shutdown('SIGINT'));
process.on('SIGTERM', () => shutdown('SIGTERM'));
```

---

## Testing

### Unit Tests

**Location**: `test/my_extension.test.js`

**Purpose**: Test extension logic without NATS

**Example**:
```javascript
import { strict as assert } from 'assert';

async function testProcessRequest() {
    const request = {
        trace_id: 'test-trace',
        tenant_id: 'test-tenant',
        payload: {
            payload: 'test message'
        }
    };
    
    // Import and test your processRequest function
    // const response = await processRequest(request);
    // assert(response, 'Response should not be null');
}
```

**Run**:
```bash
npm test
```

### E2E Tests

**Location**: `test/my_extension.e2e.js`

**Purpose**: Test extension against real NATS and Router

**Prerequisites**:
- NATS server running
- Extension registered in Router registry
- Router running (optional)

**Example**:
```javascript
import { connect, StringCodec } from 'nats';

async function testBasicFunctionality(nc) {
    const request = {
        trace_id: 'e2e-test',
        tenant_id: 'e2e-tenant',
        payload: {
            payload: 'test message'
        }
    };
    
    const response = await nc.request(SUBJECT, sc.encode(JSON.stringify(request)), {
        timeout: 5000
    });
    
    // Assert response
}
```

**Run**:
```bash
npm run test:e2e
```

**Or use script**:
```bash
./scripts/run_extension_e2e.sh my_extension
```

---

## Local Development Workflow

### 1. Start NATS

```bash
# Using Docker Compose
docker-compose up -d nats

# Or using NATS server directly
nats-server
```

### 2. Run Extension

```bash
# Option 1: Using npm
cd tools/extensions/my_extension
npm start

# Option 2: Using script
./scripts/run_extension_local.sh my_extension
```

### 3. Test Extension

**Manual Test** (using NATS CLI):
```bash
# Send test request
echo '{"trace_id":"test","tenant_id":"test","payload":{"payload":"hello"}}' | \
  nats request beamline.ext.pre.my_extension.v1
```

**Automated Test**:
```bash
npm run test:e2e
```

### 4. Monitor Logs

Extension logs to stdout in JSON format:
```json
{
  "timestamp": "2025-01-27T12:00:00Z",
  "level": "INFO",
  "component": "my_extension",
  "message": "Processing request",
  "trace_id": "uuid",
  "tenant_id": "tenant-123"
}
```

---

## Registry Registration

### Step 1: Create Registry Config

Registry config is auto-generated in `my_extension.registry.json`:

```json
{
  "id": "my_extension",
  "type": "pre",
  "subject": "beamline.ext.pre.my_extension.v1",
  "timeout_ms": 1000,
  "retry": 0,
  "enabled": true,
  "config": {},
  "metadata": {
    "description": "My pre-processor extension",
    "version": "v1"
  }
}
```

### Step 2: Copy to Router Fixtures

```bash
cp tools/extensions/my_extension/my_extension.registry.json \
   apps/otp/router/priv/fixtures/extensions/
```

### Step 3: Reload Router Registry

Router automatically loads fixtures on startup. To reload:

```erlang
% In Erlang shell or via admin API
router_extension_registry:reload().
```

### Step 4: Add to Routing Policy

Edit routing policy to include your extension:

```json
{
  "policy_id": "my_policy",
  "pre": [
    {
      "id": "my_extension",
      "mode": "required",
      "config": {}
    }
  ]
}
```

---

## PR Flow

### 1. Create Extension

```bash
# Generate extension
./scripts/generate_extension.sh my_extension pre \
  --description "My extension" \
  --author "Your Name"
```

### 2. Implement Logic

- Edit `src/my_extension.js`
- Add unit tests in `test/my_extension.test.js`
- Add E2E tests in `test/my_extension.e2e.js`

### 3. Test Locally

```bash
# Start NATS
docker-compose up -d nats

# Run extension
cd tools/extensions/my_extension
npm start

# Test manually or run E2E tests
npm run test:e2e
```

### 4. Create PR

**Required Files**:
- Extension code: `tools/extensions/my_extension/`
- Registry config: `apps/otp/router/priv/fixtures/extensions/my_extension.json`
- Documentation: Update `docs/EXTENSIONS_API.md` if needed

**PR Checklist**:
- [ ] Extension implements contract from `docs/EXTENSIONS_API.md`
- [ ] Structured JSON logging implemented
- [ ] Timeout handling implemented
- [ ] Error handling implemented
- [ ] Graceful shutdown implemented
- [ ] Unit tests pass
- [ ] E2E tests pass
- [ ] Registry config added
- [ ] README.md updated
- [ ] No PII in logs/code

### 5. Code Review

**Reviewers check**:
- Contract compliance
- Error handling
- Logging format
- Test coverage
- Security (no secrets, PII filtering)

### 6. Merge and Deploy

After merge:
1. Extension code available in `tools/extensions/my_extension/`
2. Registry config loaded by Router
3. Can be used in routing policies

---

## Common Patterns

### Pattern 1: Text Processing

```javascript
async function processRequest(request) {
    const payload = request.payload || {};
    const text = payload.payload || '';
    
    // Process text
    const processed = text
        .toLowerCase()
        .trim()
        .replace(/\s+/g, ' ');
    
    return {
        payload: {
            ...payload,
            payload: processed
        }
    };
}
```

### Pattern 2: Validation

```javascript
async function processRequest(request) {
    const payload = request.payload || {};
    const text = payload.payload || '';
    
    // Validate
    const isValid = text.length > 0 && text.length < 1000;
    
    if (!isValid) {
        return {
            status: 'reject',
            reason: 'validation_failed',
            details: {
                field: 'payload',
                message: 'Text length must be between 1 and 1000 characters'
            }
        };
    }
    
    return { status: 'ok' };
}
```

### Pattern 3: External API Call

```javascript
async function processRequest(request) {
    const payload = request.payload || {};
    
    try {
        // Call external API
        const response = await fetch('https://api.example.com/process', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(payload)
        });
        
        const result = await response.json();
        
        return {
            payload: {
                ...payload,
                payload: result.processed
            }
        };
    } catch (error) {
        throw new Error(`External API call failed: ${error.message}`);
    }
}
```

### Pattern 4: Configuration from Policy

```javascript
async function processRequest(request) {
    // Config is passed in request (from policy)
    const config = request.config || {};
    const threshold = config.threshold || 0.5;
    
    // Use config in processing
    // ...
}
```

---

## Troubleshooting

### Extension Not Receiving Requests

**Check**:
1. NATS connection: `echo '{}' | nats request beamline.ext.pre.my_extension.v1`
2. Registry config: Extension registered in Router fixtures
3. Router logs: Check if Router is calling extension
4. Subject name: Match between registry and extension code

### Timeout Errors

**Solutions**:
1. Increase timeout in registry config
2. Optimize extension logic
3. Check external API latency
4. Add timeout handling in code

### Error Responses

**Check**:
1. Error format matches contract
2. Error handling in extension code
3. Router error handling (fail-open/fail-closed)

### Logging Issues

**Check**:
1. JSON format: Valid JSON structure
2. Required fields: timestamp, level, component, message
3. PII filtering: No sensitive data in logs

---

## Best Practices

### 1. Keep It Simple

- Focus on single responsibility
- Avoid complex dependencies
- Use standard libraries

### 2. Handle Errors Gracefully

- Always return structured error responses
- Log errors with context
- Don't crash on invalid input

### 3. Test Thoroughly

- Unit tests for logic
- E2E tests for integration
- Test error scenarios

### 4. Document Clearly

- Update README.md
- Add code comments
- Document configuration options

### 5. Follow Contracts

- Implement exact contract from `docs/EXTENSIONS_API.md`
- Match request/response formats
- Handle all required fields

---

## Examples

### Reference Extensions

See `tools/extensions/src/` for complete examples:

- `normalize_text.js` - Pre-processor example
- `pii_guard.js` - Validator example
- `mask_pii.js` - Post-processor example
- `test_provider.js` - Custom provider example

### Generated Extensions

Use `./scripts/generate_extension.sh` to create new extensions with boilerplate.

---

## Advanced Topics

### Multi-Version Support

See `docs/ARCHITECTURE/EXTENSION_ROUTING_STRATEGY.md` for:
- Version routing based on tenant/environment
- Canary deployments
- Blue-green rollouts

### Load Balancing

See `docs/archive/dev/EXTENSION_ADVANCED_FEATURES_REPORT.md` for:
- Multiple instances per extension
- Weighted round-robin
- Health-based routing

### Circuit Breaker

See `docs/archive/dev/EXTENSION_ADVANCED_FEATURES_REPORT.md` for:
- Circuit breaker integration
- Error rate thresholds
- Automatic recovery

---

## References

- `docs/EXTENSIONS_API.md` - Complete API contract
- `docs/ARCHITECTURE/EXTENSION_ROUTING_STRATEGY.md` - Routing strategy
- `tools/extensions/` - Reference implementations
- `scripts/generate_extension.sh` - Extension generator
- `scripts/run_extension_local.sh` - Local runner
- `scripts/run_extension_e2e.sh` - E2E test runner

---

## Change History

**v1.0 (2025-01-27)**:
- Initial version
- Quick start guide
- Implementation requirements
- Testing guide
- PR flow
- Common patterns

