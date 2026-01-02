# Extensions Quickstart

**Version**: 1.0  
**Date**: 2025-01-27  
**Status**: ✅ Approved  
**Target Audience**: New developers setting up Extensions ecosystem for the first time

---

## Purpose

This guide provides a **minimal happy-path** for setting up the complete Extensions ecosystem in a new environment. It combines all necessary steps into a single, streamlined workflow.

**Time to Complete**: 15-30 minutes (depending on Docker image downloads)

---

## Prerequisites

Before starting, ensure you have:

- **Docker** and **Docker Compose** (for NATS and extensions)
- **Node.js** 20+ (for reference extensions, if not using Docker)
- **Erlang/OTP** 25+ (for Router, if running locally)
- **PostgreSQL** (optional, for Extension Registry database mode; fixtures mode works without it)

---

## Quick Start (Automated)

The fastest way to get started is using the automated quickstart script:

```bash
# Run the quickstart script
./scripts/extensions_quickstart.sh

# Or with Docker Compose for all services
./scripts/extensions_quickstart.sh --use-docker

# Skip tests (setup only)
./scripts/extensions_quickstart.sh --skip-tests
```

The script will:
1. ✅ Start NATS server
2. ✅ Deploy reference extensions (normalize_text, pii_guard, mask_pii, test_provider)
3. ✅ Verify extension fixtures are in place
4. ✅ Guide you through starting Router + Gateway
5. ✅ Run 1-2 key E2E tests to verify everything works

---

## Manual Setup (Step-by-Step)

If you prefer to set up manually or understand each step:

### Step 1: Start NATS Server

**Option A: Using Docker (Recommended)**

```bash
# Start NATS with JetStream
docker run -d --name nats \
  -p 4222:4222 \
  -p 8222:8222 \
  nats:2.10-alpine \
  -js -m 8222

# Verify NATS is running
curl http://localhost:8222/healthz
```

**Option B: Using Docker Compose**

```bash
# Start NATS from project docker-compose
docker-compose up -d nats

# Verify
curl http://localhost:8222/healthz
```

**Option C: Local NATS Installation**

```bash
# Install NATS server (if not installed)
# macOS: brew install nats-server
# Linux: Download from https://github.com/nats-io/nats-server/releases

# Start NATS
nats-server -js -m 8222
```

**Verify**: NATS should be accessible at `nats://localhost:4222`

---

### Step 2: Deploy Reference Extensions

Reference extensions are located in `tools/extensions/`:

- **normalize_text** (pre-processor) - Text normalization
- **pii_guard** (validator) - PII detection
- **mask_pii** (post-processor) - PII masking
- **test_provider** (custom provider) - Mock provider

**Option A: Using Docker Compose (Recommended)**

```bash
# Start all extensions via Docker Compose
docker-compose -f docker-compose.yml -f tools/extensions/docker-compose.extensions.yml up -d

# Check extension logs
docker logs extension-normalize-text
docker logs extension-pii-guard
docker logs extension-mask-pii
docker logs extension-test-provider
```

**Option B: Manual Start**

```bash
cd tools/extensions

# Install dependencies (first time only)
npm install

# Start all extensions
./start_extensions.sh

# Or start individually
NATS_URL=nats://localhost:4222 node src/normalize_text.js &
NATS_URL=nats://localhost:4222 node src/pii_guard.js &
NATS_URL=nats://localhost:4222 node src/mask_pii.js &
NATS_URL=nats://localhost:4222 node src/test_provider.js &
```

**Verify**: Extensions should be listening on their NATS subjects:
- `beamline.ext.pre.normalize_text.v1`
- `beamline.ext.validate.pii_guard.v1`
- `beamline.ext.post.mask_pii.v1`
- `beamline.provider.test_provider.v1`

Test with:
```bash
nats req "beamline.ext.pre.normalize_text.v1" '{"payload":{"payload":"Hello World"}}'
```

---

### Step 3: Enable Extension Fixtures

Extension fixtures register extensions in the Router's Extension Registry.

**Location**: `apps/otp/router/priv/fixtures/extensions/`

**Required Fixtures**:
- `normalize_text.json` - Pre-processor
- `pii_guard.json` - Validator
- `mask_pii.json` - Post-processor
- `test_provider.json` - Custom provider

**Verify Fixtures Exist**:

```bash
ls apps/otp/router/priv/fixtures/extensions/
# Should show:
# - normalize_text.json
# - pii_guard.json
# - mask_pii.json
# - test_provider.json
```

**Fixture Format** (example: `normalize_text.json`):

```json
{
  "id": "normalize_text",
  "type": "pre",
  "subject": "beamline.ext.pre.normalize_text.v1",
  "timeout_ms": 100,
  "retry": 0,
  "enabled": true,
  "config": {},
  "metadata": {
    "description": "Text normalization pre-processor",
    "version": "v1"
  }
}
```

**Note**: Router automatically loads fixtures on startup. No manual action needed if fixtures are in place.

**Alternative: Database Mode**

If using PostgreSQL (database mode), extensions are registered in the database instead of fixtures. See [Extension Registry Runbook](#extension-registry-runbook) for database setup.

---

### Step 4: Start Router + Gateway

**Router**:

```bash
cd apps/otp/router

# Option A: Using Rebar3 shell (development)
rebar3 shell

# Option B: Using Docker Compose
cd ../..
docker-compose up -d router

# Verify Router is running
curl http://localhost:9000/health
```

**Gateway**:

```bash
# Using Docker Compose (recommended)
docker compose up -d c-gateway

# Verify C-Gateway is running
curl http://localhost:8081/_health
```

**Verify Integration**:

```bash
# Check Router can see extensions
# In Erlang shell (if using rebar3 shell):
router_extension_registry:list().

# Should show all 4 extensions
```

---

### Step 5: Run Key Tests

Run 1-2 key E2E tests to verify the complete pipeline works:

```bash
cd apps/otp/router

# Set NATS URL
export NATS_URL=nats://localhost:4222

# Run full pipeline test
rebar3 ct --suite test/router_extensions_e2e_SUITE --case test_e2e_full_pipeline

# Run multiple extensions test
rebar3 ct --suite test/router_extensions_e2e_SUITE --case test_e2e_multiple_extensions
```

**Expected Results**:
- ✅ All tests pass
- ✅ Extensions are invoked correctly
- ✅ Pipeline executes: pre → validator → provider → post
- ✅ Responses are processed correctly

---

## Verification Checklist

After completing all steps, verify:

- [ ] NATS is running and accessible (`curl http://localhost:8222/healthz`)
- [ ] All 4 extensions are running (check logs or `docker ps`)
- [ ] Extension fixtures exist in `apps/otp/router/priv/fixtures/extensions/`
- [ ] Router is running and can see extensions (`router_extension_registry:list().`)
- [ ] C-Gateway is running (`curl http://localhost:8081/_health`)
- [ ] E2E tests pass (`test_e2e_full_pipeline`, `test_e2e_multiple_extensions`)

---

## Troubleshooting

### NATS Connection Issues

```bash
# Check NATS is running
curl http://localhost:8222/healthz

# Check NATS subjects
nats stream ls
nats sub "beamline.ext.>"
```

### Extension Not Responding

```bash
# Check extension logs
docker logs extension-normalize-text
# OR
tail -f /tmp/normalize_text.log

# Test extension directly
nats req "beamline.ext.pre.normalize_text.v1" '{"payload":{"payload":"test"}}'
```

### Router Not Finding Extensions

```bash
# In Erlang shell
router_extension_registry:list().

# Reload extensions
router_extension_registry:reload().

# Check fixtures
ls apps/otp/router/priv/fixtures/extensions/
```

### Test Failures

1. **Extension not found**: Ensure extensions are started and NATS is running
2. **Timeout errors**: Check extension response time (should be < timeout_ms in fixture)
3. **Invalid response format**: Check extension response matches contract from `docs/EXTENSIONS_API.md`

---

## Next Steps

Now that you have a working Extensions setup, explore:

### For Extension Developers

- **[Extensions Developer Guide](EXTENSIONS_DEVELOPER_GUIDE.md)**: Learn how to create your own extensions
  - Generate extension boilerplate
  - Implement extension logic
  - Test locally and E2E
  - Register in Router

### For Operators/SRE

- **[Extensions Runbook](../apps/otp/router/docs/EXTENSIONS_RUNBOOK.md)**: Operational guide for Extensions
  - Health checks and monitoring
  - Typical alerts and interpretation
  - Hot-reload and mode switching
  - Rollout/rollback procedures

### For Architects

- **[Extension Routing Strategy](ARCHITECTURE/EXTENSION_ROUTING_STRATEGY.md)**: Multi-tenant and multi-environment routing
  - Tenant/environment-based routing
  - Canary deployments
  - Version management

### For Security

- **[Extensions Security Guide](../apps/otp/router/docs/archive/status_reports/EXTENSIONS_SECURITY_GUIDE.md)**: Security and hardening
  - Extension authorization (RBAC)
  - Payload validation
  - Abuse prevention
  - Secure deployment checklist

---

## Detailed Documentation

### Core Documentation

- **[Extensions API](EXTENSIONS_API.md)**: Complete API specification for extensions
  - Request/response contracts
  - Extension types (pre, validator, post, provider)
  - Error handling
  - NATS subject conventions

- **[E2E Testing Guide](../apps/otp/router/docs/EXTENSIONS_E2E_GUIDE.md)**: Comprehensive E2E testing guide
  - Full test suite overview
  - Troubleshooting
  - Development workflow
  - Docker Compose integration

### Implementation Reports

- **[Extension Template Implementation](archive/dev/EXTENSION_TEMPLATE_IMPLEMENTATION_REPORT.md)**: Extension template system
- **[Extension Registry Implementation](archive/dev/EXTENSION_REGISTRY_IMPLEMENTATION_REPORT.md)**: Registry architecture
- **[E2E Implementation](archive/dev/EXTENSIONS_E2E_IMPLEMENTATION_REPORT.md)**: E2E testing implementation

---

## Quick Reference

### Common Commands

```bash
# Start everything (NATS + Extensions)
docker-compose -f docker-compose.yml -f tools/extensions/docker-compose.extensions.yml up -d

# Check extension logs
docker logs extension-normalize-text -f

# Test extension directly
nats req "beamline.ext.pre.normalize_text.v1" '{"payload":{"payload":"test"}}'

# Run E2E tests
cd apps/otp/router
export NATS_URL=nats://localhost:4222
rebar3 ct --suite test/router_extensions_e2e_SUITE --case test_e2e_full_pipeline

# Reload extension registry (in Erlang shell)
router_extension_registry:reload().
```

### File Locations

- **Extensions**: `tools/extensions/`
- **Extension Fixtures**: `apps/otp/router/priv/fixtures/extensions/`
- **E2E Tests**: `apps/otp/router/test/router_extensions_e2e_SUITE.erl`
- **Docker Compose**: `docker-compose.yml`, `tools/extensions/docker-compose.extensions.yml`
- **Quickstart Script**: `scripts/extensions_quickstart.sh`

---

## Support

If you encounter issues:

1. Check [Troubleshooting](#troubleshooting) section above
2. Review detailed guides:
   - [Extensions Developer Guide](EXTENSIONS_DEVELOPER_GUIDE.md)
   - [Extensions Runbook](../apps/otp/router/docs/EXTENSIONS_RUNBOOK.md)
   - [E2E Testing Guide](../apps/otp/router/docs/EXTENSIONS_E2E_GUIDE.md)
3. Check implementation reports in `docs/archive/dev/EXTENSIONS_*_REPORT.md`

---

**WORKER**: `wrk-3` (Extensions/DX) + `wrk-4` (Docs)  
**Status**: ✅ **COMPLETED**  
**Last Updated**: 2025-01-27

