# Beamline Gateway

NestJS Gateway - REST/SSE API for message routing.

## Quick Start

### Prerequisites

- Node.js 20+
- npm or pnpm

### Installation

```bash
npm install
# or
pnpm install
```

### Development

```bash
# Start in watch mode
npm run start:dev

# Start in debug mode
npm run start:debug
```

## Commands

### Lint

```bash
# Check linting (CI mode - no auto-fix)
npm run lint:check

# Fix linting issues
npm run lint
```

### Build

```bash
# Build for production
npm run build
```

### Test

```bash
# Run unit tests
npm run test

# Run tests in watch mode
npm run test:watch

# Run tests with coverage
npm run test:cov

# Run E2E tests
npm run test:e2e
```

### Validation Sequence

Run all checks in sequence (lint → build → test):

```bash
# Full validation
npm run validate

# Or use CI script
npm run ci
```

## Architecture

### Dependencies

Gateway is **independent** from Router and NATS:
- Uses **mocked Router client** in tests
- No runtime dependencies on Router/NATS
- Stub implementations for observability (Prometheus, Pino)

### Modules

- **Routes**: Message routing decisions (stub implementation)
- **Messages**: Message handling and processing
- **Policies**: Routing policies (stub implementation)
- **Health**: Health check endpoints
- **Observability**: Metrics and logging (stub implementation)
- **OpenAI**: OpenAI API compatibility layer

### Testing

Tests use **mocked Router client** (`RouterClientService`):
- No actual Router connection required
- Tests are stable and fast
- Can run without external dependencies

## CI/CD Integration

Gateway checks run in sequence:
1. **Lint**: `npm run lint:check`
2. **Build**: `npm run build`
3. **Test**: `npm run test`

All steps must pass for CI to succeed.

## Local Development

### Running Locally

```bash
# Install dependencies
npm install

# Run validation sequence
npm run validate

# Start development server
npm run start:dev
```

### Troubleshooting

**Build fails with missing dependencies:**
- Gateway uses stub implementations for observability
- No external dependencies required for build/test
- If you see errors about `@willsoto/nestjs-prometheus` or `nestjs-pino`, check that observability module is properly stubbed

**Tests fail:**
- Ensure all dependencies are installed: `npm install`
- Check that Router client is properly mocked in tests
- Run tests individually: `npm run test -- routes.service.spec.ts`

**Lint errors:**
- Auto-fix: `npm run lint`
- Check specific files: `npm run lint:check -- src/path/to/file.ts`

## OpenAI API Compatibility

Gateway provides OpenAI API-compatible endpoints for seamless integration with existing OpenAI clients and libraries.

### Available Endpoints

- `POST /v1/chat/completions` - Chat completions (with streaming support)
- `POST /v1/completions` - Text completions
- `POST /v1/embeddings` - Embeddings
- `GET /v1/models` - List available models

### Authentication

All endpoints support standard OpenAI authentication:
- `Authorization: Bearer <token>` header
- `X-Tenant-ID` header for tenant identification

### Examples

#### Chat Completions (Non-streaming)

```bash
curl -X POST http://localhost:8080/v1/chat/completions \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer YOUR_KEY" \
  -d '{
    "model": "gpt-3.5-turbo",
    "messages": [
      {"role": "user", "content": "Hello!"}
    ],
    "temperature": 0.7,
    "max_tokens": 100
  }'
```

#### Chat Completions (Streaming)

```bash
curl -X POST http://localhost:8080/v1/chat/completions \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer YOUR_KEY" \
  -d '{
    "model": "gpt-3.5-turbo",
    "messages": [
      {"role": "user", "content": "Hello!"}
    ],
    "stream": true
  }'
```

#### Text Completions

```bash
curl -X POST http://localhost:8080/v1/completions \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer YOUR_KEY" \
  -d '{
    "model": "text-davinci-003",
    "prompt": "Say this is a test",
    "max_tokens": 50,
    "temperature": 0.7
  }'
```

#### Embeddings

```bash
curl -X POST http://localhost:8080/v1/embeddings \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer YOUR_KEY" \
  -d '{
    "model": "text-embedding-ada-002",
    "input": "The food was delicious and the waiter..."
  }'
```

#### List Models

```bash
curl -X GET http://localhost:8080/v1/models \
  -H "Authorization: Bearer YOUR_KEY"
```

### Using OpenAI SDK

The Gateway is compatible with OpenAI SDK clients. Example with Node.js:

```javascript
import OpenAI from 'openai';

const openai = new OpenAI({
  apiKey: 'YOUR_KEY',
  baseURL: 'http://localhost:8080/v1', // Gateway base URL
});

// Chat completions
const completion = await openai.chat.completions.create({
  model: 'gpt-3.5-turbo',
  messages: [{ role: 'user', content: 'Hello!' }],
});

console.log(completion.choices[0].message.content);

// Streaming
const stream = await openai.chat.completions.create({
  model: 'gpt-3.5-turbo',
  messages: [{ role: 'user', content: 'Hello!' }],
  stream: true,
});

for await (const chunk of stream) {
  process.stdout.write(chunk.choices[0]?.delta?.content || '');
}
```

### Python Example

```python
from openai import OpenAI

client = OpenAI(
    api_key="YOUR_KEY",
    base_url="http://localhost:8080/v1"
)

# Chat completions
completion = client.chat.completions.create(
    model="gpt-3.5-turbo",
    messages=[
        {"role": "user", "content": "Hello!"}
    ]
)

print(completion.choices[0].message.content)

# Streaming
stream = client.chat.completions.create(
    model="gpt-3.5-turbo",
    messages=[
        {"role": "user", "content": "Hello!"}
    ],
    stream=True
)

for chunk in stream:
    if chunk.choices[0].delta.content:
        print(chunk.choices[0].delta.content, end="")
```

### Response Format

All endpoints return responses in OpenAI-compatible format:

- **Chat Completions**: `{ id, object, created, model, choices, usage }`
- **Text Completions**: `{ id, object, created, model, choices, usage }`
- **Embeddings**: `{ object, data, model, usage }`
- **Models**: `{ object, data }`

### Error Handling

Errors are returned in OpenAI format:

```json
{
  "error": {
    "message": "Error description",
    "type": "api_error",
    "param": null,
    "code": "internal_error"
  }
}
```

### Swagger Documentation

Full API documentation is available at:
- Swagger UI: `http://localhost:8080/api` (when running in development)

## See Also

- `scripts/run_checks.sh` - Local validation script
- `.github/workflows/validate.yml.template` - CI workflow
- `docs/LOCAL_CHECKS.md` - Local checks guide

## Maintainers

- Beamline Development: https://github.com/BeamLine-Development
