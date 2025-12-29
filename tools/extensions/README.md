# BeamLine Extensions (Reference Implementation)

Reference extensions for BeamLine Router E2E testing.

## Extensions

1. **normalize_text** (pre-processor)
   - Subject: `beamline.ext.pre.normalize_text.v1`
   - Normalizes text: lowercase, trim, remove extra spaces

2. **pii_guard** (validator)
   - Subject: `beamline.ext.validate.pii_guard.v1`
   - Validates that message doesn't contain PII

3. **mask_pii** (post-processor)
   - Subject: `beamline.ext.post.mask_pii.v1`
   - Masks PII in response text

4. **test_provider** (custom provider)
   - Subject: `beamline.provider.test_provider.v1`
   - Simple test provider that returns mock responses

## Installation

```bash
cd tools/extensions
npm install
```

## Running Extensions

### Run all extensions

```bash
npm run start:all
```

### Run individual extension

```bash
npm run start:normalize_text
npm run start:pii_guard
npm run start:mask_pii
npm run start:test_provider
```

## Environment Variables

- `NATS_URL` - NATS server URL (default: `nats://localhost:4222`)

## Usage

Extensions subscribe to their NATS subjects and respond to requests from Router.

See `docs/EXTENSIONS_API.md` for contract details.

