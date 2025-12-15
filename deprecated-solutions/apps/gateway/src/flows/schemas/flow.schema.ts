// Minimal Flow DSL JSON Schema v0 for local validation
export const flowSchema = {
  $id: 'https://beamline.ai/schemas/flow-definition.json',
  $schema: 'http://json-schema.org/draft-07/schema#',
  type: 'object',
  required: ['id', 'version', 'steps', 'edges'],
  properties: {
    id: { type: 'string', minLength: 1 },
    version: { type: 'string', minLength: 1 },
    tenant_id: { type: 'string' },
    vars: { type: 'object', additionalProperties: true },
    secrets_refs: { type: 'array', items: { type: 'string' } },
    policies: { type: 'object', additionalProperties: true },
    steps: {
      type: 'array',
      minItems: 1,
      items: {
        type: 'object',
        required: ['id', 'type'],
        properties: {
          id: { type: 'string', minLength: 1 },
          type: {
            type: 'string',
            minLength: 1,
            enum: ['http.request', 'fs.blob_put', 'fs.blob_get', 'sql.query'],
          },
          inputs: { type: 'object', additionalProperties: true },
          outputs: { type: 'object', additionalProperties: true },
          retry: {
            type: 'object',
            properties: {
              attempts: { type: 'integer', minimum: 0 },
              backoff_ms: { type: 'integer', minimum: 0 },
            },
            additionalProperties: false,
          },
          timeout_ms: { type: 'integer', minimum: 0 },
          resources: {
            type: 'object',
            properties: {
              cpu: { type: 'number', minimum: 0 },
              memory_mb: { type: 'number', minimum: 0 },
              gpu: { type: 'number', minimum: 0 },
            },
            additionalProperties: false,
          },
          guardrails: { type: 'object', additionalProperties: true },
        },
        additionalProperties: false,
      },
    },
    edges: {
      type: 'array',
      items: {
        type: 'object',
        required: ['from', 'to'],
        properties: {
          from: { type: 'string', minLength: 1 },
          to: { type: 'string', minLength: 1 },
          condition: { type: 'string' },
        },
        additionalProperties: false,
      },
    },
  },
  additionalProperties: false,
} as const;
