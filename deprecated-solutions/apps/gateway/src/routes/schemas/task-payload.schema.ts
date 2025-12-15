import Ajv from 'ajv';

const ajv = new Ajv({ allErrors: true, strict: false });

const chatSchema = {
  $id: 'beamline.router.task.chat',
  type: 'object',
  required: ['text'],
  properties: {
    text: { type: 'string', minLength: 1 },
    role: { type: 'string', enum: ['user', 'system', 'assistant'], default: 'user' },
    metadata: { type: 'object', additionalProperties: true },
  },
  additionalProperties: true,
} as const;

const completionSchema = {
  $id: 'beamline.router.task.completion',
  type: 'object',
  required: ['prompt'],
  properties: {
    prompt: { type: 'string', minLength: 1 },
    max_tokens: { type: 'number', minimum: 1 },
    temperature: { type: 'number', minimum: 0 },
  },
  additionalProperties: true,
} as const;

const embeddingSchema = {
  $id: 'beamline.router.task.embedding',
  type: 'object',
  required: ['input'],
  properties: {
    input: {
      anyOf: [
        { type: 'string', minLength: 1 },
        { type: 'array', items: { type: 'string' }, minItems: 1 },
      ],
    },
    metadata: { type: 'object', additionalProperties: true },
  },
  additionalProperties: true,
} as const;

const validators: Record<string, ReturnType<typeof ajv.compile>> = {
  chat: ajv.compile(chatSchema),
  completion: ajv.compile(completionSchema),
  embedding: ajv.compile(embeddingSchema),
};

export function validateTaskPayload(
  type: string,
  payload: Record<string, unknown>,
): { valid: boolean; errors?: string[] } | undefined {
  const validator = validators[type];
  if (!validator) return undefined;
  const valid = validator(payload) as boolean;
  if (valid) return { valid: true };
  const errors = (validator.errors || []).map((e) =>
    `${e.instancePath || ''} ${e.message || ''}`.trim(),
  );
  return { valid: false, errors };
}
