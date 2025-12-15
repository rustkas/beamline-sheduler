jest.setTimeout(20000);
process.env.OTLP_ENDPOINT = process.env.OTLP_ENDPOINT || 'http://127.0.0.1:4318';
process.env.NATS_URL = process.env.NATS_URL || 'nats://localhost:4222';
process.env.NATS_MAX_DELIVER = process.env.NATS_MAX_DELIVER || '5';
process.env.NATS_ACK_WAIT_MS = process.env.NATS_ACK_WAIT_MS || '1000';
process.env.NATS_BACKOFF_MS = process.env.NATS_BACKOFF_MS || '100';