import { validate } from 'class-validator';
import { plainToInstance } from 'class-transformer';
import { MessageDto } from './message.dto';

describe('MessageDto', () => {
  describe('validation', () => {
    it('should pass validation with valid data', async () => {
      const dto = plainToInstance(MessageDto, {
        message_id: 'msg_123',
        tenant_id: 'tenant_abc',
        message_type: 'chat',
        payload: '{"text": "Hello"}',
      });

      const errors = await validate(dto);
      expect(errors.length).toBe(0);
    });

    it('should fail validation when message_id is missing', async () => {
      const dto = plainToInstance(MessageDto, {
        tenant_id: 'tenant_abc',
        message_type: 'chat',
        payload: '{"text": "Hello"}',
      });

      const errors = await validate(dto);
      expect(errors.length).toBeGreaterThan(0);
      expect(errors[0].property).toBe('message_id');
    });

    it('should fail validation when tenant_id is missing', async () => {
      const dto = plainToInstance(MessageDto, {
        message_id: 'msg_123',
        message_type: 'chat',
        payload: '{"text": "Hello"}',
      });

      const errors = await validate(dto);
      expect(errors.length).toBeGreaterThan(0);
      expect(errors[0].property).toBe('tenant_id');
    });

    it('should fail validation when message_type is missing', async () => {
      const dto = plainToInstance(MessageDto, {
        message_id: 'msg_123',
        tenant_id: 'tenant_abc',
        payload: '{"text": "Hello"}',
      });

      const errors = await validate(dto);
      expect(errors.length).toBeGreaterThan(0);
      expect(errors[0].property).toBe('message_type');
    });

    it('should fail validation when payload is missing', async () => {
      const dto = plainToInstance(MessageDto, {
        message_id: 'msg_123',
        tenant_id: 'tenant_abc',
        message_type: 'chat',
      });

      const errors = await validate(dto);
      expect(errors.length).toBeGreaterThan(0);
      expect(errors[0].property).toBe('payload');
    });

    it('should pass validation with optional fields', async () => {
      const dto = plainToInstance(MessageDto, {
        message_id: 'msg_123',
        tenant_id: 'tenant_abc',
        message_type: 'chat',
        payload: '{"text": "Hello"}',
        trace_id: 'trace_123',
        metadata: { key: 'value' },
        timestamp_ms: 1704067200000,
      });

      const errors = await validate(dto);
      expect(errors.length).toBe(0);
    });

    it('should fail validation when timestamp_ms is not a number', async () => {
      const dto = plainToInstance(MessageDto, {
        message_id: 'msg_123',
        tenant_id: 'tenant_abc',
        message_type: 'chat',
        payload: '{"text": "Hello"}',
        timestamp_ms: 'invalid' as unknown as number,
      });

      const errors = await validate(dto);
      expect(errors.length).toBeGreaterThan(0);
    });
  });

  describe('happy path', () => {
    it('should create valid DTO with all required fields', () => {
      const dto = plainToInstance(MessageDto, {
        message_id: 'msg_123',
        tenant_id: 'tenant_abc',
        message_type: 'chat',
        payload: '{"text": "Hello"}',
      });

      expect(dto.message_id).toBe('msg_123');
      expect(dto.tenant_id).toBe('tenant_abc');
      expect(dto.message_type).toBe('chat');
      expect(dto.payload).toBe('{"text": "Hello"}');
    });

    it('should create valid DTO with all optional fields', () => {
      const dto = plainToInstance(MessageDto, {
        message_id: 'msg_123',
        tenant_id: 'tenant_abc',
        trace_id: 'trace_123',
        message_type: 'completion',
        payload: '{"prompt": "Hello"}',
        metadata: { source: 'api', version: '1.0' },
        timestamp_ms: 1704067200000,
      });

      expect(dto.trace_id).toBe('trace_123');
      expect(dto.metadata).toEqual({ source: 'api', version: '1.0' });
      expect(dto.timestamp_ms).toBe(1704067200000);
    });
  });
});
