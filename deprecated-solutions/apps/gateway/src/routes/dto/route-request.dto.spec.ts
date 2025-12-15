import { validate } from 'class-validator';
import { plainToInstance } from 'class-transformer';
import { RouteRequestDto } from './route-request.dto';

describe('RouteRequestDto', () => {
  describe('validation', () => {
    it('should pass validation with valid data', async () => {
      const dto = plainToInstance(RouteRequestDto, {
        message: {
          message_id: 'msg_123',
          tenant_id: 'tenant_abc',
          message_type: 'chat',
          payload: '{"text": "Hello"}',
        },
        policy_id: 'default',
        context: { user_id: 'user_123' },
      });

      const errors = await validate(dto);
      expect(errors.length).toBe(0);
    });

    it('should fail validation when message is missing', async () => {
      const dto = plainToInstance(RouteRequestDto, {
        policy_id: 'default',
      });

      const errors = await validate(dto);
      expect(errors.length).toBeGreaterThan(0);
      expect(errors[0].property).toBe('message');
    });

    it('should fail validation when message is invalid', async () => {
      const dto = plainToInstance(RouteRequestDto, {
        message: {
          // Missing required fields
          message_id: 'msg_123',
        },
        policy_id: 'default',
      });

      const errors = await validate(dto);
      expect(errors.length).toBeGreaterThan(0);
    });

    it('should pass validation with optional fields', async () => {
      const dto = plainToInstance(RouteRequestDto, {
        message: {
          message_id: 'msg_123',
          tenant_id: 'tenant_abc',
          message_type: 'chat',
          payload: '{"text": "Hello"}',
        },
        // policy_id and context are optional
      });

      const errors = await validate(dto);
      expect(errors.length).toBe(0);
    });

    it('should fail validation when policy_id is not a string', async () => {
      const dto = plainToInstance(RouteRequestDto, {
        message: {
          message_id: 'msg_123',
          tenant_id: 'tenant_abc',
          message_type: 'chat',
          payload: '{"text": "Hello"}',
        },
        policy_id: 123 as unknown as string, // Invalid type
      });

      const errors = await validate(dto);
      expect(errors.length).toBeGreaterThan(0);
    });

    it('should pass validation with valid context object', async () => {
      const dto = plainToInstance(RouteRequestDto, {
        message: {
          message_id: 'msg_123',
          tenant_id: 'tenant_abc',
          message_type: 'chat',
          payload: '{"text": "Hello"}',
        },
        context: {
          user_id: 'user_123',
          session_id: 'session_456',
        },
      });

      const errors = await validate(dto);
      expect(errors.length).toBe(0);
    });
  });

  describe('happy path', () => {
    it('should create valid DTO with all fields', () => {
      const dto = plainToInstance(RouteRequestDto, {
        message: {
          message_id: 'msg_123',
          tenant_id: 'tenant_abc',
          trace_id: 'trace_123',
          message_type: 'chat',
          payload: '{"text": "Hello"}',
          metadata: { key: 'value' },
          timestamp_ms: 1704067200000,
        },
        policy_id: 'weighted-policy',
        context: {
          user_id: 'user_123',
          // sticky: { session_key: 'user_id' },
        },
      });

      expect(dto.message).toBeDefined();
      expect(dto.message.message_id).toBe('msg_123');
      expect(dto.policy_id).toBe('weighted-policy');
      expect(dto.context).toBeDefined();
      expect(dto.context?.user_id).toBe('user_123');
    });
  });
});
