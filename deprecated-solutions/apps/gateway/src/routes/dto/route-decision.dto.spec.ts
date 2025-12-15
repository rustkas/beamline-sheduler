import { validate } from 'class-validator';
import { plainToInstance } from 'class-transformer';
import { RouteDecisionDto } from './route-decision.dto';

describe('RouteDecisionDto', () => {
  describe('validation', () => {
    it('should pass validation with valid data', async () => {
      const dto = plainToInstance(RouteDecisionDto, {
        provider_id: 'anthropic',
        reason: 'weighted',
        priority: 50,
        expected_latency_ms: 250,
        expected_cost: 0.001,
      });

      const errors = await validate(dto);
      expect(errors.length).toBe(0);
    });

    it('should fail validation when provider_id is missing', async () => {
      const dto = plainToInstance(RouteDecisionDto, {
        reason: 'weighted',
        priority: 50,
        expected_latency_ms: 250,
        expected_cost: 0.001,
      });

      const errors = await validate(dto);
      expect(errors.length).toBeGreaterThan(0);
      expect(errors[0].property).toBe('provider_id');
    });

    it('should fail validation when reason is missing', async () => {
      const dto = plainToInstance(RouteDecisionDto, {
        provider_id: 'openai',
        priority: 50,
        expected_latency_ms: 250,
        expected_cost: 0.001,
      });

      const errors = await validate(dto);
      expect(errors.length).toBeGreaterThan(0);
      expect(errors[0].property).toBe('reason');
    });

    it('should fail validation when priority is missing', async () => {
      const dto = plainToInstance(RouteDecisionDto, {
        provider_id: 'openai',
        reason: 'weighted',
        expected_latency_ms: 250,
        expected_cost: 0.001,
      });

      const errors = await validate(dto);
      expect(errors.length).toBeGreaterThan(0);
      expect(errors[0].property).toBe('priority');
    });

    it('should pass validation with optional metadata', async () => {
      const dto = plainToInstance(RouteDecisionDto, {
        provider_id: 'openai',
        reason: 'weighted',
        priority: 50,
        expected_latency_ms: 250,
        expected_cost: 0.001,
        metadata: { trace_id: 'trace_123', session_id: 'session_456' },
      });

      const errors = await validate(dto);
      expect(errors.length).toBe(0);
    });
  });

  describe('happy path', () => {
    it('should create valid DTO with all fields', () => {
      const dto = plainToInstance(RouteDecisionDto, {
        provider_id: 'openai',
        reason: 'sticky',
        priority: 100,
        expected_latency_ms: 200,
        expected_cost: 0.002,
        metadata: { decision_time_ms: 5 },
      });

      expect(dto.provider_id).toBe('anthropic');
      expect(dto.reason).toBe('sticky');
      expect(dto.priority).toBe(100);
      expect(dto.expected_latency_ms).toBe(200);
      expect(dto.expected_cost).toBe(0.002);
      expect(dto.metadata).toEqual({ decision_time_ms: 5 });
    });
  });
});
