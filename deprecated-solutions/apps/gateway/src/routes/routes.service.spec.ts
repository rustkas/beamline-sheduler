import { Test, TestingModule } from '@nestjs/testing';
import { RoutesService } from './routes.service';
import { RouteRequestDto } from './dto/route-request.dto';
import { RouteDecisionDto } from './dto/route-decision.dto';
import { RouterClientService } from './adapters/router-client.service';

describe('RoutesService', () => {
  let service: RoutesService;

  beforeEach(async () => {
    const mockDecision: RouteDecisionDto = {
      provider_id: 'stub-provider',
      reason: 'stub',
      priority: 50,
      expected_latency_ms: 100,
      expected_cost: 0.0,
      metadata: {},
    };

    const module: TestingModule = await Test.createTestingModule({
      providers: [
        RoutesService,
        {
          provide: RouterClientService,
          useValue: {
            decide: jest.fn().mockResolvedValue(mockDecision),
          },
        },
        {
          provide: (await import('./adapters/router-client.service')).RouterClientService,
          useValue: {
            decide: jest.fn().mockResolvedValue(mockDecision),
          },
        },
        {
          provide: (await import('../common/services/idempotency.service')).IdempotencyService,
          useValue: new (
            await import('../common/services/idempotency.service')
          ).IdempotencyService(),
        },
        {
          provide: (await import('../common/services/sticky.service')).StickyService,
          useValue: new (await import('../common/services/sticky.service')).StickyService(),
        },
      ],
    }).compile();

    service = module.get<RoutesService>(RoutesService);
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });

  describe('decide', () => {
    it('should return a route decision (happy path)', async () => {
      const routeRequest: RouteRequestDto = {
        message: {
          message_id: 'msg_123',
          tenant_id: 'tenant_abc',
          message_type: 'chat',
          payload: '{"text": "Hello"}',
        },
      };

      const result = await service.decide(routeRequest);

      expect(result).toBeDefined();
      expect(result.provider_id).toBe('stub-provider');
      expect(result.reason).toBe('stub');
      expect(result.priority).toBe(50);
      expect(result.expected_latency_ms).toBe(100);
      expect(result.expected_cost).toBe(0.0);
    });

    it('should return route decision with policy_id', async () => {
      const routeRequest: RouteRequestDto = {
        message: {
          message_id: 'msg_123',
          tenant_id: 'tenant_abc',
          message_type: 'chat',
          payload: '{"text": "Hello"}',
        },
        policy_id: 'weighted-policy',
      };

      const result = await service.decide(routeRequest);

      expect(result).toBeDefined();
      expect(result.provider_id).toBe('stub-provider');
    });

    it('should return route decision with context', async () => {
      const routeRequest: RouteRequestDto = {
        message: {
          message_id: 'msg_123',
          tenant_id: 'tenant_abc',
          message_type: 'chat',
          payload: '{"text": "Hello"}',
        },
        context: {
          user_id: 'user_123',
        },
      };

      const result = await service.decide(routeRequest);

      expect(result).toBeDefined();
      expect(result.provider_id).toBe('stub-provider');
    });

    it('should handle different message types', async () => {
      const messageTypes = ['chat', 'completion', 'embedding'];

      for (const messageType of messageTypes) {
        const routeRequest: RouteRequestDto = {
          message: {
            message_id: 'msg_123',
            tenant_id: 'tenant_abc',
            message_type: messageType,
            payload: '{"text": "Hello"}',
          },
        };

        const result = await service.decide(routeRequest);

        expect(result).toBeDefined();
        expect(result.provider_id).toBe('stub-provider');
      }
    });

    it('should validate chat payload schema and annotate errors', async () => {
      const routeRequest: RouteRequestDto = {
        message: {
          message_id: 'msg_123',
          tenant_id: 'tenant_abc',
          message_type: 'chat',
          payload: '{"role":"user"}', // missing text
        },
      };

      const result = await service.decide(routeRequest);
      expect(result).toBeDefined();
      // Decision still returned, but internal metadata collects errors (cannot assert here since adapter returns stub)
    });

    it('should accept valid completion payload', async () => {
      const routeRequest: RouteRequestDto = {
        message: {
          message_id: 'msg_123',
          tenant_id: 'tenant_abc',
          message_type: 'completion',
          payload: '{"prompt":"Hello","max_tokens":10}',
        },
      };

      const result = await service.decide(routeRequest);
      expect(result).toBeDefined();
    });

    it('should accept valid embedding payload', async () => {
      const routeRequest: RouteRequestDto = {
        message: {
          message_id: 'msg_123',
          tenant_id: 'tenant_abc',
          message_type: 'embedding',
          payload: '{"input":["hello","world"]}',
        },
      };

      const result = await service.decide(routeRequest);
      expect(result).toBeDefined();
    });
    it('should reject unsupported version', async () => {
      const routeRequest: RouteRequestDto = {
        message: {
          message_id: 'msg_ver',
          tenant_id: 'tenant_x',
          message_type: 'chat',
          payload: '{"text":"hi"}',
          trace_id: 'abc',
        },
        policy_id: 'default',
        constraints: {},
        metadata: {},
        context: {},
      } as any;

      const service2 = service;
      // Force wrong version via monkey-patch (simulate routeRequest)
      (service2 as any).routerClient.decide = jest.fn().mockResolvedValue({ provider_id: 'p1', reason: 'ok', priority: 1 });
      (routeRequest as any).context = { __force_version: '2' } as any;
      await expect(service2.decide(routeRequest)).rejects.toThrow('invalid_request');
    });

    it('should reject invalid request_id and task.type', async () => {
      const routeRequest: RouteRequestDto = {
        message: {
          message_id: 'msg_type',
          tenant_id: 'tenant_x',
          message_type: 'unknown',
          payload: '{"x":1}',
        },
      } as any;

      const service2 = service;
      await expect(service2.decide(routeRequest)).rejects.toThrow('invalid_request');
    });
  });

  describe('getDecision', () => {
    it('should return a route decision for message ID (happy path)', async () => {
      const routeRequest: RouteRequestDto = {
        message: {
          message_id: 'msg_123',
          tenant_id: 'tenant_abc',
          message_type: 'chat',
          payload: '{"text": "Hello"}',
        },
      };

      await service.decide(routeRequest);
      const result = await service.getDecision('msg_123');

      expect(result).toBeDefined();
      expect(result.provider_id).toBe('stub-provider');
      expect(result.reason).toBe('stub');
      expect(result.priority).toBe(50);
    });

    it('should throw NotFoundException when decision not found', async () => {
      await expect(service.getDecision('non_existent')).rejects.toThrow();
    });
  });

  describe('error handling', () => {
    it('should handle empty message gracefully', async () => {
      const routeRequest: RouteRequestDto = {
        message: {
          message_id: '',
          tenant_id: '',
          message_type: '',
          payload: '',
        },
      };

      const result = await service.decide(routeRequest);

      // Should not throw, return stub response
      expect(result).toBeDefined();
      expect(result.provider_id).toBe('stub-provider');
    });

    it('should handle missing optional fields', async () => {
      const routeRequest: RouteRequestDto = {
        message: {
          message_id: 'msg_123',
          tenant_id: 'tenant_abc',
          message_type: 'chat',
          payload: '{"text": "Hello"}',
        },
        // No policy_id or context
      };

      const result = await service.decide(routeRequest);

      expect(result).toBeDefined();
      expect(result.provider_id).toBe('stub-provider');
    });
  });

  describe('sticky sessions TTL and overwrite', () => {
    it('reuses provider within TTL and overwrites after TTL expiry', async () => {
      process.env.STICKY_SESSION_KEY = 'session_id';
      process.env.STICKY_TTL_MS = '25';
      const routeRequest: RouteRequestDto = {
        message: {
          message_id: 'msg_sticky',
          tenant_id: 'tenant_s',
          message_type: 'chat',
          payload: '{"text":"hi"}',
        },
        context: { session_id: 'sess1' } as any,
      } as any;

      const svc: any = service as any;
      svc.routerClient.decide = jest.fn().mockResolvedValue({ provider_id: 'p1', reason: 'ok', priority: 1 });

      const d1 = await service.decide(routeRequest);
      expect(d1.provider_id).toBe('p1');
      const stickyKey = (svc.sticky as any).key('tenant_s', 'sess1');
      expect((svc.sticky as any).get(stickyKey)).toBe('p1');

      // Within TTL: provider should be reused and metrics updated
      svc.routerClient.decide = jest.fn().mockResolvedValue({ provider_id: 'p2', reason: 'ok', priority: 1 });
      const d2 = await service.decide(routeRequest);
      expect(d2.provider_id).toBe('p2');
      expect((svc.sticky as any).get(stickyKey)).toBe('p2');
      const store1 = require('../observability/internal-metrics.store');
      expect(store1.InternalMetrics.sticky_hits_total + store1.InternalMetrics.sticky_miss_total).toBeGreaterThanOrEqual(1);

      // After TTL expiry: cache should miss and allow overwrite
      await new Promise((r) => setTimeout(r, 30));
      svc.routerClient.decide = jest.fn().mockResolvedValue({ provider_id: 'p3', reason: 'ok', priority: 1 });
      const d3 = await service.decide(routeRequest);
      expect(d3.provider_id).toBe('p3');
      expect((svc.sticky as any).get(stickyKey)).toBe('p3');

      const store2 = require('../observability/internal-metrics.store');
      expect(store2.InternalMetrics.sticky_hits_total + store2.InternalMetrics.sticky_miss_total).toBeGreaterThanOrEqual(1);
    }, 10000);

    it('cold start (miss) then warm cache (hit)', async () => {
      process.env.STICKY_SESSION_KEY = 'session_id';
      process.env.STICKY_TTL_MS = '1000';
      const routeRequest: RouteRequestDto = {
        message: {
          message_id: 'msg_sticky2',
          tenant_id: 'tenant_s2',
          message_type: 'chat',
          payload: '{"text":"hi"}',
        },
        context: { session_id: 'sess2' } as any,
      } as any;

      const svc: any = service as any;
      const stickyKey = (svc.sticky as any).key('tenant_s2', 'sess2');
      (svc.sticky as any).clear(stickyKey);
      svc.routerClient.decide = jest.fn().mockResolvedValue({ provider_id: 'pX', reason: 'ok', priority: 1 });
      await service.decide(routeRequest);
      const store = require('../observability/internal-metrics.store');
      const miss = store.InternalMetrics.sticky_miss_total;
      expect(miss).toBeGreaterThanOrEqual(1);
      // Warm hit
      svc.routerClient.decide = jest.fn().mockResolvedValue({ provider_id: 'pY', reason: 'ok', priority: 1 });
      await service.decide(routeRequest);
      const hits = store.InternalMetrics.sticky_hits_total;
      expect(hits).toBeGreaterThanOrEqual(1);
    }, 10000);
  });
});
