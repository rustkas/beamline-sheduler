import { Test, TestingModule } from '@nestjs/testing';
import { ThrottlerModule } from '@nestjs/throttler';
import { PoliciesController } from './policies.controller';
import { PoliciesService } from './policies.service';
import { ConfigModule } from '@nestjs/config';

describe('PoliciesController', () => {
  let controller: PoliciesController;
  let service: PoliciesService;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      imports: [
        ConfigModule.forRoot({ isGlobal: true }),
        ThrottlerModule.forRoot({
          throttlers: [
            {
              ttl: 60000,
              limit: 10,
            },
          ],
        }),
      ],
      controllers: [PoliciesController],
      providers: [
        {
          provide: PoliciesService,
          useValue: {
            syncPolicies: jest.fn().mockResolvedValue([]),
            reconcilePolicies: jest.fn().mockResolvedValue(undefined),
          },
        },
      ],
    }).compile();

    controller = module.get<PoliciesController>(PoliciesController);
    service = module.get<PoliciesService>(PoliciesService);
  });

  it('should sync policies', async () => {
    const res = await controller.sync('tenantA');
    expect(res).toEqual([]);
    expect(service.syncPolicies).toHaveBeenCalledWith('tenantA');
  });

  it('should reconcile policies', async () => {
    await controller.reconcile('tenantA', { desired: [] });
    expect(service.reconcilePolicies).toHaveBeenCalledWith('tenantA', []);
  });
});