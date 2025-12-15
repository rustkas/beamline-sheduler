import { Test, TestingModule } from '@nestjs/testing';
import { firstValueFrom } from 'rxjs';
import { take } from 'rxjs/operators';
import { ResultsController } from './results.controller';
import { ResultsStreamService } from './results-stream.service';

describe('ResultsController (integration)', () => {
  let controller: ResultsController;
  let _streamService: ResultsStreamService;

  beforeAll(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [ResultsStreamService],
      controllers: [ResultsController],
    }).compile();

    controller = module.get<ResultsController>(ResultsController);
    _streamService = module.get<ResultsStreamService>(ResultsStreamService);
  });

  it('filters by tenant_id and provider_id', async () => {
    const obs = controller.stream('a1', undefined, 't1', 'p1', '60000');

    controller.emitResult({
      assignment_id: 'a1',
      tenant_id: 't2',
      provider_id: 'p1',
      status: 'success',
    });
    controller.emitResult({
      assignment_id: 'a1',
      tenant_id: 't1',
      provider_id: 'p1',
      status: 'success',
    });

    const evt = (await firstValueFrom(obs.pipe(take(1)))) as { data: Record<string, unknown> };
    const data = evt.data as Record<string, unknown>;
    expect(data['tenant_id']).toBe('t1');
    expect(data['provider_id']).toBe('p1');
  });

  it('passes when only tenant_id matches without provider_id filter', async () => {
    const obs = controller.stream('a2', undefined, 't1', undefined, '60000');

    controller.emitResult({
      assignment_id: 'a2',
      tenant_id: 't1',
      provider_id: 'pX',
      status: 'success',
    });

    const evt = (await firstValueFrom(obs.pipe(take(1)))) as { data: Record<string, unknown> };
    const data = evt.data as Record<string, unknown>;
    expect(data['tenant_id']).toBe('t1');
  });
});
