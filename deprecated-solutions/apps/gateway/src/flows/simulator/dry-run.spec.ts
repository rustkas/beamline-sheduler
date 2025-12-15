import { DryRunSimulator } from './dry-run';
import { RegistryService, BlockType } from '../registry.service';

describe('DryRunSimulator', () => {
  it('should simulate steps and record latency', async () => {
    const registry = new RegistryService();
    const sim = new DryRunSimulator(registry);
    const flow = {
      id: 'f1',
      steps: [
        { id: 's1', type: 'http.request' as const, inputs: { url: 'https://example.com' } },
        { id: 's2', type: 'sql.query' as const, inputs: { sql: 'select 1' } },
      ],
    };
    const res = await sim.run(flow);
    expect(res.flow_id).toBe('f1');
    expect(res.steps.length).toBe(2);
    expect(res.total_latency_ms).toBeGreaterThanOrEqual(0);
    expect(res.steps.every((s) => s.latency_ms >= 0)).toBe(true);
  });

  it('should report unknown block types', async () => {
    const registry = new RegistryService();
    const sim = new DryRunSimulator(registry);
    const flow = {
      id: 'f2',
      steps: [{ id: 's1', type: 'unknown' as unknown as BlockType }],
    };
    const res = await sim.run(flow);
    expect(res.steps[0].success).toBe(false);
    expect(res.steps[0].error).toContain('unknown block type');
  });
});
