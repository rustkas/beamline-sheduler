import { FlowsService } from './flows.service';

describe('FlowsService dryRun', () => {
  it('should compile and simulate valid flow', async () => {
    const service = new FlowsService();
    const validFlow = {
      id: 'flow_1',
      version: 'v1',
      steps: [
        { id: 's1', type: 'http.request', inputs: { url: 'https://example.com' } },
        { id: 's2', type: 'sql.query', inputs: { sql: 'select 1' } },
      ],
      edges: [{ from: 's1', to: 's2' }],
    };

    const res = await service.dryRun({ definition: validFlow });
    expect(res.compiled).toBe(true);
    expect(res.compile_errors).toHaveLength(0);
    expect(res.flow_id).toBe('flow_1');
    expect(res.steps.length).toBe(2);
    expect(res.total_latency_ms).toBeGreaterThan(0);
  });

  it('should report compile errors and not simulate', async () => {
    const service = new FlowsService();
    const invalidFlow = {
      id: 'flow_bad',
      version: 'v1',
      steps: [{ id: 's1', type: 'http.request' }],
      edges: [{ from: 's1', to: 'missing' }],
    };
    const res = await service.dryRun({ definition: invalidFlow });
    expect(res.compiled).toBe(false);
    expect(res.compile_errors.length).toBeGreaterThan(0);
    expect(res.steps.length).toBe(0);
    expect(res.total_latency_ms).toBe(0);
  });
});
