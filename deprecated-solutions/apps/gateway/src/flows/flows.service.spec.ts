import { FlowsService } from './flows.service';

describe('FlowsService', () => {
  it('should validate a minimal valid flow definition', () => {
    const service = new FlowsService();
    const validFlow = {
      id: 'flow_1',
      version: 'v1',
      steps: [{ id: 's1', type: 'http.request', inputs: { url: 'https://example.com' } }],
      edges: [],
    };

    const res = service.validate({ definition: validFlow });
    expect(res.valid).toBe(true);
    expect(res.errors).toHaveLength(0);
  });

  it('should report errors for invalid flow definition', () => {
    const service = new FlowsService();
    const invalidFlow: Record<string, unknown> = {
      // missing id
      version: 'v1',
      steps: [], // steps must have at least 1
      edges: [],
    };

    const res = service.validate({ definition: invalidFlow });
    expect(res.valid).toBe(false);
    expect(res.errors.length).toBeGreaterThan(0);
  });
});
