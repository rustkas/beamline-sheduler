import { FlowCompiler, FlowDefinition } from './flow-compiler';

describe('FlowCompiler', () => {
  it('should pass valid references and no cycles', () => {
    const flow: FlowDefinition = {
      id: 'f1',
      version: 'v1',
      steps: [
        { id: 's1', type: 'http.request' },
        { id: 's2', type: 'fs.blob_put' },
      ],
      edges: [{ from: 's1', to: 's2' }],
    };
    const res = FlowCompiler.compile(flow);
    expect(res.valid).toBe(true);
    expect(res.errors).toHaveLength(0);
  });

  it('should detect missing references', () => {
    const flow: FlowDefinition = {
      id: 'f1',
      version: 'v1',
      steps: [{ id: 's1', type: 'http.request' }],
      edges: [{ from: 's1', to: 's2' }],
    };
    const res = FlowCompiler.compile(flow);
    expect(res.valid).toBe(false);
    expect(res.errors.some((e) => e.includes('edge.to references missing step'))).toBe(true);
  });

  it('should detect cycles', () => {
    const flow: FlowDefinition = {
      id: 'f1',
      version: 'v1',
      steps: [
        { id: 's1', type: 'http.request' },
        { id: 's2', type: 'sql.query' },
      ],
      edges: [
        { from: 's1', to: 's2' },
        { from: 's2', to: 's1' },
      ],
    };
    const res = FlowCompiler.compile(flow);
    expect(res.valid).toBe(false);
    expect(res.errors.some((e) => e.includes('cycle detected'))).toBe(true);
  });
});
