import { Injectable } from '@nestjs/common';
import { RegistryService, BlockType } from '../registry.service';

export interface DryRunStepResult {
  step_id: string;
  success: boolean;
  latency_ms: number;
  error?: string;
}

export interface DryRunResult {
  flow_id: string;
  total_latency_ms: number;
  steps: DryRunStepResult[];
}

@Injectable()
export class DryRunSimulator {
  constructor(private readonly registry: RegistryService) {}

  private async execute(
    type: BlockType,
    _inputs: Record<string, unknown>,
  ): Promise<DryRunStepResult> {
    const start = performance.now();
    try {
      // Simulate latency per block type
      const latencies: Record<BlockType, number> = {
        'http.request': 50,
        'fs.blob_put': 10,
        'fs.blob_get': 8,
        'sql.query': 20,
      };
      const delay = latencies[type] ?? 5;
      await new Promise((r) => setTimeout(r, delay));
      const latency_ms = Math.round(performance.now() - start);
      return { step_id: '', success: true, latency_ms };
    } catch (err) {
      const latency_ms = Math.round(performance.now() - start);
      return { step_id: '', success: false, latency_ms, error: (err as Error).message };
    }
  }

  async run(flow: {
    id: string;
    steps: { id: string; type: BlockType; inputs?: Record<string, unknown> }[];
  }): Promise<DryRunResult> {
    const steps: DryRunStepResult[] = [];
    for (const s of flow.steps) {
      const manifest = this.registry.get(s.type);
      if (!manifest) {
        steps.push({
          step_id: s.id,
          success: false,
          latency_ms: 0,
          error: `unknown block type: ${s.type}`,
        });
        continue;
      }
      const res = await this.execute(s.type, s.inputs || {});
      res.step_id = s.id;
      steps.push(res);
    }
    const total_latency_ms = steps.reduce((acc, s) => acc + s.latency_ms, 0);
    return { flow_id: flow.id, total_latency_ms, steps };
  }
}
