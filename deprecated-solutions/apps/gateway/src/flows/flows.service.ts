import { Injectable } from '@nestjs/common';
import Ajv, { ValidateFunction } from 'ajv';
import addFormats from 'ajv-formats';
import { FlowValidateRequestDto } from './dto/flow-validate-request.dto';
import { FlowValidateResponseDto } from './dto/flow-validate-response.dto';
import { flowSchema } from './schemas/flow.schema';
import { FlowDryRunRequestDto } from './dto/flow-dry-run-request.dto';
import { FlowDryRunResponseDto } from './dto/flow-dry-run-response.dto';
import { FlowCompiler, FlowDefinition, FlowEdge, FlowStep } from './compiler/flow-compiler';
import { DryRunSimulator } from './simulator/dry-run';
import { RegistryService } from './registry.service';
import type { BlockType } from './registry.service';

@Injectable()
export class FlowsService {
  private readonly ajv: Ajv;
  private readonly validateFn: ValidateFunction;
  private readonly simulator: DryRunSimulator;

  constructor(private readonly registry?: RegistryService) {
    this.ajv = new Ajv({ allErrors: true, strict: false });
    addFormats(this.ajv);
    this.validateFn = this.ajv.compile(flowSchema);
    this.simulator = new DryRunSimulator(this.registry || new RegistryService());
  }

  private coerceDefinition(def: Record<string, unknown>): FlowDefinition {
    type AnyRecord = Record<string, unknown>;
    const id = typeof def['id'] === 'string' ? (def['id'] as string) : '';
    const version = typeof def['version'] === 'string' ? (def['version'] as string) : 'v1';
    const stepsInput = Array.isArray(def['steps']) ? (def['steps'] as AnyRecord[]) : [];
    const steps: FlowStep[] = stepsInput
      .map((s: AnyRecord) => ({
        id: typeof s.id === 'string' ? (s.id as string) : '',
        type: typeof s.type === 'string' ? (s.type as string) : '',
      }))
      .filter((s) => s.id && s.type);
    const edgesInput = Array.isArray(def['edges']) ? (def['edges'] as AnyRecord[]) : [];
    const edges: FlowEdge[] = edgesInput
      .map((e: AnyRecord) => ({
        from: typeof e.from === 'string' ? (e.from as string) : '',
        to: typeof e.to === 'string' ? (e.to as string) : '',
      }))
      .filter((e) => e.from && e.to);
    return { id, version, steps, edges };
  }

  validate(req: FlowValidateRequestDto): FlowValidateResponseDto {
    const valid = this.validateFn(req.definition);
    if (valid) {
      return { valid: true, errors: [] };
    }
    const errors = (this.validateFn.errors || []).map((e) => ({
      instancePath: e.instancePath,
      schemaPath: e.schemaPath,
      message: e.message || '',
      params: e.params as Record<string, unknown>,
    }));
    return { valid: false, errors };
  }

  async dryRun(req: FlowDryRunRequestDto): Promise<FlowDryRunResponseDto> {
    const rawDef = req.definition as Record<string, unknown>;
    const definition = this.coerceDefinition(rawDef);
    const compile = FlowCompiler.compile(definition);
    if (!compile.valid) {
      return {
        compiled: false,
        compile_errors: compile.errors,
        flow_id: definition.id ?? '',
        total_latency_ms: 0,
        steps: [],
      };
    }
    type RawStep = { id?: string; inputs?: Record<string, unknown> };
    const stepsVal = (rawDef as { steps?: unknown }).steps;
    const rawSteps: RawStep[] = Array.isArray(stepsVal) ? (stepsVal as RawStep[]) : [];
    const simRes = await this.simulator.run({
      id: definition.id || '',
      steps: (definition.steps || []).map((s) => ({
        id: s.id,
        type: s.type as BlockType,
        inputs:
          rawSteps.find((rs) => rs && rs.id === s.id && typeof rs.inputs === 'object')?.inputs ||
          {},
      })),
    });
    return {
      compiled: true,
      compile_errors: [],
      flow_id: simRes.flow_id,
      total_latency_ms: simRes.total_latency_ms,
      steps: simRes.steps.map((s) => ({
        step_id: s.step_id,
        success: s.success,
        latency_ms: s.latency_ms,
        error: s.error,
      })),
    };
  }
}
