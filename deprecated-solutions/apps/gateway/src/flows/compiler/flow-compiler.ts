export interface FlowStep {
  id: string;
  type: string;
}

export interface FlowEdge {
  from: string;
  to: string;
}

export interface FlowDefinition {
  id: string;
  version: string;
  steps: FlowStep[];
  edges: FlowEdge[];
}

export interface CompileResult {
  valid: boolean;
  errors: string[];
}

export class FlowCompiler {
  static validateReferences(flow: FlowDefinition): string[] {
    const errors: string[] = [];
    const stepIds = new Set(flow.steps.map((s) => s.id));
    for (const e of flow.edges) {
      if (!stepIds.has(e.from)) {
        errors.push(`edge.from references missing step: ${e.from}`);
      }
      if (!stepIds.has(e.to)) {
        errors.push(`edge.to references missing step: ${e.to}`);
      }
    }
    return errors;
  }

  static detectCycles(flow: FlowDefinition): string[] {
    const errors: string[] = [];
    const adj: Map<string, string[]> = new Map();
    for (const s of flow.steps) adj.set(s.id, []);
    for (const e of flow.edges) {
      if (!adj.has(e.from)) adj.set(e.from, []);
      adj.get(e.from)!.push(e.to);
    }

    const visited = new Set<string>();
    const inStack = new Set<string>();

    const dfs = (node: string): boolean => {
      if (inStack.has(node)) return true; // cycle
      if (visited.has(node)) return false;
      visited.add(node);
      inStack.add(node);
      for (const nei of adj.get(node) || []) {
        if (dfs(nei)) return true;
      }
      inStack.delete(node);
      return false;
    };

    for (const s of flow.steps) {
      if (dfs(s.id)) {
        errors.push(`cycle detected involving step: ${s.id}`);
        break;
      }
    }
    return errors;
  }

  static compile(flow: FlowDefinition): CompileResult {
    const refErrors = this.validateReferences(flow);
    const cycleErrors = this.detectCycles(flow);
    const errors = [...refErrors, ...cycleErrors];
    return { valid: errors.length === 0, errors };
  }
}
