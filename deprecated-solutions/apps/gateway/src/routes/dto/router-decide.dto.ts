export interface RouterTask {
  type: 'chat' | 'completion' | 'embedding';
  payload: Record<string, unknown>;
}

export interface DecideRequest {
  version: string;
  tenant_id: string;
  request_id: string;
  trace_id?: string;
  task: RouterTask;
  policy_id?: string;
  constraints?: Record<string, unknown>;
  metadata?: Record<string, unknown>;
  push_assignment?: boolean;
  context?: Record<string, string>;
}

export interface DecideResponse {
  provider_id: string;
  reason: string;
  priority: number;
  metadata?: Record<string, unknown>;
}
