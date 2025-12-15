import { Injectable, NotFoundException, Logger } from '@nestjs/common';
import { PolicyDto } from './dto/policy.dto';
import { RouterAdminClientService } from '../router-admin/router-admin-client.service';

@Injectable()
export class PoliciesService {
  private readonly logger = new Logger(PoliciesService.name);
  private policies: Map<string, PolicyDto> = new Map();
  private cacheTtlMs = 60000;
  private cacheIndex: Map<string, { ts: number; items: PolicyDto[] }> = new Map();

  private getKey(tenantId: string, policyId: string): string {
    return `${tenantId}:${policyId}`;
  }

  constructor(private readonly routerAdmin: RouterAdminClientService) {}

  private isCacheValid(tenantId: string): boolean {
    const entry = this.cacheIndex.get(tenantId);
    return !!entry && Date.now() - entry.ts < this.cacheTtlMs;
  }

  private setCache(tenantId: string, items: PolicyDto[]): void {
    this.cacheIndex.set(tenantId, { ts: Date.now(), items });
  }

  async findAll(tenantId: string): Promise<PolicyDto[]> {
    if (this.isCacheValid(tenantId)) {
      return this.cacheIndex.get(tenantId)!.items;
    }
    await this.ensureConnected();
    const res = await this.routerAdmin.listPolicies({ tenant_id: tenantId });
    const items: PolicyDto[] = (res.policies || []).map((p) => ({
      tenant_id: tenantId,
      policy_id: p.policy_id,
      name: p.policy_id,
      providers: (p.providers || []).map((pr: any) => ({ id: pr.id, weight: pr.weight })),
      sticky: p.sticky ? { enabled: true, ttl_seconds: 3600, session_key: 'assignment_id' } : undefined,
      rules: (p.rules || []).map((r: any) => ({ match: r.match, prefer: r.prefer, fallback: r.fallback })),
      metadata: {},
    }));
    this.setCache(tenantId, items);
    // mirror to local store for direct access paths
    items.forEach((it) => this.policies.set(this.getKey(tenantId, it.policy_id), it));
    return items;
  }

  async findOne(tenantId: string, policyId: string): Promise<PolicyDto> {
    await this.ensureConnected();
    const res = await this.routerAdmin.getPolicy({ tenant_id: tenantId, policy_id: policyId });
    if (!res || !res.policy) {
      throw new NotFoundException(`Policy with ID ${policyId} not found for tenant ${tenantId}`);
    }
    const dto: PolicyDto = {
      tenant_id: tenantId,
      policy_id: res.policy.policy_id,
      name: res.policy.policy_id,
      providers: res.policy.providers.map((pr) => ({ id: pr.id, weight: pr.weight })),
      sticky: res.policy.sticky ? { enabled: true, ttl_seconds: 3600, session_key: 'assignment_id' } : undefined,
      rules: (res.policy.rules || []).map((r) => ({ match: (r.match as any) || {}, prefer: (r.prefer as any) || {}, fallback: (r.fallback as any) || {} })),
      metadata: {},
    };
    this.policies.set(this.getKey(tenantId, policyId), dto);
    this.invalidateCache(tenantId);
    return dto;
  }

  async create(tenantId: string, policy: PolicyDto): Promise<PolicyDto> {
    await this.ensureConnected();
    await this.routerAdmin.upsertPolicy({
      tenant_id: tenantId,
      policy: {
        policy_id: policy.policy_id,
        providers: (policy as any).providers?.map((p: any) => ({ id: p.id, weight: p.weight })) || [],
        sticky: !!policy.sticky?.enabled,
        rules: (policy as any).rules?.map((r: any) => ({ match: r.match, prefer: r.prefer, fallback: r.fallback })) || [],
      },
    });
    this.policies.set(this.getKey(tenantId, policy.policy_id), { ...policy, tenant_id: tenantId });
    this.invalidateCache(tenantId);
    return policy;
  }

  async update(tenantId: string, policyId: string, policy: PolicyDto): Promise<PolicyDto> {
    await this.ensureConnected();
    await this.routerAdmin.upsertPolicy({
      tenant_id: tenantId,
      policy: {
        policy_id: policyId,
        providers: (policy as any).providers?.map((p: any) => ({ id: p.id, weight: p.weight })) || [],
        sticky: !!policy.sticky?.enabled,
        rules: (policy as any).rules?.map((r: any) => ({ match: r.match, prefer: r.prefer, fallback: r.fallback })) || [],
      },
    });
    this.policies.set(this.getKey(tenantId, policyId), { ...policy, tenant_id: tenantId, policy_id: policyId });
    this.invalidateCache(tenantId);
    return policy;
  }

  async delete(tenantId: string, policyId: string): Promise<void> {
    await this.ensureConnected();
    await this.routerAdmin.deletePolicy({ tenant_id: tenantId, policy_id: policyId });
    const key = this.getKey(tenantId, policyId);
    this.policies.delete(key);
    this.invalidateCache(tenantId);
  }

  private async ensureConnected(): Promise<void> {
    if (!this.routerAdmin.isConnected()) {
      await this.routerAdmin.connect();
    }
  }

  private invalidateCache(tenantId: string): void {
    this.cacheIndex.delete(tenantId);
  }

  async syncPolicies(tenantId: string): Promise<PolicyDto[]> {
    await this.ensureConnected();
    const remote = await this.routerAdmin.listPolicies({ tenant_id: tenantId });
    const items: PolicyDto[] = (remote.policies || []).map((p) => ({
      tenant_id: tenantId,
      policy_id: p.policy_id,
      providers: p.providers.map((pr) => ({ id: pr.id, weight: pr.weight })),
      sticky: p.sticky ? { enabled: true, ttl_seconds: 3600, session_key: 'assignment_id' } : undefined,
      rules: (p.rules || []).map((r) => ({ match: (r.match as any) || {}, prefer: (r.prefer as any) || {}, fallback: (r.fallback as any) || {} })),
      metadata: {},
      name: p.policy_id,
      version: '1.0',
    }));
    const prefix = `${tenantId}::`;
    for (const key of Array.from(this.policies.keys())) {
      if (key.startsWith(prefix)) this.policies.delete(key);
    }
    items.forEach((it) => this.policies.set(this.getKey(tenantId, it.policy_id), it));
    this.setCache(tenantId, items);
    return items;
  }

  async reconcilePolicies(tenantId: string, desired: PolicyDto[]): Promise<void> {
    await this.ensureConnected();
    const current = await this.routerAdmin.listPolicies({ tenant_id: tenantId });
    const currentIds = new Set((current.policies || []).map((p) => p.policy_id));
    const desiredIds = new Set((desired || []).map((d) => d.policy_id));
    for (const d of desired) {
      await this.routerAdmin.upsertPolicy({
        tenant_id: tenantId,
        policy: {
          policy_id: d.policy_id,
          providers: (d as any).providers?.map((p: any) => ({ id: p.id, weight: p.weight })) || [],
          sticky: !!d.sticky?.enabled,
          rules: (d as any).rules?.map((r: any) => ({ match: r.match, prefer: r.prefer, fallback: r.fallback })) || [],
        },
      });
    }
    for (const id of Array.from(currentIds)) {
      if (!desiredIds.has(id)) {
        await this.routerAdmin.deletePolicy({ tenant_id: tenantId, policy_id: id });
      }
    }
    await this.syncPolicies(tenantId);
    this.invalidateCache(tenantId);
  }
}
