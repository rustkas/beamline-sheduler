import { RBACGuard } from './rbac.guard';
import { ExecutionContext } from '@nestjs/common';

function mockContext(
  headers: Record<string, string>,
  body?: Record<string, unknown>,
): ExecutionContext {
  return {
    switchToHttp: () => ({
      getRequest: () => ({ headers, body }),
    }),
  } as unknown as ExecutionContext;
}

describe('RBACGuard', () => {
  const guard = new RBACGuard();

  it('allows when allowlists are empty', async () => {
    process.env.GATEWAY_TENANT_ALLOWLIST = '';
    process.env.GATEWAY_POLICY_ALLOWLIST = '';
    const ctx = mockContext({});
    await expect(guard.canActivate(ctx)).resolves.toBe(true);
  });

  it('blocks tenant not in allowlist', async () => {
    process.env.GATEWAY_TENANT_ALLOWLIST = 't1,t2';
    const ctx = mockContext({ 'x-tenant-id': 't3' });
    await expect(guard.canActivate(ctx)).rejects.toThrow('Tenant not allowed');
  });

  it('allows tenant in allowlist', async () => {
    process.env.GATEWAY_TENANT_ALLOWLIST = 't1,t2';
    const ctx = mockContext({ 'x-tenant-id': 't2' });
    await expect(guard.canActivate(ctx)).resolves.toBe(true);
  });

  it('blocks policy not in allowlist', async () => {
    process.env.GATEWAY_POLICY_ALLOWLIST = 'p1,p2';
    const ctx = mockContext({}, { policy_id: 'p3' });
    await expect(guard.canActivate(ctx)).rejects.toThrow('Policy not allowed');
  });

  it('uses cache to allow repeated checks', async () => {
    process.env.GATEWAY_TENANT_ALLOWLIST = '';
    process.env.GATEWAY_POLICY_ALLOWLIST = '';
    process.env.RBAC_CACHE_TTL_MS = '1000';
    const ctx = mockContext({ 'x-role': 'viewer', 'x-resource': 'metrics', 'x-action': 'read' });
    // First call populates cache
    await expect(guard.canActivate(ctx)).resolves.toBe(true);
    // Second should be served from cache
    await expect(guard.canActivate(ctx)).resolves.toBe(true);
  });

  it('fail-closed when action not allowed', async () => {
    process.env.GATEWAY_TENANT_ALLOWLIST = '';
    process.env.GATEWAY_POLICY_ALLOWLIST = '';
    process.env.RBAC_CACHE_TTL_MS = '1000';
    const ctx = mockContext({ 'x-role': 'viewer', 'x-resource': 'config', 'x-action': 'write' });
    await expect(guard.canActivate(ctx)).rejects.toThrow('RBAC denied');
    // Cached deny
    await expect(guard.canActivate(ctx)).rejects.toThrow('RBAC denied');
  });
});
