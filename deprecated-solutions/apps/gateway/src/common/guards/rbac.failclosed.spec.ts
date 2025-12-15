import { ExecutionContext, ForbiddenException } from '@nestjs/common';
import { RBACGuard } from './rbac.guard';

function mockContext(
  headers: Record<string, string>,
  body?: Record<string, unknown>,
): ExecutionContext {
  return {
    switchToHttp: () => ({
      getRequest: () => ({ headers, body }),
      getResponse: () => ({}),
    }),
  } as unknown as ExecutionContext;
}

describe('RBACGuard fail-closed (unit)', () => {
  it('denies when Router Admin is enabled but unavailable', async () => {
    process.env.USE_ROUTER_AUTH = 'true';
    const mockRouterAuthService = {
      isEnabled: () => true,
      checkPermission: async () => {
        throw new Error('Router unavailable');
      },
    } as any;
    const guard = new RBACGuard(mockRouterAuthService);
    const ctx = mockContext({ 'x-tenant-id': 't1', 'x-user-id': 'u1', 'x-role': 'admin' }, { policy_id: 'default' });
    await expect(guard.canActivate(ctx)).rejects.toThrow(ForbiddenException);
  });
});