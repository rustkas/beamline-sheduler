export const Transport = {
  GRPC: 'grpc',
};

export class ClientsModule {
  static registerAsync(_defs: any[]): any[] {
    return [];
  }
}

export class ClientProxyFactory {
  static create(_opts: any): any {
    return {
      getService: (_name: string) => ({
        authenticate: (_r: any) => ({ subscribe: ({ next }: any) => next({ authenticated: true }) }),
        authorize: (_r: any) => ({ subscribe: ({ next }: any) => next({ authorized: true }) }),
        validateApiKey: (_r: any) => ({ subscribe: ({ next }: any) => next({ valid: true }) }),
        getQuota: (_r: any) => ({ subscribe: ({ next }: any) => next({}) }),
        checkRateLimit: (_r: any) => ({ subscribe: ({ next }: any) => next({ allowed: true }) }),
        getRoles: (_r: any) => ({ subscribe: ({ next }: any) => next({ roles: [] }) }),
        getPermissions: (_r: any) => ({ subscribe: ({ next }: any) => next({ permissions: [] }) }),
      }),
    };
  }
}

export type ClientGrpc = any;