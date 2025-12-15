import { Injectable, Logger, OnModuleInit } from '@nestjs/common';
let NestMicro: any;
try {
  // eslint-disable-next-line @typescript-eslint/no-var-requires
  NestMicro = require('@nestjs/microservices');
} catch {}
type ClientGrpc = any;

interface GrpcHealthService {
  check(request: { service?: string }): any;
}

@Injectable()
export class RouterGrpcHealthService implements OnModuleInit {
  private readonly logger = new Logger(RouterGrpcHealthService.name);
  private client: ClientGrpc;
  private healthService: GrpcHealthService;

  async onModuleInit() {
    const grpcOptions: any = {
      transport: NestMicro ? NestMicro.Transport.GRPC : undefined,
      options: {
        package: 'grpc.health.v1',
        protoPath: 'proto/grpc/health/v1/health.proto',
        url: process.env.ROUTER_GRPC_URL || 'router:9000',
        loader: { keepCase: true, longs: String, enums: String, defaults: true, oneofs: true },
      },
    };
    this.client = (NestMicro ? NestMicro.ClientProxyFactory.create(grpcOptions) : undefined) as ClientGrpc;
    this.healthService = (this.client as any)?.getService?.('Health');
  }

  async isServing(): Promise<boolean> {
    try {
      const resp = await new Promise<any>((resolve, reject) => {
        const obs = this.healthService.check({ service: '' });
        if (typeof obs?.subscribe === 'function') {
          obs.subscribe({ next: resolve, error: reject });
        } else {
          resolve(obs);
        }
      });
      const status = resp?.status || resp?.['status'];
      const serving = status === 'SERVING' || status === 1;
      if (!serving) this.logger.warn(`Router gRPC health not serving: ${status}`);
      return serving;
    } catch (e: any) {
      this.logger.warn(`Router gRPC health check failed: ${e?.message || e}`);
      return false;
    }
  }
}