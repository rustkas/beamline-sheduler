import { Controller, Get } from '@nestjs/common';
import { RouterGrpcHealthService } from './router-grpc-health.service';
import { ApiTags, ApiOperation, ApiResponse } from '@nestjs/swagger';

@ApiTags('health')
@Controller('health')
export class HealthController {
  constructor(private readonly routerGrpcHealth: RouterGrpcHealthService) {}
  @Get()
  @ApiOperation({ summary: 'Health check endpoint' })
  @ApiResponse({ status: 200, description: 'Service is healthy' })
  async check() {
    const serving = await this.routerGrpcHealth.isServing();
    return {
      status: serving ? 'ok' : 'degraded',
      timestamp: new Date().toISOString(),
      service: 'beamline-gateway',
      version: '1.0.0',
      router_grpc_serving: serving,
    };
  }

  @Get('ready')
  @ApiOperation({ summary: 'Readiness check endpoint' })
  @ApiResponse({ status: 200, description: 'Service is ready' })
  ready() {
    return {
      status: 'ready',
      timestamp: new Date().toISOString(),
    };
  }

  @Get('live')
  @ApiOperation({ summary: 'Liveness check endpoint' })
  @ApiResponse({ status: 200, description: 'Service is alive' })
  live() {
    return {
      status: 'alive',
      timestamp: new Date().toISOString(),
    };
  }
}
