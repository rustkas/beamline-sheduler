import { Module } from '@nestjs/common';
import { HealthController } from './health.controller';
import { RouterGrpcHealthService } from './router-grpc-health.service';

@Module({
  controllers: [HealthController],
  providers: [RouterGrpcHealthService],
  exports: [RouterGrpcHealthService],
})
export class HealthModule {}
