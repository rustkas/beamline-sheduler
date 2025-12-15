import { Module } from '@nestjs/common';
import { ConfigModule } from '@nestjs/config';
import { RoutesModule } from './routes/routes.module';
import { MessagesModule } from './messages/messages.module';
import { PoliciesModule } from './policies/policies.module';
import { HealthModule } from './health/health.module';
import { FlowsModule } from './flows/flows.module';
import { ObservabilityModule } from './observability/observability.module';
import { ThrottlerModule } from '@nestjs/throttler';
import { APP_GUARD } from '@nestjs/core';
import { RateLimitGuard } from './common/guards/rate-limit.guard';
import { rateLimitConfig } from './config/rate-limit.config';
import { StatusModule } from './status/status.module';
import { HistoryModule } from './history/history.module';
import { DevStateModule } from './devstate/devstate.module';

import { RouterAdminModule } from './router-admin/router-admin.module';
import { AuthModule } from './auth/auth.module';
import { MetricsController } from './metrics/metrics.controller';
import { ResultsController } from './results/results.controller';
import { ResultsStreamService } from './results/results-stream.service';
import { NatsResultsSubscriber } from './results/nats-results.subscriber';
import { IdempotencyService } from './common/services/idempotency.service';

@Module({
  imports: [
    ConfigModule.forRoot({
      isGlobal: true,
      envFilePath: ['.env.local', '.env'],
    }),
    ThrottlerModule.forRoot({
      throttlers: [
        {
          ttl: rateLimitConfig.ttlSeconds * 1000,
          limit: rateLimitConfig.globalLimit,
        },
      ],
    }),
    ObservabilityModule, // JSONL logging (OBS-1)
    RoutesModule,
    MessagesModule,
    PoliciesModule,
    HealthModule,
    FlowsModule,
    StatusModule,
    HistoryModule,
    DevStateModule,
    RouterAdminModule, // Router Admin gRPC client
    AuthModule, // Authentication and authorization
  ],
  controllers: [MetricsController, ResultsController],
  providers: [
    {
      provide: APP_GUARD,
      useClass: RateLimitGuard,
    },
    ResultsStreamService,
    IdempotencyService,
    NatsResultsSubscriber,
  ],
})
export class AppModule {}
