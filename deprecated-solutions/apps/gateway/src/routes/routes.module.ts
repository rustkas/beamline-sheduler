import { Module } from '@nestjs/common';
import { RoutesController } from './routes.controller';
import { RoutesService } from './routes.service';
import { RouterClientService } from './adapters/router-client.service';
import { IdempotencyService } from '../common/services/idempotency.service';
import { StickyService } from '../common/services/sticky.service';
import { ObservabilityModule } from '../observability/observability.module';

@Module({
  imports: [ObservabilityModule],
  controllers: [RoutesController],
  providers: [RoutesService, RouterClientService, IdempotencyService, StickyService],
  exports: [RouterClientService], // Export for use in MessagesModule
})
export class RoutesModule {}
