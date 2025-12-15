import { Module, Global } from '@nestjs/common';
import { ConfigModule } from '@nestjs/config';
import { RouterAdminClientService } from './router-admin-client.service';
import routerAdminConfig from '../config/router-admin.config';

/**
 * Router Admin Module
 * Provides Router Admin gRPC client for Gateway
 */
@Global()
@Module({
  imports: [ConfigModule.forFeature(routerAdminConfig)],
  providers: [RouterAdminClientService],
  exports: [RouterAdminClientService],
})
export class RouterAdminModule {}

