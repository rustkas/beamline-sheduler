import { Module } from '@nestjs/common';
import { PoliciesController } from './policies.controller';
import { PoliciesService } from './policies.service';
import { RouterAdminModule } from '../router-admin/router-admin.module';
import { ThrottlerModule } from '@nestjs/throttler';
import { AdminApiGuard } from '../auth/admin-api.guard';
import { ConfigModule } from '@nestjs/config';
import adminAuthConfig from '../config/admin-auth.config';

@Module({
  imports: [
    ConfigModule.forFeature(adminAuthConfig),
    RouterAdminModule,
    ThrottlerModule.forRoot([{ ttl: 60, limit: 30 }]),
  ],
  controllers: [PoliciesController],
  providers: [PoliciesService, AdminApiGuard],
})
export class PoliciesModule {}
