import { Module } from '@nestjs/common';
import { RouterAdminModule } from '../router-admin/router-admin.module';
import { RouterAuthService } from './router-auth.service';
import { UnifiedAuthContextService } from './unified-auth-context.service';
import { RbacMappingService } from './rbac-mapping.service';
import { RouterAdminGrpcService } from './router-admin-grpc.service';
import { ObservabilityModule } from '../observability/observability.module';

/**
 * Auth Module
 * Provides authentication and authorization services
 * Integrates with Router Admin gRPC for unified auth
 */
@Module({
  imports: [
    RouterAdminModule, // Router Admin gRPC client
    ObservabilityModule, // For telemetry
  ],
  providers: [
    RouterAuthService,
    UnifiedAuthContextService,
    RbacMappingService,
    RouterAdminGrpcService, // Legacy service (for backward compatibility)
  ],
  exports: [
    RouterAuthService,
    UnifiedAuthContextService,
    RbacMappingService,
  ],
})
export class AuthModule {}

