import { Module } from '@nestjs/common';
import { FlowsController } from './flows.controller';
import { FlowsService } from './flows.service';
import { RegistryService } from './registry.service';

@Module({
  controllers: [FlowsController],
  providers: [FlowsService, RegistryService],
  exports: [RegistryService],
})
export class FlowsModule {}
