import { Module } from '@nestjs/common';
import { DevStateController } from './devstate.controller';

@Module({
  controllers: [DevStateController],
})
export class DevStateModule {}
