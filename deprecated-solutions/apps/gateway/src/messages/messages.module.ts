import { Module } from '@nestjs/common';
import { MessagesController } from './messages.controller';
import { MessagesService } from './messages.service';
import { RoutesModule } from '../routes/routes.module';

@Module({
  imports: [RoutesModule], // Import RoutesModule to get RouterClientService
  controllers: [MessagesController],
  providers: [MessagesService],
})
export class MessagesModule {}
