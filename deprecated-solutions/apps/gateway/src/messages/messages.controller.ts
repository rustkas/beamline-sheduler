import { Controller, Get, Post, Body, Param, HttpCode, HttpStatus } from '@nestjs/common';
import { ApiTags, ApiOperation, ApiResponse } from '@nestjs/swagger';
import { MessagesService } from './messages.service';
import { MessageDto } from '../routes/dto/message.dto';
import { CreateMessageDto } from './dto/create-message.dto';
import { MessageAckDto } from './dto/message-ack.dto';
import { Throttle } from '@nestjs/throttler';
import { rateLimitConfig } from '../config/rate-limit.config';

@ApiTags('messages')
@Controller('api/v1/messages')
export class MessagesController {
  constructor(private readonly messagesService: MessagesService) {}

  @Post()
  @HttpCode(HttpStatus.CREATED)
  @Throttle({
    default: {
      limit: rateLimitConfig.messagesLimit,
      ttl: rateLimitConfig.ttlSeconds * 1000,
    },
  })
  @ApiOperation({ summary: 'Create and publish a new message' })
  @ApiResponse({
    status: 201,
    description: 'Message created and published successfully',
    type: MessageAckDto,
  })
  @ApiResponse({ status: 400, description: 'Invalid request' })
  async create(@Body() createMessage: CreateMessageDto): Promise<MessageAckDto> {
    return this.messagesService.create(createMessage);
  }

  @Get()
  @ApiOperation({ summary: 'Get all messages' })
  @ApiResponse({
    status: 200,
    description: 'Messages retrieved successfully',
    type: [MessageDto],
  })
  async findAll(): Promise<MessageDto[]> {
    return this.messagesService.findAll();
  }

  @Get(':messageId')
  @ApiOperation({ summary: 'Get message by ID' })
  @ApiResponse({
    status: 200,
    description: 'Message retrieved successfully',
    type: MessageDto,
  })
  @ApiResponse({ status: 404, description: 'Message not found' })
  async findOne(@Param('messageId') messageId: string): Promise<MessageDto> {
    return this.messagesService.findOne(messageId);
  }
}
