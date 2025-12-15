import { Injectable, NotFoundException } from '@nestjs/common';
import { MessageDto } from '../routes/dto/message.dto';
import { CreateMessageDto } from './dto/create-message.dto';
import { MessageAckDto } from './dto/message-ack.dto';
import { RouterClientService } from '../routes/adapters/router-client.service';

@Injectable()
export class MessagesService {
  private messages: Map<string, MessageDto> = new Map();

  constructor(private readonly routerClient: RouterClientService) {}

  /**
   * Create and publish a new message
   * Publishes to Router/NATS (or simulates) and returns acknowledgment
   */
  async create(createMessage: CreateMessageDto): Promise<MessageAckDto> {
    // Generate message ID
    const messageId = `msg_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;

    // Create message DTO
    const message: MessageDto = {
      message_id: messageId,
      tenant_id: createMessage.tenant_id,
      trace_id: createMessage.trace_id,
      message_type: createMessage.message_type,
      payload: createMessage.payload,
      metadata: createMessage.metadata,
      timestamp_ms: Date.now(),
    };

    // Store message
    this.messages.set(messageId, message);

    // Publish to Router/NATS (or simulate)
    try {
      const ack = await this.routerClient.publish(message);
      if (!ack.ack) {
        throw new Error('Failed to publish message');
      }
    } catch (error) {
      // Log error but don't fail - message is stored
      console.error('Failed to publish message:', error);
    }

    // Return acknowledgment
    return {
      message_id: messageId,
      ack_timestamp_ms: Date.now(),
      status: 'published',
    };
  }

  /**
   * Get all messages
   */
  async findAll(): Promise<MessageDto[]> {
    return Array.from(this.messages.values());
  }

  /**
   * Get message by ID
   */
  async findOne(messageId: string): Promise<MessageDto> {
    const message = this.messages.get(messageId);
    if (!message) {
      throw new NotFoundException(`Message with ID ${messageId} not found`);
    }
    return message;
  }
}
