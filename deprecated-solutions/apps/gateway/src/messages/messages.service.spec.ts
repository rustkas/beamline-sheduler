import { Test, TestingModule } from '@nestjs/testing';
import { MessagesService } from './messages.service';
import { NotFoundException } from '@nestjs/common';
import { CreateMessageDto } from './dto/create-message.dto';
import { RouterClientService } from '../routes/adapters/router-client.service';

describe('MessagesService', () => {
  let service: MessagesService;
  let routerClient: RouterClientService;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [
        MessagesService,
        {
          provide: RouterClientService,
          useValue: {
            publish: jest.fn().mockResolvedValue({ message_id: 'msg_123', ack: true }),
          },
        },
      ],
    }).compile();

    service = module.get<MessagesService>(MessagesService);
    routerClient = module.get<RouterClientService>(RouterClientService);
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });

  describe('create', () => {
    it('should create a message and return ack (happy path)', async () => {
      const createMessage: CreateMessageDto = {
        tenant_id: 'tenant_abc',
        message_type: 'chat',
        payload: '{"text": "Hello"}',
      };

      const result = await service.create(createMessage);

      expect(result).toBeDefined();
      expect(result.message_id).toBeDefined();
      expect(result.ack_timestamp_ms).toBeDefined();
      expect(result.status).toBe('published');
    });

    it('should store message for later retrieval', async () => {
      const createMessage: CreateMessageDto = {
        tenant_id: 'tenant_abc',
        message_type: 'completion',
        payload: '{"prompt": "Hello"}',
      };

      const ack = await service.create(createMessage);
      const retrieved = await service.findOne(ack.message_id);

      expect(retrieved).toBeDefined();
      expect(retrieved.message_id).toBe(ack.message_id);
      expect(retrieved.tenant_id).toBe('tenant_abc');
    });

    it('should handle message with all optional fields', async () => {
      const createMessage: CreateMessageDto = {
        tenant_id: 'tenant_abc',
        trace_id: 'trace_123',
        message_type: 'embedding',
        payload: '{"text": "Hello"}',
        metadata: { source: 'api' },
      };

      const result = await service.create(createMessage);
      const message = await service.findOne(result.message_id);

      expect(message.trace_id).toBe('trace_123');
      expect(message.metadata).toEqual({ source: 'api' });
    });
  });

  describe('findOne', () => {
    it('should return message by ID (happy path)', async () => {
      const createMessage: CreateMessageDto = {
        tenant_id: 'tenant_abc',
        message_type: 'chat',
        payload: '{"text": "Hello"}',
      };

      const ack = await service.create(createMessage);
      const result = await service.findOne(ack.message_id);

      expect(result).toBeDefined();
      expect(result.message_id).toBe(ack.message_id);
    });

    it('should throw NotFoundException when message not found', async () => {
      await expect(service.findOne('non_existent')).rejects.toThrow(NotFoundException);
      await expect(service.findOne('non_existent')).rejects.toThrow(
        'Message with ID non_existent not found',
      );
    });

    it('should return correct message for multiple messages', async () => {
      const createMessage1: CreateMessageDto = {
        tenant_id: 'tenant_abc',
        message_type: 'chat',
        payload: '{"text": "Hello 1"}',
      };

      const createMessage2: CreateMessageDto = {
        tenant_id: 'tenant_abc',
        message_type: 'chat',
        payload: '{"text": "Hello 2"}',
      };

      const ack1 = await service.create(createMessage1);
      const ack2 = await service.create(createMessage2);

      const result1 = await service.findOne(ack1.message_id);
      const result2 = await service.findOne(ack2.message_id);

      expect(result1.payload).toBe('{"text": "Hello 1"}');
      expect(result2.payload).toBe('{"text": "Hello 2"}');
    });
  });

  describe('findAll', () => {
    it('should return all messages', async () => {
      const createMessage1: CreateMessageDto = {
        tenant_id: 'tenant_abc',
        message_type: 'chat',
        payload: '{"text": "Hello 1"}',
      };

      const createMessage2: CreateMessageDto = {
        tenant_id: 'tenant_abc',
        message_type: 'chat',
        payload: '{"text": "Hello 2"}',
      };

      await service.create(createMessage1);
      await service.create(createMessage2);

      const result = await service.findAll();

      expect(result.length).toBeGreaterThanOrEqual(2);
    });
  });

  describe('error handling', () => {
    it('should handle publish failure gracefully', async () => {
      jest.spyOn(routerClient, 'publish').mockRejectedValueOnce(new Error('Publish failed'));

      const createMessage: CreateMessageDto = {
        tenant_id: 'tenant_abc',
        message_type: 'chat',
        payload: '{"text": "Hello"}',
      };

      const result = await service.create(createMessage);

      // Should still return ack even if publish fails
      expect(result).toBeDefined();
      expect(result.message_id).toBeDefined();
    });
  });
});
