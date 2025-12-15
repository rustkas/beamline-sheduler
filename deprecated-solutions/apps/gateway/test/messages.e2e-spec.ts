import { Test, TestingModule } from '@nestjs/testing';
import { INestApplication, ValidationPipe } from '@nestjs/common';
import request from 'supertest';
import { MessagesController } from '../src/messages/messages.controller';
import { MessagesService } from '../src/messages/messages.service';
import { RouterClientService } from '../src/routes/adapters/router-client.service';

describe('MessagesController (e2e)', () => {
  let app: INestApplication;

  beforeEach(async () => {
    const moduleFixture: TestingModule = await Test.createTestingModule({
      controllers: [MessagesController],
      providers: [MessagesService, RouterClientService],
    }).compile();

    app = moduleFixture.createNestApplication();
    app.useGlobalPipes(
      new ValidationPipe({
        whitelist: true,
        forbidNonWhitelisted: true,
        transform: true,
      }),
    );
    await app.init();
  });

  afterEach(async () => {
    await app.close();
  });

  describe('POST /api/v1/messages', () => {
    it('should create a message (happy path)', async () => {
      const createMessage = {
        tenant_id: 'tenant_fixture',
        message_type: 'chat',
        payload: '{"text": "Hello, world!"}',
      };
      const res = await request(app.getHttpServer())
        .post('/api/v1/messages')
        .send(createMessage)
        .expect(201);
      expect(res.body).toHaveProperty('message_id');
      expect(typeof res.body.message_id).toBe('string');
      expect(res.body.status).toBe('published');
      expect(typeof res.body.ack_timestamp_ms).toBe('number');
    });

    it('should create message with optional fields and persist them', async () => {
      const createMessageWithOptional = {
        tenant_id: 'tenant_fixture',
        trace_id: 'trace_fixture_001',
        message_type: 'completion',
        payload: '{"prompt": "Hello"}',
        metadata: { source: 'api', version: '1.0' },
      };
      const ack = await request(app.getHttpServer())
        .post('/api/v1/messages')
        .send(createMessageWithOptional)
        .expect(201);
      const messageId = ack.body.message_id;
      const getRes = await request(app.getHttpServer())
        .get(`/api/v1/messages/${messageId}`)
        .expect(200);
      expect(getRes.body.trace_id).toBe(createMessageWithOptional.trace_id);
      expect(getRes.body.metadata).toEqual(createMessageWithOptional.metadata);
      expect(getRes.body.message_type).toBe('completion');
      expect(getRes.body.payload).toBe('{"prompt": "Hello"}');
      expect(typeof getRes.body.timestamp_ms).toBe('number');
    });

    it('should return 400 for invalid message', () => {
      return request(app.getHttpServer())
        .post('/api/v1/messages')
        .send({
          // Missing required fields for CreateMessageDto
          // tenant_id, message_type, payload are required
        })
        .expect(400);
    });

    it('should return 400 when message_type is missing', () => {
      return request(app.getHttpServer())
        .post('/api/v1/messages')
        .send({
          tenant_id: 'tenant_abc',
          payload: '{"text": "Hello"}',
        })
        .expect(400);
    });

    it('should return 400 when tenant_id is missing', () => {
      return request(app.getHttpServer())
        .post('/api/v1/messages')
        .send({
          message_type: 'chat',
          payload: '{"text": "Hello"}',
        })
        .expect(400);
    });

    it('should return 400 when payload is missing', () => {
      return request(app.getHttpServer())
        .post('/api/v1/messages')
        .send({
          tenant_id: 'tenant_abc',
          message_type: 'chat',
        })
        .expect(400);
    });
  });

  describe('GET /api/v1/messages/:messageId', () => {
    it('should return message by ID (happy path)', async () => {
      // First create a message
      const createMessage = {
        tenant_id: 'tenant_fixture',
        message_type: 'chat',
        payload: '{"text": "Hello, world!"}',
      };
      const ack = await request(app.getHttpServer())
        .post('/api/v1/messages')
        .send(createMessage)
        .expect(201);

      const createdMessageId = ack.body.message_id;

      // Then retrieve it
      return request(app.getHttpServer())
        .get(`/api/v1/messages/${createdMessageId}`)
        .expect(200)
        .expect((res: any) => {
          expect(res.body.message_id).toBe(createdMessageId);
          expect(res.body.tenant_id).toBe('tenant_fixture');
        });
    });

    it('should return 404 when message not found', () => {
      return request(app.getHttpServer())
        .get('/api/v1/messages/non_existent')
        .expect(404);
    });

    it('should return correct message for multiple messages', async () => {
      const create1 = {
        tenant_id: 'tenant_fixture',
        message_type: 'chat',
        payload: '{"text": "Hello 1"}',
      };
      const create2 = {
        tenant_id: 'tenant_fixture',
        message_type: 'chat',
        payload: '{"text": "Hello 2"}',
      };

      const ack1 = await request(app.getHttpServer())
        .post('/api/v1/messages')
        .send(create1)
        .expect(201);

      const ack2 = await request(app.getHttpServer())
        .post('/api/v1/messages')
        .send(create2)
        .expect(201);

      const id1 = ack1.body.message_id;
      const id2 = ack2.body.message_id;

      const response1 = await request(app.getHttpServer())
        .get(`/api/v1/messages/${id1}`)
        .expect(200);

      const response2 = await request(app.getHttpServer())
        .get(`/api/v1/messages/${id2}`)
        .expect(200);

      expect(response1.body.payload).toBe('{"text": "Hello 1"}');
      expect(response2.body.payload).toBe('{"text": "Hello 2"}');
    });
  });
});
