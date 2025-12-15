import { Test, TestingModule } from '@nestjs/testing';
import { INestApplication, ValidationPipe } from '@nestjs/common';
import request from 'supertest';
import { FlowsController } from '../src/flows/flows.controller';
import { FlowsService } from '../src/flows/flows.service';
import { RegistryService } from '../src/flows/registry.service';

describe('FlowsController (e2e)', () => {
  let app: INestApplication;

  beforeEach(async () => {
    const moduleFixture: TestingModule = await Test.createTestingModule({
      controllers: [FlowsController],
      providers: [FlowsService, RegistryService],
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

  describe('POST /api/v1/flows/validate', () => {
    it('should return valid=true for correct flow', async () => {
      const validFlow = {
        id: 'flow_1',
        version: 'v1',
        steps: [
          { id: 's1', type: 'http.request', inputs: { url: 'https://example.com' } },
        ],
        edges: [],
      };

      return request(app.getHttpServer())
        .post('/api/v1/flows/validate')
        .send({ definition: validFlow })
        .expect(200)
        .expect((res: any) => {
          expect(res.body.valid).toBe(true);
          expect(res.body.errors).toEqual([]);
        });
    });

    it('should return valid=false for missing id', async () => {
      const invalidFlow = {
        version: 'v1',
        steps: [],
        edges: [],
      } as any;

      return request(app.getHttpServer())
        .post('/api/v1/flows/validate')
        .send({ definition: invalidFlow })
        .expect(200)
        .expect((res: any) => {
          expect(res.body.valid).toBe(false);
          expect(res.body.errors.length).toBeGreaterThan(0);
        });
    });
  });
});
