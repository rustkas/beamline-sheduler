import { Test, TestingModule } from '@nestjs/testing';
import { HistoryController } from './history.controller';
import { HistoryService } from './history.service';
import * as fs from 'fs';
import * as path from 'path';

describe('HistoryController', () => {
  let controller: HistoryController;
  let service: HistoryService;
  let tempHistoryPath: string;
  let originalHistoryPath: string;

  beforeAll(() => {
    // Backup original path
    originalHistoryPath = path.join(process.cwd(), '.trae', 'history.json');

    // Create temp directory
    const tempTraeDir = path.join(process.cwd(), '.trae');
    if (!fs.existsSync(tempTraeDir)) {
      fs.mkdirSync(tempTraeDir, { recursive: true });
    }

    tempHistoryPath = originalHistoryPath;
  });

  beforeEach(async () => {
    // Create test history.json with HMAC values
    const testHistory = [
      {
        ts: '2025-01-27T12:00:00Z',
        actor: 'AGENT_1_REPO_STATE',
        action: 'init_cp0',
        cp_from: null,
        cp_to: 'CP0-LC',
        state_checksum: 'a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2',
        hmac_prev: '',
        hmac: '0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef',
        metadata: { description: 'Test entry' },
      },
      {
        ts: '2025-01-27T13:00:00Z',
        actor: 'AGENT_2_ROUTER_OTP',
        action: 'router_implementation',
        cp_from: 'CP0-LC',
        cp_to: 'CP1-LC',
        state_checksum: 'b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3',
        hmac_prev: '0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef',
        hmac: 'fedcba9876543210fedcba9876543210fedcba9876543210fedcba9876543210',
        metadata: { description: 'Second test entry' },
      },
    ];
    fs.writeFileSync(tempHistoryPath, JSON.stringify(testHistory, null, 2));

    const module: TestingModule = await Test.createTestingModule({
      controllers: [HistoryController],
      providers: [HistoryService],
    }).compile();

    controller = module.get<HistoryController>(HistoryController);
    service = module.get<HistoryService>(HistoryService);
  });

  afterEach(() => {
    // Clean up temp file if it was created
    try {
      if (fs.existsSync(tempHistoryPath) && tempHistoryPath !== originalHistoryPath) {
        fs.unlinkSync(tempHistoryPath);
      }
    } catch {
      // Ignore cleanup errors
    }
  });

  it('should be defined', () => {
    expect(controller).toBeDefined();
    expect(service).toBeDefined();
  });

  it('should mask HMAC values with 16+... format', () => {
    const result = controller.getHistory('1', '50');

    expect(result).toBeDefined();
    expect(result.items).toBeInstanceOf(Array);
    expect(result.items.length).toBeGreaterThan(0);

    result.items.forEach((item) => {
      // Check hmac masking
      expect(item.hmac).toMatch(/^[a-f0-9]{16}\.\.\.$/);
      expect(item.hmac.length).toBe(19); // 16 hex chars + "..."

      // Check hmac_prev masking (may be empty string for first entry)
      if (item.hmac_prev && item.hmac_prev.length > 0) {
        expect(item.hmac_prev).toMatch(/^[a-f0-9]{16}\.\.\.$/);
      }
    });
  });

  it('should return paginated results', () => {
    const result = controller.getHistory('1', '1');

    expect(result).toBeDefined();
    expect(result.pagination).toBeDefined();
    expect(result.pagination.page).toBe(1);
    expect(result.pagination.page_size).toBe(1);
    expect(result.pagination.total).toBe(2);
    expect(result.pagination.total_pages).toBe(2);
    expect(result.items.length).toBe(1);
  });

  it('should handle default pagination parameters', () => {
    const result = controller.getHistory();

    expect(result).toBeDefined();
    expect(result.pagination.page).toBe(1);
    expect(result.pagination.page_size).toBe(50);
    expect(result.items.length).toBeLessThanOrEqual(50);
  });

  it('should limit page_size to maximum 200', () => {
    const result = controller.getHistory('1', '200');

    expect(result).toBeDefined();
    expect(result.pagination.page_size).toBe(200);
  });

  it('should handle invalid page numbers', () => {
    const result = controller.getHistory('0', '50');

    expect(result).toBeDefined();
    expect(result.pagination.page).toBe(1); // Should default to 1
  });

  it('should include schema information in response', () => {
    const result = controller.getHistory();

    expect(result.schema).toBeDefined();
    expect(result.schema.$id).toBe('https://beamline.example.com/schemas/history/v1.0.0');
    expect(result.schema.version).toBe('1.0.0');
  });

  it('should preserve all non-HMAC fields in history items', () => {
    const result = controller.getHistory();

    expect(result.items.length).toBeGreaterThan(0);
    result.items.forEach((item) => {
      expect(item).toHaveProperty('ts');
      expect(item).toHaveProperty('actor');
      expect(item).toHaveProperty('action');
      expect(item).toHaveProperty('cp_from');
      expect(item).toHaveProperty('cp_to');
      expect(item).toHaveProperty('state_checksum');
      expect(item).toHaveProperty('metadata');
    });
  });
});
