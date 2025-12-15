import { Injectable } from '@nestjs/common';
import * as fs from 'fs';
import * as path from 'path';
import Ajv from 'ajv';
import addFormats from 'ajv-formats';

export interface HistoryItem {
  ts: string;
  actor: string;
  action: string;
  cp_from: string | null;
  cp_to: string;
  state_checksum: string;
  hmac_prev: string;
  hmac: string;
  metadata?: Record<string, unknown>;
}

export interface HistoryResponse {
  items: HistoryItem[];
  pagination: {
    page: number;
    page_size: number;
    total: number;
    total_pages: number;
  };
  schema: {
    $id: string;
    version: string;
  };
}

/**
 * Mask HMAC value: show first 16 hex characters + "..."
 * Empty strings return empty string (for first entry hmac_prev)
 */
function maskHmac(hmac: string): string {
  if (!hmac) {
    return '';
  }
  if (hmac.length < 16) {
    return '[REDACTED]';
  }
  return hmac.substring(0, 16) + '...';
}

@Injectable()
export class HistoryService {
  private readonly ajv: Ajv;

  constructor() {
    this.ajv = new Ajv({ allErrors: true, strict: false });
    addFormats(this.ajv);
  }

  /**
   * Load and validate history from .trae/history.json
   */
  loadHistory(): HistoryItem[] {
    const historyPath = path.join(process.cwd(), '.trae', 'history.json');
    try {
      const content = fs.readFileSync(historyPath, 'utf-8');
      const history = JSON.parse(content) as HistoryItem[];

      // Validate against HISTORY.schema.json
      const schemaPath = path.join(process.cwd(), 'docs', 'HISTORY.schema.json');
      try {
        const schema = JSON.parse(fs.readFileSync(schemaPath, 'utf-8'));
        const validate = this.ajv.compile(schema);
        if (!validate(history)) {
          // Log validation errors but don't fail - return history anyway
          console.warn('History validation errors:', validate.errors);
        }
      } catch {
        // Schema not found or invalid - skip validation
        console.warn('HISTORY.schema.json not found or invalid, skipping validation');
      }

      return history;
    } catch (error) {
      console.error('Failed to load history:', error);
      return [];
    }
  }

  /**
   * Get paginated history with HMAC masking
   */
  getHistory(page: number = 1, pageSize: number = 50): HistoryResponse {
    const allHistory = this.loadHistory();
    const total = allHistory.length;

    // Limit pageSize to maximum 200
    const limitedPageSize = Math.min(200, Math.max(1, pageSize));
    const totalPages = Math.ceil(total / limitedPageSize);
    const start = (page - 1) * limitedPageSize;
    const end = start + limitedPageSize;

    // Mask HMAC values in items
    const items = allHistory.slice(start, end).map((item) => ({
      ...item,
      hmac_prev: maskHmac(item.hmac_prev),
      hmac: maskHmac(item.hmac),
    }));

    return {
      items,
      pagination: {
        page,
        page_size: limitedPageSize,
        total,
        total_pages: totalPages,
      },
      schema: {
        $id: 'https://beamline.example.com/schemas/history/v1.0.0',
        version: '1.0.0',
      },
    };
  }
}
