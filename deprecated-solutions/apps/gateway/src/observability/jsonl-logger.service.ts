/**
 * JSONL Logger Service for Gateway
 * Emits structured JSON logs in NDJSON format to .windsurf/reports/gateway.logs.jsonl
 * Follows config/observability/logging.json schema
 */

import { Injectable, LoggerService as NestLoggerService } from '@nestjs/common';
import * as fs from 'fs';
import * as path from 'path';

export interface LogEntry {
  timestamp: string;
  level: 'ERROR' | 'WARN' | 'INFO' | 'DEBUG';
  component: string;
  message: string;
  context?: Record<string, unknown>;
  tenant_id?: string;
  trace_id?: string;
  error?: {
    type: string;
    message: string;
    stack?: string;
  };
}

@Injectable()
export class JsonlLoggerService implements NestLoggerService {
  private readonly logFilePath: string;
  private readonly piiFields: string[] = [
    'password',
    'api_key',
    'secret',
    'token',
    'access_token',
    'refresh_token',
    'authorization',
    'credit_card',
    'ssn',
    'email',
    'phone',
  ];

  constructor() {
    // Ensure .windsurf/reports directory exists
    const reportsDir = path.join(process.cwd(), '.windsurf', 'reports');
    if (!fs.existsSync(reportsDir)) {
      fs.mkdirSync(reportsDir, { recursive: true });
    }
    this.logFilePath = path.join(reportsDir, 'gateway.logs.jsonl');
  }

  /**
   * Sanitize context by replacing PII fields with [REDACTED]
   */
  private sanitizeContext(context: Record<string, unknown>): Record<string, unknown> {
    const sanitized: Record<string, unknown> = {};
    for (const [key, value] of Object.entries(context)) {
      const keyLower = key.toLowerCase();
      if (this.piiFields.some((field) => keyLower.includes(field))) {
        sanitized[key] = '[REDACTED]';
      } else if (
        typeof value === 'string' &&
        this.piiFields.some((field) => value.toLowerCase().includes(field))
      ) {
        // Check if value contains PII patterns (e.g., "Bearer token123")
        sanitized[key] = '[REDACTED]';
      } else if (typeof value === 'object' && value !== null && !Array.isArray(value)) {
        sanitized[key] = this.sanitizeContext(value as Record<string, unknown>);
      } else if (Array.isArray(value)) {
        sanitized[key] = value.map((item) =>
          typeof item === 'object' && item !== null
            ? this.sanitizeContext(item as Record<string, unknown>)
            : item,
        );
      } else {
        sanitized[key] = value;
      }
    }
    return sanitized;
  }

  /**
   * Write log entry to JSONL file
   */
  private writeLog(entry: LogEntry): void {
    try {
      const line = JSON.stringify(entry) + '\n';
      fs.appendFileSync(this.logFilePath, line, { encoding: 'utf-8' });
    } catch (error) {
      // Fallback to console if file write fails
      console.error('[JsonlLogger] Failed to write log:', error);
      console.log(JSON.stringify(entry));
    }
  }

  /**
   * Create log entry with required fields
   */
  private createLogEntry(
    level: LogEntry['level'],
    message: string,
    context?: Record<string, unknown>,
    error?: Error,
  ): LogEntry {
    const entry: LogEntry = {
      timestamp: new Date().toISOString(),
      level,
      component: 'gateway',
      message,
    };

    if (context) {
      entry.context = this.sanitizeContext(context);
      // Extract tenant_id and trace_id from context if present
      if (context.tenant_id) {
        entry.tenant_id = String(context.tenant_id);
      }
      if (context.trace_id) {
        entry.trace_id = String(context.trace_id);
      }
    }

    if (error) {
      entry.error = {
        type: error.constructor.name,
        message: error.message,
        stack: level === 'DEBUG' ? error.stack : undefined,
      };
    }

    return entry;
  }

  log(message: string, context?: Record<string, unknown>): void {
    this.writeLog(this.createLogEntry('INFO', message, context));
  }

  error(message: string, trace?: string, context?: Record<string, unknown>): void {
    const error = trace ? new Error(trace) : undefined;
    this.writeLog(this.createLogEntry('ERROR', message, context, error));
  }

  warn(message: string, context?: Record<string, unknown>): void {
    this.writeLog(this.createLogEntry('WARN', message, context));
  }

  debug(message: string, context?: Record<string, unknown>): void {
    this.writeLog(this.createLogEntry('DEBUG', message, context));
  }

  verbose(message: string, context?: Record<string, unknown>): void {
    this.writeLog(this.createLogEntry('DEBUG', message, context));
  }
}
