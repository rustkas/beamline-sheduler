/**
 * Logger Service for NestJS Gateway
 * Provides structured JSON logging with PII filtering
 * Stub implementation - replace with nestjs-pino
 */

import { Injectable } from '@nestjs/common';
// import { PinoLogger } from 'nestjs-pino';

@Injectable()
export class LoggerService {
  // TODO: Inject PinoLogger
  // constructor(private readonly logger: PinoLogger) {}

  private sanitizeContext(context: Record<string, unknown>): Record<string, unknown> {
    const piiFields = [
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

    const sanitized: Record<string, unknown> = {};
    for (const [key, value] of Object.entries(context)) {
      if (piiFields.includes(key.toLowerCase())) {
        sanitized[key] = '[REDACTED]';
      } else if (typeof value === 'object' && value !== null && !Array.isArray(value)) {
        sanitized[key] = this.sanitizeContext(value as Record<string, unknown>);
      } else {
        sanitized[key] = value;
      }
    }
    return sanitized;
  }

  error(message: string, context?: Record<string, unknown>): void {
    // TODO: this.logger.error({ ...this.sanitizeContext(context || {}) }, message);
    console.error(`[ERROR] ${message}`, this.sanitizeContext(context || {}));
  }

  warn(message: string, context?: Record<string, unknown>): void {
    // TODO: this.logger.warn({ ...this.sanitizeContext(context || {}) }, message);
    console.warn(`[WARN] ${message}`, this.sanitizeContext(context || {}));
  }

  info(message: string, context?: Record<string, unknown>): void {
    // TODO: this.logger.info({ ...this.sanitizeContext(context || {}) }, message);
    console.log(`[INFO] ${message}`, this.sanitizeContext(context || {}));
  }

  debug(message: string, context?: Record<string, unknown>): void {
    // TODO: this.logger.debug({ ...this.sanitizeContext(context || {}) }, message);
    console.debug(`[DEBUG] ${message}`, this.sanitizeContext(context || {}));
  }
}
