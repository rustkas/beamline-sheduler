import { ExceptionFilter, Catch, ArgumentsHost, HttpException, HttpStatus } from '@nestjs/common';
import { Response } from 'express';

interface ExceptionResponse {
  message?: string | string[];
  details?: unknown;
}

/**
 * Global HTTP Exception Filter
 * Provides unified error response format
 */
@Catch()
export class HttpExceptionFilter implements ExceptionFilter {
  catch(exception: unknown, host: ArgumentsHost) {
    const ctx = host.switchToHttp();
    const response = ctx.getResponse<Response>();
    const request = ctx.getRequest();

    let status = HttpStatus.INTERNAL_SERVER_ERROR;
    let message = 'Internal server error';
    let details: unknown = undefined;

    if (exception instanceof HttpException) {
      status = exception.getStatus();
      const exceptionResponse = exception.getResponse();
      if (typeof exceptionResponse === 'string') {
        message = exceptionResponse;
      } else if (typeof exceptionResponse === 'object') {
        const responseObj = exceptionResponse as ExceptionResponse;
        message =
          (Array.isArray(responseObj.message)
            ? responseObj.message.join(', ')
            : responseObj.message) ||
          exception.message ||
          'Error occurred';
        details = responseObj.details;
      }
    } else if (exception instanceof Error) {
      message = exception.message;
    }

    const mapCode = (s: number, msg?: string): string => {
      if (s >= 500) return 'internal';
      switch (s) {
        case HttpStatus.BAD_REQUEST:
          if (msg && msg.toLowerCase().includes('policy')) return 'policy_not_found';
          return 'invalid_request';
        case HttpStatus.UNAUTHORIZED:
          return 'unauthorized';
        case HttpStatus.FORBIDDEN:
          if (msg && msg.toLowerCase().includes('policy')) return 'denied';
          return 'forbidden';
        case HttpStatus.NOT_FOUND:
          return 'not_found';
        case HttpStatus.TOO_MANY_REQUESTS:
          return 'rate_limited';
        default:
          return `http_${s}`;
      }
    };

    const traceId = request.headers['x-trace-id'] || request.headers['trace_id'];
    const errorResponse: {
      code: string;
      message: string;
      details?: unknown;
      timestamp: string;
      path: string;
      trace_id?: string;
    } = {
      code: mapCode(status, message),
      message,
      timestamp: new Date().toISOString(),
      path: request.url,
    };

    if (traceId) {
      response.setHeader('X-Trace-Id', String(traceId));
      errorResponse.trace_id = String(traceId);
    }

    if (details) {
      errorResponse.details = details;
    }

    response.status(status).json(errorResponse);
  }
}
