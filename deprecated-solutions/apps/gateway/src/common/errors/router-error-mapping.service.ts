import { Injectable, HttpException, HttpStatus } from '@nestjs/common';
import { TracingService } from '../../observability/tracing.service';

export enum RouterErrorCode {
  // Authentication errors
  AUTH_INVALID_TOKEN = 'AUTH_INVALID_TOKEN',
  AUTH_EXPIRED_TOKEN = 'AUTH_EXPIRED_TOKEN',
  AUTH_INSUFFICIENT_PERMISSIONS = 'AUTH_INSUFFICIENT_PERMISSIONS',
  AUTH_TENANT_NOT_FOUND = 'AUTH_TENANT_NOT_FOUND',
  AUTH_USER_NOT_FOUND = 'AUTH_USER_NOT_FOUND',
  AUTH_API_KEY_INVALID = 'AUTH_API_KEY_INVALID',
  AUTH_API_KEY_EXPIRED = 'AUTH_API_KEY_EXPIRED',
  
  // Authorization errors
  AUTHZ_ACCESS_DENIED = 'AUTHZ_ACCESS_DENIED',
  AUTHZ_RESOURCE_NOT_FOUND = 'AUTHZ_RESOURCE_NOT_FOUND',
  AUTHZ_POLICY_VIOLATION = 'AUTHZ_POLICY_VIOLATION',
  AUTHZ_QUOTA_EXCEEDED = 'AUTHZ_QUOTA_EXCEEDED',
  AUTHZ_RATE_LIMIT_EXCEEDED = 'AUTHZ_RATE_LIMIT_EXCEEDED',
  
  // Routing errors
  ROUTE_NOT_FOUND = 'ROUTE_NOT_FOUND',
  ROUTE_PROVIDER_UNAVAILABLE = 'ROUTE_PROVIDER_UNAVAILABLE',
  ROUTE_PROVIDER_ERROR = 'ROUTE_PROVIDER_ERROR',
  ROUTE_TIMEOUT = 'ROUTE_TIMEOUT',
  ROUTE_CIRCUIT_BREAKER_OPEN = 'ROUTE_CIRCUIT_BREAKER_OPEN',
  
  // Provider errors
  PROVIDER_AUTHENTICATION_FAILED = 'PROVIDER_AUTHENTICATION_FAILED',
  PROVIDER_INSUFFICIENT_CREDITS = 'PROVIDER_INSUFFICIENT_CREDITS',
  PROVIDER_RATE_LIMITED = 'PROVIDER_RATE_LIMITED',
  PROVIDER_MODEL_NOT_AVAILABLE = 'PROVIDER_MODEL_NOT_AVAILABLE',
  PROVIDER_CONTENT_POLICY_VIOLATION = 'PROVIDER_CONTENT_POLICY_VIOLATION',
  
  // Validation errors
  VALIDATION_INVALID_REQUEST = 'VALIDATION_INVALID_REQUEST',
  VALIDATION_MISSING_REQUIRED_FIELD = 'VALIDATION_MISSING_REQUIRED_FIELD',
  VALIDATION_INVALID_FIELD_VALUE = 'VALIDATION_INVALID_FIELD_VALUE',
  VALIDATION_REQUEST_TOO_LARGE = 'VALIDATION_REQUEST_TOO_LARGE',
  
  // System errors
  SYSTEM_INTERNAL_ERROR = 'SYSTEM_INTERNAL_ERROR',
  SYSTEM_SERVICE_UNAVAILABLE = 'SYSTEM_SERVICE_UNAVAILABLE',
  SYSTEM_DATABASE_ERROR = 'SYSTEM_DATABASE_ERROR',
  SYSTEM_CACHE_ERROR = 'SYSTEM_CACHE_ERROR',
  
  // Network errors
  NETWORK_CONNECTION_TIMEOUT = 'NETWORK_CONNECTION_TIMEOUT',
  NETWORK_CONNECTION_REFUSED = 'NETWORK_CONNECTION_REFUSED',
  NETWORK_DNS_RESOLUTION_FAILED = 'NETWORK_DNS_RESOLUTION_FAILED',
  NETWORK_TLS_ERROR = 'NETWORK_TLS_ERROR',
  
  // Protocol errors
  PROTOCOL_UNSUPPORTED_VERSION = 'PROTOCOL_UNSUPPORTED_VERSION',
  PROTOCOL_MESSAGE_TOO_LARGE = 'PROTOCOL_MESSAGE_TOO_LARGE',
  PROTOCOL_SERIALIZATION_ERROR = 'PROTOCOL_SERIALIZATION_ERROR',
  PROTOCOL_DESERIALIZATION_ERROR = 'PROTOCOL_DESERIALIZATION_ERROR',
}

export interface RouterError {
  code: RouterErrorCode;
  message: string;
  details?: Record<string, any>;
  trace_id?: string;
  span_id?: string;
  timestamp?: string;
  retryable?: boolean;
  retry_after?: number; // seconds
  suggested_action?: string;
}

export interface GatewayErrorResponse {
  error: {
    code: string;
    message: string;
    type: string;
    param?: string;
    trace_id?: string;
  };
  trace_id: string;
  timestamp: string;
}

export interface ErrorMapping {
  router_code: RouterErrorCode;
  http_status: HttpStatus;
  gateway_code: string;
  message_template: string;
  retryable: boolean;
  log_level: 'error' | 'warn' | 'info' | 'debug';
  metrics_label: string;
}

@Injectable()
export class RouterErrorMappingService {
  private readonly errorMappings: Map<RouterErrorCode, ErrorMapping> = new Map([
    // Authentication errors
    {
      router_code: RouterErrorCode.AUTH_INVALID_TOKEN,
      http_status: HttpStatus.UNAUTHORIZED,
      gateway_code: 'invalid_auth_token',
      message_template: 'Invalid authentication token provided',
      retryable: false,
      log_level: 'warn',
      metrics_label: 'auth_invalid_token'
    },
    {
      router_code: RouterErrorCode.AUTH_EXPIRED_TOKEN,
      http_status: HttpStatus.UNAUTHORIZED,
      gateway_code: 'auth_token_expired',
      message_template: 'Authentication token has expired',
      retryable: false,
      log_level: 'warn',
      metrics_label: 'auth_token_expired'
    },
    {
      router_code: RouterErrorCode.AUTH_INSUFFICIENT_PERMISSIONS,
      http_status: HttpStatus.FORBIDDEN,
      gateway_code: 'insufficient_permissions',
      message_template: 'Insufficient permissions to access this resource',
      retryable: false,
      log_level: 'warn',
      metrics_label: 'insufficient_permissions'
    },
    {
      router_code: RouterErrorCode.AUTH_TENANT_NOT_FOUND,
      http_status: HttpStatus.NOT_FOUND,
      gateway_code: 'tenant_not_found',
      message_template: 'Tenant not found',
      retryable: false,
      log_level: 'warn',
      metrics_label: 'tenant_not_found'
    },
    {
      router_code: RouterErrorCode.AUTH_USER_NOT_FOUND,
      http_status: HttpStatus.NOT_FOUND,
      gateway_code: 'user_not_found',
      message_template: 'User not found',
      retryable: false,
      log_level: 'warn',
      metrics_label: 'user_not_found'
    },
    {
      router_code: RouterErrorCode.AUTH_API_KEY_INVALID,
      http_status: HttpStatus.UNAUTHORIZED,
      gateway_code: 'invalid_api_key',
      message_template: 'Invalid API key provided',
      retryable: false,
      log_level: 'warn',
      metrics_label: 'invalid_api_key'
    },
    {
      router_code: RouterErrorCode.AUTH_API_KEY_EXPIRED,
      http_status: HttpStatus.UNAUTHORIZED,
      gateway_code: 'api_key_expired',
      message_template: 'API key has expired',
      retryable: false,
      log_level: 'warn',
      metrics_label: 'api_key_expired'
    },

    // Authorization errors
    {
      router_code: RouterErrorCode.AUTHZ_ACCESS_DENIED,
      http_status: HttpStatus.FORBIDDEN,
      gateway_code: 'access_denied',
      message_template: 'Access denied to this resource',
      retryable: false,
      log_level: 'warn',
      metrics_label: 'access_denied'
    },
    {
      router_code: RouterErrorCode.AUTHZ_RESOURCE_NOT_FOUND,
      http_status: HttpStatus.NOT_FOUND,
      gateway_code: 'resource_not_found',
      message_template: 'Resource not found',
      retryable: false,
      log_level: 'warn',
      metrics_label: 'resource_not_found'
    },
    {
      router_code: RouterErrorCode.AUTHZ_POLICY_VIOLATION,
      http_status: HttpStatus.FORBIDDEN,
      gateway_code: 'policy_violation',
      message_template: 'Request violates policy: {details}',
      retryable: false,
      log_level: 'warn',
      metrics_label: 'policy_violation'
    },
    {
      router_code: RouterErrorCode.AUTHZ_QUOTA_EXCEEDED,
      http_status: HttpStatus.TOO_MANY_REQUESTS,
      gateway_code: 'quota_exceeded',
      message_template: 'Quota exceeded. Retry after {retry_after} seconds',
      retryable: true,
      log_level: 'warn',
      metrics_label: 'quota_exceeded'
    },
    {
      router_code: RouterErrorCode.AUTHZ_RATE_LIMIT_EXCEEDED,
      http_status: HttpStatus.TOO_MANY_REQUESTS,
      gateway_code: 'rate_limit_exceeded',
      message_template: 'Rate limit exceeded. Retry after {retry_after} seconds',
      retryable: true,
      log_level: 'warn',
      metrics_label: 'rate_limit_exceeded'
    },

    // Routing errors
    {
      router_code: RouterErrorCode.ROUTE_NOT_FOUND,
      http_status: HttpStatus.NOT_FOUND,
      gateway_code: 'route_not_found',
      message_template: 'No suitable route found for this request',
      retryable: false,
      log_level: 'warn',
      metrics_label: 'route_not_found'
    },
    {
      router_code: RouterErrorCode.ROUTE_PROVIDER_UNAVAILABLE,
      http_status: HttpStatus.SERVICE_UNAVAILABLE,
      gateway_code: 'provider_unavailable',
      message_template: 'AI provider is currently unavailable',
      retryable: true,
      log_level: 'warn',
      metrics_label: 'provider_unavailable'
    },
    {
      router_code: RouterErrorCode.ROUTE_PROVIDER_ERROR,
      http_status: HttpStatus.BAD_GATEWAY,
      gateway_code: 'provider_error',
      message_template: 'AI provider returned an error: {details}',
      retryable: true,
      log_level: 'error',
      metrics_label: 'provider_error'
    },
    {
      router_code: RouterErrorCode.ROUTE_TIMEOUT,
      http_status: HttpStatus.GATEWAY_TIMEOUT,
      gateway_code: 'route_timeout',
      message_template: 'Request timed out while routing',
      retryable: true,
      log_level: 'warn',
      metrics_label: 'route_timeout'
    },
    {
      router_code: RouterErrorCode.ROUTE_CIRCUIT_BREAKER_OPEN,
      http_status: HttpStatus.SERVICE_UNAVAILABLE,
      gateway_code: 'circuit_breaker_open',
      message_template: 'Circuit breaker is open for this provider',
      retryable: true,
      log_level: 'warn',
      metrics_label: 'circuit_breaker_open'
    },

    // Provider errors
    {
      router_code: RouterErrorCode.PROVIDER_AUTHENTICATION_FAILED,
      http_status: HttpStatus.BAD_GATEWAY,
      gateway_code: 'provider_auth_failed',
      message_template: 'AI provider authentication failed',
      retryable: false,
      log_level: 'error',
      metrics_label: 'provider_auth_failed'
    },
    {
      router_code: RouterErrorCode.PROVIDER_INSUFFICIENT_CREDITS,
      http_status: HttpStatus.PAYMENT_REQUIRED,
      gateway_code: 'insufficient_credits',
      message_template: 'Insufficient credits for this AI provider',
      retryable: false,
      log_level: 'warn',
      metrics_label: 'insufficient_credits'
    },
    {
      router_code: RouterErrorCode.PROVIDER_RATE_LIMITED,
      http_status: HttpStatus.TOO_MANY_REQUESTS,
      gateway_code: 'provider_rate_limited',
      message_template: 'AI provider rate limit exceeded. Retry after {retry_after} seconds',
      retryable: true,
      log_level: 'warn',
      metrics_label: 'provider_rate_limited'
    },
    {
      router_code: RouterErrorCode.PROVIDER_MODEL_NOT_AVAILABLE,
      http_status: HttpStatus.NOT_FOUND,
      gateway_code: 'model_not_available',
      message_template: 'Requested AI model is not available',
      retryable: false,
      log_level: 'warn',
      metrics_label: 'model_not_available'
    },
    {
      router_code: RouterErrorCode.PROVIDER_CONTENT_POLICY_VIOLATION,
      http_status: HttpStatus.BAD_REQUEST,
      gateway_code: 'content_policy_violation',
      message_template: 'Request violates AI provider content policy: {details}',
      retryable: false,
      log_level: 'warn',
      metrics_label: 'content_policy_violation'
    },

    // Validation errors
    {
      router_code: RouterErrorCode.VALIDATION_INVALID_REQUEST,
      http_status: HttpStatus.BAD_REQUEST,
      gateway_code: 'invalid_request',
      message_template: 'Invalid request format',
      retryable: false,
      log_level: 'info',
      metrics_label: 'invalid_request'
    },
    {
      router_code: RouterErrorCode.VALIDATION_MISSING_REQUIRED_FIELD,
      http_status: HttpStatus.BAD_REQUEST,
      gateway_code: 'missing_required_field',
      message_template: 'Missing required field: {param}',
      retryable: false,
      log_level: 'info',
      metrics_label: 'missing_required_field'
    },
    {
      router_code: RouterErrorCode.VALIDATION_INVALID_FIELD_VALUE,
      http_status: HttpStatus.BAD_REQUEST,
      gateway_code: 'invalid_field_value',
      message_template: 'Invalid value for field: {param}',
      retryable: false,
      log_level: 'info',
      metrics_label: 'invalid_field_value'
    },
    {
      router_code: RouterErrorCode.VALIDATION_REQUEST_TOO_LARGE,
      http_status: 413 as any,
      gateway_code: 'request_too_large',
      message_template: 'Request payload is too large',
      retryable: false,
      log_level: 'info',
      metrics_label: 'request_too_large'
    },

    // System errors
    {
      router_code: RouterErrorCode.SYSTEM_INTERNAL_ERROR,
      http_status: HttpStatus.INTERNAL_SERVER_ERROR,
      gateway_code: 'internal_error',
      message_template: 'Internal server error',
      retryable: true,
      log_level: 'error',
      metrics_label: 'internal_error'
    },
    {
      router_code: RouterErrorCode.SYSTEM_SERVICE_UNAVAILABLE,
      http_status: HttpStatus.SERVICE_UNAVAILABLE,
      gateway_code: 'service_unavailable',
      message_template: 'Service is temporarily unavailable',
      retryable: true,
      log_level: 'error',
      metrics_label: 'service_unavailable'
    },
    {
      router_code: RouterErrorCode.SYSTEM_DATABASE_ERROR,
      http_status: HttpStatus.INTERNAL_SERVER_ERROR,
      gateway_code: 'database_error',
      message_template: 'Database error occurred',
      retryable: true,
      log_level: 'error',
      metrics_label: 'database_error'
    },
    {
      router_code: RouterErrorCode.SYSTEM_CACHE_ERROR,
      http_status: HttpStatus.INTERNAL_SERVER_ERROR,
      gateway_code: 'cache_error',
      message_template: 'Cache error occurred',
      retryable: true,
      log_level: 'warn',
      metrics_label: 'cache_error'
    },

    // Network errors
    {
      router_code: RouterErrorCode.NETWORK_CONNECTION_TIMEOUT,
      http_status: HttpStatus.GATEWAY_TIMEOUT,
      gateway_code: 'connection_timeout',
      message_template: 'Connection to service timed out',
      retryable: true,
      log_level: 'warn',
      metrics_label: 'connection_timeout'
    },
    {
      router_code: RouterErrorCode.NETWORK_CONNECTION_REFUSED,
      http_status: HttpStatus.BAD_GATEWAY,
      gateway_code: 'connection_refused',
      message_template: 'Connection to service was refused',
      retryable: true,
      log_level: 'warn',
      metrics_label: 'connection_refused'
    },
    {
      router_code: RouterErrorCode.NETWORK_DNS_RESOLUTION_FAILED,
      http_status: HttpStatus.BAD_GATEWAY,
      gateway_code: 'dns_resolution_failed',
      message_template: 'DNS resolution failed for service',
      retryable: true,
      log_level: 'warn',
      metrics_label: 'dns_resolution_failed'
    },
    {
      router_code: RouterErrorCode.NETWORK_TLS_ERROR,
      http_status: HttpStatus.BAD_GATEWAY,
      gateway_code: 'tls_error',
      message_template: 'TLS/SSL error occurred',
      retryable: true,
      log_level: 'warn',
      metrics_label: 'tls_error'
    },

    // Protocol errors
    {
      router_code: RouterErrorCode.PROTOCOL_UNSUPPORTED_VERSION,
      http_status: HttpStatus.BAD_REQUEST,
      gateway_code: 'unsupported_protocol_version',
      message_template: 'Unsupported protocol version',
      retryable: false,
      log_level: 'warn',
      metrics_label: 'unsupported_protocol_version'
    },
    {
      router_code: RouterErrorCode.PROTOCOL_MESSAGE_TOO_LARGE,
      http_status: 413 as any,
      gateway_code: 'message_too_large',
      message_template: 'Protocol message is too large',
      retryable: false,
      log_level: 'warn',
      metrics_label: 'message_too_large'
    },
    {
      router_code: RouterErrorCode.PROTOCOL_SERIALIZATION_ERROR,
      http_status: HttpStatus.BAD_REQUEST,
      gateway_code: 'serialization_error',
      message_template: 'Protocol serialization error',
      retryable: false,
      log_level: 'warn',
      metrics_label: 'serialization_error'
    },
    {
      router_code: RouterErrorCode.PROTOCOL_DESERIALIZATION_ERROR,
      http_status: HttpStatus.INTERNAL_SERVER_ERROR,
      gateway_code: 'deserialization_error',
      message_template: 'Protocol deserialization error',
      retryable: true,
      log_level: 'error',
      metrics_label: 'deserialization_error'
    },
  ].map((m) => [m.router_code, m] as [RouterErrorCode, ErrorMapping]));

  constructor(private readonly telemetry: TracingService) {}

  mapRouterError(routerError: RouterError): HttpException {
    const mapping = this.errorMappings.get(routerError.code);
    
    if (!mapping) {
      // Default mapping for unknown errors
      return new HttpException(
        {
          error: {
            code: 'unknown_error',
            message: 'An unknown error occurred',
            type: 'api_error',
            trace_id: routerError.trace_id,
          },
          trace_id: routerError.trace_id || this.telemetry.getTraceId(),
          timestamp: new Date().toISOString(),
        },
        HttpStatus.INTERNAL_SERVER_ERROR,
      );
    }

    // Format message with details
    const message = this.formatMessage(mapping.message_template, routerError.details);

    // Create gateway error response
    const gatewayError: GatewayErrorResponse = {
      error: {
        code: mapping.gateway_code,
        message,
        type: this.getErrorType(mapping.http_status),
        trace_id: routerError.trace_id || this.telemetry.getTraceId(),
      },
      trace_id: routerError.trace_id || this.telemetry.getTraceId(),
      timestamp: routerError.timestamp || new Date().toISOString(),
    };

    // Add parameter-specific information if available
    if (routerError.details?.param) {
      gatewayError.error.param = routerError.details.param;
    }

    // Log the error based on severity
    this.logError(routerError, mapping, gatewayError);

    // Record metrics
    this.recordMetrics(mapping, routerError);

    return new HttpException(gatewayError, mapping.http_status);
  }

  mapGrpcError(grpcError: any): HttpException {
    // Map gRPC status codes to router error codes
    const grpcStatus = grpcError.code;
    let routerErrorCode: RouterErrorCode;

    switch (grpcStatus) {
      case 1: // CANCELLED
        routerErrorCode = RouterErrorCode.ROUTE_TIMEOUT;
        break;
      case 2: // UNKNOWN
        routerErrorCode = RouterErrorCode.SYSTEM_INTERNAL_ERROR;
        break;
      case 3: // INVALID_ARGUMENT
        routerErrorCode = RouterErrorCode.VALIDATION_INVALID_REQUEST;
        break;
      case 4: // DEADLINE_EXCEEDED
        routerErrorCode = RouterErrorCode.ROUTE_TIMEOUT;
        break;
      case 5: // NOT_FOUND
        routerErrorCode = RouterErrorCode.AUTHZ_RESOURCE_NOT_FOUND;
        break;
      case 6: // ALREADY_EXISTS
        routerErrorCode = RouterErrorCode.VALIDATION_INVALID_REQUEST;
        break;
      case 7: // PERMISSION_DENIED
        routerErrorCode = RouterErrorCode.AUTHZ_ACCESS_DENIED;
        break;
      case 8: // RESOURCE_EXHAUSTED
        routerErrorCode = RouterErrorCode.AUTHZ_QUOTA_EXCEEDED;
        break;
      case 9: // FAILED_PRECONDITION
        routerErrorCode = RouterErrorCode.VALIDATION_INVALID_REQUEST;
        break;
      case 10: // ABORTED
        routerErrorCode = RouterErrorCode.ROUTE_CIRCUIT_BREAKER_OPEN;
        break;
      case 11: // OUT_OF_RANGE
        routerErrorCode = RouterErrorCode.VALIDATION_INVALID_FIELD_VALUE;
        break;
      case 12: // UNIMPLEMENTED
        routerErrorCode = RouterErrorCode.PROTOCOL_UNSUPPORTED_VERSION;
        break;
      case 13: // INTERNAL
        routerErrorCode = RouterErrorCode.SYSTEM_INTERNAL_ERROR;
        break;
      case 14: // UNAVAILABLE
        routerErrorCode = RouterErrorCode.SYSTEM_SERVICE_UNAVAILABLE;
        break;
      case 15: // DATA_LOSS
        routerErrorCode = RouterErrorCode.SYSTEM_DATABASE_ERROR;
        break;
      case 16: // UNAUTHENTICATED
        routerErrorCode = RouterErrorCode.AUTH_INVALID_TOKEN;
        break;
      default:
        routerErrorCode = RouterErrorCode.SYSTEM_INTERNAL_ERROR;
    }

    const routerError: RouterError = {
      code: routerErrorCode,
      message: grpcError.message || 'gRPC error occurred',
      details: { grpc_code: grpcStatus, details: grpcError.details },
      trace_id: this.telemetry.getTraceId(),
      timestamp: new Date().toISOString(),
    };

    return this.mapRouterError(routerError);
  }

  mapHttpError(httpError: any): HttpException {
    // Map HTTP status codes to router error codes
    const statusCode = httpError.status || httpError.statusCode;
    let routerErrorCode: RouterErrorCode;

    switch (statusCode) {
      case 400:
        routerErrorCode = RouterErrorCode.VALIDATION_INVALID_REQUEST;
        break;
      case 401:
        routerErrorCode = RouterErrorCode.AUTH_INVALID_TOKEN;
        break;
      case 403:
        routerErrorCode = RouterErrorCode.AUTHZ_ACCESS_DENIED;
        break;
      case 404:
        routerErrorCode = RouterErrorCode.AUTHZ_RESOURCE_NOT_FOUND;
        break;
      case 408:
        routerErrorCode = RouterErrorCode.NETWORK_CONNECTION_TIMEOUT;
        break;
      case 413:
        routerErrorCode = RouterErrorCode.VALIDATION_REQUEST_TOO_LARGE;
        break;
      case 429:
        routerErrorCode = RouterErrorCode.AUTHZ_RATE_LIMIT_EXCEEDED;
        break;
      case 500:
        routerErrorCode = RouterErrorCode.SYSTEM_INTERNAL_ERROR;
        break;
      case 502:
        routerErrorCode = RouterErrorCode.ROUTE_PROVIDER_ERROR;
        break;
      case 503:
        routerErrorCode = RouterErrorCode.SYSTEM_SERVICE_UNAVAILABLE;
        break;
      case 504:
        routerErrorCode = RouterErrorCode.ROUTE_TIMEOUT;
        break;
      default:
        routerErrorCode = RouterErrorCode.SYSTEM_INTERNAL_ERROR;
    }

    const routerError: RouterError = {
      code: routerErrorCode,
      message: httpError.message || 'HTTP error occurred',
      details: { http_status: statusCode, details: httpError.response?.data },
      trace_id: this.telemetry.getTraceId(),
      timestamp: new Date().toISOString(),
    };

    return this.mapRouterError(routerError);
  }

  createRouterError(
    code: RouterErrorCode,
    message?: string,
    details?: Record<string, any>,
    retryable?: boolean,
    retryAfter?: number
  ): RouterError {
    const mapping = this.errorMappings.get(code);
    
    return {
      code,
      message: message || mapping?.message_template || 'Unknown error',
      details,
      trace_id: this.telemetry.getTraceId(),
      timestamp: new Date().toISOString(),
      retryable: retryable ?? mapping?.retryable ?? false,
      retry_after: retryAfter,
    };
  }

  private formatMessage(template: string, details?: Record<string, any>): string {
    if (!details) return template;

    let message = template;
    for (const [key, value] of Object.entries(details)) {
      message = message.replace(new RegExp(`{${key}}`, 'g'), String(value));
    }

    return message;
  }

  private getErrorType(httpStatus: HttpStatus): string {
    if (httpStatus >= 400 && httpStatus < 500) {
      return 'invalid_request_error';
    } else if (httpStatus >= 500) {
      return 'api_error';
    }
    return 'api_error';
  }

  private logError(routerError: RouterError, mapping: ErrorMapping, gatewayError: GatewayErrorResponse): void {
    const logData = {
      router_error: routerError,
      gateway_error: gatewayError,
      mapping: {
        code: mapping.router_code,
        http_status: mapping.http_status,
        log_level: mapping.log_level,
      },
    };

    switch (mapping.log_level) {
      case 'error':
        console.error('Router Error:', JSON.stringify(logData));
        break;
      case 'warn':
        console.warn('Router Warning:', JSON.stringify(logData));
        break;
      case 'info':
        console.info('Router Info:', JSON.stringify(logData));
        break;
      case 'debug':
        console.debug('Router Debug:', JSON.stringify(logData));
        break;
    }
  }

  private recordMetrics(mapping: ErrorMapping, routerError: RouterError): void {
    // Record error metrics for monitoring
    const metricData = {
      error_code: mapping.router_code,
      gateway_code: mapping.gateway_code,
      http_status: mapping.http_status,
      retryable: mapping.retryable,
      metrics_label: mapping.metrics_label,
      trace_id: routerError.trace_id,
    };

    // This would typically be sent to a metrics service
    console.log('Error Metric:', JSON.stringify(metricData));
  }

  isRetryable(error: RouterError | HttpException): boolean {
    if (error instanceof HttpException) {
      const response = error.getResponse() as any;
      if (response?.error?.code) {
        const mapping = this.errorMappings.get(response.error.code as RouterErrorCode);
        return mapping?.retryable ?? false;
      }
      return false;
    }

    const asRouter = error as RouterError;
    if (asRouter && (asRouter.code as any)) {
      const mapping = this.errorMappings.get(asRouter.code as RouterErrorCode);
      return (mapping?.retryable ?? (typeof asRouter.retryable === 'boolean' ? asRouter.retryable : false));
    }

    return false;
  }

  getRetryAfter(error: RouterError | HttpException): number | undefined {
    if (error instanceof HttpException) {
      const response = error.getResponse() as any;
      if (response?.error?.code) {
        const mapping = this.errorMappings.get(response.error.code as RouterErrorCode);
        return mapping?.retryable ? response.retry_after : undefined;
      }
      return undefined;
    }

    if ('retry_after' in error) {
      return error.retry_after;
    }

    return undefined;
  }

  getAllErrorCodes(): RouterErrorCode[] {
    return Array.from(this.errorMappings.keys());
  }

  getErrorMapping(code: RouterErrorCode): ErrorMapping | undefined {
    return this.errorMappings.get(code);
  }
}