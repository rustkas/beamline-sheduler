import { registerAs } from '@nestjs/config';

/**
 * Router Admin Configuration
 * Configuration for Router Admin gRPC client
 */
export default registerAs('routerAdmin', () => ({
  /**
   * Router Admin gRPC endpoint URL
   * Format: host:port (e.g., 'router:50051' or 'localhost:50051')
   */
  grpcUrl: process.env.ROUTER_ADMIN_GRPC_URL || 'router:50051',

  /**
   * gRPC package name
   */
  package: 'beamline.flow.v1',

  /**
   * Proto file path
   */
  protoPath: 'proto/beamline/flow/v1/flow.proto',

  /**
   * Service name
   */
  serviceName: 'RouterAdmin',

  /**
   * Connection pool settings
   */
  connectionPool: {
    /**
     * Minimum number of connections in pool
     */
    min: parseInt(process.env.ROUTER_ADMIN_POOL_MIN || '5', 10),

    /**
     * Maximum number of connections in pool
     */
    max: parseInt(process.env.ROUTER_ADMIN_POOL_MAX || '50', 10),

    /**
     * Connection timeout in milliseconds
     */
    timeout: parseInt(process.env.ROUTER_ADMIN_TIMEOUT_MS || '5000', 10),
  },

  /**
   * Retry settings
   */
  retry: {
    /**
     * Maximum number of retry attempts
     */
    maxAttempts: parseInt(process.env.ROUTER_ADMIN_RETRY_MAX || '3', 10),

    /**
     * Initial retry delay in milliseconds
     */
    initialDelay: parseInt(process.env.ROUTER_ADMIN_RETRY_DELAY_MS || '100', 10),

    /**
     * Maximum retry delay in milliseconds
     */
    maxDelay: parseInt(process.env.ROUTER_ADMIN_RETRY_MAX_DELAY_MS || '5000', 10),

    /**
     * Retry backoff multiplier
     */
    backoffMultiplier: parseFloat(process.env.ROUTER_ADMIN_RETRY_BACKOFF || '2.0'),
  },

  /**
   * Circuit breaker settings
   */
  circuitBreaker: {
    /**
     * Failure threshold before opening circuit
     */
    failureThreshold: parseInt(process.env.ROUTER_ADMIN_CB_FAILURE_THRESHOLD || '5', 10),

    /**
     * Success threshold before closing circuit
     */
    successThreshold: parseInt(process.env.ROUTER_ADMIN_CB_SUCCESS_THRESHOLD || '2', 10),

    /**
     * Timeout in milliseconds before attempting to close circuit
     */
    timeout: parseInt(process.env.ROUTER_ADMIN_CB_TIMEOUT_MS || '60000', 10),
  },

  /**
   * TLS settings for production
   */
  tls: {
    /**
     * Enable TLS (required for production)
     */
    enabled: process.env.ROUTER_ADMIN_TLS_ENABLED === 'true',

    /**
     * Path to CA certificate file
     */
    caCertPath: process.env.ROUTER_ADMIN_TLS_CA_CERT,

    /**
     * Path to client certificate file
     */
    clientCertPath: process.env.ROUTER_ADMIN_TLS_CLIENT_CERT,

    /**
     * Path to client key file
     */
    clientKeyPath: process.env.ROUTER_ADMIN_TLS_CLIENT_KEY,
  },

  /**
   * Message size limits
   */
  messageSize: {
    /**
     * Maximum receive message size in bytes (default: 10MB)
     */
    maxReceive: parseInt(process.env.ROUTER_ADMIN_MAX_RECEIVE_SIZE || '10485760', 10),

    /**
     * Maximum send message size in bytes (default: 10MB)
     */
    maxSend: parseInt(process.env.ROUTER_ADMIN_MAX_SEND_SIZE || '10485760', 10),
  },

  /**
   * Health check settings
   */
  healthCheck: {
    /**
     * Health check interval in milliseconds
     */
    interval: parseInt(process.env.ROUTER_ADMIN_HEALTH_CHECK_INTERVAL || '30000', 10),

    /**
     * Health check timeout in milliseconds
     */
    timeout: parseInt(process.env.ROUTER_ADMIN_HEALTH_CHECK_TIMEOUT || '5000', 10),
  },
}));

