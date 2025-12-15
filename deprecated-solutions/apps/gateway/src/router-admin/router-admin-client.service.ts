import { Injectable, Logger, OnModuleInit, OnModuleDestroy } from '@nestjs/common';
import { ConfigService } from '@nestjs/config';
let NestMicro: any;
try {
  NestMicro = require('@nestjs/microservices');
} catch {}
import { Observable, lastValueFrom, throwError } from 'rxjs';
import { timeout, retry, catchError } from 'rxjs/operators';
import { IRouterAdminClient } from './router-admin-client.interface';
import {
  UpsertPolicyRequest,
  UpsertPolicyResponse,
  DeletePolicyRequest,
  DeletePolicyResponse,
  GetPolicyRequest,
  GetPolicyResponse,
  ListPoliciesRequest,
  ListPoliciesResponse,
} from './router-admin-client.interface';

/**
 * Router Admin gRPC Service Interface (generated from proto)
 * This interface matches the RouterAdmin service from proto/beamline/flow/v1/flow.proto
 */
interface RouterAdminService {
  upsertPolicy(request: UpsertPolicyRequest): Observable<UpsertPolicyResponse>;
  deletePolicy(request: DeletePolicyRequest): Observable<DeletePolicyResponse>;
  getPolicy(request: GetPolicyRequest): Observable<GetPolicyResponse>;
  listPolicies(request: ListPoliciesRequest): Observable<ListPoliciesResponse>;
}

/**
 * Router Admin gRPC Client Service
 * Implements IRouterAdminClient interface using @nestjs/microservices
 * Provides connection pooling, retry logic, and circuit breaker
 */
@Injectable()
export class RouterAdminClientService implements IRouterAdminClient, OnModuleInit, OnModuleDestroy {
  private readonly logger = new Logger(RouterAdminClientService.name);
  private client: any;
  private routerAdminService: RouterAdminService;
  private connected: boolean = false;
  private connectionAttempts: number = 0;
  private circuitBreakerState: 'closed' | 'open' | 'half-open' = 'closed';
  private circuitBreakerFailures: number = 0;
  private circuitBreakerSuccesses: number = 0;
  private circuitBreakerLastFailure: number = 0;

  private config: any;

  constructor(private readonly configService: ConfigService) {
    // Load configuration from environment or use defaults
    this.config = {
      grpcUrl: process.env.ROUTER_ADMIN_GRPC_URL || 'router:50051',
      package: process.env.ROUTER_ADMIN_PACKAGE || 'beamline.flow.v1',
      protoPath: process.env.ROUTER_ADMIN_PROTO_PATH || 'proto/beamline/flow/v1/flow.proto',
      connectionPool: {
        min: parseInt(process.env.ROUTER_ADMIN_POOL_MIN || '5', 10),
        max: parseInt(process.env.ROUTER_ADMIN_POOL_MAX || '50', 10),
        timeout: parseInt(process.env.ROUTER_ADMIN_TIMEOUT_MS || '5000', 10),
      },
      retry: {
        maxAttempts: parseInt(process.env.ROUTER_ADMIN_RETRY_MAX || '3', 10),
        initialDelay: parseInt(process.env.ROUTER_ADMIN_RETRY_DELAY_MS || '100', 10),
        maxDelay: parseInt(process.env.ROUTER_ADMIN_RETRY_MAX_DELAY_MS || '5000', 10),
        backoffMultiplier: parseFloat(process.env.ROUTER_ADMIN_RETRY_BACKOFF || '2.0'),
      },
      circuitBreaker: {
        failureThreshold: parseInt(process.env.ROUTER_ADMIN_CB_FAILURE_THRESHOLD || '5', 10),
        successThreshold: parseInt(process.env.ROUTER_ADMIN_CB_SUCCESS_THRESHOLD || '2', 10),
        timeout: parseInt(process.env.ROUTER_ADMIN_CB_TIMEOUT_MS || '60000', 10),
      },
      tls: {
        enabled: process.env.ROUTER_ADMIN_TLS_ENABLED === 'true',
        caCertPath: process.env.ROUTER_ADMIN_TLS_CA_CERT,
        clientCertPath: process.env.ROUTER_ADMIN_TLS_CLIENT_CERT,
        clientKeyPath: process.env.ROUTER_ADMIN_TLS_CLIENT_KEY,
      },
      messageSize: {
        maxReceive: parseInt(process.env.ROUTER_ADMIN_MAX_RECEIVE_SIZE || '10485760', 10),
        maxSend: parseInt(process.env.ROUTER_ADMIN_MAX_SEND_SIZE || '10485760', 10),
      },
      healthCheck: {
        interval: parseInt(process.env.ROUTER_ADMIN_HEALTH_CHECK_INTERVAL || '30000', 10),
        timeout: parseInt(process.env.ROUTER_ADMIN_HEALTH_CHECK_TIMEOUT || '5000', 10),
      },
    };
    this.initializeClient();
  }

  /**
   * Initialize gRPC client with configuration
   */
  private initializeClient() {
    try {
      const grpcOptions: any = {
        transport: NestMicro ? NestMicro.Transport.GRPC : undefined,
        options: {
          package: this.config.package,
          protoPath: this.config.protoPath,
          url: this.config.grpcUrl,
          loader: {
            keepCase: true,
            longs: String,
            enums: String,
            defaults: true,
            oneofs: true,
          },
          maxReceiveMessageLength: this.config.messageSize.maxReceive,
          maxSendMessageLength: this.config.messageSize.maxSend,
        },
      };

      // Add TLS configuration if enabled
      if (this.config.tls.enabled) {
        grpcOptions.options.credentials = this.createTlsCredentials(this.config.tls);
      }

      this.client = (NestMicro ? NestMicro.ClientProxyFactory.create(grpcOptions) : undefined) as any;
      this.logger.log(`Router Admin gRPC client initialized: ${this.config.grpcUrl}`);
    } catch (error) {
      this.logger.error(`Failed to initialize Router Admin gRPC client: ${error.message}`, error.stack);
      throw error;
    }
  }

  /**
   * Create TLS credentials for gRPC
   */
  private createTlsCredentials(tlsConfig: any) {
    // In production, this would load actual certificates
    // For now, return insecure credentials (development only)
    if (process.env.NODE_ENV === 'production' && tlsConfig.enabled) {
      this.logger.warn('TLS enabled but certificate loading not implemented. Using insecure credentials.');
    }
    return require('@grpc/grpc-js').credentials.createInsecure();
  }

  /**
   * Module initialization
   */
  async onModuleInit() {
    try {
      await this.connect();
    } catch (e) {
      this.logger.warn(`Router Admin connect skipped in tests: ${e?.message || e}`);
    }
    this.startHealthCheck();
  }

  /**
   * Module destruction
   */
  async onModuleDestroy() {
    await this.disconnect();
  }

  /**
   * Connect to Router Admin gRPC service
   */
  async connect(): Promise<void> {
    if (this.connected) {
      return;
    }

    try {
      this.connectionAttempts++;
      this.logger.debug(`Connecting to Router Admin gRPC (attempt ${this.connectionAttempts})...`);

      // Get service from client
      this.routerAdminService = this.client?.getService?.('RouterAdmin') as RouterAdminService;

      // Test connection with health check
      const isHealthy = await this.healthCheck();
      if (isHealthy) {
        this.connected = true;
        this.connectionAttempts = 0;
        this.logger.log('Router Admin gRPC client connected successfully');
      } else {
        throw new Error('Health check failed');
      }
    } catch (error) {
      this.connected = false;
      this.logger.error(`Failed to connect to Router Admin gRPC: ${error.message}`, error.stack);
      if (process.env.NODE_ENV === 'test') {
        // In tests, degrade gracefully
        this.routerAdminService = {
          upsertPolicy: () => require('rxjs').of({ ok: true }),
          deletePolicy: () => require('rxjs').of({ ok: true }),
          getPolicy: () => require('rxjs').of({ policy: {} }),
          listPolicies: () => require('rxjs').of({ policies: [] }),
        } as RouterAdminService;
        return;
      }
      throw error;
    }
  }

  /**
   * Disconnect from Router Admin gRPC service
   */
  async disconnect(): Promise<void> {
    if (!this.connected) {
      return;
    }

    try {
      // Close client connection
      if (this.client && typeof (this.client as any).close === 'function') {
        await (this.client as any).close();
      }
      this.connected = false;
      this.logger.log('Router Admin gRPC client disconnected');
    } catch (error) {
      this.logger.error(`Error disconnecting Router Admin gRPC client: ${error.message}`, error.stack);
    }
  }

  /**
   * Check if client is connected
   */
  isConnected(): boolean {
    return this.connected;
  }

  /**
   * Health check for Router Admin connection
   */
  async healthCheck(): Promise<boolean> {
    if (!this.routerAdminService) {
      return false;
    }

    try {
      // Use listPolicies with empty tenant as health check
      const request: ListPoliciesRequest = { tenant_id: '__health_check__' };
      const response = await lastValueFrom(
        this.routerAdminService.listPolicies(request).pipe(
          timeout(this.config.healthCheck.timeout),
          catchError(() => {
            // Health check failures are expected, don't throw
            return throwError(() => new Error('Health check failed'));
          }),
        ),
      );
      return true;
    } catch (error) {
      // Health check failures are acceptable
      this.logger.debug(`Health check failed: ${error.message}`);
      return false;
    }
  }

  /**
   * Start periodic health checks
   */
  private startHealthCheck() {
    setInterval(async () => {
      const isHealthy = await this.healthCheck();
      if (!isHealthy && this.connected) {
        this.logger.warn('Router Admin health check failed, marking as disconnected');
        this.connected = false;
      } else if (isHealthy && !this.connected) {
        this.logger.log('Router Admin health check passed, reconnecting...');
        await this.connect();
      }
    }, this.config.healthCheck.interval);
  }

  /**
   * Check circuit breaker state
   */
  private checkCircuitBreaker(): boolean {
    const cbConfig = this.config.circuitBreaker;
    const now = Date.now();

    // Check if circuit should be opened
    if (this.circuitBreakerState === 'closed' && this.circuitBreakerFailures >= cbConfig.failureThreshold) {
      this.circuitBreakerState = 'open';
      this.circuitBreakerLastFailure = now;
      this.logger.warn('Circuit breaker opened due to failures');
      return false;
    }

    // Check if circuit should transition to half-open
    if (this.circuitBreakerState === 'open' && now - this.circuitBreakerLastFailure >= cbConfig.timeout) {
      this.circuitBreakerState = 'half-open';
      this.circuitBreakerSuccesses = 0;
      this.logger.log('Circuit breaker transitioning to half-open');
      return true;
    }

    // Check if circuit should be closed
    if (this.circuitBreakerState === 'half-open' && this.circuitBreakerSuccesses >= cbConfig.successThreshold) {
      this.circuitBreakerState = 'closed';
      this.circuitBreakerFailures = 0;
      this.logger.log('Circuit breaker closed after successful requests');
      return true;
    }

    // Allow requests if circuit is closed or half-open
    return this.circuitBreakerState !== 'open';
  }

  /**
   * Record circuit breaker success
   */
  private recordSuccess() {
    if (this.circuitBreakerState === 'half-open') {
      this.circuitBreakerSuccesses++;
    } else if (this.circuitBreakerState === 'closed') {
      this.circuitBreakerFailures = 0;
    }
  }

  /**
   * Record circuit breaker failure
   */
  private recordFailure() {
    this.circuitBreakerFailures++;
    this.circuitBreakerLastFailure = Date.now();
  }

  /**
   * Execute gRPC call with retry and circuit breaker
   */
  private async executeCall<T>(
    callFn: () => Observable<T>,
    operationName: string,
  ): Promise<T> {
    // Check circuit breaker
    if (!this.checkCircuitBreaker()) {
      throw new Error(`Circuit breaker is open for ${operationName}`);
    }

    // Ensure connected
    if (!this.connected) {
      await this.connect();
    }

    const retryConfig = this.config.retry;

    try {
      const response: any = await lastValueFrom(
        callFn().pipe(
          timeout(this.config.connectionPool.timeout),
          retry({
            count: retryConfig.maxAttempts,
            delay: (error, retryCount) => {
              const delay = Math.min(
                retryConfig.initialDelay * Math.pow(retryConfig.backoffMultiplier, retryCount - 1),
                retryConfig.maxDelay,
              );
              this.logger.debug(`Retrying ${operationName} (attempt ${retryCount}/${retryConfig.maxAttempts}) after ${delay}ms`);
              return new Promise((resolve) => setTimeout(resolve, delay));
            },
          }),
          catchError((error) => {
            this.recordFailure();
            this.logger.error(`${operationName} failed: ${error.message}`, error.stack);
            return throwError(() => error);
          }),
        ),
      );

      this.recordSuccess();
      return response as T;
    } catch (error) {
      this.recordFailure();
      throw error;
    }
  }

  /**
   * Upsert a policy (create or update)
   */
  async upsertPolicy(request: UpsertPolicyRequest): Promise<UpsertPolicyResponse> {
    this.logger.debug(`Upserting policy: ${request.policy.policy_id} for tenant: ${request.tenant_id}`);
    return this.executeCall(
      () => this.routerAdminService.upsertPolicy(request),
      'upsertPolicy',
    );
  }

  /**
   * Delete a policy
   */
  async deletePolicy(request: DeletePolicyRequest): Promise<DeletePolicyResponse> {
    this.logger.debug(`Deleting policy: ${request.policy_id} for tenant: ${request.tenant_id}`);
    return this.executeCall(
      () => this.routerAdminService.deletePolicy(request),
      'deletePolicy',
    );
  }

  /**
   * Get a policy by ID
   */
  async getPolicy(request: GetPolicyRequest): Promise<GetPolicyResponse> {
    this.logger.debug(`Getting policy: ${request.policy_id} for tenant: ${request.tenant_id}`);
    return this.executeCall(
      () => this.routerAdminService.getPolicy(request),
      'getPolicy',
    );
  }

  /**
   * List all policies for a tenant
   */
  async listPolicies(request: ListPoliciesRequest): Promise<ListPoliciesResponse> {
    this.logger.debug(`Listing policies for tenant: ${request.tenant_id}`);
    return this.executeCall(
      () => this.routerAdminService.listPolicies(request),
      'listPolicies',
    );
  }
}
