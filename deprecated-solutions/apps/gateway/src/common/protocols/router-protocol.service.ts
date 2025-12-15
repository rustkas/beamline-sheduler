import { Injectable } from '@nestjs/common';
import { RouterAdminGrpcService } from '../../auth/router-admin-grpc.service';
import { ClientProxy, ClientProxyFactory, Transport } from '@nestjs/microservices';
import { lastValueFrom, Observable } from 'rxjs';
import { TracingService } from '../../observability/tracing.service';

export interface UnifiedRouteRequest {
  id: string;
  tenant_id: string;
  user_id: string;
  message: {
    content: string;
    type: 'text' | 'json' | 'binary';
    metadata?: Record<string, any>;
  };
  routing_policy?: {
    provider_preferences?: string[];
    cost_limit?: number;
    latency_limit?: number;
    quality_threshold?: number;
  };
  context?: {
    session_id?: string;
    request_source?: string;
    client_info?: Record<string, any>;
    trace_context?: Record<string, any>;
  };
  options?: {
    stream?: boolean;
    timeout?: number;
    retry_policy?: {
      max_retries?: number;
      backoff_strategy?: 'linear' | 'exponential';
      backoff_delay?: number;
    };
  };
}

export interface UnifiedRouteResponse {
  id: string;
  request_id: string;
  decision: {
    provider_id: string;
    model_id: string;
    cost_estimate?: number;
    latency_estimate?: number;
    confidence_score?: number;
    reasoning?: string;
  };
  routing_info: {
    route_taken: string[];
    alternatives_considered?: string[];
    policy_applied?: string;
    cache_hit?: boolean;
  };
  metadata?: {
    processing_time_ms?: number;
    cache_info?: Record<string, any>;
    provider_specific?: Record<string, any>;
  };
  trace: {
    trace_id: string;
    span_id: string;
    parent_span_id?: string;
    sampled: boolean;
  };
}

export interface RouterProtocolOptions {
  preferred_protocol?: 'grpc' | 'nats' | 'auto';
  payload_size_threshold?: number; // bytes
  latency_threshold?: number; // milliseconds
  reliability_threshold?: number; // 0-1 reliability score
  streaming_required?: boolean;
  multicast_required?: boolean;
  priority?: 'high' | 'medium' | 'low';
}

export interface ProtocolMetrics {
  protocol: string;
  success_rate: number;
  avg_latency: number;
  p95_latency: number;
  p99_latency: number;
  error_rate: number;
  throughput: number;
  last_updated: string;
}

@Injectable()
export class RouterProtocolService {
  private grpcClient: ClientProxy;
  private natsClient: ClientProxy;
  private protocolMetrics: Map<string, ProtocolMetrics> = new Map();
  private readonly DEFAULT_PAYLOAD_THRESHOLD = 1024 * 1024; // 1MB
  private readonly DEFAULT_LATENCY_THRESHOLD = 100; // 100ms
  private readonly DEFAULT_RELIABILITY_THRESHOLD = 0.95; // 95%

  constructor(
    private readonly routerAdminService: RouterAdminGrpcService,
    private readonly telemetry: TracingService
  ) {
    this.initializeClients();
  }

  private initializeClients() {
    // Initialize gRPC client for Router communication
    this.grpcClient = ClientProxyFactory.create({
      transport: Transport.GRPC,
      options: {
        package: 'beamline.router.v1',
        protoPath: 'proto/beamline/router/v1/router.proto',
        url: process.env.ROUTER_GRPC_URL || 'router:50051',
        loader: {
          keepCase: true,
          longs: String,
          enums: String,
          defaults: true,
          oneofs: true,
        },
        maxReceiveMessageLength: 1024 * 1024 * 10, // 10MB
        maxSendMessageLength: 1024 * 1024 * 10, // 10MB
      },
    });

    // Initialize NATS client for high-throughput messaging
    this.natsClient = ClientProxyFactory.create({
      transport: Transport.NATS,
      options: {
        url: process.env.NATS_URL || 'nats://nats:4222',
        queue: 'router-gateway',
        maxReconnectAttempts: -1, // Infinite reconnects
        reconnectTimeWait: 2000, // 2 seconds
        pingInterval: 5000,
        maxPingOut: 3,
      },
    });
  }

  async sendToRouter(
    request: UnifiedRouteRequest,
    options: RouterProtocolOptions = {}
  ): Promise<UnifiedRouteResponse> {
    const span = this.telemetry.startSpan('router_protocol.send_to_router', {
      'request.id': request.id,
      'request.tenant_id': request.tenant_id,
      'request.user_id': request.user_id,
    });

    try {
      // Determine the optimal protocol
      const protocol = this.determineProtocol(request, options);
      span.setAttribute('protocol.selected', protocol);

      // Send via selected protocol
      const response = await this.sendViaProtocol(request, protocol, span);
      
      // Update metrics
      this.updateProtocolMetrics(protocol, response);
      
      return response;
    } catch (error) {
      span.recordException(error);
      
      // Attempt protocol fallback on failure
      const fallbackProtocol = this.getFallbackProtocol(options.preferred_protocol);
      if (fallbackProtocol) {
        span.setAttribute('protocol.fallback', fallbackProtocol);
        return await this.sendViaProtocol(request, fallbackProtocol, span);
      }
      
      throw error;
    } finally {
      span.end();
    }
  }

  private determineProtocol(
    request: UnifiedRouteRequest,
    options: RouterProtocolOptions
  ): 'grpc' | 'nats' {
    // If protocol is explicitly specified, use it
    if (options.preferred_protocol && options.preferred_protocol !== 'auto') {
      return options.preferred_protocol;
    }

    // Calculate request characteristics
    const payloadSize = this.calculatePayloadSize(request);
    const streamingRequired = options.streaming_required || request.options?.stream;
    const multicastRequired = options.multicast_required || false;
    const priority = options.priority || 'medium';

    // Protocol selection logic
    if (streamingRequired) {
      return 'grpc'; // gRPC supports streaming better
    }

    if (multicastRequired) {
      return 'nats'; // NATS supports pub/sub patterns
    }

    // Check payload size threshold
    const payloadThreshold = options.payload_size_threshold || this.DEFAULT_PAYLOAD_THRESHOLD;
    if (payloadSize > payloadThreshold) {
      return 'grpc'; // gRPC handles large payloads better
    }

    // Check current protocol performance metrics
    const grpcMetrics = this.protocolMetrics.get('grpc');
    const natsMetrics = this.protocolMetrics.get('nats');

    if (grpcMetrics && natsMetrics) {
      // Choose based on performance metrics
      const latencyThreshold = options.latency_threshold || this.DEFAULT_LATENCY_THRESHOLD;
      const reliabilityThreshold = options.reliability_threshold || this.DEFAULT_RELIABILITY_THRESHOLD;

      if (natsMetrics.avg_latency < latencyThreshold && natsMetrics.success_rate >= reliabilityThreshold) {
        return 'nats'; // NATS is performing well
      }

      if (grpcMetrics.avg_latency < latencyThreshold && grpcMetrics.success_rate >= reliabilityThreshold) {
        return 'grpc'; // gRPC is performing well
      }

      // Choose the better performing protocol
      if (natsMetrics.success_rate > grpcMetrics.success_rate) {
        return 'nats';
      } else if (grpcMetrics.success_rate > natsMetrics.success_rate) {
        return 'grpc';
      } else if (natsMetrics.avg_latency < grpcMetrics.avg_latency) {
        return 'nats';
      }
    }

    // Default to gRPC for reliability
    return 'grpc';
  }

  private async sendViaProtocol(
    request: UnifiedRouteRequest,
    protocol: 'grpc' | 'nats',
    span: any
  ): Promise<UnifiedRouteResponse> {
    const startTime = Date.now();
    
    try {
      let response: UnifiedRouteResponse;
      
      if (protocol === 'grpc') {
        response = await this.sendViaGrpc(request);
      } else {
        response = await this.sendViaNats(request);
      }
      
      const duration = Date.now() - startTime;
      span.setAttribute('protocol.duration_ms', duration);
      span.setAttribute('protocol.success', true);
      
      return response;
    } catch (error) {
      const duration = Date.now() - startTime;
      span.setAttribute('protocol.duration_ms', duration);
      span.setAttribute('protocol.success', false);
      span.setAttribute('protocol.error', error.message);
      
      throw error;
    }
  }

  private async sendViaGrpc(request: UnifiedRouteRequest): Promise<UnifiedRouteResponse> {
    try {
      // Convert unified request to gRPC format
      const grpcRequest = this.convertToGrpcRequest(request);
      
      // Send via gRPC
      const response = await lastValueFrom(
        this.grpcClient.send('RouteMessage', grpcRequest)
      );
      
      // Convert gRPC response to unified format
      return this.convertFromGrpcResponse(response);
    } catch (error) {
      throw new Error(`gRPC routing failed: ${error.message}`);
    }
  }

  private async sendViaNats(request: UnifiedRouteRequest): Promise<UnifiedRouteResponse> {
    try {
      // Convert unified request to NATS format
      const natsRequest = this.convertToNatsRequest(request);
      
      // Send via NATS with timeout
      const timeout = request.options?.timeout || 30000;
      const response = await lastValueFrom(
        this.natsClient.send('router.route', natsRequest)
      );
      
      // Convert NATS response to unified format
      return this.convertFromNatsResponse(response);
    } catch (error) {
      throw new Error(`NATS routing failed: ${error.message}`);
    }
  }

  private convertToGrpcRequest(request: UnifiedRouteRequest): any {
    return {
      id: request.id,
      tenant_id: request.tenant_id,
      user_id: request.user_id,
      message: {
        content: request.message.content,
        type: request.message.type,
        metadata: request.message.metadata || {},
      },
      routing_policy: request.routing_policy || {},
      context: request.context || {},
      options: request.options || {},
    };
  }

  private convertFromGrpcResponse(response: any): UnifiedRouteResponse {
    return {
      id: response.id,
      request_id: response.request_id,
      decision: {
        provider_id: response.decision.provider_id,
        model_id: response.decision.model_id,
        cost_estimate: response.decision.cost_estimate,
        latency_estimate: response.decision.latency_estimate,
        confidence_score: response.decision.confidence_score,
        reasoning: response.decision.reasoning,
      },
      routing_info: {
        route_taken: response.routing_info.route_taken || [],
        alternatives_considered: response.routing_info.alternatives_considered,
        policy_applied: response.routing_info.policy_applied,
        cache_hit: response.routing_info.cache_hit,
      },
      metadata: response.metadata || {},
      trace: {
        trace_id: response.trace.trace_id,
        span_id: response.trace.span_id,
        parent_span_id: response.trace.parent_span_id,
        sampled: response.trace.sampled,
      },
    };
  }

  private convertToNatsRequest(request: UnifiedRouteRequest): any {
    return {
      subject: `router.${request.tenant_id}.${request.user_id}`,
      data: {
        id: request.id,
        message: request.message,
        routing_policy: request.routing_policy,
        context: request.context,
        options: request.options,
      },
      headers: {
        'X-Tenant-ID': request.tenant_id,
        'X-User-ID': request.user_id,
        'X-Request-ID': request.id,
        'X-Trace-ID': this.telemetry.getTraceId(),
      },
    };
  }

  private convertFromNatsResponse(response: any): UnifiedRouteResponse {
    return {
      id: response.id,
      request_id: response.request_id,
      decision: response.decision,
      routing_info: response.routing_info,
      metadata: response.metadata,
      trace: response.trace,
    };
  }

  private getFallbackProtocol(preferredProtocol?: string): 'grpc' | 'nats' | null {
    if (preferredProtocol === 'grpc') {
      return 'nats';
    } else if (preferredProtocol === 'nats') {
      return 'grpc';
    }
    return 'grpc'; // Default fallback
  }

  private calculatePayloadSize(request: UnifiedRouteRequest): number {
    // Calculate approximate payload size
    const messageSize = Buffer.byteLength(request.message.content, 'utf8');
    const metadataSize = JSON.stringify(request.message.metadata || {}).length;
    const contextSize = JSON.stringify(request.context || {}).length;
    const policySize = JSON.stringify(request.routing_policy || {}).length;
    
    return messageSize + metadataSize + contextSize + policySize;
  }

  private updateProtocolMetrics(protocol: string, response: UnifiedRouteResponse): void {
    const metrics = this.protocolMetrics.get(protocol) || {
      protocol,
      success_rate: 1.0,
      avg_latency: 0,
      p95_latency: 0,
      p99_latency: 0,
      error_rate: 0,
      throughput: 0,
      last_updated: new Date().toISOString(),
    };

    // Update success rate (exponential moving average)
    const alpha = 0.1; // Smoothing factor
    metrics.success_rate = alpha * 1.0 + (1 - alpha) * metrics.success_rate;
    
    // Update latency metrics (simplified)
    const processingTime = response.metadata?.processing_time_ms || 0;
    metrics.avg_latency = alpha * processingTime + (1 - alpha) * metrics.avg_latency;
    
    // Update throughput
    metrics.throughput += 1;
    metrics.last_updated = new Date().toISOString();
    
    this.protocolMetrics.set(protocol, metrics);
  }

  async getProtocolMetrics(protocol?: string): Promise<ProtocolMetrics | ProtocolMetrics[] | null> {
    if (protocol) {
      return this.protocolMetrics.get(protocol) || null;
    }
    
    return Array.from(this.protocolMetrics.values());
  }

  async healthCheck(): Promise<{ grpc: boolean; nats: boolean }> {
    const results = {
      grpc: false,
      nats: false,
    };

    try {
      // Test gRPC connection
      await lastValueFrom(
        this.grpcClient.send('HealthCheck', { timestamp: Date.now() })
      );
      results.grpc = true;
    } catch (error) {
      console.error('gRPC health check failed:', error.message);
    }

    try {
      // Test NATS connection
      await lastValueFrom(
        this.natsClient.send('health.check', { timestamp: Date.now() })
      );
      results.nats = true;
    } catch (error) {
      console.error('NATS health check failed:', error.message);
    }

    return results;
  }

  async closeConnections(): Promise<void> {
    try {
      await this.grpcClient.close();
    } catch (error) {
      console.error('Error closing gRPC client:', error);
    }

    try {
      await this.natsClient.close();
    } catch (error) {
      console.error('Error closing NATS client:', error);
    }
  }
}