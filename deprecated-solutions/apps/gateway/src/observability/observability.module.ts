/**
 * Observability Module for NestJS Gateway
 * Provides structured JSONL logging (OBS-1 MVP), Prometheus metrics (CP2.4), and OTLP trace export (CP2.5)
 */

import { Module, Global, OnModuleInit, OnModuleDestroy, Logger } from '@nestjs/common';
import { ConfigService } from '@nestjs/config';
import { JsonlLoggerService } from './jsonl-logger.service';
import { LoggerService } from './logger.service';
import { MetricsService } from './metrics.service';
import { PrometheusService } from './prometheus.service';
import { PrometheusController } from './prometheus.controller';
import { TracingService } from './tracing.service';
import { NodeSDK } from '@opentelemetry/sdk-node';
import { OTLPTraceExporter } from '@opentelemetry/exporter-trace-otlp-http';
import { HttpInstrumentation } from '@opentelemetry/instrumentation-http';
// OTel resources are optional; use defaults to avoid runtime/type mismatches in tests

@Global()
@Module({
  controllers: [PrometheusController], // CP2.4: Prometheus metrics endpoint
  providers: [
    {
      provide: 'LoggerService',
      useClass: JsonlLoggerService, // Use JSONL logger for OBS-1
    },
    LoggerService, // Keep stub for backward compatibility
    MetricsService,
    PrometheusService, // CP2.4: Prometheus metrics service
    TracingService,
  ],
  exports: ['LoggerService', LoggerService, MetricsService, PrometheusService, TracingService],
})
export class ObservabilityModule implements OnModuleInit, OnModuleDestroy {
  private readonly logger = new Logger(ObservabilityModule.name);
  private sdk?: NodeSDK;

  constructor(private readonly configService: ConfigService) {}

  async onModuleInit(): Promise<void> {
    // CP2.5: OTLP Trace Export
    const otlpEndpoint =
      this.configService.get<string>('OTLP_ENDPOINT') ||
      process.env.OTLP_ENDPOINT ||
      process.env.OTEL_EXPORTER_OTLP_ENDPOINT ||
      'http://localhost:4318';

    // Ensure endpoint has /v1/traces suffix for HTTP exporter
    const otlpUrl = otlpEndpoint.endsWith('/v1/traces')
      ? otlpEndpoint
      : `${otlpEndpoint}/v1/traces`;

    try {
      const exporter = new OTLPTraceExporter({
        url: otlpUrl,
      });

      this.sdk = new NodeSDK({
        traceExporter: exporter,
        instrumentations: [new HttpInstrumentation()],
      });

      await this.sdk.start();
      this.logger.log(`🚀 OTLP tracing initialized: ${otlpUrl}`);
    } catch (error) {
      // Graceful degradation: log error but don't fail startup
      this.logger.warn(`⚠️  OTLP tracing initialization failed: ${error.message}`);
      this.logger.warn('   Gateway will continue without OTLP export');
    }
  }

  async onModuleDestroy(): Promise<void> {
    try {
      if (this.sdk) {
        await this.sdk.shutdown();
        this.logger.log('OTLP tracing shutdown complete');
      }
    } catch (error) {
      this.logger.error(`Error shutting down OTLP tracing: ${error.message}`);
    }
  }
}
