import { NestFactory } from '@nestjs/core';
import { ValidationPipe } from '@nestjs/common';
import { SwaggerModule, DocumentBuilder } from '@nestjs/swagger';
import { AppModule } from './app.module';
import { HttpExceptionFilter } from './common/filters/http-exception.filter';
import { TransformInterceptor } from './common/interceptors/transform.interceptor';
import { MetricsInterceptor } from './common/interceptors/metrics.interceptor';
import { MetricsService } from './observability/metrics.service';
import { TracingService } from './observability/tracing.service';
import { validateEnvironment } from './config/env.validation';

async function bootstrap() {
  // Validate environment variables before starting the application
  try {
    validateEnvironment(process.env);
  } catch (error) {
    console.error('❌ Environment validation failed:', error.message);
    process.exit(1);
  }

  const app = await NestFactory.create(AppModule);

  // Use JSONL logger for OBS-1 (injected via ObservabilityModule)
  const logger = app.get('LoggerService');
  if (logger) {
    app.useLogger(logger);
  }

  // Enable CORS
  app.enableCors();

  // Global validation pipe
  app.useGlobalPipes(
    new ValidationPipe({
      whitelist: true,
      forbidNonWhitelisted: true,
      transform: true,
      transformOptions: {
        enableImplicitConversion: true,
      },
    }),
  );

  // Global exception filter
  app.useGlobalFilters(new HttpExceptionFilter());

  // Global transform interceptor with metrics/tracing
  const metrics = app.get(MetricsService);
  const tracing = app.get(TracingService);
  app.useGlobalInterceptors(new MetricsInterceptor(metrics, tracing), new TransformInterceptor(metrics, tracing));

  // Swagger/OpenAPI setup
  const config = new DocumentBuilder()
    .setTitle('Beamline Gateway API')
    .setDescription('REST/SSE API for Beamline message routing system')
    .setVersion('1.0')
    .addTag('routes', 'Message routing endpoints')
    .addTag('messages', 'Message management endpoints')
    .addTag('policies', 'Routing policy management endpoints')
    .addTag('flows', 'Flow DSL validation endpoints')
    .addTag('health', 'Health check endpoints')
    .addTag('status', 'Public status with versions and gates')
    .addTag('history', 'Read-only audit history with HMAC masking')
    .build();

  const document = SwaggerModule.createDocument(app, config);
  SwaggerModule.setup('api', app, document);

  // OpenAPI export to docs/ is disabled per policy; use Swagger UI at /api

  const port = process.env.PORT || 3000;
  await app.listen(port);

  console.log(`🚀 Gateway server running on http://localhost:${port}`);
  console.log(`📚 Swagger UI available at http://localhost:${port}/api`);
}

bootstrap();
