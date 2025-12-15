import { plainToInstance } from 'class-transformer';
import { IsString, validateSync } from 'class-validator';

/**
 * Environment configuration schema for Gateway.
 * Validates required environment variables on startup.
 */
export class EnvironmentConfig {
  // BEAMLINE_HMAC_SECRET is conditionally required (only in production/CI)
  // Validation is handled manually in validateEnvironment() to avoid
  // class-validator applying @IsNotEmpty/@MinLength in development
  BEAMLINE_HMAC_SECRET?: string;

  @IsString()
  NODE_ENV?: string;

  @IsString()
  PORT?: string;

  @IsString()
  GATEWAY_RATE_LIMIT_TTL_SECONDS?: string;

  @IsString()
  GATEWAY_RATE_LIMIT_GLOBAL_LIMIT?: string;

  @IsString()
  ROUTER_CLIENT?: string;

  @IsString()
  ROUTER_BASE_URL?: string;

  @IsString()
  NATS_URL?: string;

  @IsString()
  ROUTER_NATS_SUBJECT?: string;

  @IsString()
  NATS_REPLY_SUBJECT?: string;

  @IsString()
  CAF_ASSIGNMENT_SUBJECT?: string;

  @IsString()
  VERSION?: string;

  @IsString()
  NATS_REQUEST_TIMEOUT_MS?: string;

  @IsString()
  GATEWAY_TENANT_ALLOWLIST?: string;

  @IsString()
  GATEWAY_POLICY_ALLOWLIST?: string;

  @IsString()
  OTLP_ENDPOINT?: string;
}

/**
 * Validate environment variables on application startup.
 * Throws error if required variables are missing or invalid.
 */
export function validateEnvironment(config: Record<string, unknown>): EnvironmentConfig {
  const validatedConfig = plainToInstance(EnvironmentConfig, config, {
    enableImplicitConversion: true,
  });

  // In production/CI, BEAMLINE_HMAC_SECRET is required
  const isProduction =
    process.env.NODE_ENV === 'production' ||
    process.env.CI === 'true' ||
    process.env.GITHUB_ACTIONS === 'true';

  // Manual validation for BEAMLINE_HMAC_SECRET in production/CI
  if (isProduction) {
    if (!validatedConfig.BEAMLINE_HMAC_SECRET) {
      throw new Error(
        'Environment validation failed:\n' +
          '  - BEAMLINE_HMAC_SECRET: is required in production/CI environments\n\n' +
          'Please set BEAMLINE_HMAC_SECRET in your CI/CD secrets or environment variables.',
      );
    }

    // Validate length and format
    const secret = validatedConfig.BEAMLINE_HMAC_SECRET;
    if (secret.length < 16) {
      throw new Error(
        'Environment validation failed:\n' +
          `  - BEAMLINE_HMAC_SECRET: must be at least 16 characters (got ${secret.length})\n\n` +
          'Please set a valid BEAMLINE_HMAC_SECRET (minimum 16 characters, recommended 64).',
      );
    }

    // Validate hex format (optional but recommended)
    if (!/^[0-9a-fA-F]+$/.test(secret)) {
      console.warn(
        '⚠️  BEAMLINE_HMAC_SECRET contains non-hex characters (recommended: hex string)',
      );
    }

    // Log only length, not the value
    console.log(`✅ BEAMLINE_HMAC_SECRET is set (length: ${secret.length} characters)`);
  } else {
    // In development, BEAMLINE_HMAC_SECRET is optional but recommended
    if (!validatedConfig.BEAMLINE_HMAC_SECRET) {
      console.warn('⚠️  BEAMLINE_HMAC_SECRET is not set (optional in development)');
      console.warn('   For local development, use config/env/.env.dev or set BEAMLINE_HMAC_SECRET');
    } else {
      // If set in dev, validate length
      const secret = validatedConfig.BEAMLINE_HMAC_SECRET;
      if (secret.length < 16) {
        console.warn(
          `⚠️  BEAMLINE_HMAC_SECRET is too short (minimum 16 characters, got ${secret.length})`,
        );
      } else {
        console.log(`✅ BEAMLINE_HMAC_SECRET is set (length: ${secret.length} characters)`);
      }
    }
  }

  // Validate other fields using the class instance
  const errors = validateSync(validatedConfig, {
    skipMissingProperties: true,
    whitelist: true,
    forbidNonWhitelisted: false,
  });

  if (errors.length > 0) {
    const errorMessages = errors
      .map((error) => {
        const constraints = Object.values(error.constraints || {}).join(', ');
        return `  - ${error.property}: ${constraints}`;
      })
      .join('\n');

    throw new Error(
      `Environment validation failed:\n${errorMessages}\n\n` +
        'Please check your .env file or environment variables.',
    );
  }

  return validatedConfig;
}
