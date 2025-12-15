import { ApiProperty } from '@nestjs/swagger';
import { IsString, IsOptional, IsObject, IsArray } from 'class-validator';

export class PolicyDto {
  @ApiProperty({ description: 'Tenant ID', example: 'tenant_abc123' })
  @IsString()
  tenant_id: string;

  @ApiProperty({ description: 'Policy ID', example: 'default' })
  @IsString()
  policy_id: string;

  @ApiProperty({ description: 'Policy name', example: 'Default Routing Policy' })
  @IsString()
  name: string;

  @ApiProperty({ description: 'Policy version', example: '1.0', required: false })
  @IsOptional()
  @IsString()
  version?: string;

  @ApiProperty({
    description: 'Default settings',
    required: false,
    type: Object,
    additionalProperties: true,
    example: { timeout_ms: 5000, retry_count: 3 },
  })
  @IsOptional()
  @IsObject()
  defaults?: Record<string, unknown>;

  @ApiProperty({
    description: 'Escalation conditions',
    required: false,
    type: [String],
    example: ['timeout', 'error_rate_threshold'],
  })
  @IsOptional()
  @IsArray()
  escalate_on?: string[];

  @ApiProperty({
    description: 'Provider weights',
    required: false,
    type: Object,
    additionalProperties: { type: 'number' },
    example: { anthropic: 0.7, claude: 0.3 },
  })
  @IsOptional()
  @IsObject()
  weights?: Record<string, number>;

  @ApiProperty({ description: 'Providers list', required: false, type: [Object] })
  @IsOptional()
  @IsArray()
  providers?: Array<{ id: string; weight?: number }>;

  @ApiProperty({
    description: 'Fallback configuration',
    required: false,
    type: Object,
    additionalProperties: true,
    example: { provider: 'local_llm', conditions: ['all_providers_failed'] },
  })
  @IsOptional()
  @IsObject()
  fallback?: {
    provider: string;
    conditions: string[];
  };

  @ApiProperty({
    description: 'Sticky session configuration',
    required: false,
    type: Object,
    additionalProperties: true,
    example: { enabled: true, session_key: 'user_id', ttl_seconds: 3600 },
  })
  @IsOptional()
  @IsObject()
  sticky?: {
    enabled: boolean;
    session_key: string;
    ttl_seconds: number;
  };

  @ApiProperty({
    description: 'Additional metadata',
    required: false,
    type: Object,
    additionalProperties: true,
  })
  @IsOptional()
  @IsObject()
  metadata?: Record<string, unknown>;

  @ApiProperty({ description: 'Routing rules', required: false, type: [Object] })
  @IsOptional()
  @IsArray()
  rules?: Array<{ match?: Record<string, unknown>; prefer?: Record<string, unknown>; fallback?: Record<string, unknown> }>;
}
