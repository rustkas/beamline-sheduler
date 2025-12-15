import { ApiProperty } from '@nestjs/swagger';
import { IsString, IsNumber, IsObject, IsOptional } from 'class-validator';

export class RouteDecisionDto {
  @ApiProperty({
    description: 'Selected provider ID',
    example: 'anthropic',
  })
  @IsString()
  provider_id: string;

  @ApiProperty({
    description: 'Reason for selection (e.g., "weighted", "sticky", "fallback")',
    example: 'weighted',
  })
  @IsString()
  reason: string;

  @ApiProperty({
    description: 'Route priority (0-100)',
    example: 50,
    minimum: 0,
    maximum: 100,
  })
  @IsNumber()
  priority: number;

  @ApiProperty({
    description: 'Expected latency in milliseconds',
    example: 250,
  })
  @IsNumber()
  expected_latency_ms: number;

  @ApiProperty({
    description: 'Expected cost of the request',
    example: 0.001,
  })
  @IsNumber()
  expected_cost: number;

  @ApiProperty({
    description: 'Decision metadata',
    required: false,
    type: Object,
    additionalProperties: true,
  })
  @IsOptional()
  @IsObject()
  metadata?: Record<string, unknown>;

  @ApiProperty({ description: 'Sticky session key used by Router', required: false })
  @IsOptional()
  @IsString()
  sticky_key?: string;
}
