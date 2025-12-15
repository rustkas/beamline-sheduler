import { ApiProperty } from '@nestjs/swagger';
import { IsString, IsOptional, IsObject, IsNumber } from 'class-validator';

export class MessageDto {
  @ApiProperty({
    description: 'Unique message identifier',
    example: 'msg_1234567890',
  })
  @IsString()
  message_id: string;

  @ApiProperty({
    description: 'Tenant identifier',
    example: 'tenant_abc123',
  })
  @IsString()
  tenant_id: string;

  @ApiProperty({
    description: 'Trace identifier for distributed tracing',
    required: false,
    example: 'trace_abc123',
  })
  @IsOptional()
  @IsString()
  trace_id?: string;

  @ApiProperty({
    description: 'Message type (e.g., "chat", "completion", "embedding")',
    example: 'chat',
  })
  @IsString()
  message_type: string;

  @ApiProperty({
    description: 'Message payload (JSON or text)',
    example: '{"text": "Hello, world!"}',
  })
  @IsString()
  payload: string;

  @ApiProperty({
    description: 'Message metadata (key-value pairs)',
    required: false,
    type: Object,
    additionalProperties: { type: 'string' },
  })
  @IsOptional()
  @IsObject()
  metadata?: Record<string, string>;

  @ApiProperty({
    description: 'Message creation timestamp (milliseconds)',
    required: false,
    example: 1704067200000,
  })
  @IsOptional()
  @IsNumber()
  timestamp_ms?: number;
}
