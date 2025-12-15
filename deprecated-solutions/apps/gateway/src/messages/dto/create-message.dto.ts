import { ApiProperty } from '@nestjs/swagger';
import { IsString, IsOptional, IsObject, IsNotEmpty } from 'class-validator';

export class CreateMessageDto {
  @ApiProperty({
    description: 'Tenant identifier',
    example: 'tenant_abc123',
  })
  @IsString()
  @IsNotEmpty()
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
  @IsNotEmpty()
  message_type: string;

  @ApiProperty({
    description: 'Message payload (JSON or text)',
    example: '{"text": "Hello, world!"}',
  })
  @IsString()
  @IsNotEmpty()
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
}
