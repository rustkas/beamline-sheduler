import { ApiProperty } from '@nestjs/swagger';
import { IsString, IsOptional, IsObject, ValidateNested, IsNotEmpty } from 'class-validator';
import { Type } from 'class-transformer';
import { MessageDto } from './message.dto';

export class RouteRequestDto {
  @ApiProperty({
    description: 'Message to route',
    type: MessageDto,
  })
  @IsNotEmpty()
  @ValidateNested()
  @Type(() => MessageDto)
  message: MessageDto;

  @ApiProperty({
    description: 'Optional policy ID',
    required: false,
    example: 'default',
  })
  @IsOptional()
  @IsString()
  policy_id?: string;

  @ApiProperty({
    description: 'Request context (e.g., user_id for sticky sessions)',
    required: false,
    type: Object,
    additionalProperties: true,
  })
  @IsOptional()
  @IsObject()
  context?: Record<string, string>;

  @ApiProperty({ description: 'Optional runtime constraints', required: false, type: Object })
  @IsOptional()
  @IsObject()
  constraints?: Record<string, unknown>;

  @ApiProperty({ description: 'Optional metadata', required: false, type: Object })
  @IsOptional()
  @IsObject()
  metadata?: Record<string, unknown>;

  @ApiProperty({ description: 'Push assignment via JetStream', required: false, default: false })
  @IsOptional()
  push_assignment?: boolean;
}
