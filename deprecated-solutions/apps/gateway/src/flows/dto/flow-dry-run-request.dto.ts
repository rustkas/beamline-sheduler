import { ApiProperty } from '@nestjs/swagger';
import { IsNotEmpty, IsObject, IsOptional, IsString } from 'class-validator';

export class FlowDryRunRequestDto {
  @ApiProperty({
    description: 'Flow DSL definition JSON',
    type: Object,
    additionalProperties: true,
  })
  @IsNotEmpty()
  @IsObject()
  definition: Record<string, unknown>;

  @ApiProperty({ description: 'Optional tenant identifier', required: false })
  @IsOptional()
  @IsString()
  tenant_id?: string;
}
