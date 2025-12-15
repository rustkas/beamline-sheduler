import { ApiProperty } from '@nestjs/swagger';
import { IsArray, IsBoolean } from 'class-validator';

export class FlowValidationErrorDto {
  @ApiProperty({ description: 'JSON Pointer path to the instance with error' })
  instancePath: string;

  @ApiProperty({ description: 'Schema path describing the violated rule' })
  schemaPath: string;

  @ApiProperty({ description: 'Human-readable error message' })
  message: string;

  @ApiProperty({ description: 'Additional error parameters' })
  params: Record<string, unknown>;
}

export class FlowValidateResponseDto {
  @ApiProperty({ description: 'Is definition valid' })
  @IsBoolean()
  valid: boolean;

  @ApiProperty({ description: 'List of validation errors', type: [FlowValidationErrorDto] })
  @IsArray()
  errors: FlowValidationErrorDto[];
}
