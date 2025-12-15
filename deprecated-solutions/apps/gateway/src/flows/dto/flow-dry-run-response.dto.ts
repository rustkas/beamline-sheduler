import { ApiProperty } from '@nestjs/swagger';
import { IsArray, IsBoolean, IsNumber, IsString, ValidateNested } from 'class-validator';
import { Type } from 'class-transformer';

export class FlowDryRunStepDto {
  @ApiProperty()
  @IsString()
  step_id: string;

  @ApiProperty()
  @IsBoolean()
  success: boolean;

  @ApiProperty()
  @IsNumber()
  latency_ms: number;

  @ApiProperty({ required: false })
  @IsString()
  error?: string;
}

export class FlowDryRunResponseDto {
  @ApiProperty()
  @IsBoolean()
  compiled: boolean;

  @ApiProperty({ type: [String] })
  @IsArray()
  compile_errors: string[];

  @ApiProperty()
  @IsString()
  flow_id: string;

  @ApiProperty()
  @IsNumber()
  total_latency_ms: number;

  @ApiProperty({ type: [FlowDryRunStepDto] })
  @ValidateNested({ each: true })
  @Type(() => FlowDryRunStepDto)
  steps: FlowDryRunStepDto[];
}
