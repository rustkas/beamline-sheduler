import { ApiProperty } from '@nestjs/swagger';
import { IsArray, ValidateNested } from 'class-validator';
import { Type } from 'class-transformer';
import { PolicyDto } from './policy.dto';

export class ReconcileDto {
  @ApiProperty({ type: [PolicyDto] })
  @IsArray()
  @ValidateNested({ each: true })
  @Type(() => PolicyDto)
  desired: PolicyDto[];
}