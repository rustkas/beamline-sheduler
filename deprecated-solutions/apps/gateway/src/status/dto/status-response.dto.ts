import { ApiProperty } from '@nestjs/swagger';

export class LocalGateDto {
  @ApiProperty({ description: 'Gate name' })
  name: string;

  @ApiProperty({ enum: ['pass', 'fail', 'unknown', 'not_run'], description: 'Gate status' })
  status: 'pass' | 'fail' | 'unknown' | 'not_run';

  @ApiProperty({ description: 'Last run timestamp (ISO 8601)', required: false })
  last_run_at?: string;

  @ApiProperty({ description: 'Validation script path', required: false })
  script?: string;
}

export class StatusResponseDto {
  @ApiProperty({ description: 'State schema version from manifest' })
  state_version: string;

  @ApiProperty({ description: 'History schema version from manifest' })
  history_version: string;

  @ApiProperty({ description: 'Artifact checksums format name from manifest' })
  artifact_checksums_format: string;

  @ApiProperty({ type: [LocalGateDto], description: 'Local gates status' })
  gates: LocalGateDto[];

  @ApiProperty({ description: 'Gate metrics aggregated by status', required: false })
  gate_metrics?: { pass: number; fail: number; not_run: number; unknown: number };

  @ApiProperty({ description: 'Internal metrics snapshot', required: false })
  internal_metrics?: Record<string, number>;

  @ApiProperty({ description: 'Response timestamp (ISO 8601)' })
  timestamp: string;

  @ApiProperty({ description: 'State validation result', required: false })
  state_valid?: boolean;

  @ApiProperty({ description: 'State validation errors', required: false, type: [Object] })
  state_errors?: Array<{ instancePath: string; message: string }>;
}
