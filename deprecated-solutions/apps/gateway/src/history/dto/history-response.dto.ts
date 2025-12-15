import { ApiProperty } from '@nestjs/swagger';

export class HistoryItemDto {
  @ApiProperty({ description: 'ISO 8601 timestamp' })
  ts: string;

  @ApiProperty({ description: 'Agent identifier' })
  actor: string;

  @ApiProperty({ description: 'Action type' })
  action: string;

  @ApiProperty({ description: 'Source checkpoint or null', nullable: true })
  cp_from: string | null;

  @ApiProperty({ description: 'Target checkpoint' })
  cp_to: string;

  @ApiProperty({ description: 'SHA256 state checksum' })
  state_checksum: string;

  @ApiProperty({ description: 'Previous HMAC (masked: first 16 hex + "...")' })
  hmac_prev: string;

  @ApiProperty({ description: 'HMAC-SHA256 (masked: first 16 hex + "...")' })
  hmac: string;

  @ApiProperty({ description: 'Additional metadata', required: false })
  metadata?: Record<string, unknown>;
}

export class HistoryPaginationDto {
  @ApiProperty({ description: 'Current page number' })
  page: number;

  @ApiProperty({ description: 'Page size' })
  page_size: number;

  @ApiProperty({ description: 'Total number of items' })
  total: number;

  @ApiProperty({ description: 'Total number of pages' })
  total_pages: number;
}

export class HistorySchemaDto {
  @ApiProperty({ description: 'Schema $id' })
  $id: string;

  @ApiProperty({ description: 'Schema version' })
  version: string;
}

export class HistoryResponseDto {
  @ApiProperty({ type: [HistoryItemDto], description: 'History items with masked HMAC' })
  items: HistoryItemDto[];

  @ApiProperty({ type: HistoryPaginationDto, description: 'Pagination information' })
  pagination: HistoryPaginationDto;

  @ApiProperty({ type: HistorySchemaDto, description: 'Schema information' })
  schema: HistorySchemaDto;
}
