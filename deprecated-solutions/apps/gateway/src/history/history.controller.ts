import { Controller, Get, Query, HttpCode, HttpStatus } from '@nestjs/common';
import { ApiTags, ApiOperation, ApiResponse, ApiQuery } from '@nestjs/swagger';
import { HistoryService, HistoryResponse } from './history.service';
import { HistoryResponseDto } from './dto/history-response.dto';

@ApiTags('history')
@Controller('history')
export class HistoryController {
  constructor(private readonly historyService: HistoryService) {}

  @Get()
  @HttpCode(HttpStatus.OK)
  @ApiOperation({ summary: 'Get paginated history with HMAC masking' })
  @ApiQuery({
    name: 'page',
    required: false,
    type: Number,
    description: 'Page number (default: 1)',
  })
  @ApiQuery({
    name: 'page_size',
    required: false,
    type: Number,
    description: 'Page size (default: 50, max: 200)',
  })
  @ApiResponse({
    status: 200,
    description: 'History retrieved successfully with masked HMAC values',
    type: HistoryResponseDto,
  })
  @ApiResponse({ status: 400, description: 'Invalid pagination parameters' })
  getHistory(@Query('page') page?: string, @Query('page_size') pageSize?: string): HistoryResponse {
    const pageNum = page ? Math.max(1, parseInt(page, 10)) : 1;
    const pageSizeNum = pageSize ? Math.min(200, Math.max(1, parseInt(pageSize, 10))) : 50;

    return this.historyService.getHistory(pageNum, pageSizeNum);
  }
}
