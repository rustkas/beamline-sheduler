import { Body, Controller, HttpCode, HttpStatus, Post } from '@nestjs/common';
import { ApiBody, ApiOperation, ApiResponse, ApiTags } from '@nestjs/swagger';
import { FlowsService } from './flows.service';
import { FlowValidateRequestDto } from './dto/flow-validate-request.dto';
import { FlowValidateResponseDto } from './dto/flow-validate-response.dto';
import { FlowDryRunRequestDto } from './dto/flow-dry-run-request.dto';
import { FlowDryRunResponseDto } from './dto/flow-dry-run-response.dto';

@ApiTags('flows')
@Controller('api/v1/flows')
export class FlowsController {
  constructor(private readonly flowsService: FlowsService) {}

  @Post('validate')
  @HttpCode(HttpStatus.OK)
  @ApiOperation({ summary: 'Validate Flow DSL definition against JSON Schema' })
  @ApiBody({ type: FlowValidateRequestDto })
  @ApiResponse({ status: 200, description: 'Validation result', type: FlowValidateResponseDto })
  @ApiResponse({ status: 400, description: 'Invalid request' })
  async validate(@Body() req: FlowValidateRequestDto): Promise<FlowValidateResponseDto> {
    return this.flowsService.validate(req);
  }

  @Post('dry-run')
  @HttpCode(HttpStatus.OK)
  @ApiOperation({ summary: 'Dry-run Flow execution: compile and simulate latencies' })
  @ApiBody({ type: FlowDryRunRequestDto })
  @ApiResponse({ status: 200, description: 'Dry-run result', type: FlowDryRunResponseDto })
  @ApiResponse({ status: 400, description: 'Invalid request' })
  async dryRun(@Body() req: FlowDryRunRequestDto): Promise<FlowDryRunResponseDto> {
    return this.flowsService.dryRun(req);
  }
}
