import {
  Controller,
  Post,
  Body,
  HttpCode,
  HttpStatus,
  Get,
  Param,
  UseGuards,
} from '@nestjs/common';
import { ApiTags, ApiOperation, ApiResponse, ApiBody } from '@nestjs/swagger';
import { RoutesService } from './routes.service';
import { RouteRequestDto } from './dto/route-request.dto';
import { RouteDecisionDto } from './dto/route-decision.dto';
import { Throttle } from '@nestjs/throttler';
import { rateLimitConfig } from '../config/rate-limit.config';
import { RBACGuard } from '../common/guards/rbac.guard';

@ApiTags('routes')
@Controller('api/v1/routes')
export class RoutesController {
  constructor(private readonly routesService: RoutesService) {}

  @Post('decide')
  @UseGuards(RBACGuard)
  @HttpCode(HttpStatus.OK)
  @Throttle({
    default: {
      limit: rateLimitConfig.routesDecideLimit,
      ttl: rateLimitConfig.ttlSeconds * 1000,
    },
  })
  @ApiOperation({ summary: 'Route a message to a provider' })
  @ApiBody({ type: RouteRequestDto })
  @ApiResponse({
    status: 200,
    description: 'Route decision made successfully',
    type: RouteDecisionDto,
  })
  @ApiResponse({ status: 400, description: 'Invalid request' })
  @ApiResponse({ status: 500, description: 'Internal server error' })
  async decide(@Body() routeRequest: RouteRequestDto): Promise<RouteDecisionDto> {
    const tenant = routeRequest.message?.tenant_id;
    const assignmentId = (routeRequest as any)?.assignment_id;
    const ackId = (routeRequest as any)?.ack_id;
    const usageId = (routeRequest as any)?.usage_id;
    try {
      const idem = new (await import('../common/services/idempotency.service')).IdempotencyService();
      if (tenant && assignmentId) idem.markHttpIdTenant('assignment_id', tenant, assignmentId);
      if (tenant && ackId) idem.markHttpIdTenant('ack_id', tenant, ackId);
      if (tenant && usageId) idem.markHttpIdTenant('usage_id', tenant, usageId);
    } catch {}
    return this.routesService.decide(routeRequest);
  }

  @Get('decide/:messageId')
  @ApiOperation({ summary: 'Get route decision for a message' })
  @ApiResponse({
    status: 200,
    description: 'Route decision retrieved successfully',
    type: RouteDecisionDto,
  })
  @ApiResponse({ status: 404, description: 'Message not found' })
  async getDecision(@Param('messageId') messageId: string): Promise<RouteDecisionDto> {
    return this.routesService.getDecision(messageId);
  }
}
