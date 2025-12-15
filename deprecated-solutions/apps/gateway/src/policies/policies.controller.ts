import { Controller, Get, Post, Put, Delete, Body, Param, UseGuards, Headers } from '@nestjs/common';
import { ApiTags, ApiOperation, ApiResponse } from '@nestjs/swagger';
import { PoliciesService } from './policies.service';
import { PolicyDto } from './dto/policy.dto';
import { ThrottlerGuard, Throttle } from '@nestjs/throttler';
import { ReconcileDto } from './dto/reconcile.dto';
import { AdminApiGuard } from '../auth/admin-api.guard';

@ApiTags('policies')
@Controller('api/v1/policies')
@UseGuards(ThrottlerGuard, AdminApiGuard)
export class PoliciesController {
  constructor(private readonly policiesService: PoliciesService) {}

  @Get(':tenantId')
  @ApiOperation({ summary: 'Get all policies for a tenant' })
  @ApiResponse({ status: 200, description: 'Policies retrieved successfully', type: [PolicyDto] })
  async findAll(@Param('tenantId') tenantId: string): Promise<PolicyDto[]> {
    return this.policiesService.findAll(tenantId);
  }

  @Get(':tenantId/:policyId')
  @ApiOperation({ summary: 'Get policy by ID' })
  @ApiResponse({ status: 200, description: 'Policy retrieved successfully', type: PolicyDto })
  @ApiResponse({ status: 404, description: 'Policy not found' })
  async findOne(
    @Param('tenantId') tenantId: string,
    @Param('policyId') policyId: string,
  ): Promise<PolicyDto> {
    return this.policiesService.findOne(tenantId, policyId);
  }

  @Post(':tenantId')
  @ApiOperation({ summary: 'Create a new policy' })
  @ApiResponse({ status: 201, description: 'Policy created successfully', type: PolicyDto })
  async create(@Param('tenantId') tenantId: string, @Body() policy: PolicyDto): Promise<PolicyDto> {
    return this.policiesService.create(tenantId, policy);
  }

  @Put(':tenantId/:policyId')
  @ApiOperation({ summary: 'Update a policy' })
  @ApiResponse({ status: 200, description: 'Policy updated successfully', type: PolicyDto })
  async update(
    @Param('tenantId') tenantId: string,
    @Param('policyId') policyId: string,
    @Body() policy: PolicyDto,
  ): Promise<PolicyDto> {
    return this.policiesService.update(tenantId, policyId, policy);
  }

  @Delete(':tenantId/:policyId')
  @ApiOperation({ summary: 'Delete a policy' })
  @ApiResponse({ status: 200, description: 'Policy deleted successfully' })
  async delete(
    @Param('tenantId') tenantId: string,
    @Param('policyId') policyId: string,
  ): Promise<void> {
    return this.policiesService.delete(tenantId, policyId);
  }

  @Post(':tenantId/sync')
  @ApiOperation({ summary: 'Sync policies from Router Admin for tenant' })
  @ApiResponse({ status: 200, description: 'Policies synchronized successfully', type: [PolicyDto] })
  @Throttle({ default: { ttl: 60_000, limit: 10 } })
  async sync(@Param('tenantId') tenantId: string): Promise<PolicyDto[]> {
    return this.policiesService.syncPolicies(tenantId);
  }

  @Post(':tenantId/reconcile')
  @ApiOperation({ summary: 'Reconcile tenant policies with desired set' })
  @ApiResponse({ status: 200, description: 'Policies reconciled successfully' })
  @Throttle({ default: { ttl: 60_000, limit: 5 } })
  async reconcile(@Param('tenantId') tenantId: string, @Body() body: ReconcileDto): Promise<void> {
    const desired = Array.isArray(body?.desired) ? body.desired : [];
    await this.policiesService.reconcilePolicies(tenantId, desired);
  }
}
