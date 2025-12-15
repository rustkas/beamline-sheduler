import { Controller, Get } from '@nestjs/common';
import { MetricsService } from '../observability/metrics.service';

@Controller('metrics/internal')
export class MetricsController {
  constructor(private readonly metrics: MetricsService) {}

  @Get()
  async getInternalMetrics(): Promise<string> {
    return await this.metrics.render();
  }

  @Get('summary')
  getSummary(): Record<string, Record<string, { ok: number; nak: number; redelivered: number; exhausted: number }>> {
    return this.metrics.getNatsSummary();
  }
}
