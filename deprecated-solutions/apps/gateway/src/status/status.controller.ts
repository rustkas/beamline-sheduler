import { Controller, Get } from '@nestjs/common';
import { ApiTags, ApiOperation, ApiResponse } from '@nestjs/swagger';
import * as fs from 'fs';
import * as path from 'path';
import * as http from 'http';
import * as https from 'https';
import { URL } from 'url';
import Ajv from 'ajv';
import addFormats from 'ajv-formats';
import { StatusResponseDto, LocalGateDto } from './dto/status-response.dto';
import { InternalMetrics } from '../observability/internal-metrics.store';

@ApiTags('status')
@Controller('status')
export class StatusController {
  private readonly ajv: Ajv;

  constructor() {
    this.ajv = new Ajv({ allErrors: true, strict: false });
    addFormats(this.ajv);
  }

  private async verifyDevState(): Promise<'pass' | 'fail' | 'not_run'> {
    const baseUrl = process.env.DEVSTATE_BASE_URL || 'http://trae-devstate:3080';
    const url = process.env.DEVSTATE_VERIFY_URL || `${baseUrl}/v1/devstate/verify?limit=0`;
    try {
      const urlObj = new URL(url);
      const lib = urlObj.protocol === 'https:' ? https : http;
      const requestPromise = new Promise<'pass' | 'fail' | 'not_run'>((resolve) => {
        const req = lib.get(urlObj, (res) => {
          let data = '';
          res.on('data', (chunk) => (data += chunk));
          res.on('end', () => {
            try {
              const json = JSON.parse(data || '{}');
              const ok = (json.valid ?? json.ok ?? json.status === 'ok') as boolean | undefined;
              resolve(ok ? 'pass' : 'fail');
            } catch {
              resolve('fail');
            }
          });
        });
        req.on('error', () => resolve('not_run'));
      });
      const timeoutPromise = new Promise<'not_run'>((resolve) =>
        setTimeout(() => resolve('not_run'), 250),
      );
      return await Promise.race([requestPromise, timeoutPromise]);
    } catch {
      return 'not_run';
    }
  }

  @Get()
  @ApiOperation({ summary: 'Public status endpoint with versions and gates' })
  @ApiResponse({ status: 200, description: 'Status info', type: StatusResponseDto })
  async getStatus(): Promise<StatusResponseDto> {
    const manifestPath = path.join(process.cwd(), '.trae', 'manifest.json');
    const statePath = path.join(process.cwd(), '.trae', 'state.json');

    // Read manifest.json
    let manifest: Record<string, unknown> = {};
    try {
      manifest = JSON.parse(fs.readFileSync(manifestPath, 'utf-8')) as Record<string, unknown>;
    } catch (error) {
      console.warn('Failed to read manifest.json:', error);
    }

    // Read state.json
    let state: Record<string, unknown> = {};
    let stateValid = true;
    let stateErrors: Array<{ instancePath: string; message: string }> = [];

    try {
      state = JSON.parse(fs.readFileSync(statePath, 'utf-8')) as Record<string, unknown>;

      // Validate state against docs/STATE.schema.json
      const schemaPath = path.join(process.cwd(), 'docs', 'STATE.schema.json');
      try {
        const schema = JSON.parse(fs.readFileSync(schemaPath, 'utf-8'));
        const validate = this.ajv.compile(schema);
        stateValid = validate(state) as boolean;
        if (!stateValid && validate.errors) {
          stateErrors = validate.errors.map((e) => ({
            instancePath: e.instancePath || '',
            message: e.message || '',
          }));
        }
      } catch {
        // Schema not found or invalid - skip validation
        console.warn('STATE.schema.json not found or invalid, skipping validation');
      }
    } catch (error) {
      console.warn('Failed to read state.json:', error);
      stateValid = false;
    }

    // Extract versions from manifest
    const schemaVersions = (manifest.schema_versions as Record<string, { version: string }>) || {};
    const stateVersion = schemaVersions.state?.version || 'unknown';
    const historyVersion = schemaVersions.history?.version || 'unknown';

    // Extract artifact_checksums_format
    const checksumsFormat =
      (manifest.artifact_checksums_format as { name?: string })?.name || 'unknown';

    // Compute additional gates
    const devstateStatus = await this.verifyDevState();
    const transport = process.env.ROUTER_CLIENT || 'mock';
    const transportValid = (() => {
      if (transport === 'http') {
        return !!process.env.ROUTER_BASE_URL;
      }
      if (transport === 'nats') {
        return !!process.env.NATS_URL;
      }
      return true;
    })();
    // Local HMAC chain integrity check
    let hmacChainOk: 'pass' | 'fail' | 'not_run' = 'not_run';
    try {
      const historyPath = path.join(process.cwd(), '.trae', 'history.json');
      const content = fs.readFileSync(historyPath, 'utf-8');
      const items = JSON.parse(content) as Array<{ hmac_prev: string; hmac: string }>;
      if (Array.isArray(items) && items.length > 0) {
        let ok = true;
        for (let i = 1; i < items.length; i++) {
          const prev = items[i - 1]?.hmac || '';
          const curPrev = items[i]?.hmac_prev || '';
          if (prev !== curPrev) {
            ok = false;
            break;
          }
        }
        hmacChainOk = ok ? 'pass' : 'fail';
      }
    } catch {
      hmacChainOk = 'not_run';
    }

    // Schema version drift gates
    let schemaStateDrift: 'pass' | 'fail' | 'not_run' = 'not_run';
    let schemaHistoryDrift: 'pass' | 'fail' | 'not_run' = 'not_run';
    try {
      const stateSchemaPath = path.join(process.cwd(), 'docs', 'STATE.schema.json');
      const stateSchema = JSON.parse(fs.readFileSync(stateSchemaPath, 'utf-8')) as {
        $id?: string;
        version?: string;
      };
      if (stateVersion !== 'unknown' && (stateSchema.version || stateSchema.$id)) {
        const actual = stateSchema.version || stateSchema.$id;
        schemaStateDrift = actual === stateVersion ? 'pass' : 'fail';
      }
    } catch {
      schemaStateDrift = 'not_run';
    }
    try {
      const historySchemaPath = path.join(process.cwd(), 'docs', 'HISTORY.schema.json');
      const historySchema = JSON.parse(fs.readFileSync(historySchemaPath, 'utf-8')) as {
        $id?: string;
        version?: string;
      };
      if (historyVersion !== 'unknown' && (historySchema.version || historySchema.$id)) {
        const actual = historySchema.version || historySchema.$id;
        schemaHistoryDrift = actual === historyVersion ? 'pass' : 'fail';
      }
    } catch {
      schemaHistoryDrift = 'not_run';
    }

    const gates: LocalGateDto[] = [
      {
        name: 'state_validation',
        status: stateValid ? 'pass' : 'fail',
        script: 'scripts/validate_state.sh',
      },
      {
        name: 'devstate_verify',
        status: devstateStatus,
        script: 'devstate/scripts/devstate_verify.sh',
      },
      {
        name: 'router_transport',
        status: transportValid ? 'pass' : 'fail',
        script: `transport:${transport}`,
      },
      {
        name: 'hmac_chain_integrity',
        status: hmacChainOk,
        script: 'devstate/scripts/devstate_verify.sh',
      },
      {
        name: 'schema_validation',
        status: 'not_run',
        script: 'scripts/check_schema_changes.sh',
      },
      {
        name: 'schema_version_drift_state',
        status: schemaStateDrift,
        script: 'scripts/check_schema_version.sh',
      },
      {
        name: 'schema_version_drift_history',
        status: schemaHistoryDrift,
        script: 'scripts/check_schema_version.sh',
      },
      {
        name: 'hmac_masking',
        status: 'not_run',
        script: 'scripts/check_hmac_masking.sh',
      },
    ];

    return {
      state_version: stateVersion,
      history_version: historyVersion,
      artifact_checksums_format: checksumsFormat,
      gates,
      gate_metrics: {
        pass: gates.filter((g) => g.status === 'pass').length,
        fail: gates.filter((g) => g.status === 'fail').length,
        not_run: gates.filter((g) => g.status === 'not_run').length,
        unknown: gates.filter((g) => g.status === 'unknown').length,
      },
      internal_metrics: {
        ...InternalMetrics,
        router_http_retries_max: parseInt(process.env.ROUTER_HTTP_RETRY_MAX ?? '3', 10),
        router_nats_retries_max: parseInt(process.env.ROUTER_NATS_RETRY_MAX ?? '3', 10),
        nats_request_timeout_ms: parseInt(process.env.NATS_REQUEST_TIMEOUT_MS ?? '1000', 10),
      } as Record<string, number>,
      timestamp: new Date().toISOString(),
      state_valid: stateValid,
      state_errors: stateErrors.length > 0 ? stateErrors : undefined,
    };
  }
}
