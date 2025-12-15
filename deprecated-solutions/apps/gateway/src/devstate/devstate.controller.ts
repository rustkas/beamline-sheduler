import { Controller, Get, Post, Delete, Query, Body, Param } from '@nestjs/common';
import { ApiOperation, ApiResponse, ApiTags } from '@nestjs/swagger';
import * as http from 'http';
import * as https from 'https';
import { URL } from 'url';

function httpGetJson(targetUrl: string): Promise<Record<string, unknown>> {
  return new Promise((resolve, reject) => {
    try {
      const urlObj = new URL(targetUrl);
      const lib = urlObj.protocol === 'https:' ? https : http;
      const req = lib.get(urlObj, (res) => {
        let data = '';
        res.on('data', (chunk) => (data += chunk));
        res.on('end', () => {
          try {
            resolve(JSON.parse(data));
          } catch {
            reject(new Error('Invalid JSON from devstate service'));
          }
        });
      });
      req.on('error', (err) => reject(err));
    } catch (err) {
      reject(err);
    }
  });
}

function httpPostJson(
  targetUrl: string,
  payload: Record<string, unknown>,
): Promise<Record<string, unknown>> {
  return new Promise((resolve, reject) => {
    try {
      const urlObj = new URL(targetUrl);
      const lib = urlObj.protocol === 'https:' ? https : http;
      const data = JSON.stringify(payload || {});
      const options: Record<string, unknown> = {
        method: 'POST',
        hostname: urlObj.hostname,
        port: urlObj.port,
        path: urlObj.pathname + (urlObj.search || ''),
        headers: {
          'Content-Type': 'application/json',
          'Content-Length': Buffer.byteLength(data),
        },
      };
      const req = lib.request(options, (res) => {
        let body = '';
        res.on('data', (chunk) => (body += chunk));
        res.on('end', () => {
          try {
            resolve(JSON.parse(body));
          } catch {
            reject(new Error('Invalid JSON from devstate service'));
          }
        });
      });
      req.on('error', (err) => reject(err));
      req.write(data);
      req.end();
    } catch (err) {
      reject(err);
    }
  });
}

function httpDelete(targetUrl: string): Promise<Record<string, unknown>> {
  return new Promise((resolve, reject) => {
    try {
      const urlObj = new URL(targetUrl);
      const lib = urlObj.protocol === 'https:' ? https : http;
      const options: Record<string, unknown> = {
        method: 'DELETE',
        hostname: urlObj.hostname,
        port: urlObj.port,
        path: urlObj.pathname + (urlObj.search || ''),
      };
      const req = lib.request(options, (res) => {
        let body = '';
        res.on('data', (chunk) => (body += chunk));
        res.on('end', () => {
          try {
            resolve(JSON.parse(body || '{}'));
          } catch {
            reject(new Error('Invalid JSON from devstate service'));
          }
        });
      });
      req.on('error', (err) => reject(err));
      req.end();
    } catch (err) {
      reject(err);
    }
  });
}

@ApiTags('devstate')
@Controller('api/v1/devstate')
export class DevStateController {
  @Get('verify')
  @ApiOperation({ summary: 'Verify DevState HMAC chain via internal service' })
  @ApiResponse({ status: 200, description: 'Verification result' })
  async verify(@Query('limit') limit?: string): Promise<Record<string, unknown>> {
    const baseUrl = process.env.DEVSTATE_BASE_URL || 'http://trae-devstate:3080';
    const base = process.env.DEVSTATE_VERIFY_URL || `${baseUrl}/v1/devstate/verify`;
    const url = limit ? `${base}?limit=${encodeURIComponent(limit)}` : base;
    return await httpGetJson(url);
  }

  @Get('state')
  @ApiOperation({ summary: 'Get DevState current state via internal service' })
  @ApiResponse({ status: 200, description: 'Current state' })
  async state(): Promise<Record<string, unknown>> {
    const baseUrl = process.env.DEVSTATE_BASE_URL || 'http://trae-devstate:3080';
    const url = `${baseUrl}/v1/devstate/state`;
    return await httpGetJson(url);
  }

  @Post('state')
  @ApiOperation({ summary: 'Update DevState current state via internal service' })
  @ApiResponse({ status: 200, description: 'Update result' })
  async updateState(@Body() patch: Record<string, unknown>): Promise<Record<string, unknown>> {
    const baseUrl = process.env.DEVSTATE_BASE_URL || 'http://trae-devstate:3080';
    const url = `${baseUrl}/v1/devstate/state`;
    return await httpPostJson(url, patch);
  }

  @Post('history')
  @ApiOperation({ summary: 'Append DevState history entry' })
  @ApiResponse({ status: 200, description: 'Append result' })
  async appendHistory(@Body() entry: Record<string, unknown>): Promise<Record<string, unknown>> {
    const baseUrl = process.env.DEVSTATE_BASE_URL || 'http://trae-devstate:3080';
    const url = `${baseUrl}/v1/devstate/history`;
    return await httpPostJson(url, entry);
  }

  @Delete('history/:id')
  @ApiOperation({ summary: 'Tombstone DevState history entry' })
  @ApiResponse({ status: 200, description: 'Delete result' })
  async deleteHistory(@Param('id') id: string): Promise<Record<string, unknown>> {
    const baseUrl = process.env.DEVSTATE_BASE_URL || 'http://trae-devstate:3080';
    const url = `${baseUrl}/v1/devstate/history/${encodeURIComponent(id)}`;
    return await httpDelete(url);
  }

  @Get('history/search')
  @ApiOperation({ summary: 'Search DevState history' })
  @ApiResponse({ status: 200, description: 'Search results' })
  async searchHistory(@Query() q: Record<string, string>): Promise<Record<string, unknown>> {
    const baseUrl = process.env.DEVSTATE_BASE_URL || 'http://trae-devstate:3080';
    const params = new URLSearchParams(q || {}).toString();
    const url = `${baseUrl}/v1/devstate/history/search${params ? `?${params}` : ''}`;
    return await httpGetJson(url);
  }

  @Get('export')
  @ApiOperation({ summary: 'Export DevState artifacts (.trae/state.json, .trae/history.json)' })
  @ApiResponse({ status: 200, description: 'Exported artifacts' })
  async export(): Promise<Record<string, unknown>> {
    const baseUrl = process.env.DEVSTATE_BASE_URL || 'http://trae-devstate:3080';
    const url = `${baseUrl}/v1/devstate/export`;
    return await httpGetJson(url);
  }

  @Post('import')
  @ApiOperation({ summary: 'Import DevState artifacts (.trae/state.json, .trae/history.json)' })
  @ApiResponse({ status: 200, description: 'Import result' })
  async import(@Body() payload: Record<string, unknown>): Promise<Record<string, unknown>> {
    const baseUrl = process.env.DEVSTATE_BASE_URL || 'http://trae-devstate:3080';
    const url = `${baseUrl}/v1/devstate/import`;
    return await httpPostJson(url, payload);
  }

  @Get('no-drift-gate')
  @ApiOperation({ summary: 'Check No-Drift gate (blocks when no_drift=false)' })
  @ApiResponse({ status: 200, description: 'Gate status' })
  async noDriftGate(): Promise<{ gate: string; block: boolean; reason?: string }> {
    const baseUrl = process.env.DEVSTATE_BASE_URL || 'http://trae-devstate:3080';
    const url = `${baseUrl}/v1/devstate/state`;
    const state = await httpGetJson(url);
    const block = state?.no_drift === false;
    return {
      gate: 'no_drift',
      block,
      reason: block ? (state?.blocked_reason as string) || 'no_drift=false' : undefined,
    };
  }
}
