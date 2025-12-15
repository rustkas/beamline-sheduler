/**
 * Integration tests for dry-run validation
 * Tests dry-run validation flows without actual execution
 */

import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import { exec } from 'child_process';
import { promisify } from 'util';
import * as path from 'path';
import * as fs from 'fs';

const execAsync = promisify(exec);

const PROJECT_ROOT = path.join(__dirname, '../../../');
const DRY_RUN_SCRIPT = path.join(PROJECT_ROOT, 'scripts/dry_run_ci.sh');
const VALIDATE_STATE_SCRIPT = path.join(PROJECT_ROOT, 'scripts/validate_state.sh');

describe('Dry-Run Validation', () => {
  describe('validate_state.sh', () => {
    it('should validate state successfully in development mode', async () => {
      const { stdout, stderr } = await execAsync(
        `bash "${VALIDATE_STATE_SCRIPT}"`,
        { cwd: PROJECT_ROOT }
      );
      
      expect(stderr).toBe('');
      expect(stdout).toContain('[OK]');
    }, 30000);
    
    it('should validate state with HMAC secret', async () => {
      const { stdout, stderr } = await execAsync(
        `BEAMLINE_HMAC_SECRET='beamline-secret-key-v1' bash "${VALIDATE_STATE_SCRIPT}"`,
        { cwd: PROJECT_ROOT }
      );
      
      expect(stderr).toBe('');
      expect(stdout).toContain('[OK]');
    }, 30000);
  });
  
  describe('dry_run_ci.sh', () => {
    it('should run GitHub Actions scenario', async () => {
      const { stdout, stderr } = await execAsync(
        `GITHUB_ACTIONS=true BEAMLINE_HMAC_SECRET='beamline-secret-key-v1' bash "${DRY_RUN_SCRIPT}" github`,
        { cwd: PROJECT_ROOT }
      );
      
      expect(stderr).toBe('');
      expect(stdout).toContain('GitHub Actions');
    }, 60000);
    
    it('should run development scenario', async () => {
      const { stdout, stderr } = await execAsync(
        `bash "${DRY_RUN_SCRIPT}" development`,
        { cwd: PROJECT_ROOT }
      );
      
      expect(stderr).toBe('');
      expect(stdout).toContain('Development');
    }, 60000);
  });
  
  describe('State File Validation', () => {
    it('should validate .trae/state.json exists', () => {
      const statePath = path.join(PROJECT_ROOT, '.trae/state.json');
      expect(fs.existsSync(statePath)).toBe(true);
    });
    
    it('should validate .trae/history.json exists', () => {
      const historyPath = path.join(PROJECT_ROOT, '.trae/history.json');
      expect(fs.existsSync(historyPath)).toBe(true);
    });
    
    it('should validate STATE.schema.json exists', () => {
      const schemaPath = path.join(PROJECT_ROOT, 'docs/STATE.schema.json');
      expect(fs.existsSync(schemaPath)).toBe(true);
    });
    
    it('should validate HISTORY.schema.json exists', () => {
      const schemaPath = path.join(PROJECT_ROOT, 'docs/HISTORY.schema.json');
      expect(fs.existsSync(schemaPath)).toBe(true);
    });
  });
  
  describe('Artifact Checksums', () => {
    it('should validate artifact checksums format', async () => {
      const statePath = path.join(PROJECT_ROOT, '.trae/state.json');
      const state = JSON.parse(fs.readFileSync(statePath, 'utf-8'));
      
      if (state.artifact_checksums && Array.isArray(state.artifact_checksums)) {
        for (const artifact of state.artifact_checksums) {
          expect(artifact).toHaveProperty('path');
          expect(artifact).toHaveProperty('hash');
          expect(artifact).toHaveProperty('ts');
          expect(artifact.hash).toMatch(/^[a-f0-9]{64}$/);
        }
      }
    });
  });
});

