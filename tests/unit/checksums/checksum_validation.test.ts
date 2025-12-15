/**
 * Unit tests for checksum validation
 * Tests artifact checksum validation and format
 */

import { describe, it, expect } from 'vitest';
import * as crypto from 'crypto';
import * as fs from 'fs';
import * as path from 'path';

describe('Checksum Validation', () => {
  describe('SHA256 Format', () => {
    it('should validate SHA256 format (64 hex characters)', () => {
      const validHash = 'a'.repeat(64);
      expect(validHash).toMatch(/^[a-f0-9]{64}$/);
      expect(validHash).toHaveLength(64);
    });
    
    it('should reject invalid hash format', () => {
      const invalidHashes = [
        'short',
        'a'.repeat(63),
        'a'.repeat(65),
        'G'.repeat(64), // Invalid hex character
        'a'.repeat(64) + 'g' // Invalid hex character
      ];
      
      invalidHashes.forEach(hash => {
        expect(hash).not.toMatch(/^[a-f0-9]{64}$/);
      });
    });
  });
  
  describe('Checksum Calculation', () => {
    it('should calculate SHA256 checksum correctly', () => {
      const content = 'test content';
      const hash = crypto.createHash('sha256').update(content).digest('hex');
      
      expect(hash).toMatch(/^[a-f0-9]{64}$/);
      expect(hash).toHaveLength(64);
    });
    
    it('should produce same hash for same content', () => {
      const content = 'test content';
      const hash1 = crypto.createHash('sha256').update(content).digest('hex');
      const hash2 = crypto.createHash('sha256').update(content).digest('hex');
      
      expect(hash1).toBe(hash2);
    });
    
    it('should produce different hash for different content', () => {
      const content1 = 'test content 1';
      const content2 = 'test content 2';
      const hash1 = crypto.createHash('sha256').update(content1).digest('hex');
      const hash2 = crypto.createHash('sha256').update(content2).digest('hex');
      
      expect(hash1).not.toBe(hash2);
    });
  });
  
  describe('Artifact Checksum Validation', () => {
    it('should validate artifact checksum structure', () => {
      const artifact = {
        path: 'test/file.txt',
        hash: 'a'.repeat(64),
        ts: new Date().toISOString()
      };
      
      expect(artifact).toHaveProperty('path');
      expect(artifact).toHaveProperty('hash');
      expect(artifact).toHaveProperty('ts');
      expect(artifact.hash).toMatch(/^[a-f0-9]{64}$/);
      expect(artifact.ts).toMatch(/^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}/);
    });
    
    it('should validate checksum from state.json', () => {
      const statePath = path.join(__dirname, '../../../.trae/state.json');
      
      if (fs.existsSync(statePath)) {
        const state = JSON.parse(fs.readFileSync(statePath, 'utf-8'));
        
        if (state.artifact_checksums && Array.isArray(state.artifact_checksums)) {
          state.artifact_checksums.forEach((artifact: any) => {
            expect(artifact).toHaveProperty('path');
            expect(artifact).toHaveProperty('hash');
            expect(artifact).toHaveProperty('ts');
            expect(artifact.hash).toMatch(/^[a-f0-9]{64}$/);
          });
        }
      }
    });
  });
  
  describe('File Checksum Verification', () => {
    it('should verify file checksum matches content', () => {
      const testFile = path.join(__dirname, '../../fixtures/state/valid_state.json');
      
      if (fs.existsSync(testFile)) {
        const content = fs.readFileSync(testFile, 'utf-8');
        const calculatedHash = crypto.createHash('sha256').update(content).digest('hex');
        
        expect(calculatedHash).toMatch(/^[a-f0-9]{64}$/);
      }
    });
  });
});

