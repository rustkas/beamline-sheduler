/**
 * History validator utilities for testing
 * Validates .trae/history.json and HMAC chain integrity
 */

import * as fs from 'fs';
import * as path from 'path';
import * as crypto from 'crypto';
import Ajv from 'ajv';
import addFormats from 'ajv-formats';

const ajv = new Ajv({ allErrors: true, strict: false });
addFormats(ajv);

const HISTORY_SCHEMA_PATH = path.join(__dirname, '../../../docs/HISTORY.schema.json');

export interface ValidationResult {
  valid: boolean;
  errors?: string[];
}

export interface HMACValidationResult {
  valid: boolean;
  errors?: string[];
  brokenChainIndex?: number;
}

/**
 * Load and validate history JSON against schema
 */
export function validateHistory(historyPath: string): ValidationResult {
  try {
    const historyContent = fs.readFileSync(historyPath, 'utf-8');
    const history = JSON.parse(historyContent);
    
    const schemaContent = fs.readFileSync(HISTORY_SCHEMA_PATH, 'utf-8');
    const schema = JSON.parse(schemaContent);
    
    // Create fresh AJV instance to avoid schema registration conflicts
    const freshAjv = new Ajv({ allErrors: true, strict: false });
    addFormats(freshAjv);
    
    const validate = freshAjv.compile(schema);
    const valid = validate(history);
    
    if (!valid) {
      return {
        valid: false,
        errors: validate.errors?.map(err => `${err.instancePath}: ${err.message}`) || []
      };
    }
    
    return { valid: true };
  } catch (error) {
    return {
      valid: false,
      errors: [error instanceof Error ? error.message : 'Unknown error']
    };
  }
}

/**
 * Calculate HMAC for history entry
 */
export function calculateHMAC(
  secret: string,
  ts: string,
  actor: string,
  action: string,
  stateChecksum: string,
  hmacPrev: string
): string {
  const data = `${ts}${actor}${action}${stateChecksum}${hmacPrev}`;
  return crypto.createHmac('sha256', secret).update(data).digest('hex');
}

/**
 * Validate HMAC chain integrity
 */
export function validateHMACChain(
  history: any[],
  secret: string
): HMACValidationResult {
  const errors: string[] = [];
  let brokenChainIndex: number | undefined;
  
  for (let i = 0; i < history.length; i++) {
    const entry = history[i];
    const prevEntry = i > 0 ? history[i - 1] : null;
    
    const expectedHmacPrev = prevEntry ? prevEntry.hmac : '';
    const actualHmacPrev = entry.hmac_prev;
    
    if (expectedHmacPrev !== actualHmacPrev) {
      errors.push(`Entry ${i}: hmac_prev mismatch. Expected: ${expectedHmacPrev}, Got: ${actualHmacPrev}`);
      brokenChainIndex = i;
    }
    
    const expectedHmac = calculateHMAC(
      secret,
      entry.ts,
      entry.actor,
      entry.action,
      entry.state_checksum,
      expectedHmacPrev
    );
    
    if (entry.hmac !== expectedHmac) {
      errors.push(`Entry ${i}: HMAC mismatch. Expected: ${expectedHmac}, Got: ${entry.hmac}`);
      if (brokenChainIndex === undefined) {
        brokenChainIndex = i;
      }
    }
  }
  
  return {
    valid: errors.length === 0,
    errors: errors.length > 0 ? errors : undefined,
    brokenChainIndex
  };
}

/**
 * Validate history structure (basic checks)
 */
export function validateHistoryStructure(history: any[]): ValidationResult {
  const errors: string[] = [];
  
  if (!Array.isArray(history)) {
    return {
      valid: false,
      errors: ['History must be an array']
    };
  }
  
  for (let i = 0; i < history.length; i++) {
    const entry = history[i];
    
    if (!entry.ts || typeof entry.ts !== 'string') {
      errors.push(`Entry ${i}: Missing or invalid ts field`);
    }
    
    if (!entry.actor || typeof entry.actor !== 'string') {
      errors.push(`Entry ${i}: Missing or invalid actor field`);
    }
    
    if (!entry.action || typeof entry.action !== 'string') {
      errors.push(`Entry ${i}: Missing or invalid action field`);
    }
    
    if (entry.cp_from !== null && typeof entry.cp_from !== 'string') {
      errors.push(`Entry ${i}: Invalid cp_from field (must be string or null)`);
    }
    
    if (!entry.cp_to || typeof entry.cp_to !== 'string') {
      errors.push(`Entry ${i}: Missing or invalid cp_to field`);
    }
    
    if (!entry.state_checksum || typeof entry.state_checksum !== 'string') {
      errors.push(`Entry ${i}: Missing or invalid state_checksum field`);
    }
    
    if (typeof entry.hmac_prev !== 'string') {
      errors.push(`Entry ${i}: Missing or invalid hmac_prev field`);
    }
    
    if (!entry.hmac || typeof entry.hmac !== 'string') {
      errors.push(`Entry ${i}: Missing or invalid hmac field`);
    }
  }
  
  return {
    valid: errors.length === 0,
    errors: errors.length > 0 ? errors : undefined
  };
}

