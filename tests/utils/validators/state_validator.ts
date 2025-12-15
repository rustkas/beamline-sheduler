/**
 * State validator utilities for testing
 * Validates .trae/state.json against JSON-Schema
 */

import * as fs from 'fs';
import * as path from 'path';
import Ajv from 'ajv';
import addFormats from 'ajv-formats';

const ajv = new Ajv({ allErrors: true, strict: false });
addFormats(ajv);

const STATE_SCHEMA_PATH = path.join(__dirname, '../../../docs/STATE.schema.json');

export interface ValidationResult {
  valid: boolean;
  errors?: string[];
}

/**
 * Load and validate state JSON against schema
 */
export function validateState(statePath: string): ValidationResult {
  try {
    const stateContent = fs.readFileSync(statePath, 'utf-8');
    const state = JSON.parse(stateContent);
    
    const schemaContent = fs.readFileSync(STATE_SCHEMA_PATH, 'utf-8');
    const schema = JSON.parse(schemaContent);
    
    // Create fresh AJV instance to avoid schema registration conflicts
    const freshAjv = new Ajv({ allErrors: true, strict: false });
    addFormats(freshAjv);
    
    const validate = freshAjv.compile(schema);
    const valid = validate(state);
    
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
 * Validate state structure (basic checks)
 */
export function validateStateStructure(state: any): ValidationResult {
  const errors: string[] = [];
  
  if (!state.project || typeof state.project !== 'string') {
    errors.push('Missing or invalid project field');
  }
  
  if (!state.version || typeof state.version !== 'string') {
    errors.push('Missing or invalid version field');
  }
  
  if (!state.current_cp || typeof state.current_cp !== 'string') {
    errors.push('Missing or invalid current_cp field');
  }
  
  if (typeof state.no_drift !== 'boolean') {
    errors.push('Missing or invalid no_drift field');
  }
  
  if (!Array.isArray(state.agents)) {
    errors.push('Missing or invalid agents array');
  }
  
  if (!Array.isArray(state.artifact_checksums)) {
    errors.push('Missing or invalid artifact_checksums array');
  }
  
  if (!state.updated_at || typeof state.updated_at !== 'string') {
    errors.push('Missing or invalid updated_at field');
  }
  
  return {
    valid: errors.length === 0,
    errors: errors.length > 0 ? errors : undefined
  };
}

