import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import Ajv from 'ajv';
import addFormats from 'ajv-formats';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const ajv = new Ajv({ allErrors: true, strict: false });
addFormats(ajv);

// Test state validation
console.log('=== Testing State Validation ===');
const statePath = path.join(__dirname, 'fixtures/state/invalid_state_no_drift_false.json');
const stateSchemaPath = path.join(__dirname, '../docs/STATE.schema.json');

try {
  const stateContent = fs.readFileSync(statePath, 'utf-8');
  const state = JSON.parse(stateContent);
  
  const schemaContent = fs.readFileSync(stateSchemaPath, 'utf-8');
  const schema = JSON.parse(schemaContent);
  
  const validate = ajv.compile(schema);
  const valid = validate(state);
  
  console.log('State validation result:', valid);
  if (!valid) {
    console.log('Validation errors:', validate.errors);
  }
} catch (error) {
  console.log('State validation error:', error.message);
}

// Test history validation
console.log('\n=== Testing History Validation ===');
const historyPath = path.join(__dirname, 'fixtures/history/invalid_history_broken_chain.json');
const historySchemaPath = path.join(__dirname, '../docs/HISTORY.schema.json');

try {
  const historyContent = fs.readFileSync(historyPath, 'utf-8');
  const history = JSON.parse(historyContent);
  
  const schemaContent = fs.readFileSync(historySchemaPath, 'utf-8');
  const schema = JSON.parse(schemaContent);
  
  const validate = ajv.compile(schema);
  const valid = validate(history);
  
  console.log('History validation result:', valid);
  if (!valid) {
    console.log('Validation errors:', validate.errors);
  }
  
  // Check individual entries
  history.forEach((entry, index) => {
    console.log(`Entry ${index}:`);
    console.log(`  state_checksum: "${entry.state_checksum}"`);
    console.log(`  state_checksum length: ${entry.state_checksum?.length}`);
    console.log(`  hmac: "${entry.hmac}"`);
    console.log(`  hmac length: ${entry.hmac?.length}`);
    console.log(`  hmac_prev: "${entry.hmac_prev}"`);
    console.log(`  hmac_prev length: ${entry.hmac_prev?.length}`);
    
    // Test individual entry validation
    const entryValid = validate([entry]);
    console.log(`  Individual entry valid: ${entryValid}`);
    if (!entryValid) {
      console.log(`  Entry errors:`, validate.errors);
    }
  });
} catch (error) {
  console.log('History validation error:', error.message);
}