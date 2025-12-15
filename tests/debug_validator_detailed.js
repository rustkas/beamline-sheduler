import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import Ajv from 'ajv';
import addFormats from 'ajv-formats';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const ajv = new Ajv({ allErrors: true, strict: false });
addFormats(ajv);

// Test the exact same thing as the unit test
console.log('=== Testing State Validation (like unit test) ===');
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
  console.log('Expected: true (according to test)');
  console.log('Actual:', valid);
  
  if (!valid) {
    console.log('Validation errors:', validate.errors);
    console.log('Error details:');
    validate.errors?.forEach((err, i) => {
      console.log(`  ${i + 1}. ${err.instancePath}: ${err.message}`);
      console.log(`     Data: ${JSON.stringify(err.data)}`);
      console.log(`     Schema: ${JSON.stringify(err.schema)}`);
    });
  }
} catch (error) {
  console.log('State validation error:', error.message);
  console.log('Stack:', error.stack);
}

// Test history validation
console.log('\n=== Testing History Validation (like unit test) ===');
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
  console.log('Expected: true (according to test)');
  console.log('Actual:', valid);
  
  if (!valid) {
    console.log('Validation errors:', validate.errors);
    console.log('Error details:');
    validate.errors?.forEach((err, i) => {
      console.log(`  ${i + 1}. ${err.instancePath}: ${err.message}`);
      console.log(`     Data: ${JSON.stringify(err.data)}`);
      console.log(`     Schema: ${JSON.stringify(err.schema)}`);
    });
  }
} catch (error) {
  console.log('History validation error:', error.message);
  console.log('Stack:', error.stack);
}