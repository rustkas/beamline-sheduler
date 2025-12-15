const Ajv = require('ajv').default;
const fs = require('fs');

const ajv = new Ajv({allErrors: true});
const schema = JSON.parse(fs.readFileSync('docs/HISTORY.schema.json', 'utf-8'));
const data = JSON.parse(fs.readFileSync('tests/fixtures/history/valid_history.json', 'utf-8'));

const validate = ajv.compile(schema);
const valid = validate(data);

console.log('Valid:', valid);
if (!valid) {
  console.log('Errors:', JSON.stringify(validate.errors, null, 2));
}
