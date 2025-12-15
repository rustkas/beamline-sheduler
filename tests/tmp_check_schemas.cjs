const Ajv = require('ajv').default;
const fs = require('fs');

const ajv = new Ajv({ allErrors: true, strict: false });

function check(schemaPath, dataPath, label){
  const schema = JSON.parse(fs.readFileSync(schemaPath, 'utf-8'));
  const data = JSON.parse(fs.readFileSync(dataPath, 'utf-8'));
  const validate = ajv.compile(schema);
  const ok = validate(data);
  console.log(`[${label}] valid=`, ok);
  if(!ok){
    console.log(`[${label}] errors=`, validate.errors);
  }
}

check('docs/HISTORY.schema.json','tests/fixtures/history/invalid_history_broken_chain.json','history-broken');
check('docs/STATE.schema.json','tests/fixtures/state/invalid_state_no_drift_false.json','state-no-drift-false');
