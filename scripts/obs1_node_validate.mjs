#!/usr/bin/env node
// OBS-1 JSON log conformance validator (Node.js, no deps)
// Usage: node scripts/obs1_node_validate.mjs <log_file>
// Exits non-zero if validation fails.

import fs from 'node:fs';
import readline from 'node:readline';

if (process.argv.length < 3) {
  console.error('Usage: node scripts/obs1_node_validate.mjs <log_file>');
  process.exit(2);
}

const logFile = process.argv[2];
if (!fs.existsSync(logFile)) {
  console.error(`[ERROR] File not found: ${logFile}`);
  process.exit(2);
}

const requiredKeys = ['timestamp', 'level', 'msg', 'trace_id', 'tenant_id'];
let total = 0;
let ok = 0;
let fail = 0;

const rl = readline.createInterface({
  input: fs.createReadStream(logFile, 'utf8'),
  crlfDelay: Infinity
});

rl.on('line', (line) => {
  total++;
  if (!line || !line.trim()) return;
  try {
    const obj = JSON.parse(line);

    // Required keys present
    const missing = requiredKeys.filter(k => !(k in obj));
    if (missing.length) {
      console.log(`[FAIL] Line ${total}: missing keys: ${missing.join(',')}`);
      fail++;
      return;
    }

    // Basic types
    const typeOk = typeof obj.timestamp === 'string'
      && typeof obj.level === 'string'
      && typeof obj.msg === 'string'
      && typeof obj.trace_id === 'string'
      && typeof obj.tenant_id === 'string';
    if (!typeOk) {
      console.log(`[FAIL] Line ${total}: type mismatch for required keys`);
      fail++;
      return;
    }

    // Sensitive fields masking (if present)
    const sensitiveKeys = ['api_key', 'token', 'secret', 'password'];
    for (const k of sensitiveKeys) {
      if (k in obj) {
        const v = String(obj[k]);
        if (!(v === '[REDACTED]' || v === '[MASKED]')) {
          console.log(`[FAIL] Line ${total}: sensitive field '${k}' not masked`);
          fail++;
          return;
        }
      }
    }

    ok++;
  } catch (e) {
    console.log(`[FAIL] Line ${total}: not valid JSON`);
    fail++;
  }
});

rl.on('close', () => {
  if (fail === 0) {
    console.log(`[OK] OBS-1 conformance passed (${ok}/${total})`);
    process.exit(0);
  } else {
    console.log(`[FAIL] OBS-1 conformance failed (${fail} failures out of ${total} lines)`);
    process.exit(1);
  }
});
