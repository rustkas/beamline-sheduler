#!/usr/bin/env python3
"""
OBS-1 Log Conformance Validator

Validates structured JSON log entries across components against
config/observability/logging.json expectations.

Usage:
  python3 tests/utils/log_conformance_validator.py --paths reports/dry-run-logs/*.log apps/gateway/dist/logs/*.log

Exit codes:
  0 = OK, 1 = violations found, 2 = error running validator
"""

import sys
import json
import argparse
from pathlib import Path

REQUIRED_FIELDS = {"timestamp", "level", "component", "message"}
OPTIONAL_FIELDS = {"context", "tenant_id", "trace_id", "error"}
LEVELS = {"ERROR", "WARN", "INFO", "DEBUG"}
PII_FIELDS = {
    "password",
    "api_key",
    "secret",
    "token",
    "access_token",
    "refresh_token",
    "authorization",
    "credit_card",
    "ssn",
    "email",
    "phone",
}

def is_masked(value: str) -> bool:
    return value == "[REDACTED]"

def validate_entry(entry: dict, file: Path, line_no: int):
    errors = []
    # Required fields
    missing = REQUIRED_FIELDS - set(entry.keys())
    if missing:
        errors.append(f"missing fields: {sorted(missing)}")
    # Level
    lvl = entry.get("level")
    if lvl and lvl not in LEVELS:
        errors.append(f"invalid level: {lvl}")
    # Component
    comp = entry.get("component")
    if comp and not isinstance(comp, str):
        errors.append("component must be string")
    # Context PII filtering
    ctx = entry.get("context", {})
    if isinstance(ctx, dict):
        for k, v in ctx.items():
            if k in PII_FIELDS:
                if not (isinstance(v, str) and is_masked(v)):
                    errors.append(f"PII not masked in context: {k}")
    # Error section PII (message is ok, stack optional)
    err = entry.get("error")
    if isinstance(err, dict):
        for k, v in err.items():
            if k in PII_FIELDS and not (isinstance(v, str) and is_masked(v)):
                errors.append(f"PII not masked in error: {k}")

    return errors

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--paths", nargs="*", default=[], help="Paths or glob patterns to log files")
    args = ap.parse_args()

    if not args.paths:
        print("[WARN] No log paths provided; nothing to validate")
        return 0

    files = []
    for p in args.paths:
        for matched in Path().glob(p):
            if matched.is_file():
                files.append(matched)

    if not files:
        print("[WARN] No log files matched")
        return 0

    violations = 0
    for f in files:
        try:
            for i, line in enumerate(f.read_text().splitlines(), start=1):
                line = line.strip()
                if not line:
                    continue
                try:
                    entry = json.loads(line)
                except json.JSONDecodeError:
                    print(f"[FAIL] {f}:{i}: not valid JSON line")
                    violations += 1
                    continue
                errs = validate_entry(entry, f, i)
                if errs:
                    for e in errs:
                        print(f"[FAIL] {f}:{i}: {e}")
                    violations += 1
        except Exception as ex:
            print(f"[ERROR] Failed reading {f}: {ex}")
            return 2

    if violations:
        print(f"[FAIL] OBS-1 violations found: {violations}")
        return 1
    print("[OK] OBS-1 log conformance passed")
    return 0

if __name__ == "__main__":
    sys.exit(main())

