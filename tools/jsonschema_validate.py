#!/usr/bin/env python3
import argparse
import json
import sys

try:
    import jsonschema
    from jsonschema import Draft7Validator
except Exception as e:
    print(f"jsonschema import error: {e}", file=sys.stderr)
    sys.exit(2)

def load_json(path: str):
    with open(path, 'r', encoding='utf-8') as f:
        return json.load(f)


def validate_schema(schema_obj):
    # Strict Draft-07 schema check
    Draft7Validator.check_schema(schema_obj)


def main():
    p = argparse.ArgumentParser(description="Strict JSON Schema (Draft-07) validator for schemas themselves")
    p.add_argument('--input', required=True, help='Path to input schema JSON file')
    p.add_argument('--output', required=True, help='Path to output schema JSON file')
    args = p.parse_args()

    try:
        in_schema = load_json(args.input)
        out_schema = load_json(args.output)
    except Exception as e:
        print(f"Failed to load schemas: {e}", file=sys.stderr)
        sys.exit(1)

    try:
        validate_schema(in_schema)
        validate_schema(out_schema)
    except jsonschema.SchemaError as se:
        print(f"SchemaError: {se}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Validation error: {e}", file=sys.stderr)
        sys.exit(1)

    # Success
    print("OK")
    sys.exit(0)

if __name__ == '__main__':
    main()
