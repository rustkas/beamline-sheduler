#!/usr/bin/env python3
"""
Utility for verifying HMAC chain integrity in .trae/history.json

Usage:
    python3 scripts/verify_hmac_chain.py [--secret SECRET] [--verbose]

Options:
    --secret SECRET    Secret key for HMAC verification (default: from BEAMLINE_HMAC_SECRET)
    --verbose          Verbose output for each entry
    --help             Show help
"""

import json
import hmac
import hashlib
import sys
import os
import argparse
from pathlib import Path


def verify_hmac_chain(history_file: str, secret: str, verbose: bool = False) -> bool:
    """
    Verifies HMAC chain integrity in history.json
    
    Args:
        history_file: Path to .trae/history.json
        secret: Secret key for HMAC verification
        verbose: Verbose output
        
    Returns:
        True if chain is valid, False otherwise
    """
    try:
        with open(history_file, 'r', encoding='utf-8') as f:
            history = json.load(f)
    except FileNotFoundError:
        print(f"[FAIL] File not found: {history_file}")
        return False
    except json.JSONDecodeError as e:
        print(f"[FAIL] Invalid JSON: {e}")
        return False
    
    if not isinstance(history, list):
        print("[FAIL] History must be a JSON array")
        return False
    
    if len(history) == 0:
        print("[WARN] History is empty")
        return True
    
    print(f"[INFO] Checking {len(history)} history entries...")
    print(f"[INFO] Using secret: {'*' * 20} (length: {len(secret)})")
    
    all_valid = True
    
    for i, entry in enumerate(history):
        ts = entry.get('ts', '')
        actor = entry.get('actor', '')
        action = entry.get('action', '')
        state_checksum = entry.get('state_checksum', '')
        hmac_prev = entry.get('hmac_prev', '')
        stored_hmac = entry.get('hmac', '')
        
        # Check required fields
        missing_fields = []
        if not ts:
            missing_fields.append('ts')
        if not actor:
            missing_fields.append('actor')
        if not action:
            missing_fields.append('action')
        if not state_checksum:
            missing_fields.append('state_checksum')
        if not stored_hmac:
            missing_fields.append('hmac')
        
        if missing_fields:
            print(f"[FAIL] Entry {i}: Missing required fields: {', '.join(missing_fields)}")
            if verbose:
                print(f"  Actor: {actor}, Action: {action}, TS: {ts}")
            all_valid = False
            continue
        
        # Check hmac_prev for all entries except first
        if i > 0:
            if not hmac_prev:
                print(f"[FAIL] Entry {i}: hmac_prev is empty (should reference previous entry)")
                if verbose:
                    print(f"  Actor: {actor}, Action: {action}")
                all_valid = False
                continue
            
            prev_hmac = history[i-1].get('hmac', '')
            if hmac_prev != prev_hmac:
                print(f"[FAIL] Entry {i}: hmac_prev does not match previous entry hmac")
                if verbose:
                    print(f"  Actor: {actor}, Action: {action}")
                    print(f"  Expected: {prev_hmac[:16]}...")
                    print(f"  Actual:   {hmac_prev[:16]}...")
                all_valid = False
                continue
            
            if verbose:
                print(f"[OK] Entry {i}: hmac_prev link verified")
        else:
            # First entry should have empty hmac_prev
            if hmac_prev:
                print(f"[WARN] Entry {i}: First entry should have empty hmac_prev")
            if verbose:
                print(f"[OK] Entry {i}: Initial entry (no hmac_prev required)")
        
        # Calculate expected HMAC
        data = f"{ts}{actor}{action}{state_checksum}{hmac_prev}"
        expected_hmac = hmac.new(secret.encode(), data.encode(), hashlib.sha256).hexdigest()
        
        if stored_hmac != expected_hmac:
            print(f"[FAIL] Entry {i}: HMAC mismatch")
            if verbose:
                print(f"  Summary: {actor} / {action} at {ts}")
                print(f"  Stored:   {stored_hmac[:16]}...")
                print(f"  Expected: {expected_hmac[:16]}...")
            all_valid = False
        else:
            if verbose:
                print(f"[OK] Entry {i}: HMAC verified ({actor} / {action})")
    
    if all_valid:
        print(f"[OK] HMAC chain integrity verified ({len(history)} entries)")
        return True
    else:
        print(f"[FAIL] HMAC chain validation failed")
        return False


def main():
    parser = argparse.ArgumentParser(
        description='Verify HMAC chain integrity in .trae/history.json',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Use default secret from environment
  python3 scripts/verify_hmac_chain.py
  
  # Use custom secret
  python3 scripts/verify_hmac_chain.py --secret "my-secret-key"
  
  # Verbose output
  python3 scripts/verify_hmac_chain.py --verbose
        """
    )
    
    parser.add_argument(
        '--secret',
        type=str,
        default=None,
        help='HMAC secret key (default: from BEAMLINE_HMAC_SECRET env var or default)'
    )
    
    parser.add_argument(
        '--verbose', '-v',
        action='store_true',
        help='Verbose output for each entry'
    )
    
    parser.add_argument(
        '--history-file',
        type=str,
        default='.trae/history.json',
        help='Path to history.json file (default: .trae/history.json)'
    )
    
    args = parser.parse_args()
    
    # Determine secret
    if args.secret:
        secret = args.secret
    elif 'BEAMLINE_HMAC_SECRET' in os.environ:
        secret = os.environ['BEAMLINE_HMAC_SECRET']
    else:
        secret = 'beamline-secret-key-v1'
        print("[WARN] Using default secret (set BEAMLINE_HMAC_SECRET for production)")
    
    # Check file existence
    if not Path(args.history_file).exists():
        print(f"[FAIL] History file not found: {args.history_file}")
        sys.exit(1)
    
    # Perform verification
    is_valid = verify_hmac_chain(args.history_file, secret, args.verbose)
    
    sys.exit(0 if is_valid else 1)


if __name__ == '__main__':
    main()
