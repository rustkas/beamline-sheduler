#!/usr/bin/env python3
"""
Check secret compatibility with current HMAC chain.

Usage:
    python3 scripts/check_secret_compatibility.py [--secret SECRET]
    python3 scripts/check_secret_compatibility.py --secret "your-secret-key"
"""

import json
import sys
import hmac
import hashlib
import argparse
from pathlib import Path

# File paths
PROJECT_ROOT = Path(__file__).parent.parent
HISTORY_FILE = PROJECT_ROOT / ".trae" / "history.json"
STATE_FILE = PROJECT_ROOT / ".trae" / "state.json"

# Default secret (for development)
DEFAULT_SECRET = "beamline-secret-key-v1"


def calculate_hmac(secret: str, ts: str, actor: str, action: str, state_checksum: str, hmac_prev: str) -> str:
    """Calculate HMAC for an entry."""
    data = f"{ts}{actor}{action}{state_checksum}{hmac_prev}"
    return hmac.new(secret.encode(), data.encode(), hashlib.sha256).hexdigest()


def verify_entry(entry: dict, secret: str, prev_hmac: str = "") -> tuple[bool, str, str]:
    """
    Verify an entry with the given secret.
    
    Returns:
        (is_valid, stored_hmac, calculated_hmac)
    """
    # Get state_checksum from current state.json
    try:
        with open(STATE_FILE, 'rb') as f:
            current_state_checksum = hashlib.sha256(f.read()).hexdigest()
    except FileNotFoundError:
        # If state.json doesn't exist, use state_checksum from entry
        current_state_checksum = entry.get('state_checksum', '')
    
    # Use state_checksum from entry for historical integrity check
    state_checksum = entry.get('state_checksum', '')
    ts = entry.get('ts', '')
    actor = entry.get('actor', '')
    action = entry.get('action', '')
    hmac_prev = entry.get('hmac_prev', '')
    
    stored_hmac = entry.get('hmac', '')
    calculated_hmac = calculate_hmac(secret, ts, actor, action, state_checksum, hmac_prev)
    
    is_valid = stored_hmac == calculated_hmac
    
    return is_valid, stored_hmac, calculated_hmac


def check_secret_compatibility(secret: str, verbose: bool = False) -> tuple[bool, int, int]:
    """
    Check secret compatibility with current HMAC chain.
    
    Returns:
        (is_compatible, total_entries, valid_entries)
    """
    if not HISTORY_FILE.exists():
        print(f"[ERROR] History file not found: {HISTORY_FILE}")
        sys.exit(1)
    
    with open(HISTORY_FILE, 'r', encoding='utf-8') as f:
        history = json.load(f)
    
    if not history:
        print("[WARN] History is empty")
        return True, 0, 0
    
    total_entries = len(history)
    valid_entries = 0
    prev_hmac = ""
    
    print(f"[INFO] Checking {total_entries} history entries with secret: {'*' * min(len(secret), 20)}...")
    print("")
    
    for i, entry in enumerate(history):
        is_valid, stored_hmac, calculated_hmac = verify_entry(entry, secret, prev_hmac)
        
        if is_valid:
            valid_entries += 1
            status = "[OK]"
        else:
            status = "[FAIL]"
        
        if verbose or not is_valid:
            actor = entry.get('actor', 'unknown')
            action = entry.get('action', 'unknown')
            ts = entry.get('ts', 'unknown')
            
            print(f"{status} Entry {i}: {actor} / {action} at {ts}")
            
            if not is_valid:
                print(f"  Stored:   {stored_hmac[:16]}...")
                print(f"  Expected: {calculated_hmac[:16]}...")
        
        # Check link with previous entry
        if i > 0:
            entry_hmac_prev = entry.get('hmac_prev', '')
            if entry_hmac_prev != prev_hmac:
                print(f"[WARN] Entry {i}: hmac_prev mismatch")
                print(f"  Stored:   {entry_hmac_prev[:16] if entry_hmac_prev else '(empty)'}...")
                print(f"  Expected: {prev_hmac[:16] if prev_hmac else '(empty)'}...")
        
        prev_hmac = entry.get('hmac', '')
    
    print("")
    print(f"[INFO] Valid entries: {valid_entries}/{total_entries}")
    
    is_compatible = valid_entries == total_entries
    
    if is_compatible:
        print("[OK] Secret is compatible with current HMAC chain")
    else:
        print(f"[FAIL] Secret is NOT compatible: {total_entries - valid_entries} entry/entries failed")
        print("[INFO] Consider using recalculate_hmac_chain.py to rebuild the chain with this secret")
    
    return is_compatible, total_entries, valid_entries


def main():
    parser = argparse.ArgumentParser(
        description="Check secret compatibility with current HMAC chain"
    )
    parser.add_argument(
        '--secret',
        type=str,
        default=None,
        help='Secret to check (default: read from BEAMLINE_HMAC_SECRET env or use default)'
    )
    parser.add_argument(
        '--verbose',
        action='store_true',
        help='Show detailed information for each entry'
    )
    
    args = parser.parse_args()
    
    # Get secret
    if args.secret:
        secret = args.secret
    elif 'BEAMLINE_HMAC_SECRET' in os.environ:
        secret = os.environ['BEAMLINE_HMAC_SECRET']
        print(f"[INFO] Using secret from BEAMLINE_HMAC_SECRET environment variable")
    else:
        secret = DEFAULT_SECRET
        print(f"[WARN] No secret provided, using default (development only)")
        print(f"[WARN] For production, set BEAMLINE_HMAC_SECRET or use --secret")
    
    # Check compatibility
    is_compatible, total, valid = check_secret_compatibility(secret, args.verbose)
    
    # Exit code
    sys.exit(0 if is_compatible else 1)


if __name__ == '__main__':
    import os
    main()
