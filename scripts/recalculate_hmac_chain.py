#!/usr/bin/env python3
"""
Safely recalculate HMAC chain with a new secret.

Usage:
    python3 scripts/recalculate_hmac_chain.py --secret "new-secret-key" [--rebuild] [--backup]
    python3 scripts/recalculate_hmac_chain.py --secret "new-secret-key" --rebuild --backup
"""

import json
import sys
import os
import hmac
import hashlib
import argparse
import shutil
from pathlib import Path
from datetime import datetime

# File paths
PROJECT_ROOT = Path(__file__).parent.parent
HISTORY_FILE = PROJECT_ROOT / ".trae" / "history.json"
STATE_FILE = PROJECT_ROOT / ".trae" / "state.json"
BACKUP_DIR = PROJECT_ROOT / ".trae" / "backups"

# Default secret (for development)
DEFAULT_SECRET = "beamline-secret-key-v1"


def calculate_hmac(secret: str, ts: str, actor: str, action: str, state_checksum: str, hmac_prev: str) -> str:
    """Calculate HMAC for an entry."""
    data = f"{ts}{actor}{action}{state_checksum}{hmac_prev}"
    return hmac.new(secret.encode(), data.encode(), hashlib.sha256).hexdigest()


def calculate_state_checksum() -> str:
    """Calculate SHA256 checksum for current state.json."""
    if not STATE_FILE.exists():
        raise FileNotFoundError(f"State file not found: {STATE_FILE}")
    
    with open(STATE_FILE, 'rb') as f:
        return hashlib.sha256(f.read()).hexdigest()


def create_backup(backup_dir: Path) -> Path:
    """Create backup of history.json and state.json."""
    backup_dir.mkdir(parents=True, exist_ok=True)
    
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    backup_path = backup_dir / f"history_{timestamp}.json"
    state_backup_path = backup_dir / f"state_{timestamp}.json"
    
    # Copy history.json
    if HISTORY_FILE.exists():
        shutil.copy2(HISTORY_FILE, backup_path)
        print(f"[OK] Created backup: {backup_path}")
    
    # Copy state.json
    if STATE_FILE.exists():
        shutil.copy2(STATE_FILE, state_backup_path)
        print(f"[OK] Created backup: {state_backup_path}")
    
    return backup_path


def recalculate_hmac_chain(secret: str, rebuild: bool = False, update_state_checksum: bool = True) -> bool:
    """
    Recalculate HMAC for all entries in history.json.
    
    Args:
        secret: Secret for HMAC
        rebuild: If True, recalculate all HMAC from scratch
        update_state_checksum: If True, update state_checksum for each entry
    
    Returns:
        True if successful, False otherwise
    """
    if not HISTORY_FILE.exists():
        print(f"[ERROR] History file not found: {HISTORY_FILE}")
        return False
    
    # Load history
    with open(HISTORY_FILE, 'r', encoding='utf-8') as f:
        history_data = json.load(f)
    
    # Handle both array and object with entries format
    history = history_data if isinstance(history_data, list) else history_data.get('entries', [])
    
    if not history:
        print("[WARN] History is empty, nothing to recalculate")
        return True
    
    print(f"[INFO] Recalculating HMAC chain for {len(history)} entries")
    print(f"[INFO] Using secret: {'*' * min(len(secret), 20)}...")
    print("")
    
    # Calculate current state_checksum
    if update_state_checksum:
        try:
            current_state_checksum = calculate_state_checksum()
            print(f"[INFO] Current state_checksum: {current_state_checksum[:16]}...")
        except FileNotFoundError:
            print("[WARN] State file not found, using state_checksum from entries")
            current_state_checksum = None
    else:
        current_state_checksum = None
    
    # Recalculate HMAC for each entry
    prev_hmac = ""
    for i, entry in enumerate(history):
        # Update state_checksum if needed
        if update_state_checksum and current_state_checksum:
            entry['state_checksum'] = current_state_checksum
        
        # Update hmac_prev
        entry['hmac_prev'] = prev_hmac
        
        # Calculate new HMAC
        ts = entry.get('ts', '')
        actor = entry.get('actor', '')
        action = entry.get('action', '')
        state_checksum = entry.get('state_checksum', '')
        
        new_hmac = calculate_hmac(secret, ts, actor, action, state_checksum, prev_hmac)
        
        # Save old HMAC for information
        old_hmac = entry.get('hmac', '')
        entry['hmac'] = new_hmac
        
        # Print information
        if old_hmac != new_hmac:
            print(f"[OK] Entry {i}: {actor} / {action}")
            print(f"  Old HMAC: {old_hmac[:16]}...")
            print(f"  New HMAC: {new_hmac[:16]}...")
        else:
            print(f"[OK] Entry {i}: {actor} / {action} (unchanged)")
        
        prev_hmac = new_hmac
    
    # Save updated history
    with open(HISTORY_FILE, 'w', encoding='utf-8') as f:
        # Save in the same format as loaded
        if isinstance(history_data, dict) and 'entries' in history_data:
            history_data['entries'] = history
            json.dump(history_data, f, indent=4, ensure_ascii=False)
        else:
            json.dump(history, f, indent=4, ensure_ascii=False)
    
    print("")
    print(f"[OK] HMAC chain recalculated for {len(history)} entries")
    
    return True


def verify_chain(secret: str) -> bool:
    """Verify integrity of recalculated chain."""
    print("")
    print("[INFO] Verifying recalculated chain...")
    
    if not HISTORY_FILE.exists():
        print(f"[ERROR] History file not found: {HISTORY_FILE}")
        return False
    
    with open(HISTORY_FILE, 'r', encoding='utf-8') as f:
        history_data = json.load(f)
    
    # Handle both array and object with entries format
    history = history_data if isinstance(history_data, list) else history_data.get('entries', [])
    
    if not history:
        return True
    
    prev_hmac = ""
    all_valid = True
    
    for i, entry in enumerate(history):
        ts = entry.get('ts', '')
        actor = entry.get('actor', '')
        action = entry.get('action', '')
        state_checksum = entry.get('state_checksum', '')
        hmac_prev = entry.get('hmac_prev', '')
        stored_hmac = entry.get('hmac', '')
        
        # Check link with previous entry
        if i > 0 and hmac_prev != prev_hmac:
            print(f"[FAIL] Entry {i}: hmac_prev mismatch")
            all_valid = False
        
        # Calculate expected HMAC
        calculated_hmac = calculate_hmac(secret, ts, actor, action, state_checksum, hmac_prev)
        
        if stored_hmac != calculated_hmac:
            print(f"[FAIL] Entry {i}: HMAC mismatch")
            print(f"  Stored:   {stored_hmac[:16]}...")
            print(f"  Expected: {calculated_hmac[:16]}...")
            all_valid = False
        else:
            print(f"[OK] Entry {i}: {actor} / {action} - verified")
        
        prev_hmac = stored_hmac
    
    if all_valid:
        print("[OK] HMAC chain integrity verified")
    else:
        print("[FAIL] HMAC chain verification failed")
    
    return all_valid


def main():
    parser = argparse.ArgumentParser(
        description="Safely recalculate HMAC chain with a new secret"
    )
    parser.add_argument(
        '--secret',
        type=str,
        required=True,
        help='New secret for HMAC calculation'
    )
    parser.add_argument(
        '--rebuild',
        action='store_true',
        help='Rebuild entire chain from scratch'
    )
    parser.add_argument(
        '--backup',
        action='store_true',
        help='Create backup before recalculating'
    )
    parser.add_argument(
        '--no-verify',
        action='store_true',
        help='Skip verification after recalculation'
    )
    parser.add_argument(
        '--no-update-state-checksum',
        action='store_true',
        help='Do not update state_checksum for entries'
    )
    
    args = parser.parse_args()
    
    # Check secret
    if not args.secret:
        print("[ERROR] Secret is required. Use --secret option")
        sys.exit(1)
    
    if len(args.secret) < 16:
        print("[WARN] Secret is too short (minimum 16 characters recommended)")
        response = input("Continue anyway? (yes/no): ")
        if response.lower() != 'yes':
            print("[INFO] Aborted")
            sys.exit(1)
    
    # Create backup
    if args.backup:
        print("[INFO] Creating backup...")
        backup_path = create_backup(BACKUP_DIR)
        print(f"[OK] Backup created: {backup_path}")
        print("")
    
    # Recalculate HMAC chain
    success = recalculate_hmac_chain(
        args.secret,
        rebuild=args.rebuild,
        update_state_checksum=not args.no_update_state_checksum
    )
    
    if not success:
        print("[ERROR] Failed to recalculate HMAC chain")
        sys.exit(1)
    
    # Verify integrity
    if not args.no_verify:
        if not verify_chain(args.secret):
            print("[ERROR] Verification failed after recalculation")
            if args.backup:
                print(f"[INFO] Restore from backup: {backup_path}")
            sys.exit(1)
    
    print("")
    print("[OK] HMAC chain recalculation completed successfully")
    print("[INFO] Next steps:")
    print("  1. Verify locally: bash scripts/validate_state.sh")
    print("  2. Update BEAMLINE_HMAC_SECRET in CI/CD systems")
    print("  3. Run CI/CD pipelines to verify")


if __name__ == '__main__':
    main()
