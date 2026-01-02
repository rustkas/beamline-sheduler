# Rule Update Notification for Cursor Agents

## Rule Change: Language Rules Update

**Date**: 2025-11-09  
**Rule File**: `.cursor/rules/agents/language-rules.mdc`  
**Rule Version**: v10.0 → v10.1  
**Message Protocol**: v1

## Summary

Updated language rules to explicitly require all documentation, reports, and code comments to be written in English, even if the user requests them in Russian.

## Changes

1. **Added explicit requirement for reports**:
   - All reports (`*_REPORT.md`, `TASK*.md`, etc.) must be in English
   - Added to the "Documentation and Code Comments" section

2. **Added explicit requirement for code comments**:
   - All code comments in source files (`.erl`, `.py`, `.js`, `.ts`, etc.) must be in English
   - Clarified that inline documentation (docstrings, JSDoc) must be in English

3. **Added enforcement note**:
   - Even if the user requests documentation or reports in Russian, the AI must write all documentation, reports, and code comments in English

## Impact

**Affects**: All WORKERs and agents in the project

**Automatic Application**: 
- Rule has `alwaysApply: true`, so it applies automatically to all agents
- No manual action required from agents

**Verification**:
- Updated checksum in `.trae/state.json`
- Added audit entry in `.trae/history.json`

## How Cursor Agents Are Notified

### Automatic Mechanism

1. **Automatic Loading**: 
   - Rules with `alwaysApply: true` are automatically loaded by Cursor
   - Cursor reads `.cursor/rules/**/*.mdc` files on each session
   - Changes to rule files are detected automatically

2. **Version Tracking**:
   - Rule version updated: `v10.0` → `v10.1`
   - Version is in rule file metadata: `rule_version: v10.1`
   - Agents can check version in rule file header

3. **Auto-Apply Configuration**:
   - `.cursor/config.json` includes `.cursor/**/*.mdc` in `autoApplyPatterns`
   - Rules are automatically applied to all agents

### No Manual Action Required

**Cursor agents (WORKERs) do NOT need to:**
- Manually reload rules
- Restart Cursor
- Take any explicit action

**Rules are applied automatically because:**
- `alwaysApply: true` in rule metadata
- `.cursor/**/*.mdc` in `autoApplyPatterns` in config.json
- Cursor automatically detects file changes

## Action Required from Agents

**None** - Rules are applied automatically. Agents should:
- Continue following existing language rules
- Ensure all new documentation, reports, and code comments are in English
- Reference `.cursor/rules/agents/language-rules.mdc` for current requirements
- Check `rule_version` in rule file header if unsure about current version

## References

- Rule file: `.cursor/rules/agents/language-rules.mdc`
- State entry: `.trae/state.json` (artifact_checksums)
- History entry: `.trae/history.json` (latest entry)

