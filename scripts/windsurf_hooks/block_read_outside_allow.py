#!/usr/bin/env python3
"""Policy hook that blocks read access outside allowlist directories.

This script reads a JSON payload from stdin for the "pre_read_code" action and
exits with code 2 to deny access when the requested path is not under one of
the allowed prefixes relative to the current working directory. Prints "OK"
otherwise.
"""
import sys
import json
import os
# Разрешённые префиксы относительно корня воркспейса
ALLOW = [
    "src/",
    "gateway/",
    "router/",
    "proto/",
    "scripts/",
    "docs/",
    ".windsurf/",
    ".cursor/",
    ".trae/",
]
def abspath(path):
    """Return a normalized absolute path for the given path."""
    if os.path.isabs(path):
        return os.path.normpath(path)
    return os.path.normpath(os.path.join(os.getcwd(), path))
def is_allowed(file_path: str) -> bool:
    """Return True if the path is inside the allowed prefixes relative to CWD."""
    # Разрешаем только внутри указанных каталогов
    cwd = os.getcwd()
    ap = abspath(file_path)
    try:
        rel = os.path.relpath(ap, cwd)
    except ValueError:
        return False
    rel_norm = rel.replace("\\", "/")
    return any(rel_norm.startswith(p) for p in ALLOW)
def main():
    """Entry point for the Windsurf pre_read_code hook policy."""
    data = json.loads(sys.stdin.read() or "{}")
    action = data.get("agent_action_name", "")
    tool = data.get("tool_info", {})
    path = tool.get("file_path") or tool.get("directory_path") or ""
    if action == "pre_read_code" and path:
        if not is_allowed(path):
            sys.stderr.write(f"Access denied by hook: {path} is outside ALLOW list\n")
            # Код выхода 2 — блокировка (per Windsurf Hooks spec)
            sys.exit(2)
    print("OK")
if __name__ == "__main__":
    try:
        main()
    except (json.JSONDecodeError, UnicodeDecodeError, OSError) as e:
        sys.stderr.write(f"Hook error: {e}\n")
        # Ошибка не должна раскрывать детали; блокируем безопасно
        sys.exit(2)
