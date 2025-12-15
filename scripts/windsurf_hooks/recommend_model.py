#!/usr/bin/env python3
import sys, os, json, subprocess, shlex
from typing import List, Tuple

BANNER = "🧭 Model Suggestion:"

def git_changed_files() -> List[str]:
    try:
        out = subprocess.check_output(["git", "diff", "--name-only"], text=True)
        files = [l.strip() for l in out.splitlines() if l.strip()]
        if not files:
            # fallback to last commit
            out2 = subprocess.check_output(["git", "diff", "--name-only", "HEAD~1..HEAD"], text=True)
            files = [l.strip() for l in out2.splitlines() if l.strip()]
        return files
    except Exception:
        return []

def classify(files: List[str]) -> Tuple[str, str]:
    fset = set(files)
    has_proto = any(f.endswith(".proto") for f in files) or any("proto/" in f for f in files)
    has_adr = any(f.lower().startswith(("adr/",)) or f.lower().startswith(("docs/adr",)) or f.upper().startswith(("ADR",)) for f in files)
    touches_registry = any("api-registry" in f.lower() for f in files)
    touches_cp1 = any("CP1_BOUNDARIES_AND_CONTRACTS.md" in f or "ROUTING_POLICY.md" in f for f in files)
    many_files = len(files) >= 6
    multi_lang = any(f.endswith(ext) for ext in files for ext in (".ts",".tsx",".js",".go",".rs",".py",".svelte",".kt",".java",".cpp"))
    docs_only = all(f.lower().endswith((".md",".mdx",".adoc")) for f in files) and files

    if docs_only:
        return ("Claude Sonnet 4.5", "Docs/PR summaries — человекочитаемые обзоры")

    if touches_cp1 or has_adr:
        return ("GPT-5 (high reasoning)", "Архитектурные изменения/ADR/CP1‑границы")

    if has_proto or touches_registry:
        return ("GPT-5‑Codex", "Преобразования контрактов/DTO/Proto")

    if many_files or multi_lang:
        return ("SWE-1.5 (Promo)", "Многофайловые правки/TDD‑цикл")

    # default
    return ("GPT-5 (low reasoning)", "Интерактивные правки и быстрый цикл")

def hook_mode(stdin_data: str):
    try:
        data = json.loads(stdin_data or "{}")
    except Exception:
        data = {}
    # Try to infer target file if present
    file_path = ""
    if isinstance(data, dict):
        tool = data.get("tool_info", {})
        file_path = tool.get("file_path") or ""

    files = git_changed_files()
    if file_path and file_path not in files:
        files = [file_path] + files

    model, reason = classify(files)
    print(f"{BANNER} Recommended = {model}  |  Why: {reason}")
    # Optional: write to audit
    try:
        audit_dir = os.path.join(".windsurf", ".audit")
        os.makedirs(audit_dir, exist_ok=True)
        with open(os.path.join(audit_dir, "model_suggestions.log"), "a", encoding="utf-8") as f:
            f.write(f"{model}\t{reason}\t{';'.join(files)}\n")
    except Exception:
        pass

def cli_mode(args: List[str]):
    # CLI: you can pass file list, else infer from git
    files = args if args else git_changed_files()
    model, reason = classify(files)
    print(f"{BANNER} Recommended = {model}  |  Why: {reason}")
    if files:
        print("Files:", ", ".join(files))

def main():
    if not sys.stdin.isatty():
        # Probably hook mode
        hook_mode(sys.stdin.read())
    else:
        cli_mode(sys.argv[1:])

if __name__ == "__main__":
    main()
