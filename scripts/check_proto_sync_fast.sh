#!/usr/bin/env bash
set -euo pipefail

# CP0 Proto Sync (fast): сравнивает дерево proto из корня и у роутера.
# Вывод в формате "file:line:col: error|warn: msg" для Problems.

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")"/.. && pwd)"
ROOT_PROTO_DIR="$ROOT_DIR/proto/beamline"
ROUTER_PROTO_DIR="$ROOT_DIR/apps/otp/router/proto/beamline"

pass() { printf "[PASS] %s\n" "$1"; }
emit_error() { printf "%s:1:1: error: %s\n" "$1" "$2"; }
emit_warn()  { printf "%s:1:1: warn: %s\n"  "$1" "$2"; }

# Быстрые дайджесты по содержимому .proto (независимо от порядка файлов)
digest() {
  # sha256sum или shasum -a 256 (macOS)
  if command -v sha256sum >/dev/null 2>&1; then
    find "$1" -type f -name '*.proto' -print0 | sort -z | xargs -0 cat | sha256sum | awk '{print $1}'
  elif command -v shasum >/dev/null 2>&1; then
    find "$1" -type f -name '*.proto' -print0 | sort -z | xargs -0 cat | shasum -a 256 | awk '{print $1}'
  else
    # fallback md5
    find "$1" -type f -name '*.proto' -print0 | sort -z | xargs -0 cat | md5sum | awk '{print $1}'
  fi
}

# Проверка наличия директорий
if [ ! -d "$ROOT_PROTO_DIR" ]; then
  emit_error "$ROOT_DIR/proto" "root proto directory '$ROOT_PROTO_DIR' missing"
  exit 1
fi
if [ ! -d "$ROUTER_PROTO_DIR" ]; then
  emit_error "$ROOT_DIR/apps/otp/router/proto" "router proto directory '$ROUTER_PROTO_DIR' missing"
  exit 1
fi

sum_root="$(digest "$ROOT_PROTO_DIR")"
sum_router="$(digest "$ROUTER_PROTO_DIR")"

diffs=0
if [ "$sum_root" != "$sum_router" ]; then
  emit_error "$ROOT_DIR/proto" "Proto trees are NOT synchronized (differ)"
  # Детальный отчёт
  while IFS= read -r line; do
    case "$line" in
      "Only in "*)
        # Пример: Only in <dir>: file.proto
        emit_warn "$ROOT_DIR/proto" "$line"
        ;;
      "Files "*)
        # Пример: Files A and B differ
        a="$(echo "$line" | awk '{print $2}')"
        b="$(echo "$line" | awk '{print $4}')"
        printf "%s:1:1: error: differs from %s\n" "$a" "$b"
        # Подсказка для быстрого cp (не матчится проблем-матчером)
        rel="${a#"$ROOT_PROTO_DIR/"}"
        echo "  hint: cp '$a' '$ROUTER_PROTO_DIR/$rel'  # or reverse"
        diffs=$((diffs+1))
        ;;
    esac
  done < <(diff -qr "$ROOT_PROTO_DIR" "$ROUTER_PROTO_DIR" || true)
else
  pass "CP0: proto trees are synchronized"
fi

exit $(( diffs > 0 ? 1 : 0 ))
