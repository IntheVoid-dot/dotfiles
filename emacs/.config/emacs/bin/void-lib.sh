# ~/.config/emacs/bin/void-lib.sh
#!/usr/bin/env bash
set -euo pipefail

export GIT_TERMINAL_PROMPT=0
export GIT_ASKPASS=true

# Resolve VOID_DIR from this script location (works with symlinks).
_void_resolve() {
  local src="$1"
  while [ -h "$src" ]; do
    local dir
    dir="$(cd -P "$(dirname "$src")" && pwd)"
    src="$(readlink "$src")"
    [[ "$src" != /* ]] && src="$dir/$src"
  done
  cd -P "$(dirname "$src")" && pwd
}

readonly VOID_BIN_DIR="$(_void_resolve "${BASH_SOURCE[0]}")"
readonly VOID_DIR="$(cd -P "${VOID_BIN_DIR}/.." && pwd)"
readonly SITE_LISP_DIR="${VOID_DIR}/site-lisp"
readonly LISP_DIR="${VOID_DIR}/lisp"
readonly PINS_FILE="${LISP_DIR}/void-pins.el"
readonly CACHE_DIR="${VOID_DIR}/cache"
readonly VAR_DIR="${VOID_DIR}/var"

log() { printf '%s\n' "$*"; }
err() { printf 'error: %s\n' "$*" >&2; }

need_cmd() {
  command -v "$1" >/dev/null 2>&1 || { err "missing command: $1"; exit 127; }
}

# Print pins as: name<TAB>url<TAB>ref
pins_tsv() {
  need_cmd emacs
  [ -f "${PINS_FILE}" ] || { err "missing pins file: ${PINS_FILE}"; exit 2; }

  emacs --batch -Q \
    --eval "(progn
              (add-to-list 'load-path \"${LISP_DIR}\")
              (load-file \"${PINS_FILE}\")
              (dolist (e void-pins)
                (let* ((name (symbol-name (car e)))
                       (plist (cdr e))
                       (url (plist-get plist :url))
                       (ref (plist-get plist :ref)))
                  (princ name)
                  (princ \"\t\")
                  (princ (or url \"\"))
                  (princ \"\t\")
                  (princ (or ref \"\"))
                  (princ \"\n\"))))"
}

ensure_dirs() {
  mkdir -p "${SITE_LISP_DIR}" "${CACHE_DIR}" "${VAR_DIR}"
}
