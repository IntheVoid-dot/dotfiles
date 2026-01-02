# ~/.config/emacs/bin/void-bootstrap
#!/usr/bin/env bash
set -euo pipefail
source "$(dirname "$0")/void-lib.sh"

compile=0
dry_run=0
quiet=0

usage() {
  cat <<'EOF'
void-bootstrap [--compile] [--dry-run] [--quiet]

Clone missing deps into site-lisp/, checkout pinned commits, optionally compile.
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --compile) compile=1 ;;
    --dry-run) dry_run=1 ;;
    --quiet) quiet=1 ;;
    -h|--help) usage; exit 0 ;;
    *) err "unknown arg: $1"; usage; exit 2 ;;
  esac
  shift
done

need_cmd git
need_cmd emacs
ensure_dirs

[ -f "${PINS_FILE}" ] || { err "missing pins file: ${PINS_FILE}"; exit 2; }

while IFS=$'\t' read -r name url ref; do
  [ -n "${name}" ] || continue
  [ -n "${url}" ] || { err "pin ${name} has empty :url"; exit 3; }

  repo="${SITE_LISP_DIR}/${name}"

  if [ ! -d "${repo}/.git" ]; then
    if (( dry_run )); then
      log "[dry] git clone ${url} ${repo}"
    else
      (( quiet )) || log "clone ${name}"
      git clone --filter=blob:none --quiet "${url}" "${repo}"
    fi
  fi

  if (( dry_run )); then
    if [ -n "${ref}" ]; then
      log "[dry] git -C ${repo} fetch --all --tags --prune"
      log "[dry] git -C ${repo} checkout --detach ${ref}"
    else
      log "[dry] (no ref) keep default branch for ${name}"
    fi
  else
    (( quiet )) || log "sync ${name}"
    git -C "${repo}" fetch --all --tags --prune --quiet
    if [ -n "${ref}" ]; then
      git -C "${repo}" checkout --detach --quiet "${ref}"
    fi
  fi
done < <(pins_tsv)

if (( compile )); then
  if (( dry_run )); then
    log "[dry] emacs --batch -Q --eval '(native-compile-async ...)'"
  else
    # Minimal policy: let Emacs do native compilation opportunistically.
    # (We’ll make it stricter later when we wire void-sanity.el.)
    (( quiet )) || log "compile: (deferred/native as Emacs decides)"
  fi
fi

(( quiet )) || log "done."
