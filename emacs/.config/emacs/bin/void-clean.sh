# ~/.config/emacs/bin/void-clean
#!/usr/bin/env bash
set -euo pipefail
source "$(dirname "$0")/void-lib.sh"

do_eln=0
do_cache=0
do_var=0
dry_run=0

usage() {
  cat <<'EOF'
void-clean [--eln] [--cache] [--var] [--all] [--dry-run]

Safe cleanup of cache/ and selected var/ subdirs (whitelist only).
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --eln) do_eln=1 ;;
    --cache) do_cache=1 ;;
    --var) do_var=1 ;;
    --all) do_eln=1; do_cache=1; do_var=1 ;;
    --dry-run) dry_run=1 ;;
    -h|--help) usage; exit 0 ;;
    *) err "unknown arg: $1"; usage; exit 2 ;;
  esac
  shift
done

ensure_dirs

rmrf() {
  local p="$1"
  [ -e "$p" ] || return 0
  if (( dry_run )); then
    log "[dry] rm -rf $p"
  else
    rm -rf -- "$p"
    log "removed $p"
  fi
}

if (( do_eln )); then
  rmrf "${CACHE_DIR}/eln-cache"
fi

if (( do_cache )); then
  # Whitelist of cache entries we allow to purge
  rmrf "${CACHE_DIR}/tmp"
fi

if (( do_var )); then
  # Whitelist only (never wipe var/ wholesale)
  rmrf "${VAR_DIR}/auto-save-list"
fi

log "done."
