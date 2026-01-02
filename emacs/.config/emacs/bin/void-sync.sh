# ~/.config/emacs/bin/void-sync
#!/usr/bin/env bash
set -euo pipefail
source "$(dirname "$0")/void-lib.sh"

force=0
bootstrap_missing=0
dry_run=0

usage() {
  cat <<'EOF'
void-sync [--force] [--bootstrap-missing] [--dry-run]

Force site-lisp/ repos to exactly match pins (checkout :ref).
By default refuses to touch dirty repos.
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --force) force=1 ;;
    --bootstrap-missing) bootstrap_missing=1 ;;
    --dry-run) dry_run=1 ;;
    -h|--help) usage; exit 0 ;;
    *) err "unknown arg: $1"; usage; exit 2 ;;
  esac
  shift
done

need_cmd git
ensure_dirs

while IFS=$'\t' read -r name url ref; do
  [ -n "${name}" ] || continue
  repo="${SITE_LISP_DIR}/${name}"

  if [ ! -d "${repo}/.git" ]; then
    if (( bootstrap_missing )); then
      if (( dry_run )); then
        log "[dry] git clone ${url} ${repo}"
      else
        log "clone ${name}"
        git clone --filter=blob:none --quiet "${url}" "${repo}"
      fi
    else
      err "missing repo: ${repo} (use --bootstrap-missing)"
      exit 4
    fi
  fi

  if ! git -C "${repo}" diff --quiet || ! git -C "${repo}" diff --cached --quiet; then
    if (( force )); then
      if (( dry_run )); then
        log "[dry] git -C ${repo} reset --hard"
        log "[dry] git -C ${repo} clean -fd"
      else
        log "force-clean ${name}"
        git -C "${repo}" reset --hard --quiet
        git -C "${repo}" clean -fd --quiet
      fi
    else
      err "dirty repo: ${name} (use --force to reset/clean)"
      exit 5
    fi
  fi

  if [ -z "${ref}" ]; then
    err "pin ${name} has empty :ref (run void-pin after bootstrap)"
    exit 6
  fi

  if (( dry_run )); then
    log "[dry] git -C ${repo} fetch --all --tags --prune"
    log "[dry] git -C ${repo} checkout --detach ${ref}"
  else
    log "checkout ${name}"
    git -C "${repo}" fetch --all --tags --prune --quiet
    git -C "${repo}" checkout --detach --quiet "${ref}"
  fi
done < <(pins_tsv)

log "done."
