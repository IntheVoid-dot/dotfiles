# ~/.config/emacs/bin/void-update
#!/usr/bin/env bash
set -euo pipefail
source "$(dirname "$0")/void-lib.sh"

repin=0
force=0
dry_run=0

usage() {
  cat <<'EOF'
void-update [--repin] [--force] [--dry-run]

Fetch updates for all pinned repos.
- Without --repin: only fetch (keeps pins untouched).
- With --repin: checkout default branch, fast-forward, then void-pin.
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --repin) repin=1 ;;
    --force) force=1 ;;
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
  [ -d "${repo}/.git" ] || { err "missing repo: ${repo}"; exit 4; }

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
      err "dirty repo: ${name} (use --force)"
      exit 5
    fi
  fi

  if (( dry_run )); then
    log "[dry] git -C ${repo} fetch --all --tags --prune"
  else
    log "fetch ${name}"
    git -C "${repo}" fetch --all --tags --prune --quiet
  fi

  if (( repin )); then
    # Move to default branch (remote HEAD) then fast-forward.
    if (( dry_run )); then
      log "[dry] git -C ${repo} checkout -B void-update --track \$(git -C ${repo} symbolic-ref refs/remotes/origin/HEAD)"
      log "[dry] git -C ${repo} pull --ff-only"
    else
      # Determine origin/HEAD -> origin/main (or similar)
      origin_head="$(git -C "${repo}" symbolic-ref -q --short refs/remotes/origin/HEAD || true)"
      if [ -z "${origin_head}" ]; then
        err "cannot determine origin/HEAD for ${name} (set it in the repo, or repin manually)"
        exit 6
      fi
      # Checkout a local branch pointing to origin/HEAD target, then ff-only pull
      target="${origin_head#origin/}"
      git -C "${repo}" checkout -B void-update --track -f "origin/${target}" >/dev/null 2>&1 || true
      git -C "${repo}" pull --ff-only --quiet
    fi
  fi
done < <(pins_tsv)

if (( repin )); then
  if (( dry_run )); then
    log "[dry] bin/void-pin"
  else
    "${VOID_BIN_DIR}/void-pin"
  fi
fi

log "done."
