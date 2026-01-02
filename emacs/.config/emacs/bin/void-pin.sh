# ~/.config/emacs/bin/void-pin
#!/usr/bin/env bash
set -euo pipefail
source "$(dirname "$0")/void-lib.sh"

check=0
dry_run=0

usage() {
  cat <<'EOF'
void-pin [--check] [--dry-run]

Update lisp/void-pins.el :ref fields to current HEAD of each repo in site-lisp/.
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --check) check=1 ;;
    --dry-run) dry_run=1 ;;
    -h|--help) usage; exit 0 ;;
    *) err "unknown arg: $1"; usage; exit 2 ;;
  esac
  shift
done

need_cmd git
need_cmd emacs
ensure_dirs

[ -f "${PINS_FILE}" ] || { err "missing pins file: ${PINS_FILE}"; exit 2; }

# Optional check: ensure repos exist + clean
if (( check )); then
  while IFS=$'\t' read -r name url ref; do
    [ -n "${name}" ] || continue
    repo="${SITE_LISP_DIR}/${name}"
    [ -d "${repo}/.git" ] || { err "missing repo: ${repo}"; exit 4; }
    git -C "${repo}" diff --quiet || { err "dirty repo: ${name}"; exit 5; }
    git -C "${repo}" diff --cached --quiet || { err "dirty index: ${name}"; exit 5; }
  done < <(pins_tsv)
fi

if (( dry_run )); then
  log "[dry] would rewrite ${PINS_FILE} with current HEAD SHAs"
  exit 0
fi

# Rewrite pins file via Emacs Lisp (keeps order, updates only :ref).
tmp="${PINS_FILE}.tmp"
emacs --batch -Q \
  --eval "(progn
            (setq vc-follow-symlinks t)
            (add-to-list 'load-path \"${LISP_DIR}\")
            (load-file \"${PINS_FILE}\")
            (defun void--git-head (dir)
              (car (process-lines \"git\" \"-C\" dir \"rev-parse\" \"HEAD\")))
            (setq void-pins
                  (mapcar (lambda (e)
                            (let* ((name (car e))
                                   (plist (cdr e))
                                   (dir (expand-file-name (symbol-name name) \"${SITE_LISP_DIR}\"))
                                   (sha (void--git-head dir)))
                              (cons name (plist-put plist :ref sha))))
                          void-pins))
            (with-temp-file \"${tmp}\"
              (insert \";;; void-pins.el --- pinned deps for Void  -*- lexical-binding: t; -*-\\n\")
              (insert \";;; This file is generated/updated by bin/void-pin\\n\\n\")
              (insert \"(defconst void-pins-version 1)\\n\\n\")
              (insert \"(defconst void-pins\\n  '\")
              (prin1 void-pins (current-buffer))
              (insert \"\\n  \\\"Pinned dependency list for Void.\\\")\\n\\n\")
              (insert \"(provide 'void-pins)\\n\")
              (insert \";;; void-pins.el ends here\\n\")))"

mv -f "${tmp}" "${PINS_FILE}"
log "pinned: ${PINS_FILE}"
