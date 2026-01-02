# ~/.config/emacs/bin/void-audit
#!/usr/bin/env bash
set -euo pipefail
source "$(dirname "$0")/void-lib.sh"

need_cmd emacs

# Build load-path like init.el does, but WITHOUT loading init (which may fail).
emacs --batch -Q --eval "
(let* ((void-dir (file-name-as-directory \"${VOID_DIR}/\"))
       (lisp-dir (expand-file-name \"lisp/\" void-dir))
       (site-dir (expand-file-name \"site-lisp/\" void-dir)))
  (add-to-list 'load-path lisp-dir)
  (when (file-directory-p site-dir)
    (dolist (d (sort (directory-files site-dir t \"^[^.].*\") #'string<))
      (when (file-directory-p d) (add-to-list 'load-path d))))
  (require 'void-audit)
  (princ (void-audit-missing-deps t)))"
