# ~/.config/emacs/bin/void-treesit-install
#!/usr/bin/env bash
set -euo pipefail
source "$(dirname "$0")/void-lib.sh"

need_cmd emacs

usage() {
  cat <<'EOF'
void-treesit-install [--list] [--all] [LANG ...]

Install tree-sitter grammars into ~/.config/emacs/cache/treesit/.

Examples:
  void-treesit-install --list
  void-treesit-install --all
  void-treesit-install python rust cpp typescript tsx json css bash toml yaml html

Notes:
- Requires Emacs built with tree-sitter support.
- Building grammars needs a C toolchain (and often CMake).
EOF
}

list=0
all=0
langs=()

while [[ $# -gt 0 ]]; do
  case "$1" in
    --list) list=1 ;;
    --all)  all=1 ;;
    -h|--help) usage; exit 0 ;;
    --*) echo "error: unknown option: $1" >&2; usage; exit 2 ;;
    *) langs+=("$1") ;;
  esac
  shift
done

# Export flags/args for the Emacs batch process (simple and robust)
export VOID_TS_LIST="$list"
export VOID_TS_ALL="$all"
export VOID_TS_LANGS="${langs[*]:-}"

emacs --batch -Q --eval '
(progn
  (unless (and (fboundp '\''treesit-available-p) (treesit-available-p))
    (princ "Void: tree-sitter not available in this Emacs build\n")
    (kill-emacs 2))

  (let* ((void-dir (file-name-as-directory user-emacs-directory))
         (cache-dir (expand-file-name "cache/" void-dir))
         (ts-dir (expand-file-name "treesit/" cache-dir)))

    (make-directory ts-dir t)
    (when (boundp '\''treesit-extra-load-path)
      (add-to-list '\''treesit-extra-load-path ts-dir))

    ;; Source list (adjust anytime; keep it explicit = Void)
    (setq treesit-language-source-alist
          '\''((bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
            (c          . ("https://github.com/tree-sitter/tree-sitter-c"))
            (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp"))
            (css        . ("https://github.com/tree-sitter/tree-sitter-css"))
            (html       . ("https://github.com/tree-sitter/tree-sitter-html"))
            (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
            (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
            (rust       . ("https://github.com/tree-sitter/tree-sitter-rust"))
            (toml       . ("https://github.com/tree-sitter/tree-sitter-toml"))
            (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
            (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
            (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml"))
            (xml        . ("https://github.com/tree-sitter-grammars/tree-sitter-xml" "master" "xml/src"))))

    (defun void--known-langs ()
      (mapcar #'\''car treesit-language-source-alist))

    (defun void--install (lang)
      (condition-case err
          (progn
            (princ (format "Void: install %s...\n" lang))
            (treesit-install-language-grammar lang)
            (princ (format "Void: ok %s\n" lang)))
        (error
         (princ (format "Void: FAIL %s: %s\n" lang err))
         (kill-emacs 1))))

    (let* ((do-list (string= (or (getenv "VOID_TS_LIST") "0") "1"))
           (do-all  (string= (or (getenv "VOID_TS_ALL")  "0") "1"))
           (raw     (string-trim (or (getenv "VOID_TS_LANGS") "")))
           (wanted  (if (string-empty-p raw) nil (split-string raw "[ \t\n]+" t))))

      (when do-list
        (dolist (l (void--known-langs)) (princ (format "%s\n" l)))
        (kill-emacs 0))

      (cond
       (do-all
        (dolist (l (void--known-langs)) (void--install l)))

       ((and wanted (> (length wanted) 0))
        (dolist (s wanted)
          (let ((sym (intern s)))
            (unless (assoc sym treesit-language-source-alist)
              (princ (format "Void: unknown grammar: %s\n" s))
              (kill-emacs 2))
            (void--install sym))))

       (t
        (princ "Void: nothing to do (use --list, --all, or LANG...)\n")
        (kill-emacs 2))))))'
