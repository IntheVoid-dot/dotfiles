;;; lisp/void-core.el --- Void core primitives  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'cl-lib)

(defgroup void nil
  "Signal's Emacs configuration: Void."
  :group 'convenience)

(defconst void-dir (file-name-as-directory user-emacs-directory))
(defconst void-lisp-dir  (expand-file-name "lisp/" void-dir))
(defconst void-site-lisp-dir (expand-file-name "site-lisp/" void-dir))
(defconst void-var-dir   (expand-file-name "var/" void-dir))
(defconst void-cache-dir (expand-file-name "cache/" void-dir))

(defun void--ensure-dir (dir)
  "Ensure DIR exists (create parents)."
  (unless (file-directory-p dir)
    (make-directory dir t))
  dir)

(defun void-path (relative)
  "Return RELATIVE path inside `void-dir`."
  (expand-file-name relative void-dir))

(defun void-deps-missing-p ()
  "Return non-nil if at least one pinned dependency directory is missing."
  (when (and (boundp 'void-pins) (listp void-pins))
    (cl-some
     (lambda (e)
       (let ((name (symbol-name (car e))))
         (not (file-directory-p (expand-file-name name void-site-lisp-dir)))))
     void-pins)))

(defun void-maybe-warn-missing-deps ()
  "Warn (non-fatally) if deps are missing; do not break startup."
  (when (void-deps-missing-p)
    (message "Void: deps missing. Run: ~/.config/emacs/bin/void-bootstrap (then void-pin)")))

(add-hook 'emacs-startup-hook #'void-maybe-warn-missing-deps)


(provide 'void-core)
;;; void-core.el ends here
