;;; lisp/void-org.el --- Void Org setup  -*- lexical-binding: t; -*-
;;; Commentary:
;; Org for documentation and thinking. No decoration, no tangle.
;;; Code:

(require 'void-core)
(require 'org)

;; Org files location (optional convention)
(setq org-directory (expand-file-name "org/" (expand-file-name "doc/" void-dir))
      org-default-notes-file (expand-file-name "notes.org" org-directory))

;; Basic Org sanity
(setq org-startup-indented t
      org-hide-leading-stars t
      org-startup-folded 'content
      org-adapt-indentation nil
      org-ellipsis "…")

;; No accidental code execution
(setq org-confirm-babel-evaluate t)

;; Visual calm
(setq org-fontify-quote-and-verse-blocks t
      org-fontify-whole-heading-line nil
      org-fontify-done-headline nil)

;; Org buffers benefit from focus mode
(add-hook 'org-mode-hook #'void-focus-mode)

(provide 'void-org)
;;; void-org.el ends here
