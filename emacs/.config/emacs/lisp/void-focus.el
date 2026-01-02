;;; lisp/void-focus.el --- Void focus & writing  -*- lexical-binding: t; -*-
;;; Commentary:
;; Focus mode using Olivetti. Calm, reversible, predictable.
;;; Code:

(require 'void-core)
(require 'olivetti)

;; Width tuned for reading & writing
(setq olivetti-body-width 90
      olivetti-minimum-body-width 80
      olivetti-recall-visual-line-mode-entry-state t)

(defun void-focus-mode ()
  "Toggle Void focus mode."
  (interactive)
  (if (bound-and-true-p olivetti-mode)
      (progn
        (olivetti-mode -1)
        (setq-local line-spacing nil)
        (display-line-numbers-mode 1))
    (olivetti-mode 1)
    (setq-local line-spacing 0.2)
    (display-line-numbers-mode -1)))

;; Convenience binding (local & global safe)
(global-set-key (kbd "C-c z") #'void-focus-mode)

(provide 'void-focus)
;;; void-focus.el ends here
