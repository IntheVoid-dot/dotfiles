;;; void-rainbow.el --- Void rainbow toggle  -*- lexical-binding: t; -*-
;;; Commentary:
;; Thin wrapper around rainbow-mode (if available).
;;; Code:

(require 'void-core)

(when (require 'rainbow-mode nil 'noerror)
  (defun void-rainbow ()
    "Toggle `rainbow-mode` in the current buffer."
    (interactive)
    (if (bound-and-true-p rainbow-mode)
        (rainbow-mode -1)
      (rainbow-mode 1)))
  (global-set-key (kbd "C-c R") #'void-rainbow))

(provide 'void-rainbow)
;;; void-rainbow.el ends here
