;;; lisp/void-treesit.el --- Void tree-sitter helpers  -*- lexical-binding: t; -*-
;;; Commentary:
;; Safe tree-sitter remapping. Never crashes if grammar is missing.
;;; Code:

(require 'void-core)

(when (and (fboundp 'treesit-available-p)
           (treesit-available-p))

  (defun void--treesit-ready-p (lang)
    "Return non-nil if tree-sitter grammar LANG is available."
    (treesit-language-available-p lang))

  (setq major-mode-remap-alist
        (delq nil
              (list
               (when (void--treesit-ready-p 'python)
                 '(python-mode . python-ts-mode))
               (when (void--treesit-ready-p 'c)
                 '(c-mode . c-ts-mode))
               (when (void--treesit-ready-p 'cpp)
                 '(c++-mode . c++-ts-mode))
               (when (void--treesit-ready-p 'rust)
                 '(rust-mode . rust-ts-mode))
               (when (void--treesit-ready-p 'typescript)
                 '(typescript-mode . typescript-ts-mode))
               (when (void--treesit-ready-p 'tsx)
                 '(tsx-mode . tsx-ts-mode))
               (when (void--treesit-ready-p 'json)
                 '(json-mode . json-ts-mode))
               (when (void--treesit-ready-p 'css)
                 '(css-mode . css-ts-mode))
               (when (void--treesit-ready-p 'bash)
                 '(sh-mode . bash-ts-mode))))))

(provide 'void-treesit)
;;; void-treesit.el ends here
