;;; lisp/void-modes.el --- Void specialized modes  -*- lexical-binding: t; -*-
;;; Commentary:
;; Optional “specialized modes” loaded only if present.
;; Native modes are preferred when available.
;;; Code:

(require 'void-core)

(defun void--require (feature)
  "Require FEATURE if available; return non-nil if loaded."
  (require feature nil 'noerror))

;; --- YAML ------------------------------------------------------------------
;; Not builtin (yet) → external yaml-mode is correct.
(when (void--require 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode)))

;; --- Markdown --------------------------------------------------------------
;; markdown-ts-mode exists, but markdown-mode is still richer.
(when (void--require 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode)))

;; --- CSV / TSV -------------------------------------------------------------
(when (void--require 'csv-mode)
  (add-to-list 'auto-mode-alist '("\\.csv\\'" . csv-mode))
  (add-to-list 'auto-mode-alist '("\\.tsv\\'" . tsv-mode)))

;; --- PlantUML --------------------------------------------------------------
(when (void--require 'plantuml-mode)
  (add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode)))

;; --- Gnuplot ---------------------------------------------------------------
(when (void--require 'gnuplot)
  (add-to-list 'auto-mode-alist '("\\.gp\\'" . gnuplot-mode))
  (add-to-list 'auto-mode-alist '("\\.gnuplot\\'" . gnuplot-mode))
  (add-to-list 'auto-mode-alist '("\\.plt\\'" . gnuplot-mode)))

(provide 'void-modes)
;;; void-modes.el ends here
