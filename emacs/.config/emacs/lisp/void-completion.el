;;; lisp/void-completion.el --- Void completion stack  -*- lexical-binding: t; -*-
;;; Commentary:
;; Minibuffer: vertico + marginalia + orderless + embark (+ consult)
;; In-buffer: corfu + cape
;;; Code:

(require 'void-core)

;; --- Minibuffer completion -------------------------------------------------

;; Save minibuffer history (file path already set in void-sanity)
(savehist-mode 1)

;; Vertico
(require 'vertico)
(vertico-mode 1)

;; Tweak: keep the minibuffer calm and useful
(setq vertico-cycle t
      vertico-resize t
      vertico-count 12)

;; Marginalia (annotations)
(require 'marginalia)
(marginalia-mode 1)

;; Orderless (matching)
(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides
      '((file (styles basic partial-completion))))

;; Optional but very nice with Orderless
(setq orderless-component-separator #'orderless-escapable-split-on-space)

;; Consult (commands)
(require 'consult)

;; A few “Void default” bindings (kept minimal; you can remap later)
(global-set-key (kbd "C-s") #'consult-line)
(global-set-key (kbd "C-x b") #'consult-buffer)
(global-set-key (kbd "M-y") #'consult-yank-pop)
(global-set-key (kbd "C-c h") #'consult-history)

;; Embark (actions)
(require 'embark)
(global-set-key (kbd "C-.") #'embark-act)
(global-set-key (kbd "C-;") #'embark-dwim)
(global-set-key (kbd "C-h B") #'embark-bindings)

;; Embark ↔ Consult integration (ships as embark-consult.el in the embark repo)
;; We only require it if available; no extra clone needed.
(require 'embark-consult nil 'noerror)

;; Reduce “action buffer” noise: show it in a predictable place
(setq embark-quit-after-action nil)

;; --- In-buffer completion --------------------------------------------------

(require 'corfu)
(setq corfu-auto t
      corfu-auto-delay 0.08
      corfu-auto-prefix 2
      corfu-cycle t
      corfu-quit-at-boundary 'separator
      corfu-quit-no-match t
      corfu-preview-current nil
      corfu-preselect 'first
      corfu-on-exact-match nil
      corfu-scroll-margin 2)

(global-corfu-mode 1)

;; CAPE: completion sources for Corfu via completion-at-point-functions
(require 'cape)

;; Default CAPF stack:
;; - file completion
;; - dabbrev (words from buffers)
;; - keyword / elisp symbols handled by major modes automatically
;;
;; We add a few safe ones; keep it short for maintainability.
(defun void--setup-capf ()
  "Void default `completion-at-point-functions` setup."
  (setq-local completion-at-point-functions
              (delq nil
                    (list
                     (when (fboundp 'cape-file)    #'cape-file)
                     (when (fboundp 'cape-dabbrev) #'cape-dabbrev)
                     ;; Some Cape versions don't provide `cape-keyword`.
                     (when (fboundp 'cape-keyword) #'cape-keyword)))))


(add-hook 'prog-mode-hook #'void--setup-capf)
(add-hook 'text-mode-hook #'void--setup-capf)

;; Convenience: manual completion in any buffer
(global-set-key (kbd "M-TAB") #'completion-at-point)

(provide 'void-completion)
;;; void-completion.el ends here
