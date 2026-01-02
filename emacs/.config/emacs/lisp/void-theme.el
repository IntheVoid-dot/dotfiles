;;; void-theme.el --- Void theme  -*- lexical-binding: t; -*-
;;; Commentary:
;; Void — dark, minimal, neural-inspired theme.
;; Silence, focus, low chroma, controlled contrast.
;;; Code:

(deftheme void
  "Void — a dark, minimal, neural-inspired theme.")

;; ---------------------------------------------------------------------------
;; Palette
;; ---------------------------------------------------------------------------

(let* (;; Core
       (bg        "#0b0f14")  ;; deep void
       (bg-alt    "#101622")
       (fg        "#cfd6e6")
       (fg-dim    "#8b93a7")

       ;; Accents (desaturated / fog)
       (violet    "#a58cff")  ;; avant #b26bff
       (magenta   "#e07bb8")  ;; avant #ff6bd6
       (blue      "#6f8fd8")  ;; avant #6aa9ff (moins électrique)
       (cyan      "#6fd0d6")  ;; avant #5ee7ff (plus doux)

       ;; States
       (green     "#6fcf97")
       (amber     "#e0af68")
       (red       "#ff6b81")

       ;; UI
       (border    "#1a2030")
       (hl-line   "#121826")
       (region    "#1b2336"))

  ;; -------------------------------------------------------------------------
  ;; Basic faces
  ;; -------------------------------------------------------------------------

  (custom-theme-set-faces
   'void

   ;; Core
   `(default ((t (:background ,bg :foreground ,fg))))
   `(cursor  ((t (:background ,violet))))
   `(fringe  ((t (:background ,bg :foreground ,fg-dim))))
   `(region  ((t (:background ,region))))
   `(hl-line ((t (:background ,hl-line))))

   ;; Text
   `(shadow ((t (:foreground ,fg-dim))))
   `(link   ((t (:foreground ,cyan :underline t))))
   `(error  ((t (:foreground ,magenta :weight bold))))
   `(warning((t (:foreground ,amber))))
   `(success((t (:foreground ,green))))

   ;; Font lock (syntax)
   `(font-lock-comment-face        ((t (:foreground ,fg-dim :slant italic))))
   `(font-lock-doc-face            ((t (:foreground ,fg-dim))))
   `(font-lock-string-face         ((t (:foreground ,magenta))))
   `(font-lock-keyword-face        ((t (:foreground ,violet))))
   `(font-lock-function-name-face  ((t (:foreground ,blue))))
   `(font-lock-variable-name-face  ((t (:foreground ,fg))))
   `(font-lock-type-face           ((t (:foreground ,cyan))))
   `(font-lock-constant-face       ((t (:foreground ,cyan))))
   `(font-lock-builtin-face        ((t (:foreground ,violet))))
   `(font-lock-warning-face        ((t (:foreground ,amber :weight bold))))

   ;; Minibuffer / completion
   `(minibuffer-prompt ((t (:foreground ,violet :weight bold))))

   ;; Mode line (very restrained)
   `(mode-line
     ((t (:background ,bg-alt :foreground ,fg
                      :box (:line-width 1 :color ,border)))))
   `(mode-line-inactive
     ((t (:background ,bg :foreground ,fg-dim
                      :box (:line-width 1 :color ,border)))))


   ;; --- Completion: Vertico / Corfu ----------------------------------------

   ;; Corfu popup
   `(corfu-default ((t (:background ,bg-alt :foreground ,fg))))
   `(corfu-current ((t (:background ,region :foreground ,fg :weight bold))))
   `(corfu-border  ((t (:background ,border))))
   `(corfu-annotations ((t (:foreground ,fg-dim))))
   `(corfu-deprecated ((t (:foreground ,fg-dim :strike-through t))))

   ;; Completion-common (shared)
   `(completion-preview ((t (:foreground ,fg-dim))))
   `(completions-common-part ((t (:foreground ,violet :weight bold))))
   `(completions-first-difference ((t (:foreground ,cyan :weight bold))))

   ;; Vertico
   `(vertico-current ((t (:background ,region :foreground ,fg :weight bold))))
   `(vertico-group-title ((t (:foreground ,fg-dim :weight bold))))
   `(vertico-group-separator ((t (:foreground ,border))))

   ;; Marginalia
   `(marginalia-documentation ((t (:foreground ,fg-dim))))
   `(marginalia-key ((t (:foreground ,cyan))))
   `(marginalia-value ((t (:foreground ,fg-dim))))
   `(marginalia-date ((t (:foreground ,fg-dim))))
   `(marginalia-size ((t (:foreground ,fg-dim))))
   `(marginalia-file-name ((t (:foreground ,fg))))
   `(marginalia-file-priv-dir ((t (:foreground ,fg-dim))))
   `(marginalia-file-priv-no ((t (:foreground ,fg-dim))))

   ;; Embark
   `(embark-target ((t (:background ,bg-alt :foreground ,violet :weight bold))))
   `(embark-collect-title ((t (:foreground ,violet :weight bold))))
   `(embark-collect-marked ((t (:foreground ,cyan :weight bold))))

   ;; Consult previews
   `(consult-preview-match ((t (:background ,hl-line :foreground ,violet :weight bold))))
   `(consult-highlight-mark ((t (:background ,hl-line :foreground ,cyan :weight bold))))
   `(consult-highlight-match ((t (:background ,hl-line :foreground ,violet :weight bold))))

   ;; --- Search / highlights -------------------------------------------------

   `(isearch ((t (:background ,hl-line :foreground ,magenta :weight bold))))
   `(lazy-highlight ((t (:background ,hl-line :foreground ,cyan :weight bold))))
   `(match ((t (:background ,hl-line :foreground ,violet :weight bold))))

   ;; --- Diagnostics: Flymake / Eglot ----------------------------------------

   `(flymake-error ((t (:underline (:style wave :color ,magenta)))))
   `(flymake-warning ((t (:underline (:style wave :color ,amber)))))
   `(flymake-note ((t (:underline (:style wave :color ,cyan)))))

   ;; Eldoc (Eglot uses it a lot)
   `(eldoc-highlight-function-argument ((t (:foreground ,cyan :weight bold))))

   ;; --- Org -----------------------------------------------------------------

   `(org-document-title ((t (:foreground ,fg :weight bold :height 1.2))))
   `(org-level-1 ((t (:foreground ,violet :weight bold :height 1.15))))
   `(org-level-2 ((t (:foreground ,blue :weight bold :height 1.10))))
   `(org-level-3 ((t (:foreground ,cyan :weight bold :height 1.05))))
   `(org-level-4 ((t (:foreground ,fg :weight bold))))
   `(org-level-5 ((t (:foreground ,fg))))
   `(org-level-6 ((t (:foreground ,fg-dim))))
   `(org-level-7 ((t (:foreground ,fg-dim))))
   `(org-level-8 ((t (:foreground ,fg-dim))))

   `(org-link ((t (:foreground ,cyan :underline t))))
   `(org-code ((t (:foreground ,cyan))))
   `(org-verbatim ((t (:foreground ,cyan))))
   `(org-quote ((t (:foreground ,fg-dim :slant italic))))
   `(org-block ((t (:background ,bg-alt :foreground ,fg))))
   `(org-block-begin-line ((t (:background ,bg-alt :foreground ,fg-dim))))
   `(org-block-end-line ((t (:background ,bg-alt :foreground ,fg-dim))))
   `(org-done ((t (:foreground ,green :weight bold))))
   `(org-todo ((t (:foreground ,magenta :weight bold))))
   `(org-date ((t (:foreground ,fg-dim))))

   ;; --- Mode line: more Void -------------------------------------------------

   `(mode-line
     ((t (:background ,bg-alt :foreground ,fg
                      :box (:line-width (1 . 1) :color ,border)))))
   `(mode-line-inactive
     ((t (:background ,bg :foreground ,fg-dim
                      :box (:line-width (1 . 1) :color ,border)))))
   `(header-line
     ((t (:background ,bg :foreground ,fg-dim :box nil))))
 ))


;;;###autoload
(defun void-theme-load ()
  "Load the Void theme."
  (interactive)
  (load-theme 'void t))

(provide-theme 'void)
(provide 'void-theme)
;;; void-theme.el ends here
