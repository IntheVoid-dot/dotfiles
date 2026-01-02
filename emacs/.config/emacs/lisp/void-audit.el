;;; lisp/void-audit.el --- Void dependency audit  -*- lexical-binding: t; -*-
;;; Commentary:
;; Scan Void (lisp/) + vendors (site-lisp/) for (require 'FEATURE) forms and
;; report missing libraries (locate-library fails) without evaluating code.
;; Goal: avoid "whack-a-mole" missing deps when vendoring packages.
;;; Code:

(require 'cl-lib)

(defgroup void-audit nil
  "Void dependency audit."
  :group 'convenience)

(defcustom void-audit-ignore-features
  '(;; Things that often appear but are built-in / harmless / or not libraries
    t nil
    ;; Some packages require these in weird ways; keep list small.
    )
  "Features ignored by Void audit."
  :type '(repeat symbol))

(defun void-audit--el-files (dir)
  "Return a list of .el files under DIR recursively (ignoring dot dirs, dev/, test/)."
  (when (file-directory-p dir)
    (cl-remove-if
     (lambda (f)
       (string-match-p "/\\(dev\\|test\\|tests\\|example\\|examples\\)/" f))
     (directory-files-recursively
      dir "\\.el\\'" nil
      (lambda (d)
        (not (string-match-p "/\\.[^/]*\\'" d)))))))


(defun void-audit--read-forms (file)
  "Read all top-level forms from FILE without evaluating them."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (forms)
      (condition-case _
          (while t
            (push (read (current-buffer)) forms))
        (end-of-file nil)
        (error nil))
      (nreverse forms))))

(defun void-audit--collect-requires (form acc file)
  "Walk FORM, collect required features into ACC with origin FILE.
ACC is a hash-table feature->list-of-files."
  (cond
   ((consp form)
    (let ((head (car form)))
      (when (eq head 'require)
        ;; (require 'foo) or (require 'foo nil t)
        (let ((feat (cadr form)))
          (when (and (consp feat) (eq (car feat) 'quote))
            (let ((sym (cadr feat)))
              (when (symbolp sym)
                (let ((lst (gethash sym acc)))
                  (puthash sym (if lst (cons file lst) (list file)) acc)))))))
      ;; Recurse
      (void-audit--collect-requires head acc file)
      (void-audit--collect-requires (cdr form) acc file)))
   ((vectorp form)
    (mapc (lambda (x) (void-audit--collect-requires x acc file))
          (append form nil)))
   (t nil))
  acc)

(defun void-audit--requires-table (roots)
  "Return hash-table of required features found under ROOTS."
  (let ((acc (make-hash-table :test #'eq)))
    (dolist (root roots)
      (dolist (f (void-audit--el-files root))
        (dolist (form (void-audit--read-forms f))
          (void-audit--collect-requires form acc f))))
    acc))

(defun void-audit--missing-features (requires)
  "From REQUIRES (hash-table), return list of (feature . files) missing."
  (let (missing)
    (maphash
     (lambda (feat files)
       (unless (or (memq feat void-audit-ignore-features)
                   ;; Feature already provided in current session
                   (featurep feat)
                   ;; Library exists on load-path
                   (locate-library (format "%s" feat)))
         (push (cons feat (cl-remove-duplicates files :test #'equal)) missing)))
     requires)
    (sort missing (lambda (a b) (string< (symbol-name (car a)) (symbol-name (car b)))))))

;;;###autoload
(defun void-audit-missing-deps (&optional print-all)
  "Audit Void and report missing `require`d deps.
With prefix arg PRINT-ALL, also print provided/found deps count."
  (interactive "P")
  (let* ((void-dir user-emacs-directory)
         (roots (list (expand-file-name "lisp/" void-dir)
                      (expand-file-name "site-lisp/" void-dir)))
         (req (void-audit--requires-table roots))
         (missing (void-audit--missing-features req))
         (buf (get-buffer-create "*Void Audit*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (format "Void audit (%s)\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
      (when print-all
        (insert (format "Total distinct requires found: %d\n\n" (hash-table-count req))))
      (if (null missing)
          (insert "OK: no missing deps detected via (require '...).\n")
        (insert "Missing deps (feature -> first seen in file):\n\n")
        (dolist (cell missing)
          (let* ((feat (car cell))
                 (files (cdr cell))
                 (first (car files)))
            (insert (format "- %s\n    %s\n" feat first))))
      (goto-char (point-min))
      (setq buffer-read-only t))
    (if (called-interactively-p 'interactive)
        (pop-to-buffer buf)
      (with-current-buffer buf (buffer-string))))))

(provide 'void-audit)
;;; void-audit.el ends here
