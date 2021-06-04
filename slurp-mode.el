;;; slurp-mode.el --- Support for the SluRp syntax  -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2021 Jay Morgan


;; Author: Jay Morgan <jay@morganwastaken.com>
;; Created: 23 May 2021

;; Keywords: lisp R
;; URL: https://github.com/jaypmorgan/slurp-mode

;;; Code:

(defconst slurp-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; string deliminter
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
    ;; comments
    (modify-syntax-entry ?\; ". 12" table)
    (modify-syntax-entry ?\n ">" table)
    table))

(defvar slurp-mode-map
  (let ((map (nconc (make-sparse-keymap) lisp-mode-map)))
    map))

(defconst slurp-mode-pretty-symbols-alist
  '(("lambda" . ?λ)))

(defconst slurp-mode-font-lock-keywords
  ;; (regexp-opt '("defparam" "defun" "library" "lambda") t)
  (list '("\\<\\(def\\(?:param\\|un\\)\\|library\\|lambda\\)\\>" . font-lock-builtin-face)
        '("\\(\:[a-z0-9_]+\\)" . font-lock-keyword-face)
        '("\\([0-9L.]+\\)" . font-lock-constant-face))
  "Default highlighting for SluRp mode")

(defun slurp-mode ()
  "Major mode for SluRp"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table slurp-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(slurp-mode-font-lock-keywords))
  (set (make-local-variable 'prettify-symbols-alist) slurp-mode-pretty-symbols-alist)
  (set (make-local-variable 'lisp-body-indent) 2)
  (set (make-local-variable 'indent-line-function) 'lisp-indent-line)
  (set (make-local-variable 'comment-start) ";;")
  (set (make-local-variable 'mode-name) "SluRp"))

(add-to-list 'auto-mode-alist '("\\.slurp\\'" . slurp-mode))

(provide 'slurp-mode)
