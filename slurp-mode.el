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

;;; repl

(defvar slurp-repl-location "" "Path to the slurp repl file")
(defvar slurp-repl-arguments '() "Arguments to pass to slurp upon startup")
(defvar slurp-repl-prompt-regex "^Slurp> " "Prompt for repl")
(defvar slurp-repl-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    map)
  "Basic mode map for the slurp repl")

(defun slurp-repl--get-buffer-contents ()
  "Get buffer contents as a string"
  (buffer-substring-no-properties 1 (point-max)))

(defun slurp-repl--get-region-contents ()
  "Get the text of highlighted region as string"
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun slurp-repl-send-buffer ()
  "Send entire buffer to the repl"
  (interactive)
  (comint-send-string "*SluRp*" (slurp-repl--get-buffer-contents)))

(defun slurp-repl-send-region ()
  "Send highlighted region to repl"
  (interactive)
  (comint-send-string "*SluRp*" (slurp-repl--get-region-contents)))

(defun run-slurp ()
  "Run an inferior instance of the slurp repl inside Emacs."
  (interactive)
  (let* ((slurp-repl slurp-repl-location)
         (buffer (comint-check-proc "SluRp")))
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'slurp-repl-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer "*SluRp*"))
       (current-buffer)))
    (unless buffer
      (apply 'make-comint-in-buffer "SluRp" buffer
             slurp-repl slurp-repl-arguments)
      (slurp-repl-mode))))

(defun slurp-repl-mode--initialise ()
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))

(define-derived-mode slurp-repl-mode comint-mode "SluRp"
  "Major mode for SluRp repl"
  (setq comint-prompt-regexp slurp-repl-prompt-regex
        comint-prompt-read-only t)
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'paragraph-start) slurp-repl-prompt-regex))

(add-hook 'slurp-repl-mode 'slurp-repl-mode--initialise)

(provide 'slurp-repl-mode)
(provide 'slurp-mode)
