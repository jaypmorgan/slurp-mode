;;; slurp-repl-mode.el --- Inferior SluRp REPL  -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2021 Jay Morgan


;; Author: Jay Morgan <jay@morganwastaken.com>
;; Created: 23 May 2021

;; Keywords: lisp R
;; URL: https://github.com/jaypmorgan/slurp-mode

;;; Code:
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

(defun slurp-repl--get-line-contents ()
  "Get the text of the line where the cursor is"
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun slurp-repl--send-something (fun)
  (comint-send-string "*SluRp*" (funcall fun)))

(defun slurp-repl-send-buffer ()
  "Send entire buffer to the repl"
  (interactive)
  (slurp-repl--send-something #'slurp-repl--get-buffer-contents))

(defun slurp-repl-send-region ()
  "Send highlighted region to repl"
  (interactive)
  (slurp-repl--send-something #'slurp-repl--get-region-contents))

(defun slurp-repl-send-line ()
  "Send the current cursor line to repl"
  (interactive)
  (slurp-repl--send-something #'slurp-repl--get-line-contents))

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

(defun run-slurp-other-window ()
  "Split the window below and run a slurp repl"
  (interactive)
  (split-window-below)
  (other-window 1)
  (run-slurp))

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
