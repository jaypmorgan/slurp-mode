# SluRp Mode

An emacs major mode for interacting with the [SluRp
language](https://github.com/jaypmorgan/slurp).

## Installation

Below is a basic installation and configuration. The `slurp-model.el` file contains both the major more for slurp scripting, as well as the interface for communicating with the slurp repl.

```emacs-lisp
(use-package slurp-mode
  :load-path "~/path/to/slurp-mode.el"
  :bind (("C-c C-c" . slurp-repl-send-region)
         ("C-c C-b" . slurp-repl-send-buffer)
         ("C-c C-z" . run-slurp))
  :init
  (require 'slurp-repl-mode)
  (setq slurp-repl-location "~/path/to/slurp"))
```
