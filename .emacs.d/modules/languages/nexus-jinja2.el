(require 'nexus-prog-mode)

(use-package jinja2-mode
  :hook
  (jinja2-mode . nexus-jinja2-mode-setup)

  :init
  (defun nexus-jinja2-mode-setup ()
    (run-hooks 'prog-mode-hook)
    (subword-mode +1)))

(provide 'nexus-jinja2)
