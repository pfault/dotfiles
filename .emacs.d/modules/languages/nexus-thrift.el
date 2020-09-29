(require 'nexus-prog-mode)

(use-package thrift
  :mode "\\.thrift\\'"
  :hook (thrift-mode . nexus-thrift-mode-setup)

  :init
  (defun nexus-thrift-mode-setup ()
    (run-hooks 'prog-mode-hook)
    (setq tab-width 2)

    (subword-mode +1)))

(provide 'nexus-thrift)
