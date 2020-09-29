(use-package slim-mode
  :mode "\\.slim\\'"
  :hook (slim-mode . nexus-slim-mode-hook)

  :init
  (defun nexus-slim-mode-setup ()))

(provide 'nexus-slim)
