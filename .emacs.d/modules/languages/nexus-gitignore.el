(use-package gitignore-mode
  :mode "\\.gitignore" "gitignore\\'"
  :hook (gitignore-mode . nexus-gitignore-mode-setup)

  :init
  (defun nexus-gitignore-mode-setup ()))

(provide 'nexus-gitignore)
