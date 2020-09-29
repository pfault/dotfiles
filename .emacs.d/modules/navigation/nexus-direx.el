(use-package direx
  :bind ("C-x j" . direx-project:jump-to-project-root)
  :hook
  (direx-mode . nexus-direx-mode-setup)

  :custom
  (direx:closed-icon " + ")
  (direx:open-icon " - ")

  :init
  (defun nexus-direx-mode-setup ()))

(provide 'nexus-direx)
