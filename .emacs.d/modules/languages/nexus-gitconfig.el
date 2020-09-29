(use-package gitconfig-mode
  :mode "\\.gitconfig\\'" "gitconfig\\'" "\\.git/config\\'"
  :hook (gitconfig-mode . nexus-gitconfig-mode-setup)

  :init
  (defun nexus-gitconfig-mode-setup ()))

(provide 'nexus-gitconfig)
