(require 'nexus-prog-mode)

(use-package feature-mode
  :mode "\\.feature\\'"
  :interpreter "cucumber"
  :hook (feature-mode . nexus-feature-mode-setup)

  :init
  (defun nexus-feature-mode-setup ()
    (run-hooks 'prog-mode-hook)
    (setq tab-width 2)))

(provide 'nexus-cucumber)
