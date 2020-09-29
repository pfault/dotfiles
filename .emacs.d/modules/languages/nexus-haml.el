(use-package haml-mode
  :mode "\\.haml\\'" "\\.hamlc\\'"
  :hook (haml-mode . nexus-haml-mode-setup)

  :init
  (defun nexus-haml-mode-setup ()
    (setq tab-width 2)))

(provide 'nexus-haml)
