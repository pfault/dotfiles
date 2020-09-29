(use-package plantuml-mode
  :mode "\\.uml\\'"
  :hook
  (plantuml-mode . nexus-plantuml-mode-setup)

  :custom
  (plantuml-default-exec-mode 'executable)

  :init
  (defun nexus-plantuml-mode-setup ()
    (setq tab-width 2)))

(provide 'nexus-plantuml)
