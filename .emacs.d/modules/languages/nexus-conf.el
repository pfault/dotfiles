(require 'nexus-prog-mode)

(use-package conf-mode
  :straight (:type built-in)
  :mode
  "/Procfile\\'"
  "/\\.env\\'"
  "/\\.env\\.[^/]+\\'"
  "\\.cfg\\'"
  "\\.conf\\'"

  :hook (conf-mode . nexus-conf-mode-setup)

  :init
  (defun nexus-conf-mode-setup ()
    (run-hooks 'prog-mode-hook)
    (setq tab-width 2)))

(provide 'nexus-conf)
