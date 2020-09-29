(require 'nexus-helm)
(require 'nexus-projectile)

(use-package helm-projectile
  :after (helm-global-bindings)
  :bind
  ("C-x ;" . helm-projectile-find-file)
  ("C-x C-;" . helm-projectile-find-file)
  ("C-c ;" . helm-projectile-switch-project)
  ("C-c C-;" . helm-projectile-switch-project))

(provide 'nexus-helm-projectile)
