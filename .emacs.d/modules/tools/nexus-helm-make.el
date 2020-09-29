(require 'nexus-helm)

(use-package helm-make
  :after (helm-global-bindings)
  :bind
  (:map helm-command-map
        ("m" . helm-make-projectile))

  :custom
  (helm-make-cache-targets nil)
  (helm-make-do-save t)
  (helm-make-fuzzy-matching t)
  (helm-make-list-target-method 'qp))

(provide 'nexus-helm-make)
