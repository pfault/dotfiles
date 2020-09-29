(use-package kubernetes
  :defer t

  :init
  (defalias 'ko 'kubernetes-overview))

(provide 'nexus-kubernetes)
