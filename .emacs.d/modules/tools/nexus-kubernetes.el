(use-package kubernetes
  :defer t

  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600)
  :init
  (defalias 'ko 'kubernetes-overview))

(provide 'nexus-kubernetes)
