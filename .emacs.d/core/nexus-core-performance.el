(use-package gcmh
  :hook
  (emacs-startup . (lambda() (gcmh-mode +1)))
  (focus-out-hook . gcmh-idle-garbage-collect)

  :custom
  (gcmh-idle-delay 10)
  (gcmh-high-cons-threshold 104857600))

(provide 'nexus-core-performance)
