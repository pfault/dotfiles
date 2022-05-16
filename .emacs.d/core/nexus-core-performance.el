;; Setup and use gcmh-mode for improved garbage collection.
(use-package gcmh
  :demand
  :hook
  (focus-out-hook . gcmh-idle-garbage-collect)

  :custom
  (gcmh-idle-delay 10)
  (gcmh-high-cons-threshold 104857600)

  :config
  (gcmh-mode +1))

(provide 'nexus-core-performance)
