(use-package imenu
  :straight (:type built-in)

  :custom
  (imenu-auto-rescan t)
  (imenu-max-item-length 160)
  (imenu-max-items 400))

(use-package imenu-anywhere
  :bind
  ("C-c t" . imenu-anywhere))

(provide 'nexus-imenu)
