(require 'nexus-windmove)

(use-package vterm
  :hook
  (vterm-mode . nexus-vterm-mode-setup)

  :bind
  (:map vterm-mode-map
        ("M-i" . nexus-windmove-up)
        ("M-k" . nexus-windmove-down)
        ("M-j" . nexus-windmove-left)
        ("M-l" . nexus-windmove-right))

  :custom
  (vterm-always-compile-module t)

  :init
  (defun nexus-vterm-mode-setup ()
    (hl-line-mode -1)))

(provide 'nexus-vterm)
