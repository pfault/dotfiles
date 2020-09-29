(require 'nexus-ido)

(use-package dumb-jump
  :bind
  ("C-c C-j" . dumb-jump-go)
  ("M-g o" . dumb-jump-go-other-window)
  ("M-g j" . dumb-jump-go)
  ("M-g b" . dumb-jump-back)
  ("M-g i" . dumb-jump-go-prompt)
  ("M-g x" . dumb-jump-go-prefer-external)
  ("M-g z" . dumb-jump-go-prefer-external-other-window)

  :custom
  (dumb-jump-selector 'ido))

(provide 'nexus-dumb-jump)
