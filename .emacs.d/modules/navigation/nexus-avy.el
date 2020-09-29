(use-package avy
  :bind
  ("C-c j" . avy-goto-word-or-subword-1)
  ("C-c SPC" . avy-goto-char)

  :custom
  (avy-background t)
  (avy-style 'at-full))

(use-package ace-window
  :bind
  ("C-M-o" . ace-window))

(provide 'nexus-avy)
