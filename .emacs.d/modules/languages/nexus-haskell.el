(require 'nexus-company)
(require 'nexus-folding)

(use-package haskell-mode
  :bind (:map haskell-mode-map
              ("RET" . newline-and-indent))

  :hook
  (haskell-mode . nexus-haskell-mode-setup)

  :init
  (defun nexus-haskell-mode-setup ()
    (company-mode +1)
    (nexus-folding)
    (subword-mode +1)))

(provide 'nexus-haskell)
