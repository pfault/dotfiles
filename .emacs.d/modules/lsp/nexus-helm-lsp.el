(require 'nexus-helm)
(require 'nexus-lsp)

(use-package helm-lsp
  :after (helm-global-bindings)
  :bind
  ("C-c '" . helm-lsp-workspace-symbol)
  ("C-c C-'" . helm-lsp-workspace-symbol))

(provide 'nexus-helm-lsp)
