(require 'nexus-lsp)

(use-package lsp-ui
  :defer t
  :commands
  lsp-ui-mode
  :bind (:map lsp-ui-mode-map
              ("C-c C-d" . lsp-ui-doc-show)
              ("M-?" . lsp-ui-peek-find-references)
              ("C-c C-j" . lsp-ui-peek-find-definitions))

  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-update-mode 'line)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-hover t)
  (lsp-eldoc-enable-hover nil)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-imenu-enable t)
  (lsp-ui-sideline-ignore-duplicate t))
;;  (lsp-ui-sideline-enable nil))

(provide 'nexus-lsp-ui)
