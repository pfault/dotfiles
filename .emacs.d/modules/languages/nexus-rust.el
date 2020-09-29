(require 'nexus-company)
(require 'nexus-folding)
(require 'nexus-lsp)

(use-package rust-mode
  :mode "\\.rs\\'"
  :interpreter "rust"
  :commands rust-mode
  :bind (:map rust-mode-map
              ("RET" . newline-and-indent))

  :hook
  (rust-mode . nexus-rust-mode-setup)

  :init
  (defun nexus-rust-mode-setup ()
    (setq rust-format-on-save t)

    (company-mode +1)
    (lsp)
    (nexus-folding)
    (subword-mode +1)))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :after rust-mode
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package rust-playground)

(provide 'nexus-rust)
