(require 'nexus-company)
(require 'nexus-folding)
(require 'nexus-lsp)

(use-package rustic
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)
  (setq lsp-rust-analyzer-server-command '("~/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/bin/rust-analyzer"))

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'pfault/rustic-mode-hook))

(defun pfault/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(use-package flycheck-rust
  :after rust-mode
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package rust-playground)

(provide 'nexus-rustic)
