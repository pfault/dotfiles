(use-package lsp-mode
  :bind (:map lsp-mode-map
              ("C-c C-." . lsp-rename)
              ("C-c C-f" . lsp-format-buffer)
          ("C-c C-t" . lsp-describe-thing-at-point))

  :commands
  lsp
  lsp-deferred

  :hook
  (lsp-mode . nexus-lsp-mode-setup)

  :custom
  (lsp-eldoc-render-all t)
  (lsp-enable-xref t)
  (lsp-enable-file-watchers t)
  (lsp-enable-imenu t)
  (lsp-keymap-prefix "M-;")
  (lsp-prefer-capf t)
  (lsp-idle-delay 0.6)
  ;; Rust
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  ;; Python remote
  (lsp-register-client
    (make-lsp-client :new-connection (lsp-tramp-connection "pyls")
                     :major-modes '(python-mode)
                     :remote? t
                     :server-id 'pyls-remote))


  ;; Set read process output to 1MB, instead of default 4KB. As many language
  ;; servers produce output ranging from 800KB to 3MB, leaving it at 4KB affects
  ;; performance. More info here:
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (read-process-output-max (* 1024 1024))

  :init
  (defun nexus-lsp-mode-setup ()
    (lsp-enable-which-key-integration)
    (setq-local company-idle-delay 0.0
                company-minimum-prefix-length 1)))

(provide 'nexus-lsp)
