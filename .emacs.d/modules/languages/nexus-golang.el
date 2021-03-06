(require 'nexus-company)
(require 'nexus-display-indentation)
(require 'nexus-flycheck)
(require 'nexus-folding)
(require 'nexus-lsp)
(require 'nexus-projectile)

(use-package go-mode
  :mode "\\.go\\'"
  :interpreter "go"
  :commands go-mode
  :bind (:map go-mode-map
              ("RET" . newline-and-indent)
              ("C-h f" . godoc-at-point))

  :hook
  (go-mode . nexus-go-mode-setup)

  :init
  (with-eval-after-load "projectile"
    (add-to-list 'projectile-globally-ignored-directories "Godeps")
    (add-to-list 'projectile-globally-ignored-directories "vendor/github.com")
    (add-to-list 'projectile-globally-ignored-directories "vendor/gopkg.in"))

  (defun nexus-go-mode-setup ()
    (setq-local tab-width 4
                company-echo-delay 0.5
                company-minimum-prefix-length 1
                whitespace-style (delete 'indentation whitespace-style))

    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t)

    (when (fboundp 'highlight-symbol-mode)
      (highlight-symbol-mode -1))
    (when (fboundp 'auto-highlight-symbol-mode)
      (auto-highlight-symbol-mode -1))

    (nexus-display-indentation -1)
    (company-mode +1)
    (nexus-folding)
    (subword-mode +1))

  :config
  (message "loading go-mode")

  (when (memq window-system '(mac ns))
    (exec-path-from-shell-copy-env "GOPATH"))

  (define-key 'help-command (kbd "G") 'godoc)

  ;; Ignore go test -c output files
  (add-to-list 'completion-ignored-extensions ".test"))

(use-package lsp-go
  :straight lsp-mode

  :hook
  (go-mode . lsp-deferred))

(use-package go-dlv
  :defer t)

(use-package gotest
  :defer t
  :after (go-mode)
  :bind (:map go-mode-map
              ("C-c , a" . go-test-current-project)
              ("C-c , v" . go-test-current-file)
              ("C-c , s" . go-test-current-test)
              ("C-c , c" . go-test-current-coverage)
              ("C-c , b" . go-test-current-benchmark)
              ("C-c , B" . go-test-current-project-benchmarks)
              ("C-c , r" . go-run))

  :custom
  (go-test-verbose t))

(use-package go-gen-test
  :defer t
  :after (go-mode)
  :bind (:map go-mode-map
              ("C-c , g" . go-gen-test-dwim)
              ("C-c , G" . go-gen-test-exported)))

(use-package go-projectile
  :defer t
  :after (go-mode)
  :hook (go-mode . nexus-go-projectile-setup)

  :custom
  ;; prevent go-projectile from screwing up GOPATH
  (go-projectile-switch-gopath 'never)

  :init
  (defun nexus-go-projectile-setup ()))

(use-package go-playground
  :defer t)

(provide 'nexus-golang)
