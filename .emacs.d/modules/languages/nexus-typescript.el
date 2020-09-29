(require 'nexus-company)
(require 'nexus-flycheck)
(require 'nexus-folding)
(require 'nexus-lsp)
(require 'nexus-prettier-js)
(require 'nexus-web-mode)

(use-package typescript-mode
  :defer t
  :mode "\\.ts\\'"
  :hook
  (typescript-mode . nexus-typescript-mode-setup)

  :bind (:map typescript-mode-map
              ("C-j" . newline-and-indent)
              ("C-c C-h" . nexus-folding-toggle))

  :init
  (defun nexus-typescript-mode-setup ()
    (let ((width 2))
      (setq typescript-indent-level width
            indent-level width
            tab-width width))

    (company-mode +1)
    (lsp)
    (subword-mode +1)
    (nexus-folding)))

(use-package tide
  :hook
  (typescript-mode . nexus-tide-mode-setup)
  (web-mode . nexus-tide-web-mode-setup)

  :init
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'typescript-tslint 'web-mode))

  (defun nexus-tide-web-mode-setup ()
    (when (string-equal "tsx" (file-name-extension buffer-file-name))
      (nexus-tide-mode-setup)))

  (defun nexus-tide-mode-setup ()
    (interactive)
    (tide-setup)

    (setq flycheck-check-syntax-automatically '(save mode-enabled)
          company-tooltip-align-annotations t)

    (prettier-js-mode +1)
    (flycheck-mode +1)
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1)))

(provide 'nexus-typescript)
