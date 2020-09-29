(require 'nexus-company)
(require 'nexus-lsp)
(require 'nexus-prettier-js)
(require 'nexus-rainbow)

(use-package css-mode
  :mode "\\.css\\'"

  :hook
  (css-mode . nexus-css-mode-setup)

  :custom
  (css-indent-offset 2)

  :init
  (defun nexus-css-mode-setup ()
    (setq tab-width 2)

    (company-mode +1)
    (prettier-js-mode)
    (lsp)
    (rainbow-mode +1)))

(provide 'nexus-css)
