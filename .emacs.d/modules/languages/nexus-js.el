(require 'nexus-company)
(require 'nexus-folding)
(require 'nexus-lsp)
(require 'nexus-prettier-js)

(use-package js-mode
  :straight (:type built-in)
  :mode
  "\\.js\\'"
  "\\.pac\\'"

  :bind (:map js-mode-map
              ("C-j" . newline-and-indent)
              ("C-c C-h" . nexus-toggle-hiding))

  :hook
  (js-mode . nexus-js-mode-setup)

  :init
  (defun nexus-js-mode-setup ()
    "Default tweaks for `js-mode'."
    (let ((width 2))
      (setq js-indent-level width
            indent-level width
            tab-width width))

    (prettier-js-mode)
    (company-mode)
    (lsp)
    (subword-mode)
    (nexus-folding)))

(provide 'nexus-js)
