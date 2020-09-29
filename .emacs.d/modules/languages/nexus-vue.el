(require 'nexus-company)
(require 'nexus-prettier-js)
(require 'nexus-mmm)

(use-package vue-mode
  :hook
  (vue-mode . nexus-vue-mode-setup)

  :init
  (defun nexus-vue-mode-setup ()
    (prettier-js-mode)
    (company-mode)
    (lsp)
    (subword-mode)
    (nexus-folding)))

(provide 'nexus-vue)
