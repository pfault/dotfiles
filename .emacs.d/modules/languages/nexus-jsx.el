(require 'nexus-folding)
(require 'nexus-prettier-js)

(use-package rjsx-mode
  :mode "components\\/.*\\.js\\'"

  :hook (rjsx-mode . nexus-rjsx-mode-setup)

  :init
  (defun nexus-rjsx-mode-setup ()
    (prettier-js-mode +1)
    (company-mode +1)
    (subword-mode +1)
    (nexus-folding)))

(provide 'nexus-jsx)
