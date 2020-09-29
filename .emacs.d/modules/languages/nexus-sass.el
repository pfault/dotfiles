(require 'nexus-css)

(use-package sass-mode
  :mode "\\.sass\\'"

  :hook
  (sass-mode . nexus-sass-mode-setup)

  :custom
  ;; Turn off annoying auto-compile on save.
  (sass-compile-at-save nil)

  :init
  (defun nexus-sass-mode-setup ()
    (nexus-css-mode-css)))

(provide 'nexus-sass)
