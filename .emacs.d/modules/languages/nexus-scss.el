(require 'nexus-css)

(use-package scss-mode
  :mode "\\.scss\\'"

  :hook
  (scss-mode . nexus-scss-mode-setup)

  :custom
  ;; Turn off annoying auto-compile on save.
  (scss-compile-at-save nil)

  :init
  (defun nexus-scss-mode-setup ()
    (nexus-css-mode-setup)))

(provide 'nexus-scss)
