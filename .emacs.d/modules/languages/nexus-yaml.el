(require 'nexus-prettier-js)
(require 'nexus-prog-mode)

(use-package yaml-mode
  :mode "\\.yml\\'" "\\.yaml\\'"
  :bind (:map yaml-mode-map
              ("RET" . newline-and-indent))

  :hook
  (yaml-mode . nexus-yaml-mode-setup)

  :init
  (defun nexus-yaml-mode-setup ()
    (run-hooks 'prog-mode-hook)
    (setq tab-width 2)
    (prettier-js-mode)
    (subword-mode +1)))

(use-package yaml-imenu
  :config
  (yaml-imenu-enable))

(provide 'nexus-yaml)
