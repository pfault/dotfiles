(require 'nexus-ido)

(use-package xwidget-plus
  :straight (:type git :host github :repo "canatella/xwidget-plus")

  :hook
  (xwidget-webkit-mode . nexus-xwidget-plus-setup)

  :custom
  (xwidget-plus-completion-backend 'ido)

  :init
  (defun nexus-xwidget-plus-setup ()
    (define-key xwidget-webkit-mode-map (kbd "v") 'xwidget-plus-follow-link)))

(provide 'nexus-xwidget-plus)
