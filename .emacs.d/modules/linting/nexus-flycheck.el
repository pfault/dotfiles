(use-package flycheck
  :hook
  (prog-mode . flycheck-mode)

  :custom
;;  (flycheck-completion-system 'ido)
  (flycheck-idle-change-delay 1.0)
  (flycheck-indication-mode 'right-fringe)
  (flycheck-ruby-rubocop-executable "rubocop-bundle-safe")
  (flycheck-javascript-standard-executable "semistandard"))

(provide 'nexus-flycheck)
