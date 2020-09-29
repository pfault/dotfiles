(use-package prettier-js
  :defer t
  :hook (prettier-js-mode . nexus-prettier-js-mode-setup)

  :init
  (defun nexus-prettier-js-mode-setup ()))

(provide 'nexus-prettier-js)
