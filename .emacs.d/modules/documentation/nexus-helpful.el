(use-package helpful
  :bind
  ("C-h k" . helpful-key)
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h C" . helpful-command)
  ("C-h F" . helpful-function)
  (:map emacs-lisp-mode-map
        ("C-c C-d" . helpful-at-point))

  :custom
  (helm-describe-function-function 'helpful-function)
  (helm-describe-variable-function 'helpful-variable))

(provide 'nexus-helpful)
