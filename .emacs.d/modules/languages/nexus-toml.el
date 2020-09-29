(require 'nexus-prettier-js)
(require 'nexus-prog-mode)

(use-package conf-toml-mode
  :straight (:type built-in)
  :mode "\\.toml\\'"
  :hook (conf-toml-mode . nexus-toml-mode-setup)

  :init
  (defun nexus-toml-mode-setup ()
    (run-hooks 'prog-mode-hook)
    (setq tab-width 2)
    (prettier-js-mode)))

(provide 'nexus-toml)
