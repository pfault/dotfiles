(require 'nexus-prettier-js)
(require 'nexus-prog-mode)

(use-package nxml-mode
  :straight (:type built-in)
  :defer t
  :hook (nxml-mode . nexus-xml-setup)

  :custom
  (nxml-attribute-indent 2)
  (nxml-child-indent 2)

  :init
  (defun nexus-xml-setup ()
    (run-hooks 'prog-mode-hook)
    (setq tab-width 2)
    (prettier-js-mode)))

(provide 'nexus-xml)
