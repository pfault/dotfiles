(use-package special-mode
  :straight (:type built-in)
  :defer t

  :hook
  (special-mode . nexus-special-mode-setup)

  :init
  (defun nexus-special-mode-setup ()
    (hl-line-mode t)))

(provide 'nexus-special-mode)
