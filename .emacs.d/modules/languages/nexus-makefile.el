(use-package make-mode
  :straight (:type built-in)
  :hook
  (makefile-mode . nexus-makefile-mode-setup)

  :init
  (add-to-list 'nexus-indent-sensitive-modes 'makefile-mode)

  (defun nexus-makefile-mode-setup ()
    (subword-mode +1)
    (setq tab-width 4)))

(provide 'nexus-makefile)
