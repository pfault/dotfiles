(use-package fundamental-mode
  :straight (:type built-in)
  :defer t

  :hook
  (fundamental-mode . nexus-fundamental-mode-setup)

  :init
  (defun nexus-fundamental-mode-setup ()
    (hl-line-mode t)))

(provide 'nexus-fundamental-mode)
