(use-package text-mode
  :straight (:type built-in)
  :defer t
  :hook (text-mode . nexus-text-mode-setup)
  :init
  (defun nexus-text-mode-setup ()
    (setq fill-column 80)

    (hl-line-mode t)
    (visual-line-mode t)))

(provide 'nexus-text-mode)
