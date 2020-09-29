(require 'nexus-display-indentation)

(use-package prog-mode
  :straight (:type built-in)
  :hook
  (prog-mode . nexus-prog-mode-setup)

  :init
  (defun nexus-prog-mode-setup ()
    "Default coding hook, useful with any programming language."
    (setq fill-column 80
          whitespace-action '(auto-cleanup))

    (nexus-display-indentation 1)
    (hl-line-mode t)
    (visual-line-mode t)
    (whitespace-mode t)))

(provide 'nexus-prog-mode)
