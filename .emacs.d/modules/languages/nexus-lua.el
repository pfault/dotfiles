(use-package lua-mode
  :hook
  (lua-mode . nexus-lua-mode-setup)

  :init
  (defun nexus-lua-mode-setup ()
    (setq lua-indent-level 2
          whitespace-action '(auto-cleanup))

    (subword-mode +1)))

(provide 'nexus-lua)
