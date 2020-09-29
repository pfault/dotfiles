(use-package dockerfile-mode
  :mode
  "/Dockerfile\\'"
  "/Dockerfile\\.[^/]+\\'"

  :hook (dockerfile-mode . nexus-dockerfile-mode-setup)

  :init
  (defun nexus-dockerfile-mode-setup ()
    (subword-mode +1)))

(provide 'nexus-dockerfile)
