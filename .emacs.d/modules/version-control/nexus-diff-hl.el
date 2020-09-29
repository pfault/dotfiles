(require 'nexus-magit)

(use-package diff-hl
  :hook
  (emacs-startup . global-diff-hl-mode)
  (emacs-startup . diff-hl-flydiff-mode)
  (dired-mode . diff-hl-dired-mode)
  (magit-post-refresh . diff-hl-magit-post-refresh))

(provide 'nexus-diff-hl)
