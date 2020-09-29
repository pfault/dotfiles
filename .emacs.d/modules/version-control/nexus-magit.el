(require 'nexus-display-fill-column)
(require 'nexus-display-line-numbers)
(require 'nexus-flyspell)

(use-package magit
  :bind
  ("C-x g". magit-status)

  :hook
  (nexus-magit-mode . nexus-magit-mode-setup)
  (git-commit-setup . nexus-git-commit-mode-setup)

  :custom
  (magit-bury-buffer-function 'magit-mode-quit-window)
  (magit-commit-arguments '("-S"))
  (magit-completing-read-function 'magit-ido-completing-read)
  (magit-default-tracking-name-function
   'magit-default-tracking-name-branch-only)
  (magit-diff-refine-hunk t)
  (magit-display-buffer-function
   'magit-display-buffer-same-window-except-diff-v1)
  (magit-repository-directories '(("~/Projects" . 2)
                                  ("~/src" . 1)
                                  ("~/.emacs.d" . 0)
                                  ("~/.dotfiles" . 2)))
  (magit-restore-window-configuration nil)
  (magit-revert-buffers 'silent)
  (magit-status-buffer-switch-function 'switch-to-buffer)

  :init
  (defalias 'bl 'magit-blame)

  (defun nexus-magit-mode-setup ())

  (defun nexus-git-commit-mode-setup ()
    (setq tab-width 2
          fill-column 72)

    (nexus-display-fill-column)
    (nexus-display-line-numbers)
    (subword-mode)
    (flyspell-mode)
    (auto-fill-mode))

  :config
  (require 'magit))

(use-package transient
  :defer t
  :custom
  (transient-history-file
   (expand-file-name "transient/history.el" nexus-cache-dir))
  (transient-levels-file
   (expand-file-name "transient/levels.el" nexus-cache-dir))
  (transient-values-file
   (expand-file-name "transient/values.el" nexus-cache-dir)))

(provide 'nexus-magit)
