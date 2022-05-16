(use-package centaur-tabs
  :commands centaur-tabs-mode

  :bind
  ("C-x <C-left>" . centaur-tabs-backward-tab)
  ("C-x <C-right>" . centaur-tabs-forward-tab)
  ("C-x <C-down>" . centaur-tabs-backward-group)
  ("C-x <C-up>" . centaur-tabs-forward-group)

  :custom
  (centaur-tabs-height 32)
  (centaur-tabs-modified-marker "•") ;; Unicode Bullet (0x2022)
  (centaur-tabs-set-bar 'left)
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-show-navigation-buttons nil)
  (centaur-tabs-enable-ido-completion nil)
  (centaur-tabs-style "bar")

  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  (centaur-tabs-group-by-projectile-project))

  :init
  ;; Enable centaur-tabs without faulty theming in daemon mode.
  (if (not (daemonp))
	 (centaur-tabs-mode)

  (defun centaur-tabs-daemon-mode (frame)
	 (unless (and (featurep 'centaur-tabs) (centaur-tabs-mode-on-p))
		(run-at-time nil nil (lambda () (centaur-tabs-mode)))))
  (add-hook 'after-make-frame-functions #'centaur-tabs-daemon-mode))

(provide 'nexus-centaur-tabs)
