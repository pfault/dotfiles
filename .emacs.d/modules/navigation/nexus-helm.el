;; Helm fails with tramp-methods symbol being void if tramp is not required.
(require 'tramp)

(use-package helm-global-bindings
  :straight helm
  :demand t
  :bind
  (:map helm-command-map
        ("M" . helm-man-woman))

  :custom
  (helm-command-prefix-key "C-c h"))

(use-package helm
  :after (helm-global-bindings)
  :defer t
  :hook
  (helm-minibuffer-set-up . nexus-helm--hide-minibuffer-maybe)
  (helm-after-initialize . nexus-helm--toggle-source-header-line)
  (helm-minibuffer-set-up . nexus-helm--popwin-help-mode-off)
  (helm-cleanup . nexus-helm--popwin-help-mode-on)
  (helm-cleanup . nexus-helm--show-neotree-maybe)
  (helm-cleanup . nexus-helm--show-treemacs-maybe)

  :custom
  (helm-always-two-windows t)
  (helm-autoresize-max-height 48)
  (helm-autoresize-min-height 10)
  (helm-autoresize-mode nil)
  (helm-case-fold-search 'smart)
  (helm-display-header-line t)
  (helm-echo-input-in-header-line t)
  (helm-file-name-case-fold-search 'smart)
  (helm-split-window-default-side 'below)
  (nexus-helm--did-hide-neotree nil)
  (nexus-helm--did-hide-treemacs nil)

  :init
  ;; From: https://www.reddit.com/r/emacs/comments/3asbyn/new_and_very_useful_helm_feature_enter_search/
  (defun nexus-helm--hide-minibuffer-maybe ()
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))

  ;; From: https://github.com/emacs-helm/helm/issues/918#issuecomment-81555133
  (defun nexus-helm--toggle-source-header-line ()
    (if (= (length (ignore-errors (with-helm-buffer helm-sources))) 1)
        (set-face-attribute 'helm-source-header nil :height 0.1)
      (set-face-attribute 'helm-source-header nil :height 1.0)))

  ;; From: https://github.com/emacs-helm/helm/wiki/Popwin
  (defun nexus-helm--popwin-help-mode-off ()
    "Turn `popwin-mode' off for *Help* buffers."
    (when (boundp 'popwin:special-display-config)
      (customize-set-variable 'popwin:special-display-config
                              (delq 'help-mode popwin:special-display-config))))

  ;; From: https://github.com/emacs-helm/helm/wiki/Popwin
  (defun nexus-helm--popwin-help-mode-on ()
    "Turn `popwin-mode' on for *Help* buffers."
    (when (boundp 'popwin:special-display-config)
      (customize-set-variable 'popwin:special-display-config
                              (add-to-list 'popwin:special-display-config
                                           'help-mode nil #'eq))))

  (defun nexus-helm--hide-neotree (&rest plist)
    (when (and (fboundp 'neotree-hide)
               (fboundp 'neo-global--window-exists-p)
               (neo-global--window-exists-p))
      (setq nexus-helm--did-hide-neotree t)
      (neotree-hide)))

  (defun nexus-helm--show-neotree-maybe ()
    (when (and (fboundp 'neotree-show)
               nexus-helm--did-hide-neotree)
      (setq nexus-helm--did-hide-neotree nil)
      (run-with-timer 0.01 nil #'neotree-show)))

  (defun nexus-helm--hide-treemacs (&rest plist)
    (when (fboundp 'treemacs-get-local-window)
      (let ((win (treemacs-get-local-window)))
        (when win
          (setq nexus-helm--did-hide-treemacs t)
          (delete-window win)))))

  (defun nexus-helm--show-treemacs-maybe ()
    (when nexus-helm--did-hide-treemacs
      (setq nexus-helm--did-hide-treemacs nil)
      (run-with-timer 0.01 nil #'nexus-helm--show-treemacs)))

  (defun nexus-helm--show-treemacs ()
    (when (fboundp 'treemacs-select-window)
      (let ((win (selected-window)))
        (treemacs-select-window)
        (select-window win))))

  :config
  (advice-add 'helm :before 'nexus-helm--hide-neotree)
  (advice-add 'helm :before 'nexus-helm--hide-treemacs))

(use-package helm-command
  :straight helm
  :after (helm-global-bindings)
  :bind
  ("C-c C-m" . helm-M-x)

  :custom
  (helm-M-x-always-save-history t)
  (helm-M-x-fuzzy-match t))

(use-package helm-elisp
  :straight helm
  :after (helm-global-bindings)
  :bind
  (:map helm-command-map
        ("d" . helm-apropos)))

(use-package helm-files
  :straight helm
  :after (helm-global-bindings)
  :bind
  ("C-x C-f" . helm-find-files)

  :custom
  (helm-buffer-max-length 64)
  (helm-ff-file-name-history-use-recentf t)
  (helm-ff-search-library-in-sexp t))

(use-package helm-for-files
  :straight helm
  :after (helm-global-bindings)
  :bind
  ("C-c f f" . helm-for-files)
  ("C-c f r" . helm-recentf))

(use-package helm-imenu
  :straight helm
  :after (helm-global-bindings)
  :bind
  ("C-t" . helm-imenu))

(use-package helm-ring
  :straight helm
  :after (helm-global-bindings)
  :defer t
  :init
  ;; This advice is borrowed from the browse-kill-ring package.
  (defadvice yank-pop (around kill-ring-browse-maybe (arg))
    "If last action was not a yank, run `helm-show-kill-ring' instead."
    (interactive "p")
    (if (not (eq last-command 'yank))
        (helm-show-kill-ring)
      (barf-if-buffer-read-only)
      ad-do-it))

  (ad-activate 'yank-pop))

(use-package helm-descbinds
  :defer t
  :after (helm-global-bindings))

(use-package helm-describe-modes
  :defer t
  :after (helm-global-bindings))

(provide 'nexus-helm)
