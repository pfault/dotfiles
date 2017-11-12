(setq global-mark-ring-max 5000         ; increase mark ring to contains 5000 entries
      mark-ring-max 5000                ; increase kill ring to contains 5000 entries
      mode-require-final-newline t      ; add a newline to end of file
      tab-width 4                       ; default to 4 visible spaces to display a tab
      )

(add-hook 'sh-mode-hook (lambda ()
                          (setq tab-width 4)))

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(setq-default indent-tabs-mode nil)
(delete-selection-mode)
(global-set-key (kbd "RET") 'newline-and-indent)

;; GROUP: Editing -> Killing
(setq kill-ring-max 5000 ; increase kill-ring capacity
      kill-whole-line t  ; if NIL, kill whole line and move the next line up
      )

;; show whitespace in diff-mode
(add-hook 'diff-mode-hook (lambda ()
                            (setq-local whitespace-style
                                        '(face
                                          tabs
                                          tab-mark
                                          spaces
                                          space-mark
                                          trailing
                                          indentation::space
                                          indentation::tab
                                          newline
                                          newline-mark))
                            (whitespace-mode 1)))

;; Package: volatile-highlights
;; GROUP: Editing -> Volatile Highlights
(use-package volatile-highlights
  :init
  (volatile-highlights-mode t))

;; Package: undo-tree
;; GROUP: Editing -> Undo -> Undo Tree
(use-package undo-tree
  :init
  (global-undo-tree-mode 1))


;; Package: yasnippet
;; GROUP: Editing -> Yasnippet
;; Package: yasnippet
(use-package yasnippet
  :defer t
  :init
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (yas-global-mode 1)
  (yas-reload-all))

;; Package: clean-aindent-mode
(use-package clean-aindent-mode
  :init
  (add-hook 'prog-mode-hook 'clean-aindent-mode))

;; Package: dtrt-indent
(use-package dtrt-indent
  :init
  (dtrt-indent-mode 1)
  (setq dtrt-indent-verbosity 0))

;; Package: ws-butler
(use-package ws-butler
  :init
  (add-hook 'prog-mode-hook 'ws-butler-mode)
  (add-hook 'text-mode 'ws-butler-mode)
  (add-hook 'fundamental-mode 'ws-butler-mode))

;; PACKAGE: comment-dwim-2
(global-set-key (kbd "M-;") 'comment-dwim-2)

;; PACKAGE: anzu
;; GROUP: Editing -> Matching -> Isearch -> Anzu
(use-package anzu
  :init
  (global-anzu-mode)
  (global-set-key (kbd "M-%") 'anzu-query-replace)
  (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp))

;; PACKAGE: iedit
(use-package iedit
  :bind (("C-;" . iedit-mode))
  :init
  (setq iedit-toggle-key-default nil))

(use-package modern-cpp-font-lock
  :init
  (modern-c++-font-lock-global-mode t))

(use-package markdown-mode
  :init)

(use-package cmake-mode
  :init)

(use-package company-cmake
  :init)

(use-package header2
  :defer 10
  :config
  (progn
    (setq make-header-hook '(
      ;; header-title
      header-blank
      header-file-name
      header-blank
      my/header-projectname
      header-blank
      header-description
      header-blank
      header-author
      ;; header-maintainer
      header-copyright
      header-creation-date
      header-blank
      header-version
      header-modification-date
      header-modification-author
      ;; header-pkg-requires
      ;; header-update-count
      ;; header-url
      ;; header-doc-url
      ;; header-keywords
      ;; header-compatibility
      ;; header-blank
      ;; header-lib-requires
      ;; header-end-line
      ;; header-commentary
      ;; header-end-line
      ;; header-history
      ;; header-blank
      header-end-line
      ;; header-free-software
      header-code
      header-eof))
    (setq header-copyright-notice
      "Copyright (C) 2016-2017 account@serv.tld\n")
    (setq header-date-format "%Y-%m-%d %T %z")
    (make-local-variable 'user-full-name)
    (make-local-variable 'user-mail-address)
    (defsubst my/header-projectname ()
      "Insert Project Name"
      (insert header-prefix-string "Project: "
        (when (featurep 'projectile)
          (replace-regexp-in-string ".*?/git/\\(.*\\)/.*"
                                    "\\1"
                                    (projectile-project-root)))
                                    "\n"))
    (add-hook 'write-file-hooks 'auto-update-file-header)
    (add-hook 'emacs-lisp-mode-hook 'auto-make-header)
    (add-hook 'c-mode-common-hook   'auto-make-header)
    (add-hook 'c++-mode-hook   'auto-make-header)))

(use-package yaml-mode)

(define-derived-mode saltstack-mode yaml-mode "Saltstack"
		       "Minimal Saltstack mode, based on `yaml-mode'."
		         (setq tab-width 2
			               indent-tabs-mode nil))

(add-to-list 'auto-mode-alist '("\\.sls\\'" . saltstack-mode))

(provide 'setup-editing)
