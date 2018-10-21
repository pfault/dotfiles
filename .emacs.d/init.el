(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("org" . "http://orgmode.org/elpa/")
                  ("elpa" . "http://tromey.com/elpa/")
                  ("melpa" . "http://melpa.milkbox.net/packages/")
                  ))
  (add-to-list 'package-archives source t))
(package-initialize)

(when (not package-archive-contents)
    (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(add-to-list 'load-path "~/.emacs.d/custom")

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium")

(defun package-menu-find-marks ()
  "Find packages marked for action in *Packages*."
  (interactive)
  (occur "^[A-Z]"))

(defun package-menu-filter-by-status (status)
  "Filter the *Packages* buffer by status."
  (interactive
   (list (completing-read
          "Status: " '("new" "installed" "dependency" "obsolete"))))
  (package-menu-filter (concat "status:" status)))

(define-key package-menu-mode-map "s" #'package-menu-filter-by-status)
(define-key package-menu-mode-map "a" #'package-menu-find-marks)

(require 'setup-general)
(require 'setup-magit)
;;(require 'setup-helm)
;;(require 'setup-helm-gtags)
(require 'setup-rtags-irony)
(require 'setup-cedet)
(require 'setup-editing)
(require 'setup-org)
(require 'setup-notmuch)
(require 'setup-python)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(package-selected-packages
   (quote
    (elpy nyan-mode groovy-mode restclient org-plus-contrib org-notmuch exec-path-from-shell modern-cpp-font-lock header2 solarized-theme cff company-cmake cmake-mode cmake-ide flycheck-irony flycheck company-irony-c-headers company-irony irony irony-mode zygospore projectile company use-package)))
 '(safe-local-variable-values
   (quote
    ((org-export-allow-bind-keywords . t)
     (org-hugo-auto-export-on-save . t)
     (org-export-allow-bind-keywords . t)
     (org-confirm-babel-evaluate)
     (header-auto-update-enabled)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(use-package solarized-theme
  :init)

(load-theme 'solarized-dark)
