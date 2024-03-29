;; Enable transparent titlebar
(use-package ns-auto-titlebar
	       :config
	         (ns-auto-titlebar-mode))

;; Enable use of macOS trash
(use-package osx-trash
  :custom
  (delete-by-moving-to-trash t)

  :config
  (osx-trash-setup))

;; When running in GUI mode.
(when window-system
    ;; Set default font
      (set-face-attribute 'default nil :family "Monaco" :height 120)

        ;; Fix the default default-directory value.
	  (if (string= default-directory "/")
	          (setq default-directory "~/")))

;; macOS Fullscreen (requires Emacs 24.4 or later)
(global-set-key (kbd "s-<return>") 'toggle-frame-fullscreen)

;; modifier keys
;; (setq mac-command-modifier 'super)
;; (setq mac-option-modifier 'meta)
;; (setq ns-alternate-modifier 'meta)
;; (setq ns-command-modifier 'super)
(setq ns-function-modifier 'hyper)

;; Don't use macOS' Native fullscreen mode.
(setq ns-use-native-fullscreen nil)

;; Set initial frame to fullscreen when Emacs starts.
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(provide 'nexus-core-macos)
