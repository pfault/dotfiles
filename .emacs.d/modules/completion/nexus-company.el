(use-package company
  :bind
  ;; Enable indent and complete at point functionality by pressing tab.
  ("TAB" . company-indent-or-complete-common)
  ;; Scroll through company suggestions with C-n and C-p.
  (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
	      ("M-<". company-select-first)
      	      ("M->". company-select-last))

  :custom
  (company-begin-commands '(self-insert-command))
  (company-dabbrev-downcase nil)
  (company-echo-delay 0)
  (company-idle-delay 0.15)
  (company-minimum-prefix-length 2)
  (company-tooltip-limit 20)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (company-tooltip-flip-when-above t)

  :config
  (defvar-local nexus-company--fci-mode-on-p nil)
  (global-company-mode 1)

  (with-eval-after-load 'fill-column-indicator
    (defun nexus-company--turn-off-fci (&rest ignore)
      (when (boundp 'fci-mode)
        (when fci-mode
          (turn-off-fci-mode)
          (setq nexus-company--fci-mode-on-p t))))

    (defun nexus-company--maybe-turn-on-fci (&rest ignore)
      (when nexus-company--fci-mode-on-p
        (turn-on-fci-mode)
        (setq nexus-company--fci-mode-on-p nil)))

    (add-hook 'company-completion-started-hook
              #'nexus-company--turn-off-fci)
    (add-hook 'company-completion-finished-hook
              #'nexus-company--maybe-turn-on-fci)
    (add-hook 'company-completion-cancelled-hook
              #'nexus-company--maybe-turn-on-fci)))

(provide 'nexus-company)
