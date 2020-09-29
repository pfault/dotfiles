;; Enable dabbrev-expand via custom keybinding.
(global-set-key (kbd "C-x M-/") 'dabbrev-expand)

;; Easier version of "C-x k" to kill buffer
;;(global-set-key (kbd "C-x C-k") 'kill-buffer)

;; Evaluate buffer
(global-set-key (kbd "C-c C-e") 'eval-buffer)

;; Window switching
(global-set-key (kbd "C-x i")   'nexus-other-window-reverse)
(global-set-key (kbd "C-x C-o") 'other-window)
(global-set-key (kbd "C-x C-i") 'nexus-other-window-reverse)

;; Window management
(global-set-key (kbd "C-x C-SPC") 'balance-windows)
(global-set-key (kbd "C-x SPC") 'balance-windows)

;; Kill-Ring related
(global-set-key (kbd "M-Y") 'nexus-yank-pop-forwards)

;; Align to equal signs
(global-set-key (kbd "C-x a =") 'nexus-align-region-to-equals)
(global-set-key (kbd "C-x a {") 'nexus-align-region-to-opening-brace)

;; align-regexp
(global-set-key (kbd "C-c a") 'align-regexp)

;; Toggle auto-fill-mode.
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;; iBuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Rename current file and buffer
(global-set-key (kbd "C-c r")  'nexus-rename-file-and-buffer)

;; Mac OS X specific keybindings
(when (eq system-type 'darwin)
  ;; Move to beginning/end of buffer
  (global-set-key (kbd "s-<up>") 'beginning-of-buffer)
  (global-set-key (kbd "s-<down>") 'end-of-buffer)

  ;; Move to beginning/end of line
  (global-set-key (kbd "s-<left>") 'beginning-of-line)
  (global-set-key (kbd "s-<right>") 'end-of-line))

(define-prefix-command 'pfault-window-map)
(global-set-key (kbd "M-o") 'pfault-window-map)
(define-key global-map (kbd "M-o") 'pfault-window-map)

(global-set-key (kbd "M-o b")   'switch-to-buffer)

(provide 'nexus-global-keybindings)
