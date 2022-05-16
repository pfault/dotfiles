(if window-system
    (set-face-attribute 'default nil :family "Hack" :height 100))

;; Keybindinds
(global-set-key (kbd "C-M-<return>") 'nexus-linux-toggle-fullscreen)

;; Fullscreen helper function
(defun nexus-linux-toggle-fullscreen ()
  "Toggle full screen on X11."
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(provide 'nexus-core-linux)
