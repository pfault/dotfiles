(defun downcase-region-only (beg end &rest args)
  "Only downcase if region (BEG END) is active.
Avoids accidental downcase when region is not active.  Passes all
additional ARGS passed along to `downcase-region'."
  (interactive "r")
  (when (region-active-p)
    (apply #'downcase-region beg end args)))

(defun upcase-region-only (beg end &rest args)
  "Only upcase if region (BEG END) is active.
Avoids accidental upcase when region is not active.  Passes all
additional ARGS passed along to `upcase-region'."
  (interactive "r")
  (when (region-active-p)
    (apply #'upcase-region beg end args)))

(global-set-key (kbd "C-x C-l") 'downcase-region-only)
(global-set-key (kbd "C-x C-u") 'upcase-region-only)

(provide 'nexus-safe-change-case)
