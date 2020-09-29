(use-package hideshow
  :straight (:type built-in)

  :bind
  ("C-=" . nexus-folding-toggle-selective-display)
  ("C-c C-h" . nexus-folding-toggle)

  :init
  (defun nexus-folding (&optional arg)
    "Activate or deactivate code folding.
Optional ARG is passed directly to mode toggle function."
    (hs-minor-mode (or arg t))
    (if window-system (hideshowvis-minor-mode (or arg t))))

  (defun nexus-folding-toggle (column)
    "Toggle hiding/showing blocks via hs-mode.
Borrowed from: http://www.emacswiki.org/emacs/HideShow"
    (interactive "P")
    (if (bound-and-true-p hs-minor-mode)
        (if (condition-case nil
                (hs-toggle-hiding)
              (error t))
            (hs-show-all))
      (nexus-folding-toggle-selective-display column)))

  (defun nexus-folding-toggle-selective-display (column)
    "Helper function for `nexus-folding-toggle'."
    (interactive "P")
    (set-selective-display
     (or column
         (unless selective-display
           (1+ (current-column)))))))

(use-package hideshowvis
  :defer t)

(provide 'nexus-folding)
