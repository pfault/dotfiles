;; (use-package hideshow
;;   :straight (:type built-in)

;;   :bind
;;   ("C-=" . nexus-folding-toggle-selective-display)
;;   ("C-c C-f" . nexus-folding-toggle)

;;   :init
;;   (defun nexus-folding (&optional arg)
;;     "Activate or deactivate code folding.
;; Optional ARG is passed directly to mode toggle function."
;;     (hs-minor-mode (or arg t)))

;;   (defun nexus-folding-toggle (column)
;;     "Toggle hiding/showing blocks via hs-mode.
;; Borrowed from: http://www.emacswiki.org/emacs/HideShow"
;;     (interactive "P")
;;     (if hs-minor-mode
;;         (if (condition-case nil
;;                 (hs-toggle-hiding)
;;               (error t))
;;             (hs-show-all))
;;       (nexus-folding-toggle-selective-display column)))

;;   (defun nexus-folding-toggle-selective-display (column)
;;     "Helper function for `nexus-folding-toggle'."
;;     (interactive "P")
;;     (set-selective-display
;;      (or column
;;          (unless selective-display
;;            (1+ (current-column)))))))

(use-package origami
  :bind ("C-c h o" . hydra-origami/body)
  :config
  (defhydra hydra-origami (:color red
                                  :hint nil)
    "
_t_: toggle    _r_: redo    _p_: prev        _c_: close all
_u_: undo      _n_: next    _o_: open all    _q_: quit
"
    ("t" origami-recursively-toggle-node)
    ("u" origami-undo)
    ("r" origami-redo)
    ("p" origami-previous-fold)
    ("n" origami-next-fold)
    ("o" origami-open-all-nodes)
    ("c" origami-close-all-nodes)
    ("q" nil "Quit" :color blue))

  (global-origami-mode))

;; lsp-origami provides support for origami.el using language server protocol’s
;; textDocument/foldingRange functionality.
;; https://github.com/emacs-lsp/lsp-origami/
(use-package lsp-origami
  :hook ((lsp-after-open . lsp-origami-mode)))

(provide 'nexus-folding)
