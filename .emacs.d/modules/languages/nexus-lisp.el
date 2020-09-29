(define-key read-expression-map (kbd "TAB") 'completion-at-point)

;; wrap keybindings
(define-key lisp-mode-shared-map (kbd "M-(") (nexus-wrap-with "("))
;; FIXME: Pick terminal-friendly binding.
;;(define-key lisp-mode-shared-map (kbd "M-[") (nexus-wrap-with "["))
(define-key lisp-mode-shared-map (kbd "M-\"") (nexus-wrap-with "\""))

;; a great lisp coding hook
(defun nexus-lisp-coding-hook ())

;; interactive modes don't need whitespace checks
(defun nexus-interactive-lisp-coding-hook ()
  (whitespace-mode -1))

(provide 'nexus-lisp)
