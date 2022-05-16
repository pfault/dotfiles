(require 'nexus-mini-frame)
(require 'nexus-prescient)

(use-package selectrum
         :init
         (selectrum-mode +1))

(use-package selectrum-prescient
  :after selectrum
  :config
  ;; to make sorting and filtering more intelligent
  (selectrum-prescient-mode +1)

  ;; to save your command history on disk, so the sorting gets more
  ;; intelligent over time
  (prescient-persist-mode +1)
  (defun selectrum-fido-backward-updir ()
    "Delete char before or go up directory, like `ido-mode'."
    (interactive)
    (if (and (eq (char-before) ?/)
             (eq (selectrum--get-meta 'category) 'file))
        (save-excursion
          (goto-char (1- (point)))
          (when (search-backward "/" (point-min) t)
            (delete-region (1+ (point)) (point-max))))
      (call-interactively 'backward-delete-char)))

  (defun selectrum-fido-delete-char ()
    "Delete char or maybe call `dired', like `ido-mode'."
    (interactive)
    (let ((end (point-max)))
      (if (or (< (point) end) (not (eq (selectrum--get-meta 'category) 'file)))
          (call-interactively 'delete-char)
        (dired (file-name-directory (minibuffer-contents)))
        (exit-minibuffer))))

  (defun selectrum-fido-ret ()
    "Exit minibuffer or enter directory, like `ido-mode'."
    (interactive)
    (let* ((dir (and (eq (selectrum--get-meta 'category) 'file)
                     (file-name-directory (minibuffer-contents))))
           (current (selectrum-get-current-candidate))
           (probe (and dir current
                       (expand-file-name (directory-file-name current) dir))))
      (cond ((and probe (file-directory-p probe) (not (string= current "./")))
             (selectrum-insert-current-candidate))
            (t
             (selectrum-select-current-candidate)))))


  (define-key selectrum-minibuffer-map (kbd "RET") 'selectrum-fido-ret)
  (define-key selectrum-minibuffer-map (kbd "DEL") 'selectrum-fido-backward-updir)
  (define-key selectrum-minibuffer-map (kbd "C-d") 'selectrum-fido-delete-char))

(provide 'nexus-selectrum)
