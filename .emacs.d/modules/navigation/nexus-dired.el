(use-package dired
  :straight (:type built-in)
  :bind (:map dired-mode-map
              ("M-?" . nexus-dired-display-size))

  :init
  (defun nexus-dired-display-size (arg)
    "Display disk usage of marked items in Dired.
    When given a PREFIX, display raw size of items instead of disk usage."
    (interactive "P")
    (if arg
        (nexus-dired-get-size nil)
      (nexus-dired-get-disk-usage)))

  ;; Based on dired-get-size from:
  ;; https://www.emacswiki.org/emacs/DiredGetFileSize
  (defun nexus-dired-get-disk-usage ()
    "Display total disk usage of marked items in Dired."
    (interactive)
    (let ((files (dired-get-marked-files)))
      (with-temp-buffer
        (apply 'call-process (executable-find "du") nil t nil "-sch" files)
        (message "Size of all marked files: %s"
                 (progn
                   (re-search-backward "^\s*?\\([0-9.,]+[A-Za-z]+\\).*total$")
                   (match-string 1))))))

  (defun nexus-dired-get-size (arg)
    "Display the total size of marked files in Dired."
    (interactive "P")
    (let ((size (nexus-file-sizes (dired-get-marked-files))))
      (message "Size of all marked files: %s"
               (if arg
                   (format "%.0f" size)
                 (file-size-human-readable size 'si)))))

  (defun nexus-directory-size (dirname)
    "Return the size of DIRNAME in bytes."
    (nexus-file-sizes (directory-files-recursively dirname "")))

  (defun nexus-file-sizes (filename-list)
    "Return the sum of sizes of FILENAME-LIST in bytes."
    (apply '+
           (mapcar 'nexus-file-size filename-list)))

  (defun nexus-file-size (filename)
    "Return size of file FILENAME in bytes.
    The size is converted to float for consistency.
    This doesn't recurse directories."
    (float (if (file-directory-p filename)
               (nexus-directory-size filename)
             (file-attribute-size ; might be int or float
              (file-attributes filename)))))

  :config
  (when (string-match-p "^gnu" (symbol-name system-type))
    (setq dired-use-ls-dired t
          dired-listing-switches "-aBhl"))

  (when (string= system-type "darwin")
    (let ((gls (executable-find "gls")))
      (when gls
        (setq dired-use-ls-dired t
              insert-directory-program gls
              dired-listing-switches "-aBhl")))))

(use-package dired-x
  :straight (:type built-in))

(use-package dired+
  :demand t
  :bind (:map dired-mode-map
              ("c" . dired-create-directory)
              ("C-l" . diredp-up-directory-reuse-dir-buffer))

  :hook
  (dired-mode . nexus-diredp-mode-setup)

  :init
  (defun nexus-diredp-mode-setup ()
    (toggle-diredp-find-file-reuse-dir 1))

  :config
  (unbind-key "M-i" dired-mode-map)
  (unbind-key "M-l" dired-mode-map))

(use-package dired-subtree
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)))

(use-package dired-narrow
  :bind (:map dired-mode-map
              ("C-s" . dired-narrow))

  :custom
  (dired-narrow-exit-action 'dired-narrow-find-file)
  (dired-narrow-exit-when-one-left nil))

(provide 'nexus-dired)
