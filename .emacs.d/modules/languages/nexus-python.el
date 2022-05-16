(defun poetry-which (prog dir)
  "Find program using poetry run which."
  (let (
        (stdout-buffer (generate-new-buffer "tmp-poetry-stdout"))
        (default-directory dir)
        )
    (unwind-protect
        (let (
              (exit-status
               (condition-case nil (call-process "poetry" nil (list stdout-buffer nil) nil "run" "which" prog) (file-missing nil))
               )
              )
          (if (eq exit-status 0)
              (progn
                (with-current-buffer stdout-buffer
                  (string-trim (buffer-string))
                  )
                )
            )
          )
      (kill-buffer stdout-buffer)
      )
    )
  )

(defun locate-venv (dir)
  "Find a poetry venv."
  ;;poetry env info -p
  (let (
        (stdout-buffer (generate-new-buffer "tmp-poetry-stdout"))
        (default-directory dir)
        )
    (unwind-protect
        (let (
              (exit-status
               (condition-case nil (call-process "poetry" nil (list stdout-buffer nil) nil "env" "info" "-p") (file-missing nil))
               )
              )
          (if (eq exit-status 0)
              (progn
                (with-current-buffer stdout-buffer
                  (string-trim (buffer-string))
                  )
                )
            )
          )
      (kill-buffer stdout-buffer)
      )
    )
  )

(defun locate-pyproject-directory ()
  "Adapt lsp-python-ms for poetry."
  (let ((pypoetry-file (locate-dominating-file (buffer-file-name) "pyproject.toml")))
    pypoetry-file
    )
  )


(defun python-fmt ()
  "format python."
  (python-fmt-black)
  (python-fmt-isort)
  )

(defun python-fmt-black ()
  "Run black."
  (let (
        (stdout-buffer (generate-new-buffer "tmp-fmt-stdout"))
        (default-directory poetry-root)
        )
    (unwind-protect
        (let ((exit-status (call-process-region nil nil black-bin nil stdout-buffer nil "--quiet" "--fast" "-")))
          (if (eq exit-status 0)
              (save-excursion
                (replace-buffer-contents stdout-buffer)
                )
            (message "FAILED")
            )
          )
      (kill-buffer stdout-buffer)
      )
    )
  )

(defun python-fmt-isort ()
  "Run isort."
  (let (
        (stdout-buffer (generate-new-buffer "tmp-fmt-stdout"))
        (default-directory poetry-root)
        )
    (unwind-protect
        (let ((exit-status (call-process-region nil nil isort-bin nil stdout-buffer nil "-")))
          (if (eq exit-status 0)
              (save-excursion
                (replace-buffer-contents stdout-buffer)
                )
            (message "FAILED")
            )
          )
      (kill-buffer stdout-buffer)
      )
    )
  )

;; (use-package lsp-python-ms
;;   :config
;;   ;; for dev build of language server
;;   (setq lsp-python-ms-dir (expand-file-name "/usr/lib/microsoft-python-language-server/"))
;;   ;; for executable of language server, if it's not symlinked on your PATH
;;   (setq lsp-python-ms-executable "/usr/bin/mspyls")
;;   )

(use-package lsp-python-ms)

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :commands python-mode
;;  :pin manual
  :hook (
         (python-mode . lsp)
         (python-mode . dap-mode)
         (python-mode . dap-ui-mode)
 ;;        (python-mode . company-mode)
         (python-mode . (lambda () (let ((venv (locate-venv default-directory))) (when venv (setq-local lsp-pyls-plugins-jedi-environment venv)))))
         (python-mode . (lambda () (setq-local isort-bin (or (poetry-which "isort" default-directory) "isort"))))
         (python-mode . (lambda () (setq-local black-bin (or (poetry-which "black" default-directory) "black"))))
         (python-mode . (lambda () (setq-local poetry-root (or (locate-pyproject-directory) default-directory))))
         ;; (python-mode . (lambda () (setq-local lsp-python-ms-python-executable-cmd (combined-which "python" default-directory))))
         ;; (python-mode . (lambda () (if (string-match-p (regexp-quote "/tests/") default-directory) (setq-local lsp-python-ms-extra-paths (list default-directory)))))
         (python-mode . (lambda () (add-hook 'before-save-hook 'python-fmt nil 'local)))
         )
  ;;:bind ((:map python-mode-map ([tab] . company-indent-or-complete-common)))
  )

(provide 'nexus-python)
