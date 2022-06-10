(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-variables '("PATH"
                                    "MANPATH"
                                    "TMPDIR"
                                    "GOPATH"
                                    "GOBIN"
                                    "GOROOT"
                                    "GOPRIVATE"
                                    "GOENV_GOPATH_PREFIX"
                                    "GOENV_VERSION"
                                    "KUBECONFIG"))
  (exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-debug nil)

  :config
  ;(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize));)

;; Set temporary-file-directory to match TMPDIR environment variable
(let ((tmpdir (getenv "TMPDIR")))
  (when (and tmpdir (not (string-blank-p tmpdir)))
    (setq temporary-file-directory tmpdir)))

;; Add bin directory within emacs configuration dir to `exec-path'.
(add-to-list 'exec-path (expand-file-name "bin" nexus-dir))

(provide 'nexus-core-env)
