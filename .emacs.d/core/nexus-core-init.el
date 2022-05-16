(defun display-startup-echo-area-message ()
    "Display startup echo area message."
      (message "Nexus initialized in %s" (emacs-init-time)))

(message "Nexus is powering up... Be patient, Master %s!"
              (getenv (if (equal system-type 'windows-nt) "USERNAME" "USER")))

;; Check Emacs version.
(when (version< emacs-version "26.1")
    (error "Nexus requires at least GNU Emacs 26.1, but you're running %s"
                emacs-version))

;; Setup basic paths.
(defvar nexus-core-dir (file-name-directory load-file-name)
    "Core directory within Emacs Nexus configuration.")
(defvar nexus-dir (expand-file-name ".." nexus-core-dir)
    "Root directory of Emacs Nexus configuration files.")

;; Configure nexus-cache-dir
(defvar nexus-cache-dir (expand-file-name "cache" user-emacs-directory)
    "Main cache directory which packages should be configured to use.")

(unless (file-exists-p nexus-cache-dir)
    (make-directory nexus-cache-dir))

;; Setup load-path
(add-to-list 'load-path nexus-core-dir)

;; Core stuff
(require 'nexus-core-custom)
(require 'nexus-core-utils)
(require 'nexus-core-userconfig)

;; Continue core stuff
(require 'nexus-core-packages)
(require 'nexus-core-package-overrides)
(require 'nexus-core-performance)
(require 'nexus-core-env)
(require 'nexus-core-ui)
(require 'nexus-core-editor)

;; macOS specific
(when (eq system-type 'darwin)
    (require 'nexus-core-macos))

;; Linux specific
(when (eq system-type 'gnu/linux)
    (require 'nexus-core-linux))

;; Config changes made through the customize UI will be store here
(setq custom-file (expand-file-name "custom.el" nexus-dir))
(load-file custom-file)

;; Enable custom themes
(require 'nexus-core-themes)

;; The modules
(require 'nexus-core-modules)

(provide 'nexus-core-init)
