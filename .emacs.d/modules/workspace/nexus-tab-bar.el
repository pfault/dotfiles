(require 'nexus-workspace-map)

(use-package tab-bar
  :straight (:type built-in)

  :hook
  (emacs-startup . nexus-tab-bar-setup)

  :bind
  ("s-}" . tab-next)
  ("s-{" . tab-previous)
  (:map nexus-workspace-map
        ("C-n" . tab-next)
        ("C-p" . tab-previous)
        ("c" . tab-new)
        ("C-c" . tab-new)
        ("k" . tab-close)
        ("C-k" . tab-close)
        ("l" . tab-recent)
        ("C-l" . tab-recent)
        ("r" . tab-rename)
        ("C-r" . tab-rename)
        ("n" . nexus-tab-bar-move-tab-right)
        ("p" . nexus-tab-bar-move-tab-left)
        ("s" . nexus-tab-bar-switch-to-or-create-tab)
        ("C-s" . nexus-tab-bar-switch-to-or-create-tab)
        ("e" . tab-switcher)
        ("C-e" . tab-switcher)
        ("u" . tab-undo)
        ("C-u" . tab-undo)
        (";" . nexus-tab-bar-echo-tab-list)
        ("C-;" . nexus-tab-bar-echo-tab-list)
        ("0" . nexus-tab-bar-switch-to-index)
        ("1" . nexus-tab-bar-switch-to-index)
        ("2" . nexus-tab-bar-switch-to-index)
        ("3" . nexus-tab-bar-switch-to-index)
        ("4" . nexus-tab-bar-switch-to-index)
        ("5" . nexus-tab-bar-switch-to-index)
        ("6" . nexus-tab-bar-switch-to-index)
        ("7" . nexus-tab-bar-switch-to-index)
        ("8" . nexus-tab-bar-switch-to-index)
        ("9" . nexus-tab-bar-switch-to-index)

        ("b" . tab-bar-history-back)
        ("C-b" . tab-bar-history-back)
        ("f" . tab-bar-history-forward)
        ("C-f" . tab-bar-history-forward))

  :custom
  (tab-bar-history-limit 25)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-tab-hints nil)
  (tab-bar-show nil)

  :init
  (defun nexus-tab-bar-setup ()
    (tab-bar-mode)
    (tab-bar-history-mode))

  (defgroup nexus-tab-bar nil
    "Siren specific tweaks to tar-bar-mode."
    :group 'tab-bar)

  (defcustom nexus-tab-bar-echo-tab-list t
    "When t and print list of tabs in echo area when changing tabs."
    :type 'boolean
    :group 'nexus-tab-bar)

  (defface nexus-tab-bar-echo-default
    '((t :inherit default))
    "Face for tab names in echo area."
    :group 'nexus-tab-bar)

  (defface nexus-tab-bar-echo-current
    '((t :inherit font-lock-keyword-face))
    "Face for current tab name in echo area."
    :group 'nexus-tab-bar)

  (defface nexus-tab-bar-echo-index
    '((t :inherit font-lock-comment-face))
    "Face for index numbers in echo area."
    :group 'nexus-tab-bar)

  (defun nexus-tab-bar-switch-to-or-create-tab (name)
    "Switch to or create a tab by NAME."
    (interactive
     (let* ((recent-tabs (mapcar (lambda (tab) (alist-get 'name tab))
                                 (tab-bar--tabs-recent))))
       (list (completing-read "Switch to tab by name (default recent): "
                              recent-tabs nil nil nil nil recent-tabs))))
    (let ((tab-names (mapcar (lambda (tab) (alist-get 'name tab))
                              (funcall tab-bar-tabs-function))))
      (if (member name tab-names)
          (tab-bar-switch-to-tab name)
        (nexus-tab-bar-new-named-tab name)))
    (tab-bar-select-tab (1+ (or (tab-bar--tab-index-by-name name) 0))))

  (defun nexus-tab-bar-new-named-tab (name)
    "Create a new tab named NAME."
    (interactive "MName for new tab (leave blank for automatic naming): ")
    (tab-new 99999)
    (if (not (string= name ""))
        (tab-rename name)))

  (defun nexus-tab-bar-switch-to-index (&optional arg)
    "Switch to tab with index ARG.
When this command is bound to a numeric key, calling it without
an argument will translate its bound numeric key to the numeric
argument.
ARG counts from 1."
    (interactive "P")
    (unless (integerp arg)
      (let ((key (event-basic-type last-command-event)))
        (setq arg (if (and (characterp key) (>= key ?0) (<= key ?9))
                      (- key ?0)
                    0))))

    (tab-bar-select-tab (1+ arg)))

  (defun nexus-tab-bar-move-tab-left ()
    "Move current tab to the left."
    (interactive)
    (tab-move -1))

  (defun nexus-tab-bar-move-tab-right ()
    "Move current tab to the right."
    (interactive)
    (tab-move 1))

  (defun nexus-tab-bar-echo-tab-list ()
    "Echo list of tabs"
    (interactive)
    (let* ((tabs (funcall tab-bar-tabs-function))
           (current-index (or (tab-bar--current-tab-index tabs) 0))
           (output "")
           (index 0))
      (dolist (tab tabs)
        (setq output
              (concat output
                      (propertize (format "%d:" index)
                                  'face 'nexus-tab-bar-echo-index)
                      (propertize (alist-get 'name tab)
                                  'face (if (eq index current-index)
                                            'nexus-tab-bar-echo-current
                                          'nexus-tab-bar-echo-default))
                      " ")
              index (1+ index)))

      (message "tabs: %s" output)))

  (defun nexus-tab-bar-echo-tab-list-advice (&rest _)
    (when nexus-tab-bar-echo-tab-list
      (nexus-tab-bar-echo-tab-list)))

  (advice-add 'tab-bar-close-tab :after #'nexus-tab-bar-echo-tab-list-advice)
  (advice-add 'tab-bar-move-tab-to :after #'nexus-tab-bar-echo-tab-list-advice)
  (advice-add 'tab-bar-new-tab-to :after #'nexus-tab-bar-echo-tab-list-advice)
  (advice-add 'tab-bar-rename-tab :after #'nexus-tab-bar-echo-tab-list-advice)
  (advice-add 'tab-bar-select-tab :after #'nexus-tab-bar-echo-tab-list-advice)
  (advice-add 'tab-switcher-select :after #'nexus-tab-bar-echo-tab-list-advice))

(provide 'nexus-tab-bar)
