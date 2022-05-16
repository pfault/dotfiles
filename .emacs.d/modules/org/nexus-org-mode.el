;;; package --- nexus-org-mode
;;; Commentary:
;;; org-mode configuration

;;; Code:

(require 'nexus-display-fill-column)
(require 'nexus-display-indentation)
(require 'nexus-display-line-numbers)
(require 'nexus-flyspell)
(require 'nexus-smartparens)

(use-package org
  :straight org-contrib
  :bind
  (("C-c c" . org-capture)
   ("C-c a" . org-agenda)
   ("M-o a" . org-agenda)
   ("M-o c" . org-cycle-agenda-files)
   ("M-o t l" . org-toggle-link-display)
   ("M-o t i" . org-toggle-inline-images)
   :map org-mode-map
   ("C-c i" . org-clock-in)
   ("C-c o" . org-clock-out)
   ("C-c e" . org-set-effort)
   ("C-c >" . org-time-stamp-inactive)
   ("C-c l" . hydra-org-clock/body))
  (:map org-mode-map
   ("C-j" . org-return-indent)
   ;;("RET" . org-return-indent)
   ("M-{" . org-promote-subtree)
   ("M-}" . org-demote-subtree)
   ("M-P" . org-metaup)
   ("M-N" . org-metadown)
   ("C-M-n" . outline-next-visible-heading)
   ("C-M-p" . outline-previous-visible-heading))
  (:map org-src-mode-map
   ("C-c C-c" . org-edit-src-exit))
  :hook
  (org-mode . nexus-org-mode-setup)
  :config
  (setq ;; paths
   org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar"
   org-plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar"
   org-directory (if (file-directory-p "~/git/org/")
                     "~/git/org/" "~/org/")
   org-metadir (concat org-directory "orgmeta/")
   org-default-notes-file (concat org-directory "refile.org")
   org-archive-location "%s_archive::* Archived Tasks"
   diary-file (concat org-directory "diary.org")
   org-agenda-file-regexp "^[a-z0-9-_]+.org"

   ;; key behaviour
   org-support-shift-select 'always
   org-return-follows-link t
   org-special-ctrl-a/e t
   org-special-ctrl-k t

   ;; crypt config
   org-tags-exclude-from-inheritance (quote ("crypt"))
   org-crypt-key "5E05F53FB06BFA30"
   org-crypt-disable-auto-save t

   ;; general settings
   org-blank-before-new-entry '((heading . auto) (plain-list-item . nil))
   org-catch-invisible-edits 'show
   org-use-speed-commands t

   org-startup-indented t
   org-confirm-babel-evaluate nil
   org-babel-results-keyword "results"
   org-startup-folded 'content
   org-startup-with-inline-images t
   org-id-method (quote uuidgen)
   org-read-date-prefer-future 'time
   org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)
   org-time-stamp-rounding-minutes (quote (1 1))
   org-deadline-warning-days 30
   org-alphabetical-lists t
   org-modules (quote (org-annotate-file org-bbdb org-checklist org-collector
                                org-crypt org-eval org-id
                                org-info org-interactive-query org-habit
                                org-inlinetask org-mouse org-panel
                                org-protocol org-screen org-toc))
   org-capture-templates `(("t" "todo" entry (file+headline ,(concat org-directory "refile.org") "Tasks")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "respond" entry (file ,(concat org-directory "refile.org"))
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-
finish t)
              ("n" "note" entry (file+headline ,(concat org-directory "refile.org") "Notes")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("d" "Diary" entry (file+olp+datetree ,(concat org-directory "diary.org"))
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file+headline ,(concat org-directory "refile.org") "Review")
               "* TODO Review %c\n%U\n" :immediate-finish t)
              ("m" "Meeting" entry (file ,(concat org-directory "refile.org"))
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("c" "Phone call" entry (file ,(concat org-directory "refile.org"))
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file ,(concat org-directory "refile.org"))
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE:
 habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
              ("p" "Protocol" entry (file+headline ,(concat org-directory "refile.org") "Review")
               "* TODO Review %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
              ("L" "Protocol Link" entry (file+headline ,(concat org-directory "refile.org") "Review")
               "* TODO Review %? [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n")
              )

   ;; export
   org-export-coding-system 'utf-8
   org-export-with-timestamps nil
   org-table-export-default-format "orgtbl-to-csv"

   ;; tags
   org-use-fast-tag-selection t
   org-fast-tag-selection-single-key 'expert
   org-tag-alist (quote ((:startgroup)
                         ("@errand" . ?e)
                         ("@office" . ?o)
                         ("@home" . ?H)
                         ("@garden" . ?g)
                         (:endgroup)
                         ("WAITING" . ?w)
                         ("HOLD" . ?h)
                         ("PERSONAL" . ?P)
                         ("CODING" . ?0)
                         ("WORK" . ?W)
                         ("PERSONC" . ?C)
                         ("WORKPLACE" . ?Z)
                         ("GARDEN" . ?G)
                         ("ORG" . ?O)
                         ("EXAMPLE" . ?N)
                         ("crypt" . ?E)
                         ("NOTE" . ?n)
                         ("CANCELLED" . ?c)
                         ("FLAGGED" . ??)))

   ;; todos
   org-use-fast-todo-selection t
   org-enforce-todo-dependencies t
   org-hierarchical-todo-statistics nil
   org-provide-todo-statistics t
   org-fontify-done-headline t
   org-goto-interface 'outline-path-completion
   org-todo-keywords
   (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
           (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))
   org-todo-keyword-faces
   (quote (("TODO" :foreground "red" :weight bold)
           ("MEXT" :foreground "blue" :weight bold)
           ("DONE" :foreground "forest green" :weight bold)
           ("WAITING" :foreground "orange" :weight bold)
           ("HOLD" :foreground "magenta" :weight bold)
           ("CANCELLED" :foreground "forest green" :weight bold)
           ("MEETING" :foreground "forest green" :weight bold)
           ("PHONE" :foreground "forest green" :weight bold)))
   org-todo-state-tags-triggers
   (quote (("CANCELLED" ("CANCELLED" . t))
           ("WAITING" ("WAITING" . t))
           ("HOLD" ("WAITING") ("HOLD" . t))
           (done ("WAITING") ("HOLD"))
           ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
           ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
           ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))

   ;; logs
   org-log-into-drawer t
   org-drawers (quote ("PROPERTIES" "LOGBOOK"))
   org-log-done t
   org-log-redeadline 'note
   org-log-reschedule 'time

   ;; agenda
   org-agenda-files (list org-directory
                          (concat org-directory "pfault")
                          (concat org-directory "workplace"))
   org-agenda-include-diary nil
   org-agenda-insert-diary-extract-time t
   org-agenda-diary-file "~/git/org/diary.org"
   org-agenda-start-with-log-mode t
   org-agenda-log-mode-items '(closed clock state)
   org-agenda-skip-deadline-prewarning-if-scheduled t
   org-agenda-dim-blocked-tasks t
   org-agenda-skip-deadline-if-done t
   org-agenda-skip-scheduled-if-done t
   org-agenda-skip-archived-trees nil
   ;;org-agenda-todo-ignore-scheduled "future"
   org-agenda-ignore-properties '(effort appt category)
   org-agenda-todo-ignore-scheduled 'future

   ;; habits
   org-habit-show-habits-only-for-today nil

   ;; refile
   org-refile-targets (quote ((nil :maxlevel . 9)
                              (org-agenda-files :maxlevel . 9)))
   org-reverse-note-order nil    ; File at the bottom of an entry
   org-refile-allow-creating-parent-nodes 'confirm
   org-refile-use-outline-path 'buffer-name
   org-outline-path-complete-in-steps nil
   org-refile-use-cache t

   ;; src handling
   org-src-preserve-indentation nil
   org-edit-src-content-indentation 0
   org-src-fontify-natively t

   )

  ;; Remove empty LOGBOOK drawers on clock out
  (defun pfault/remove-empty-drawer-on-clock-out ()
    (interactive)
    (save-excursion
      (beginning-of-line 0)
      (org-remove-empty-drawer-at (point))))


  (defun pfault/org-refile-filter-targets (orig-fun &rest args)
    (let ((targets (apply orig-fun args))
          (agenda-files (mapcar #'(lambda (f) (buffer-name (buffer-base-buffer (org-get-agenda-file-buffer f)))) org-agenda-files)))
      (cl-remove-if (lambda (x)
                      (or (string-prefix-p "birthdays.org" (car x)) (string-prefix-p "calendar.org" (car x))))
                    targets)))


  :init
  (org-crypt-use-before-save-magic)
  (add-hook 'org-mode-hook 'turn-on-flyspell :append)
  (add-hook 'org-clock-out-hook 'pfault/remove-empty-drawer-on-clock-out :append)
  (advice-add 'org-refile-get-targets :around #'pfault/org-refile-filter-targets)
  (defun nexus-org-mode-setup ()
    (org-babel-do-load-languages 'org-babel-load-languages '((dot . t)
                                                             (ditaa . t)
                                                             (C . t)
                                                             (emacs-lisp . t)
                                                             (python . t)
                                                             (shell . t)
                                                             (js . t)
                                                             (ledger . t)
                                                             (org . t)
                                                             (plantuml . t)
                                                             (latex . t)
                                                             (R . t)))
    (setq fill-column 80
          whitespace-action '(auto-cleanup))

    (setcar (nthcdr 4 org-emphasis-regexp-components) 20)
    (org-set-emph-re 'org-emphasis-regexp-components
                     org-emphasis-regexp-components)

    (nexus-display-fill-column)
    (nexus-display-indentation)
    (nexus-display-line-numbers)
    (flyspell-mode)
    (smartparens-mode +1)
    (visual-line-mode +1)
    (whitespace-mode +1))
  )

(provide 'nexus-org-mode)
;;; nexus-org-mode.el ends here
