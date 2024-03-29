(require 'nexus-all-the-icons)
(require 'nexus-doom-modeline)

(use-package doom-themes
  :custom

  ;; Global doom-themes options
  (doom-themes-enable-bold t)    ; if nil, bold is universally disabled
  (doom-themes-enable-italic t)  ; if nil, italics is universally disabled
  (doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-enable-variable-pitch nil)

  (doom-nord-light-brighter-comments nil)
  (doom-nord-light-brighter-modeline nil)
  (doom-nord-light-comment-bg nil)
  (doom-nord-light-padded-modeline nil)

  (doom-vibrant-brighter-comments nil)
  (doom-vibrant-brighter-modeline nil)
  (doom-vibrant-comment-bg nil)
  (doom-vibrant-padded-modeline nil)

  (nlinum-highlight-current-line t)

  :config
  ;; By default load the doom-vibrant theme.
  (nexus-doom-themes-load nexus-doom-themes-init-theme)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Configure treemacs styling
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  :init
  ;; Initial theme to load.
  (setq nexus-doom-themes-init-theme 'doom-vibrant)

  (defun nexus-doom-themes-load (theme)
    (interactive (list (completing-read "Choose theme: "
                                        (nexus-doom-themes-list))))

    (mapc #'disable-theme custom-enabled-themes)
    (load-theme (if (string= (type-of theme) "string") (intern theme) theme) t)
    (nexus-doom-themes-overrides))

  (defun nexus-doom-themes-vibrant-theme ()
    (interactive)
    (nexus-doom-themes-load 'doom-vibrant))

  (defun nexus-doom-themes-nord-light-theme ()
    (interactive)
    (nexus-doom-themes-load 'doom-nord-light))

  (defun nexus-doom-themes-list ()
    (seq-filter
     (lambda (n) (string-prefix-p "doom-" (symbol-name n)))
     (custom-available-themes)))

  (defun nexus-doom-themes-overrides ()
    (interactive)
    ;; Override some of doom-vibrant's colors.
    (set-face-attribute 'font-lock-variable-name-face nil
                         :foreground (doom-lighten 'blue 0.25))
    (set-face-attribute 'vertical-border nil
                        :foreground (doom-darken 'vertical-bar 0.1)
                        :background (doom-darken 'vertical-bar 0.1))

    (when (not (version< emacs-version "27.0"))
      (set-face-attribute 'fill-column-indicator nil
                          :foreground (doom-lighten 'base3 0.10)))

    (with-eval-after-load 'diff-hl
      (set-face-attribute 'diff-hl-insert nil
                          :foreground (doom-blend 'vc-added 'bg 0.7)
                          :background (doom-blend 'vc-added 'bg 0.2))
      (set-face-attribute 'diff-hl-delete nil
                          :foreground (doom-blend 'vc-deleted 'bg 0.7)
                          :background (doom-blend 'vc-deleted 'bg 0.2))
      (set-face-attribute 'diff-hl-change nil
                          :foreground (doom-blend 'vc-modified 'bg 0.7)
                          :background (doom-blend 'vc-modified 'bg 0.2)))

    (with-eval-after-load 'hideshowvis
      (set-face-attribute 'hideshowvis-hidable-face nil
                          :foreground (doom-color 'base7)))

    (with-eval-after-load 'auto-highlight-symbol
      (set-face-attribute 'ahs-definition-face nil
                          :foreground 'unspecified
                          :background (doom-lighten 'bg 0.1)
                          :underline t)
      (set-face-attribute 'ahs-edit-mode-face nil
                          :foreground (doom-lighten 'fg 1.0)
                          :background (doom-darken 'red 0.25))
      (set-face-attribute 'ahs-face nil
                          :foreground 'unspecified
                          :background (doom-lighten 'bg 0.1))
      (set-face-attribute 'ahs-plugin-bod-face nil
                          :foreground 'unspecified
                          :background (doom-color 'bg-alt))
      (set-face-attribute 'ahs-plugin-defalt-face nil
                          :foreground 'unspecified
                          :background (doom-color 'bg-alt))
      (set-face-attribute 'ahs-plugin-whole-buffer-face nil
                          :foreground 'unspecified
                          :background (doom-color 'bg-alt))
      (set-face-attribute 'ahs-warning-face nil
                          :foreground (doom-color 'red)
                          :background 'unspecified))

    (with-eval-after-load 'dired+
      (set-face-attribute 'diredp-dir-heading nil
                          :weight 'bold
                          :foreground (doom-color 'magenta))
      (set-face-attribute 'diredp-dir-name nil
                          :weight 'bold
                          :foreground (doom-lighten 'magenta 0.25))
      (set-face-attribute 'diredp-flag-mark nil
                          :foreground (doom-color 'green)
                          :background (doom-blend 'bg 'orange 0.5))
      (set-face-attribute 'diredp-flag-mark-line nil
                          :foreground (doom-color 'base8)
                          :background (doom-blend 'bg 'orange 0.7))
      (set-face-attribute 'diredp-deletion nil
                          :foreground (doom-color 'yellow)
                          :background (doom-darken 'red 0.25))
      (set-face-attribute 'diredp-deletion-file-name nil
                          :foreground (doom-color 'red))
      (set-face-attribute 'diredp-compressed-file-name nil
                          :foreground (doom-color 'blue)))

    (with-eval-after-load 'highlight-indent-guides
      (highlight-indent-guides-auto-set-faces))

    (with-eval-after-load 'zoom-window
      (setq zoom-window-mode-line-color (doom-blend 'magenta 'bg 0.2)))

    (with-eval-after-load 'fill-column-indicator
      (setq fci-rule-color (doom-lighten (doom-color 'base3) 0.10)))))

(provide 'nexus-doom-themes)
