(use-package flyspell
  :straight (:type built-in)
  :defer t
  :diminish flyspell-mode

  :hook
  (prog-mode . flyspell-prog-mode)

  :custom
  (ispell-program-name "hunspell") ;; use hunspell instead of ispell

  :config
  ;; Unbind keys used by nexus-goto-chg module.
  (unbind-key "C-." flyspell-mode-map)
  (unbind-key "C-," flyspell-mode-map)
  ;; Unbind keys used by nexus-resize-window module.
  (unbind-key "C-;" flyspell-mode-map)
  (when (executable-find "hunspell")
      (setq ispell-program-name (executable-find "hunspell"))
      (setq ispell-really-hunspell t)
      (setenv "DICTIONARY" "en_US")
      (ispell-set-spellchecker-params)
      (ispell-hunspell-add-multi-dic "de_DE,de_CH,en_GB,en_US"))
      (setq ispell-dictionary "de_DE,de_CH,en_GB,en_US")
      (setq ispell-personal-dictionary "~/.hunspell_personal")
      ;; ispell-set-spellchecker-params has to be called
      ;; before ispell-hunspell-add-multi-dic will work
;;  (setq ispell-dictionary "english")
  ;; The personal dictionary file has to exist, otherwise hunspell will
  ;; silently not use it.
  (unless (file-exists-p ispell-personal-dictionary)
    (write-region "" nil ispell-personal-dictionary nil 0)))


(use-package flyspell-correct
  :bind ("C-/" . flyspell-correct-wrapper)
  :custom
  (flyspell-correct-interface #'flyspell-correct-dummy))

(provide 'nexus-flyspell)
