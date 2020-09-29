(defgroup nexus nil
  "Basic settings for Nexus."
  :group 'tools)

(defcustom nexus-yank-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur."
  :type 'number
  :group 'nexus)

(defcustom nexus-indent-sensitive-modes
  '(coffee-mode conf-mode haml-mode makefile-automake-mode makefile-bsdmake-mode
                makefile-gmake-mode makefile-imake-mode makefile-makepp-mode
                makefile-mode python-mode slim-mode yaml-mode)
  "Major modes for which auto-indenting is suppressed."
  :type '(repeat symbol)
  :group 'nexus)

(defcustom nexus-yank-indent-modes '(LaTeX-mode TeX-mode)
  "Major modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here."
  :type '(repeat symbol)
  :group 'nexus)

(defcustom nexus-transparency-level 99
  "The default frame transparency level for Emacs frames."
  :type 'number
  :group 'nexus)

(provide 'nexus-core-custom)
