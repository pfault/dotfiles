(require 'nexus-highlight-indent-guides)

(defun nexus-display-indentation (&optional arg)
  "Activate or deactivate indentation guides.
Optional ARG is passed directly to mode toggle function."
  (interactive)
  (highlight-indent-guides-mode arg))

(provide 'nexus-display-indentation)
