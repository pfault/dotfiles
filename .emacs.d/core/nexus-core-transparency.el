(defun nexus-transparency-fix ()
    "Set initial value of alpha parameter for the current frame."
      (interactive)
        (if (equal (frame-parameter nil 'alpha) nil)
		(set-frame-parameter nil 'alpha 100)))

(defun nexus-transparency-increase ()
    "Increase level of transparency for the current frame."
      (interactive)
      (nexus-transparency-fix)
      (if (> (frame-parameter nil 'alpha) 0)
	  (let ((new-level (+ (frame-parameter nil 'alpha) -1)))
	    (set-frame-parameter nil 'alpha new-level)
	    (message "Frame transparency set to %s" new-level))
      (message "This is a minimum value of transparency!")))

(defun nexus-transparency-decrease ()
    "Decrease level of transparency for the current frame."
      (interactive)
        (nexus-transparency-fix)
	  (if (< (frame-parameter nil 'alpha) 100)
		  (let ((new-level (+ (frame-parameter nil 'alpha) +1)))
			    (set-frame-parameter nil 'alpha (+ (frame-parameter nil 'alpha) +1))
				    (message "Frame transparency set to %s" new-level))
		      (message "This is a minimum value of transparency!")))

(defun nexus-transparency (numb)
    "Set level of transparency for the current frame by providing NUMB."
      (interactive "nEnter transparency level in range 0-100: ")
        (if (> numb 100)
		(message "Error! The maximum value for transparency is 100!")
		    (if (< numb 0)
			      (message "Error! The minimum value for transparency is 0!")
				    (set-frame-parameter nil 'alpha numb))))

(nexus-transparency nexus-transparency-level)
(add-hook 'after-make-frame-functions
		    (lambda (selected-frame)
				  (set-frame-parameter selected-frame 'alpha nexus-transparency-level)))

;; Keybindings
(global-set-key (kbd "C-M-|") 'nexus-transparency)
(global-set-key (kbd "C-M-<") 'nexus-transparency-increase)
(global-set-key (kbd "C-M->") 'nexus-transparency-decrease)

(provide 'nexus-core-transparency)
