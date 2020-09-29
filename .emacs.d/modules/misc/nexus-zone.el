(use-package zone
  :straight (:type built-in)

  :init
  (defun zone-choose (pgm)
    "Choose a PGM to run for `zone'."
    (interactive
     (list
      (completing-read
       "Program: "
       (mapcar 'symbol-name zone-programs))))
    (let ((zone-programs (list (intern pgm))))
      (zone))))

(use-package zone-nyan)

(provide 'nexus-zone)
