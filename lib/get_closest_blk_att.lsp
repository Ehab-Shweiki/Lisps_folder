(defun get-closest-attribute (atts pt)
  (car (vl-sort atts
    (function
      (lambda (a b)
        (<
         (distance pt (vlax-get a 'InsertionPoint))
         (distance pt (vlax-get b 'InsertionPoint))
        ))))))


