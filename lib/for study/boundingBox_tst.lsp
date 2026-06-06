(defun ent-center (obj) ;; #TODO: check
    (cond
      ((= (vla-get-objectname obj) "AcDbMText")
       (if (and (vlax-property-available-p obj 'boundingbox))
         (let* ((ext (vlax-get obj 'boundingbox)))
           (if (and (car ext) (cadr ext))
             (let ((bbox_min (vlax-safearray->list (car ext)))
                   (bbox_max (vlax-safearray->list (cadr ext))))
               (mapcar '(lambda (a b) (/ (+ a b) 2.0)) bbox_min bbox_max)
             )
           )
         )
       )
      )
      ((= (vla-get-objectname obj) "AcDbText")
       (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint obj))))
    )
  )

;; for check
(progn
  (setq obj (vlax-ename->vla-object (car (entsel "\nSelect an entity: "))))
  (print (vla-get-objectname obj))
  (princ "\n")
  (vlax-property-available-p obj 'boundingbox)
  (setq res (vl-catch-all-apply 'vlax-get (list obj 'BoundingBox)))
)