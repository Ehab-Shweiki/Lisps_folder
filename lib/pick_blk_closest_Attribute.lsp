(defun c:get_blk_closest_att_SinglePick ( / result ent pickpt obj atts att txt val newnum newtxt dec inc)

  ;; Select top-level block only
  (setq result (entsel "\nSelect block with attribute: "))
  (if result
    (progn
      (setq ent (car result))
      (setq pickpt (cadr result))
      (setq obj (vlax-ename->vla-object ent))

      ;; Ensure it's a block with attributes
      (if (and (= (vla-get-objectname obj) "AcDbBlockReference")
               (vlax-method-applicable-p obj 'GetAttributes))
        (progn
          (setq atts (vlax-invoke obj 'GetAttributes))
          (setq atts (vl-remove-if (function (lambda (a) (= (vla-get-TextString a) ""))) atts))

          (if (> (length atts) 0)
            (progn
              (setq att (get-closest-attribute atts pickpt))
              (setq txt (vla-get-TextString att))
              (setq val (read txt))

              (princ (strcat "Value: " (vl-princ-to-string val)))
              (if (numberp val)
                (princ "\nClosest attribute is numeric.")
                (princ "\nClosest attribute is not numeric.")
              )
            )
            (princ "\nNo valid attributes found.")
          )
        )
        (princ "\nNot a block reference with attributes.")
      )
    )
    (princ "\nNothing selected.")
  )
  (princ)
)

;-----------------
; SubFunctions
;-----------------	

(defun get-closest-attribute (atts pt)
  (car (vl-sort atts
    (function
      (lambda (a b)
        (<
         (distance pt (vlax-get a 'InsertionPoint))
         (distance pt (vlax-get b 'InsertionPoint))
        ))))))
