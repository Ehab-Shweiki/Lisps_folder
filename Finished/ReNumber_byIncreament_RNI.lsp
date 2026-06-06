(defun c:RENUM_Inc ( / sel inc i ent ename txt newnum newtxt pos obj atts att oldval oldvalStr)

  ;; Ask for increment as real (supports decimal)
  (setq inc (getreal "\nEnter increment value (default 1): "))
  (if (not inc) (setq inc 1.0))  ;; Default is 1.0 (real)

  ;; Select TEXT, MTEXT, and INSERT blocks
  (setq sel (ssget '((0 . "TEXT,MTEXT,INSERT"))))

  (if sel
    (progn
      (setq i 0)
      (repeat (sslength sel)
        (setq ent (ssname sel i))
        (setq obj (vlax-ename->vla-object ent))

        (cond
          ;; For TEXT or MTEXT
          ((member (vla-get-objectname obj) '("AcDbText" "AcDbMText"))
           (setq txt (vla-get-TextString obj))
           (setq oldval (read txt))
           (if (numberp oldval)
             (progn
               (setq newnum (+ oldval inc))
               (setq dec (count-decimals txt))
               (setq newtxt (rtos newnum 2 dec))
               (vla-put-TextString obj newtxt)
             )
           )
          )

          ;; For block with exactly one attribute
          ((and (= (vla-get-objectname obj) "AcDbBlockReference")
                (vlax-method-applicable-p obj 'GetAttributes))
           (setq atts (vlax-invoke obj 'GetAttributes))
           (foreach att atts
             (setq txt (vla-get-TextString att))
             (if (and (/= txt "") (numberp (read txt)))
               (progn
                 (setq oldval (read txt))
                 (setq newnum (+ oldval inc))
                 (setq dec (count-decimals txt))
                 (setq newtxt (rtos newnum 2 dec))
                 (vla-put-TextString att newtxt)
               )
             )
           )
          )
        )
        (setq i (1+ i))
      )
      (princ "\nRenumbering complete.")
    )
    (princ "\nNo valid objects selected.")
  )
  (princ)
)

(defun c:RENUM_Inc_PickAtt ( / result ent obj txt oldval newnum newtxt dec inc)

  ;; Ask increment
  (setq inc (getreal "\nEnter increment value (default 1): "))
  (if (not inc) (setq inc 1.0))

  ;; Pick exact attribute (nentsel allows targeting inside blocks)
  (setq result (nentsel "\nSelect an attribute inside a block: "))
  (if result
    (progn
      (setq ent (car result)) ; <- only use entity name
      (setq obj (vlax-ename->vla-object ent))

      ;; Check if it's an attribute reference
      (if (= (vla-get-objectname obj) "AcDbAttribute")
        (progn
          (setq txt (vla-get-TextString obj))
          (setq oldval (read txt))

          (if (numberp oldval)
            (progn
              (setq newnum (+ oldval inc))
              (setq dec (count-decimals txt))
              (setq newtxt (rtos newnum 2 dec))
              (vla-put-TextString obj newtxt)
              (princ (strcat "\nUpdated attribute value: " newtxt))
            )
            (princ "\nAttribute is not numeric.")
          )
        )
        (princ "\nPlease click directly on an attribute.")
      )
    )
    (princ "\nNothing selected.")
  )
  (princ)
)
;-----------------
; SubFunctions
;-----------------	
(defun count-decimals (s)
  (if (and (setq pos (vl-string-search "." s))
           (< pos (strlen s)))
    (- (strlen s) (1+ pos))
    0))

(defun get-closest-attribute (atts pt)
  (car (vl-sort atts
    (function
      (lambda (a b)
        (<
         (distance pt (vlax-get a 'InsertionPoint))
         (distance pt (vlax-get b 'InsertionPoint))
        ))))))

;---------------------
(defun c:RNI () (c:RENUM_Inc))
(defun c:RNIAtt () (c:RENUM_Inc_PickAtt))

(princ "\nType 'RNI' to run the ReNumber_byIncreament command.")
(princ "\nType 'RNIAtt' to run the ReNumber_byIncreament_Att command.")
(princ)
