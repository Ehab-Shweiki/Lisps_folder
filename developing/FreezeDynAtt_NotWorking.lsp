(defun c:FreezeDynAtt ( / ss i ent obj atts att val field)
  ; converts dynamic (field-based) block attributes into fixed/static text values
  
  (vl-load-com)

  (prompt "\nSelect blocks with dynamic attributes to freeze: ")
  (setq ss (ssget '((0 . "INSERT")))) ; select block references only

  (if (null ss)
    (prompt "\nNo blocks selected.")
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq attrib (entnext ent))
        (setq attribData (entget attrib))
        (if (assoc 2 attribData) #debug : notworking
          (progn
            ;; Get the displayed text value
            (setq textValue (cdr (assoc 1 attribData)))
            
            ;; Remove field code (group code 2)
            (setq attribData (vl-remove (assoc 2 attribData) attribData))
            
            ;; Update the text value to be static
            (setq attribData (subst (cons 1 textValue) 
                                    (assoc 1 attribData) 
                                    attribData))
            
            ;; Apply changes
            (entmod attribData)
            (entupd (cdr (assoc -1 attribData)))
          )
        )
        
        ; (setq ent (ssname ss i))
        ; (setq obj (vlax-ename->vla-object ent))
        ; (setq atts (vlax-invoke obj 'GetAttributes))
        ; (foreach att atts
        ;   (setq att (nth 0 atts))
        ;   (freeze-att-to-text att) #debug : notworking
        ; )

        (setq i (1+ i))
      )
    )
  )
  (princ)
)

(defun freeze-att-to-text (att / val inspt height rot layer color style txtObj)
  (vl-load-com)
  (setq val   (vla-get-TextString att)
        inspt (vla-get-InsertionPoint att)
        height (vla-get-Height att)
        rot   (vla-get-Rotation att)
        layer (vla-get-Layer att)
        color (vla-get-Color att)
        style (vla-get-StyleName att))

  ;; Create TEXT entity in ModelSpace
  (setq txtObj
    (vla-AddText
      (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
      val
      inspt
      height))

  ;; Copy appearance
  (vla-put-Rotation txtObj rot)
  (vla-put-Layer txtObj layer)
  (vla-put-Color txtObj color)
  (vla-put-StyleName txtObj style)

  txtObj
)