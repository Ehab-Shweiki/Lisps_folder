(defun c:SetLayersColor ( / ss i ent lay tblRec colorStr colorVal)
  (vl-load-com)

  ;; Prompt user for color number with default = 9
  (initget 6) ; Only allow positive integers (excluding 0)
  (setq colorStr (getint "\nEnter color number to apply to layers [Default = 9]: "))
  (if (not colorStr) (setq colorStr 9)) ; Default to 9 if Enter pressed
  (setq colorVal colorStr)

  ;; Select entities
  (setq ss (ssget))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq lay (cdr (assoc 8 (entget ent)))) ; Get layer name

        ;; Get the layer table record
        (setq tblRec (tblobjname "LAYER" lay))
        (if tblRec
          (vla-put-Color
            (vlax-ename->vla-object tblRec)
            colorVal
          )
        )

        (setq i (1+ i))
      )
      (princ (strcat "\nLayer colors updated to color " (itoa colorVal) "."))
    )
    (princ "\nNo entities selected.")
  )
  (princ)
)
