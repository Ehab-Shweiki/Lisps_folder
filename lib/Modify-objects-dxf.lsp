; TODO: Develop and check

(defun set-props-dxf (ent / dxf)
  (setq dxf (entget ent))
  ;; change layer and color
  (setq dxf (subst (cons 8 "0") (assoc 8 dxf) dxf))    ; Layer → "0"
  (setq dxf (subst (cons 62 256) (assoc 62 dxf) dxf))  ; Color → ByLayer
  (setq dxf (subst (cons 6 "BYLAYER") (assoc 6 dxf) dxf)) ; Linetype
  (entmod dxf)
  (entupd ent)
)

(defun reset:ByLayer-DXF (/ ss i e dxf)
  (if (setq ss (ssget "_X"))
    (progn
      (repeat (setq i (sslength ss))
        (setq e (ssname ss (setq i (1- i))))
        (setq dxf (entget e))
        ;; skip non-graphical objects
        (if (assoc 8 dxf)
          (progn
            (setq dxf (subst (cons 8 "0") (assoc 8 dxf) dxf))
            (setq dxf (subst (cons 62 256) (assoc 62 dxf) dxf))
            (setq dxf (subst (cons 6 "BYLAYER") (assoc 6 dxf) dxf))
            (entmod dxf)
          )
        )
      )
      (princ "\n✅ All objects set to ByLayer using DXF.")
    )
  )
  (princ)
)
