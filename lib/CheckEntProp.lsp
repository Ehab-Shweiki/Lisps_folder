(defun c:CheckEntProp ( / ent obj )
  (vl-load-com)
  (setq ent (car (entsel "\nSelect entity to inspect: ")))
  (if ent
    (progn
      (setq obj (vlax-ename->vla-object ent))
      (prompt (strcat "\nDXF 0: " (cdr (assoc 0 (entget ent)))))
      (prompt (strcat "\nObjectName: " (vla-get-objectname obj)))
    )
  )
  (princ)
)