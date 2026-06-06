(progn
  (setq ent (nentsel "\nSelect block with attribute: "))
  (if ent
    (progn
      (setq pickpt (cadr ent)) ; point clicked
      (setq ent (car ent))     ; entity name
      (setq obj (vlax-ename->vla-object ent))
      
      ;; get obj name
      (vla-get-objectname obj)
	)
  )
)