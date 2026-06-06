(defun c:CheckRev ( / ent obj )
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


(defun c:RevClouds_Real ( / ss i ent obj newSS )
  (vl-load-com)
  (prompt "\nSelecting real Revision Clouds (via ObjectName)...")
  (setq ss (ssget '())) ; كل LWPOLYLINEs

  (if ss
    (progn
      (setq newSS (ssadd) i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))

        ;; تحقق من الاسم الفعلي للكائن
        (if (= (strcase (vla-get-objectname obj)) "ACDBREVCLOUD")
          (setq newSS (ssadd ent newSS)))

        (setq i (1+ i))
      )

      ;; تسليط الضوء
      (sssetfirst nil newSS)
      (prompt (strcat "\nFound " (itoa (sslength newSS)) " Revision Cloud(s)."))
    )
    (prompt "\nNo polylines found.")
  )
  (princ)
)
