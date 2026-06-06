(defun c:TestRevClouds ( / ss i ent dxf addIt newSS )
  (prompt "\nSelecting Revision Clouds only...")
  (setq ss (ssget '((0 . "LWPOLYLINE")))) ; كل البوليلانات الخفيفة

  (if ss
    (progn
      (setq newSS (ssadd) i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i)
              dxf (entget ent)
              addIt T)

        ;; نعتبر أنه RevCloud فقط إذا كان مغلق (70=1) وعدد النقاط كثير (90>=10)
        (if (or (/= (cdr (assoc 70 dxf)) 1)
                (< (cdr (assoc 90 dxf)) 10))
          (setq addIt nil))

        (if addIt
          (setq newSS (ssadd ent newSS)))

        (setq i (1+ i))
      )
      ;; تسليط الضوء على النتيجة
      (sssetfirst nil newSS)
      (prompt (strcat "\nFound " (itoa (sslength newSS)) " Revision Cloud(s)."))
    )
    (prompt "\nNo polylines found.")
  )
  (princ)
)
