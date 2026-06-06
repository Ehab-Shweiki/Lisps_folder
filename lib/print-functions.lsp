; print point
(princ (strcat "\n(pt1): " 
               (rtos (car pt1) 2 3)
               ", "
               (rtos (cadr pt1) 2 3)))

; or
(princ (strcat "\n(pt1): "
               (vl-princ-to-string start)))
                