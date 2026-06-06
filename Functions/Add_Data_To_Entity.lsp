(regapp "MYDATA") ; register app name once

(defun c:AddData (/ e)
  (setq e (car (entsel "\nSelect entity: ")))
  (entmod
    (append
      (entget e)
      (list
        (list -3
          (list "MYDATA"
                (cons 1000 "WindowEdge")  ; string
                (cons 1040 2.75)          ; real
                (cons 1070 3)             ; integer
          )
        )
      )
    )
  )
  (princ "\n✅ XData attached.")
)
