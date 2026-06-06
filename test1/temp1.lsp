(setq vec
  (list
    ;; A = 65, 4 نقاط (مجرّد مثال بسيط)
    (cons 65
      (list
        (list 0 0)
        (list 1 0)
        (list 1 1)
        (list 0 1)
      )
    )
    ;; B = 66 (لو حبيت تضيفه)
    (cons 66
      (list
        (list 0 0)
        (list 1 0)
        (list 1 1)
      )
    )
  )
)

(LM:GrText-Debug "A A")


(defun get-endpoints (ename)
  (setq data (entget ename))
  (cond
    ((eq (cdr (assoc 0 data)) "LINE")
      (list (cdr (assoc 10 data)) (cdr (assoc 11 data)))
    )
    ((eq (cdr (assoc 0 data)) "ARC")
      (let* ((obj (vlax-ename->vla-object ename))
             (p1 (vlax-get obj 'StartPoint))
             (p2 (vlax-get obj 'EndPoint))
             (p3 (vlax-get-property obj 'EndPoint))
      )
      (list p1 p2)
    ))
    ((wcmatch (cdr (assoc 0 data)) "*POLYLINE")
       ;; use vlax-curve object
       (let* ((o (vlax-ename->vla-object ename))
              (p1 (vlax-curve-getStartPoint o))
              (p2 (vlax-curve-getEndPoint o)))
         (list p1 p2)
       )
    )
  )
)
