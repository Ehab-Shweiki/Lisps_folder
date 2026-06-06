(setq textheight 25) ; default text height for labeling points

(defun label-point (pt / offsetPt)
  "Places a small POINT marker and label text near the given 3D point.
   pt — list (x y z)"
  (if (and (listp pt) (= (length pt) 3))
    (progn
      ;; Slight offset for text so it doesn't overlap the point
      (setq offsetPt (mapcar '+ pt '(0.2 0.2 0.0)))

      ;; Create a POINT marker at pt
      (entmake (list '(0 . "POINT") (cons 10 pt)))

      ;; Create a TEXT label near the point
      (entmake
        (list
          '(0 . "TEXT")
          (cons 10 offsetPt)   ; position of text
          (cons 40 textheight)       ; text height
          (cons 1 "Point")     ; text string (fixed)
          (cons 7 "Standard")  ; text style
          (cons 50 0.0)        ; rotation angle
          (cons 62 1)          ; color (1 = red)
        )
      )

      (princ (strcat
        "\n✅ Point labeled at "
        (rtos (car pt) 2 3) ", "
        (rtos (cadr pt) 2 3)
      ))
    )
    (princ "\n⚠️ label-point: invalid input (must be a 3-element list).")
  )
  (princ)
)

(defun c:LabelPoints ( / ss i ent pt txt)
  (vl-load-com)
  (if (setq ss (ssget "_:S" '((0 . "POINT"))))
    (progn
      (repeat (setq i (sslength ss))
        (setq ent (ssname ss (setq i (1- i))))
        (setq pt (cdr (assoc 10 (entget ent))))
        (setq txt (strcat
          (rtos (car pt) 2 3) ", "
          (rtos (cadr pt) 2 3)
          ", " (rtos (caddr pt) 2 3)))
        (entmake
          (list
            '(0 . "TEXT")
            (cons 10 (mapcar '+ pt '(0.2 0.2 0))) ; offset slightly from point
            (cons 40 textheight) ; text height
            (cons 1 txt)
            (cons 7 "Standard") ; text style
            (cons 50 0.0)       ; rotation
          )
        )
      )
      (princ "\n✅ Coordinates labeled as text.")
    )
    (princ "\nNo points selected.")
  )
  (princ)
)

(defun c:PickToLabel ( / pt txt)
  (while (setq pt (getpoint "\nPick point (or Enter to finish): "))
    (setq txt (strcat
      (rtos (car pt) 2 3) ", "
      (rtos (cadr pt) 2 3)
      ", " (rtos (caddr pt) 2 3)))
    (entmake
      (list
        '(0 . "TEXT")
        (cons 10 (mapcar '+ pt '(0.2 0.2 0)))
        (cons 40 textheight)
        (cons 1 txt)
        (cons 7 "Standard")
        (cons 50 0.0)
      )
    )
  )
  (princ)
)


(defun c:ListPts ( / ss i pt )
  (if (setq ss (ssget "_:S" '((0 . "POINT"))))
    (repeat (setq i (sslength ss))
      (setq pt (cdr (assoc 10 (entget (ssname ss (setq i (1- i)))))))
      (princ (strcat "\nPoint: " (rtos (car pt) 2 3) ", "
                     (rtos (cadr pt) 2 3) ", "
                     (rtos (caddr pt) 2 3)))
    )
  )
  (princ)
)

