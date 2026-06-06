(defun c:DynamicRectangle ( / pt1 pt2 input x1 y1 x2 y2 p1 p2 p3 p4)
  (setq pt1 (getpoint "\nPick first corner: "))
  (while T
    (setq input (grread T 15 0)) ; Wait for mouse move or click
    (cond
      ((= (car input) 5) ; Mouse moved
        (setq pt2 (cadr input))
        (redraw)
        ;; Rectangle corners
        (setq x1 (car pt1) y1 (cadr pt1)
              x2 (car pt2) y2 (cadr pt2)
              p1 (list x1 y1)
              p2 (list x2 y1)
              p3 (list x2 y2)
              p4 (list x1 y2))
        ;; Draw lines between corners
        (grdraw p1 p2 3 1)
        (grdraw p2 p3 3 1)
        (grdraw p3 p4 3 1)
        (grdraw p4 p1 3 1)
      )
      ((= (car input) 3) ; Left-click to finalize
        (command "_.PLINE" p1 p2 p3 p4 "C")
        (redraw)
        (princ "\nRectangle created.")
      )
    )
  )
)

