(defun draw:make (dxfList / ent)
  (if (and (listp dxfList) (= (type (cdr (assoc 0 dxfList))) 'STR))
    (progn
      (setq ent (entmake dxfList))
      (if ent
        ent
        (prompt (strcat "\n⚠️ Failed to create: " (cdr (assoc 0 dxfList))))
      )
    )
    (prompt "\n❌ Invalid DXF list!")
  )
)

(defun draw:Donut (center inner outer / width x y rad)
  "Creates a perfect donut (2-arc polyline, same as AutoCAD DONUT command)."
  (setq width (- outer inner)
        rad     (/ (+ inner outer) 2.0)
        x     (car center)
        y     (cadr center))
  (entmake
    (list
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      '(100 . "AcDbPolyline")
      '(90 . 2)                        ; vertex count
      '(70 . 1)                        ; closed
      (cons 43 width)                  ; constant width
      (cons 10 (list (+ x rad) y))     ; vertex 1
      (cons 42 1.0)                    ; bulge (half-circle)
      (cons 10 (list (- x rad) y))     ; vertex 2
      (cons 42 1.0)                    ; bulge (half-circle)
    )
  )
)


(defun draw:Line (p1 p2)
  (draw:make
    (list '(0 . "LINE")
          (cons 10 p1)
          (cons 11 p2)
    )
  )
)

(defun draw:Circle (center radius)
  (draw:make
    (list '(0 . "CIRCLE")
          (cons 10 center)
          (cons 40 radius)
    )
  )
)

(defun draw:Arc (center radius start end)
  (draw:make
    (list '(0 . "ARC")
          (cons 10 center)
          (cons 40 radius)
          (cons 50 start)
          (cons 51 end)
    )
  )
)

(defun draw:Text (pt height str)
  (draw:make
    (list '(0 . "TEXT")
          (cons 10 pt)
          (cons 40 height)
          (cons 1 str)
    )
  )
)

(defun draw:LWPoly (points closed / n)
  (setq n (length points))
  (draw:make
    (append
      (list '(0 . "LWPOLYLINE")
            (cons 90 n)
            (cons 70 (if closed 1 0))
      )
      (mapcar '(lambda (p) (cons 10 p)) points)
    )
  )
)

(defun draw:SolidHatch (points / dxf-list)
  ;; points = list of 2D points, closed automatically
  ;; Example: '((0 0 0) (100 0 0) (100 100 0) (0 100 0))
  (setq dxf-list
    (append
      (list
        '(0 . "HATCH")                ; entity type
        '(100 . "AcDbEntity")
        '(100 . "AcDbHatch")
        '(10 0.0 0.0 0.0)             ; elevation point
        '(70 . 1)                     ; solid fill
        '(71 . 0)                     ; associative = no
        '(91 . 1)                     ; number of loops
        '(92 . 7)                     ; loop type (7 = polyline)
        (cons 93 (length points))     ; number of vertices
      )
      (mapcar '(lambda (p) (cons 10 p)) points)
      (list
        '(72 . 1)                     ; closed polyline
        '(91 . 0)                     ; no more loops
        '(75 . 0)                     ; solid fill style
        '(76 . 1)                     ; pattern type = predefined
        '(52 . 0.0)                   ; pattern-angle
        '(41 . 1.0)                   ; pattern scale
        '(2 . "SOLID")                ; pattern name
      )
    )
  )
  (entmake dxf-list)
)

(defun draw:PatternHatch (points pattern scale angl / dxf-list)
  (setq dxf-list
    (append
      (list
        '(0 . "HATCH")
        '(100 . "AcDbEntity")
        '(100 . "AcDbHatch")
        '(10 0.0 0.0 0.0)
        '(70 . 0)                     ; pattern (not solid)
        '(71 . 0)
        '(91 . 1)
        '(92 . 7)
        (cons 93 (length points))
      )
      (mapcar '(lambda (p) (cons 10 p)) points)
      (list
        '(72 . 1)
        '(91 . 0)
        '(75 . 0)
        '(76 . 1)
        (cons 52 angl)
        (cons 41 scale)
        (cons 2 pattern)
      )
    )
  )
  (entmake dxf-list)
)

