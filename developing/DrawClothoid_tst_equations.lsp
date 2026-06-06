(defun clothoid-integrals (s steps / i t dt sumC sumS)
  (setq dt (/ s steps)
        sumC 0.0
        sumS 0.0
        i 0
  )
  (while (<= i steps)
    (setq t (* i dt))
    (setq w (* pi 0.5 t t))
    ;; Simpson’s Rule Coefficients
    (setq coeff (cond
                  ((or (= i 0) (= i steps)) 1)
                  ((= (rem i 2) 0) 2)
                  (T 4)
                )
    )
    (setq sumC (+ sumC (* coeff (cos w))))
    (setq sumS (+ sumS (* coeff (sin w))))
    (setq i (1+ i))
  )
  (list (* (/ dt 3.0) sumC) (* (/ dt 3.0) sumS))
)

(defun generate-clothoid (L A steps / pts s p x y)
  (setq pts '() s 0)
  (repeat (1+ steps)
    (setq s (/ (* L s) steps)) ; arc length at step
    (setq p (clothoid-integrals (/ s A) 100))
    (setq x (* A (car p))
          y (* A (cadr p))
    )
    (setq pts (cons (list x y 0.0) pts))
    (setq s (1+ s))
  )
  (reverse pts)
)

(defun rotate-translate (pts base ang / m tx ty rot-pts)
  (setq rot-pts '())
  (foreach pt pts
    (setq tx (+ (* (car pt) (cos ang)) (* (cadr pt) (- (sin ang)))))
    (setq ty (+ (* (car pt) (sin ang)) (* (cadr pt) (cos ang))))
    (setq rot-pts (cons (list (+ (car base) tx) (+ (cadr base) ty) 0.0) rot-pts))
  )
  (reverse rot-pts)
)

(defun draw-polyline (pts / pline)
  (if pts
    (progn
      (entmakex (append (list '(0 . "LWPOLYLINE")
                              '(100 . "AcDbEntity")
                              '(100 . "AcDbPolyline")
                              (cons 90 (length pts))
                              '(70 . 0))
                        (mapcar '(lambda (pt) (cons 10 pt)) pts)))
    )
  )
)

(defun c:CLOTHOID ( / pt1 pt2 ang1 ang2 A L steps dir pts vec startAng rot)
  (setq pt1 (getpoint "\nStart point: "))
  (setq ang1 (getangle pt1 "\nStart tangent angle: "))
  (setq pt2 (getpoint "\nEnd point: "))
  (setq ang2 (getangle pt2 "\nEnd tangent angle: "))
  (setq L (getreal "\nTotal clothoid length: "))
  (setq steps (getint "\nNumber of segments (e.g. 50): "))
  (setq A (/ L (sqrt pi))) ; Approximate scale factor

  ;; Generate ideal clothoid from (0,0) heading 0
  (setq pts (generate-clothoid L A steps))

  ;; Rotate to match start tangent
  (setq pts (rotate-translate pts pt1 ang1))

  ;; Optional: Align to end tangent (not applied here, as it requires nonlinear solve)

  ;; Draw result
  (draw-polyline pts)
  (princ "\nClothoid drawn.")
  (princ)
)
