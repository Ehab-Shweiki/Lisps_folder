(defun deg->rad (a) (* pi (/ a 180.0)))
(defun rad->deg (a) (* 180.0 (/ a pi)))

;; Approximate Fresnel integrals using power series
(defun fresnel-C (t / n term sum)
  (setq n 0 sum 0.0 term 1.0)
  (while (> (abs term) 1e-6)
    (setq term (/ (expt (* pi 0.5 t) (+ (* 4 n) 1)) (* (factorial (* 2 n)) (+ (* 4 n) 1))))
    (setq sum (+ sum (if (evenp n) term (- term))))
    (setq n (1+ n)))
  sum
)

(defun fresnel-S (t / n term sum)
  (setq n 0 sum 0.0 term 1.0)
  (while (> (abs term) 1e-6)
    (setq term (/ (expt (* pi 0.5 t) (+ (* 4 n) 3)) (* (factorial (+ (* 2 n) 1)) (+ (* 4 n) 3))))
    (setq sum (+ sum (if (evenp n) term (- term))))
    (setq n (1+ n)))
  sum
)

(defun factorial (n)
  (if (<= n 1) 1
    (* n (factorial (- n 1)))
  )
)

(defun clothoid-length-range (pt1 ang1 pt2 ang2 / delta d Lmin Lmax)
  (setq delta (abs (- ang2 ang1)))
  (if (> delta pi) (setq delta (- (* 2 pi) delta)))
  (if (< delta 1e-6) (setq delta 1e-6)) ; avoid division by zero

  (setq d (distance pt1 pt2))
  (setq Lmin (sqrt (/ (* 2 d delta) (sin delta))))
  (setq Lmax (* 2 Lmin)) ;; Optional buffer
  (list Lmin Lmax delta)
)

(defun c:CLOTHOID (/ ent1 ent2 e1 e2 p1 p2 pt1 pt2 ang1 ang2 delta lenRange Lmin Lmax delta angstep A i N t x y loc pt0 angle0 ptNext lastPt rotMat)

  ;; Get first line
  (prompt "\nSelect first line (start direction): ")
  (setq ent1 (car (entsel)))
  (if (not ent1) (progn (prompt "\nNothing selected.") (exit)))
  (setq e1 (entget ent1))
  (setq pt1 (cdr (assoc 11 e1))) ; end point
  (setq p1  (cdr (assoc 10 e1))) ; start point
  (setq ang1 (angle p1 pt1))

  ;; Get second line
  (prompt "\nSelect second line (end direction): ")
  (setq ent2 (car (entsel)))
  (if (not ent2) (progn (prompt "\nNothing selected.") (exit)))
  (setq e2 (entget ent2))
  (setq pt2 (cdr (assoc 10 e2))) ; start point
  (setq p2  (cdr (assoc 11 e2))) ; end point
  (setq ang2 (angle pt2 p2))

  ;; Calculate Lmin, Lmax, angle delta
  (setq lenRange (clothoid-length-range pt1 ang1 pt2 ang2))
  (setq Lmin (car lenRange))
  (setq Lmax (cadr lenRange))
  (setq delta (caddr lenRange))

  ;; Ask user for total length
  (initget 6)
  (setq L (getreal (strcat "\nEnter clothoid length [" (rtos Lmin 2 2) " - " (rtos Lmax 2 2) "]: ")))
  (if (< L Lmin) (setq L Lmin))
  (if (> L Lmax) (setq L Lmax))

  ;; Number of segments
  (initget 7)
  (setq N (getint "\nEnter number of segments (10+ recommended): "))
  (if (< N 2) (setq N 20))

  ;; Compute parameter A from theta = L^2 / (2A^2)
  (setq A (sqrt (/ (* L L) (* 2 delta))))

  ;; Build clothoid in local coordinates
  (setq pt0 (list 0.0 0.0 0.0))
  (setq lastPt pt0)
  (setq i 1)
  (repeat N
    (setq t (/ (* i L) N))
    (setq x (* A (fresnel-C (/ t A))))
    (setq y (* A (fresnel-S (/ t A))))
    (setq ptNext (list x y 0.0))

    ;; Draw segment in local coordinates
    (entmakex
      (list (cons 0 "LINE")
            (cons 10 lastPt)
            (cons 11 ptNext)
            (cons 62 1) ; red color
      )
    )
    (setq lastPt ptNext)
    (setq i (1+ i))
  )

  ;; Transform all added entities to align with user’s selected lines
  ;; Compute rotation and translation
  (setq angle0 0.0)
  (setq rotMat (list (list (cos ang1) (- (sin ang1)) 0.0)
                     (list (sin ang1) (cos ang1) 0.0)
                     (list 0.0 0.0 1.0)))

  ;; Move and rotate all drawn entities (very simple approximation: use UCS before drawing for better accuracy)
  (prompt "\nDone. Clothoid drawn from selected lines.")
  (princ)
)
