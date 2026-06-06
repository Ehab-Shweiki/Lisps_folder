"Method 1"

(defun dist-pt-line (pt p0 dir / v len cross)
  ;; dir is direction of line (not necessarily unit)
  (setq v   (v- pt p0)
        len (vlength dir))
  (if (zerop len)
    0.0
    (progn
      ;; 2D cross product magnitude / |dir|
      (setq cross (- (* (car v) (cadr dir))
                     (* (cadr v) (car dir))))
      (/ (abs cross) len)
    )
  )
)

(setq Q   (line-intersection P1 T1 P2 T2)) ; intersection of the two tangents
(setq u1  (vunit T1)
      u2  (vunit T2))
(setq b   (vunit (v+ u1 u2))) ; one angle bisector direction

(defun C-of (t) (v+ Q (v* b t))) ; center as function of t


(defun f (t / C dLine dMid)
  (setq C     (C-of t)
        dLine (dist-pt-line C P1 T1)
        dMid  (distance C M))
  (- dMid dLine)   ; want this = 0
)

(defun solve-on-bisector (t1 t2 / i mid f1 f2 fm)
  (setq f1 (f t1)
        f2 (f t2))
  (if (>= (* f1 f2) 0.0)
    nil ; no sign change ⇒ no guaranteed root in [t1,t2]
    (progn
      (repeat 40 ; iterations
        (setq mid (/ (+ t1 t2) 2.0)
              fm  (f mid))
        (if (< (* f1 fm) 0.0)
          (setq t2 mid f2 fm)
          (setq t1 mid f1 fm)
        )
      )
      (/ (+ t1 t2) 2.0) ; approx t root
    )
  )
)

(setq dirQM (v- M Q))
(setq sign  (if (> (vdot dirQM b) 0.0) 1.0 -1.0))
(setq t-root (solve-on-bisector 0.0 (* sign 10000.0))) ; 10km in that direction :)

(setq C (C-of t-root)
      R (distance C M))

(defun proj-on-line (pt p0 dir / u t)
  (setq u (vunit dir))
  (setq t (vdot (v- pt p0) u))          ; scalar along the line
  (v+ p0 (v* u t))                      ; projected point
)

(setq E1 (proj-on-line C P1 T1))
(setq E2 (proj-on-line C P2 T2))

(command "ARC" "C" C E1 E2)
;; or with start, mid, end as needed once you pick direction



