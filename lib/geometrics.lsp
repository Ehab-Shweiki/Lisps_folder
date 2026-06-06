;; ---------------- VECTOR HELPERS ----------------
(defun v+ (a b) (mapcar '+ a b))
(defun v- (a b) (mapcar '- a b))
(defun v* (a s) (mapcar '(lambda (x) (* x s)) a))
(defun vdot (a b) (+ (* (car a) (car b)) (* (cadr a) (cadr b))))
(defun vlen (v) (distance '(0 0 0) v))
(defun vunit (v) (if (zerop (vlen v)) '(0 0 0) (v* v (/ 1.0 (vlen v)))))
(defun vperpL (v) (list (- (cadr v)) (car v) 0.0)) ;; left normal (perpendicular to v in 2D space)
(defun vperpR (v) (list (cadr v) (- (car v)) 0.0)) ;; right normal (perpendicular to v in 2D space)
(defun 2d (pt) (list (car pt) (cadr pt) 0.0))

;; ---------------- GEOMETRY ----------------
(defun line-intersection-v (pt1 dir1 pt2 dir2 / det t1)
  "Return intersection point of two infinite lines defined by (p1,d1) and (p2,d2).
   Returns NIL if lines are parallel or nearly parallel.
   Uses 2D vector cross product."
  ; L1​:P1​+tD1​ , L2​:P2​+sD2​
  ; P1​+tD1​ = P2​+sD2​
  ; t = ((P2​−P1​)×D2​)/(D1​×D2​)
  (setq det (v× dir1 dir2))
  (if (equal det 0.0 1e-12)
    nil ; lines are parallel
    (progn
      (setq t1 (/ (v× (v- pt2 pt1) dir2) det)) ; t parameter along line 1
      (v+ pt1 (v* dir1 t1))  ; the intersection point
    )
  )
)

(defun line-intersection (pt1 dir1 pt2 dir2 / a b c d dx dy det t1)
  "Return intersection point of two infinite lines defined by (pt1,dir1) and (pt2,dir2).
   Returns NIL if lines are parallel or nearly parallel."
  ; L1​:P1​+tD1​ , L2​:P2​+sD2​
  ; P1​+tD1​ = P2​+sD2​
  ; => t = (x2​−x1​)d​−(y2​−y1​)c / (ad-bc)
  (setq a (car dir1) b (cadr dir1)
        c (car dir2) d (cadr dir2)
        dx (- (car pt2) (car pt1))
        dy (- (cadr pt2) (cadr pt1))
        det (- (* a d) (* b c)))
  (if (equal det 0.0 1e-12)
    nil ; lines are parallel
    (progn
      (setq t1 (/ (- (* dx d) (* dy c)) det)) ; t parameter along line 1
      (v+ pt1 (v* dir1 t1)) ; the intersection point
    )
  )
)

(defun line-intersection-ray (pt1 dir1 pt2 dir2 / a b c d dx dy det t1 s)
  "Return intersection point of two rays (pt1→dir1, pt2→dir2).
   Returns NIL if rays are parallel or intersect behind either point."

  (setq a (car dir1) b (cadr dir1)
        c (car dir2) d (cadr dir2)
        dx (- (car pt2) (car pt1))
        dy (- (cadr pt2) (cadr pt1))
        det (- (* a d) (* b c))
  )

  (if (equal det 0.0 1e-12)
    nil ; lines are parallel
    (progn
      ;; solve the linear system for t, s (the parameters along each ray)
      (setq
        t1 (/ (- (* dx d) (* dy c)) det)
        s (/ (- (* dx b) (* dy a)) det)
      )

      ;; accept intersection only if it's forward on both rays
      (if (and (> t1 0.0) (> s 0.0))
        (v+ pt1 (v* dir1 t1))
        nil
      )
      
    )
  )
)

(defun curve-near+tan (vlaObj pt / closest param deriv dir)  ; debug. check compared with pline-segment-refpt+tan
  "Return nearest point and unit tangent on VLA curve to raw point PT."
  (setq closest (vlax-curve-getClosestPointTo vlaObj (2d pt))
        param   (vlax-curve-getParamAtPoint vlaObj closest)
        deriv   (vlax-curve-getFirstDeriv vlaObj param)
        dir     (vunit (list (car deriv) (cadr deriv) 0.0)))
  (list closest dir)
)

(defun is-tangent-cw? (cen pt1 tan1 / perp dot)
  "Return T if tangent at pt1 is clockwise (i.e. opposite to CCW direction)."
  (setq perp (vperpL (vunit (v- (2d pt1) (2d cen))))
        dot  (vdot (vunit perp) (vunit tan1)))
  (if (> dot 0.0) nil T))