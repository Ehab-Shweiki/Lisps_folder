;; Line-Plane Intersection  -  Lee Mac
;; Returns the point of intersection of a line defined by
;; points p1,p2 and a plane defined by its origin and normal

(defun LM:inters-line-plane ( p1 p2 org nm )
    (setq org (trans org 0 nm)
          p1  (trans p1  0 nm)
          p2  (trans p2  0 nm)
    )
    (trans
        (inters p1 p2
            (list (car p1) (cadr p1) (caddr org))
            (list (car p2) (cadr p2) (caddr org))
            nil
        )
        nm 0
    )
)

;; Line-Circle Intersection  -  Lee Mac
;; Returns the point(s) of intersection between an infinite line defined by
;; points p,q and circle with centre c and radius r

(defun LM:inters-line-circle ( p q c r / a d n s )
    (setq n (mapcar '- q p)
          p (trans p 0 n)
          c (trans c 0 n)
          a (list (car p) (cadr p) (caddr c))
    )
    (cond
        (   (equal r (setq d (distance c a)))
            (list (trans a n 0))
        )
        (   (< d r)
            (setq s (sqrt (- (* r r) (* d d))))
            (list
                (trans (list (car p) (cadr p) (- (caddr c) s)) n 0)
                (trans (list (car p) (cadr p) (+ (caddr c) s)) n 0)
            )
        )
    )
)

;; Line-Circle Intersection (vector version)  -  Lee Mac
;; Returns the point(s) of intersection between an infinite line defined by
;; points p,q and circle with centre c and radius r

(defun LM:inters-line-circle ( p q c r / v s )
    (setq v (mapcar '- q p)
          s (mapcar '- p c)
    )
    (mapcar '(lambda ( s ) (mapcar '+ p (vxs v s)))
        (quad (vxv v v) (* 2 (vxv v s)) (- (vxv s s) (* r r)))
    )
)

;; 2-Circle Intersection  -  Lee Mac
;; Returns the point(s) of intersection between two circles
;; with centres c1,c2 and radii r1,r2

(defun LM:inters-circle-circle ( c1 r1 c2 r2 / a d m l x y )
    (if (and (<= (setq d (distance c1 c2)) (+ r1 r2))
             (<= (abs (- r1 r2)) d)
        )
        (progn
            (if (equal r1 (setq x (/ (- (+ (* r1 r1) (* d d)) (* r2 r2)) (+ d d))) 1e-8)
                (setq  l  (list (list x 0.0 0.0)))
                (setq  y  (sqrt (- (* r1 r1) (* x x)))
                       l  (list (list x y 0.0) (list x (- y) 0.0))
                )
            )
            (setq a (angle c1 c2)
                  m (list (list (cos a) (- (sin a)) 0) (list (sin a) (cos a) 0) '(0 0 1))
            )
            (mapcar '(lambda ( v ) (mapcar '+ c1 (mxv m v))) l)
        )
    )
)

;; 2-Circle Intersection (trans version)  -  Lee Mac
;; Returns the point(s) of intersection between two circles
;; with centres c1,c2 and radii r1,r2

(defun LM:inters-circle-circle ( c1 r1 c2 r2 / n d1 x z )
    (if
        (and
            (< (setq d1 (distance c1 c2)) (+ r1 r2))
            (< (abs (- r1 r2)) d1)
        )
        (progn
            (setq n  (mapcar '- c2 c1)
                  c1 (trans c1 0 n)
                  z  (/ (- (+ (* r1 r1) (* d1 d1)) (* r2 r2)) (+ d1 d1))
            )
            (if (equal z r1 1e-8)
                (list (trans (list (car c1) (cadr c1) (+ (caddr c1) z)) n 0))
                (progn
                    (setq x (sqrt (- (* r1 r1) (* z z))))
                    (list
                        (trans (list (- (car c1) x) (cadr c1) (+ (caddr c1) z)) n 0)
                        (trans (list (+ (car c1) x) (cadr c1) (+ (caddr c1) z)) n 0)
                    )
                )
            )
        )
    )
)

;; 2-Arc Intersection  -  Lee Mac
;; Returns the point(s) of intersection between two arcs
;; with centres c1,c2 radii r1,r2, start angles s1,s2 & end angles e1,e2

(defun LM:inters-arc-arc ( c1 r1 s1 e1 c2 r2 s2 e2 )
    (cond
        (   (< e1 s1) (LM:inters-arc-arc c1 r1 s1 (+ e1 pi pi) c2 r2 s2 e2))
        (   (< e2 s2) (LM:inters-arc-arc c1 r1 s1 e1 c2 r2 s2 (+ e2 pi pi)))
        (   (vl-remove-if-not
               '(lambda ( pt ) (and (<= s1 (angle c1 pt) e1) (<= s2 (angle c2 pt) e2)))
                (LM:inters-circle-circle c1 r1 c2 r2)
            )
        )
    )
)