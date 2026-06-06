;; Midpoint  -  Lee Mac
;; Returns the midpoint of two points

(defun mid ( a b )
    (mapcar (function (lambda ( a b ) (/ (+ a b) 2.0))) a b)
)

;; Polygon Centroid  -  Lee Mac
;; Returns the WCS Centroid of an LWPolyline Polygon Entity

(defun LM:PolyCentroid ( e / l )
    (foreach x (setq e (entget e))
        (if (= 10 (car x)) (setq l (cons (cdr x) l)))
    )
    (
        (lambda ( a )
            (if (not (equal 0.0 a 1e-8))
                (trans
                    (mapcar '/
                        (apply 'mapcar
                            (cons '+
                                (mapcar
                                    (function
                                        (lambda ( a b )
                                            (
                                                (lambda ( m )
                                                    (mapcar
                                                        (function
                                                            (lambda ( c d ) (* (+ c d) m))
                                                        )
                                                        a b
                                                    )
                                                )
                                                (- (* (car a) (cadr b)) (* (car b) (cadr a)))
                                            )
                                        )
                                    )
                                    l (cons (last l) l)
                                )
                            )
                        )
                        (list a a)
                    )
                    (cdr (assoc 210 e)) 0
                )
            )
        )
        (* 3.0
            (apply '+
                (mapcar
                    (function
                        (lambda ( a b )
                            (- (* (car a) (cadr b)) (* (car b) (cadr a)))
                        )
                    )
                    l (cons (last l) l)
                )
            )
        )
    )
)

;; 3-Point Circle  -  Lee Mac
;; Returns the center and radius of the circle defined by three supplied points.

(defun LM:3PCircle ( p1 p2 p3 / cn m1 m2 )
    (setq m1 (mid p1 p2)
          m2 (mid p2 p3)
    )
    (if
        (setq cn
            (inters
                m1 (polar m1 (+ (angle p1 p2) (/ pi 2.)) 1.0)
                m2 (polar m2 (+ (angle p2 p3) (/ pi 2.)) 1.0)
                nil
            )
        )
        (list cn (distance cn p1))
    )
)

;; 3-Point Circle (Cartesian)  -  Lee Mac
;; Returns the center and radius of the circle defined by the supplied three points.

(defun LM:3PCircle ( p1 p2 p3 / a b c d )
    (setq p2 (mapcar '- p2 p1)
          p3 (mapcar '- p3 p1)
          a  (* 2.0 (- (* (car p2) (cadr p3)) (* (cadr p2) (car p3))))
          b  (distance '(0.0 0.0) p2)
          c  (distance '(0.0 0.0) p3)
          b  (* b b)
          c  (* c c)
    )
    (if (not (equal 0.0 a 1e-8))
        (list
            (setq d
                (mapcar '+ p1
                    (list
                        (/ (- (* (cadr p3) b) (* (cadr p2) c)) a)
                        (/ (- (* (car  p2) c) (* (car  p3) b)) a)
                        0.0
                    )
                )
            )
            (distance d p1)
        )
    )
)

;; 3-Point Arc  -  Lee Mac
;; Returns the center, start/end angle and radius of the arc defined by three supplied points.

(defun LM:3PArc ( p1 p2 p3 / cn m1 m2 )
    (setq m1 (mid p1 p2)
          m2 (mid p2 p3)
    )
    (if
        (setq cn
            (inters
                m1 (polar m1 (+ (angle p1 p2) (/ pi 2.)) 1.0)
                m2 (polar m2 (+ (angle p2 p3) (/ pi 2.)) 1.0)
                nil
            )
        )
        (append (list cn)
            (if (LM:Clockwise-p p1 p2 p3)
                (list (angle cn p3) (angle cn p1))
                (list (angle cn p1) (angle cn p3))
            )
            (list (distance cn p1))
        )
    )
)

;; 2-Circle Tangents  -  Lee Mac
;; Returns the two groups of points for which a line from a point in
;; each group is tangent to both circles with centres c1,c2 and radii r1,r2

(defun LM:2CircleTangents ( c1 r1 c2 r2 / d1 d2 a1 a2 )
    (if (< (abs (setq d1 (- r1 r2))) (setq d2 (distance c1 c2)))
        (progn
            (setq a1 (atan (sqrt (- (* d2 d2) (* d1 d1))) d1)
                  a2 (angle c1 c2)
            )
            (list
                (list (polar c1 (+ a2 a1) r1) (polar c1 (- a2 a1) r1))
                (list (polar c2 (+ a2 a1) r2) (polar c2 (- a2 a1) r2))
            )
        )
    )
)

;; Point-Circle Tangents  -  Lee Mac
;; Returns the two points for which a line from 'pt' to each point returned
;; is tangent to the circle with centre c1 and radius r1

(defun LM:PointCircleTangents ( pt c1 r1 / a1 a2 d1 )
    (if (< r1 (setq a1 (angle c1 pt) d1 (distance pt c1)))
        (progn
            (setq a2 (atan (sqrt (- (* d1 d1) (* r1 r1))) r1))
            (list
                (polar c1 (+ a1 a2) r1)
                (polar c1 (- a1 a2) r1)
            )
        )
    )
)