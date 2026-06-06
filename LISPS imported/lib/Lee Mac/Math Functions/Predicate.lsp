;; Collinear-p  -  Lee Mac
;; Returns T if p1,p2,p3 are collinear

(defun LM:Collinear-p ( p1 p2 p3 )
    (
        (lambda ( a b c )
            (or
                (equal (+ a b) c 1e-8)
                (equal (+ b c) a 1e-8)
                (equal (+ c a) b 1e-8)
            )
        )
        (distance p1 p2) (distance p2 p3) (distance p1 p3)
    )
)

;; List Collinear-p  -  Lee Mac
;; Returns T if all points in a list are collinear

(defun LM:ListCollinear-p ( lst )
    (or (null (cddr lst))
        (and
            (equal 1.0
                (abs
                    (vxv
                        (vx1 (mapcar '- (car lst) (cadr  lst)))
                        (vx1 (mapcar '- (car lst) (caddr lst)))
                    )
                )
                1e-8
            )
            (LM:ListCollinear-p (cdr lst))
        )
    )
)

;; Coplanar-p  -  Lee Mac
;; Returns T if points p1,p2,p3,p4 are coplanar

(defun LM:Coplanar-p ( p1 p2 p3 p4 )
    (
        (lambda ( n )
            (equal
                (last (trans p3 0 n))
                (last (trans p4 0 n))
                1e-8
            )
        )
        (v^v (mapcar '- p1 p2) (mapcar '- p1 p3))
    )
)

;; List Coplanar-p  -  Lee Mac
;; Returns T if all points in a list are coplanar

(defun LM:ListCoplanar-p ( lst )
    (or (null (cdddr lst))
        (and
            (
                (lambda ( n )
                    (equal
                        (last (trans (caddr  lst) 0 n))
                        (last (trans (cadddr lst) 0 n))
                        1e-8
                    )
                )
                (v^v (mapcar '- (car lst) (cadr lst)) (mapcar '- (car lst) (caddr lst)))
            )
            (LM:ListCoplanar-p (cdr lst))
        )
    )
)

;; Perpendicular-p  -  Lee Mac
;; Returns T if vectors v1,v2 are perpendicular

(defun LM:Perpendicular-p ( v1 v2 )
    (equal 0.0 (vxv v1 v2) 1e-8)
)

;; Clockwise-p - Lee Mac
;; Returns T if p1,p2,p3 are clockwise oriented

(defun LM:Clockwise-p ( p1 p2 p3 )
    (<
        (* (- (car  p2) (car  p1)) (- (cadr p3) (cadr p1)))
        (* (- (cadr p2) (cadr p1)) (- (car  p3) (car  p1)))
    )
)

;; List Clockwise-p - Lee Mac
;; Returns T if the point list is clockwise oriented

(defun LM:ListClockwise-p ( lst )
    (minusp
        (apply '+
            (mapcar
                (function
                    (lambda ( a b )
                        (- (* (car b) (cadr a)) (* (car a) (cadr b)))
                    )
                )
                lst (cons (last lst) lst)
            )
        )
    )
)

;; InsideTriangle-p  -  Lee Mac
;; Returns T if pt lies inside the triangle formed by p1,p2,p3
;; If ie is T, triangle edges are included, else edges are excluded

(defun LM:InsideTriangle-p ( pt p1 p2 p3 ie / c1 c2 c3 z1 z2 z3 )
    (cond
        (   (or (equal pt p1 1e-8)
                (equal pt p2 1e-8)
                (equal pt p3 1e-8)
            )
            ie
        )
        (   (progn
                (setq c1 (v2^v2 (mapcar '- p2 p1) (mapcar '- pt p1))
                      c2 (v2^v2 (mapcar '- p3 p2) (mapcar '- pt p2))
                      c3 (v2^v2 (mapcar '- p1 p3) (mapcar '- pt p3))
                )
                (setq z1 (equal c1 0.0 1e-8)
                      z2 (equal c2 0.0 1e-8)
                      z3 (equal c3 0.0 1e-8)
                )
                (and (not ie) (or z1 z2 z3))
            )
            nil
        )
        (   (or (and (or z1 (< 0.0 c1)) (or z2 (< 0.0 c2)) (or z3 (< 0.0 c3)))
                (and (or z1 (< c1 0.0)) (or z2 (< c2 0.0)) (or z3 (< c3 0.0)))
            )
        )
    )
)