;; Quadratic Solution  -  Lee Mac
;; Args: a,b,c - coefficients of ax^2 + bx + c = 0
 
(defun quad ( a b c / d r )
    (cond
        (   (equal 0.0 (setq d (- (* b b) (* 4.0 a c))) 1e-8)
            (list (/ b (* -2.0 a)))
        )
        (   (< 0 d)
            (setq r (sqrt d))
            (list (/ (- r b) (* 2.0 a)) (/ (- (- b) r) (* 2.0 a)))
        )
    )
)

;; Least Common Multiple  -  Lee Mac
;; Args: a,b - positive non-zero integers

(defun lcm ( a b ) (* b (/ a (gcd a b))))

;; Least Common Multiple of List  -  Lee Mac
;; Args: l - list of positive non-zero integers

(defun lcml ( l )
   (if (cddr l)
       (lcm (car l) (lcml (cdr l)))
       (apply 'lcm l)
   )
)

;; Prime Factors  -  Lee Mac
;; Args: n - positive non-zero integer

(defun pf ( n / m p r )
    (setq p 2)
    (while (< 1 n)
        (while (zerop (rem n p))
            (setq r (cons p r)
                  n (/ n p)
            )
        )
        (if (< 1 (setq m (sqrt n)) (setq p (if (= p 2) 3 (+ 2 p))))
            (setq r (cons n r)
                  n 0
            )
            (while (and (<= p m) (< 0 (rem n p)))
                (setq p (+ 2 p))
            )
        )
    )
    (reverse r)
)

;; Prime-p  -  Lee Mac
;; Args: n - positive non-zero integer

(defun prime-p ( n / m p )
    (or (= 2 n)
        (and
            (< 2 n)
            (= 1 (rem n 2))
            (progn
                (setq m (1+ (sqrt n))
                      p 3
                )
                (while (and (< p m) (< 0 (rem n p)))
                    (setq p (+ 2 p))
                )
                (< m p)
            )
        )
    )
)