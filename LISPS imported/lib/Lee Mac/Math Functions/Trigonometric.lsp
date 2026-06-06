;; Tangent  -  Lee Mac
;; Args: x - real

(defun tan ( x )
    (if (not (equal 0.0 (cos x) 1e-10))
        (/ (sin x) (cos x))
    )
)

;; ArcSine  -  Lee Mac
;; Args: -1 <= x <= 1

(defun asin ( x )
    (if (<= -1.0 x 1.0)
        (atan x (sqrt (- 1.0 (* x x))))
    )
)

;; ArcCosine  -  Lee Mac
;; Args: -1 <= x <= 1

(defun acos ( x )
    (if (<= -1.0 x 1.0)
        (atan (sqrt (- 1.0 (* x x))) x)
    )
)

;; Hyperbolic Sine  -  Lee Mac
;; Args: x - real

(defun sinh ( x )
    (/ (- (exp x) (exp (- x))) 2.0)
)

;; Hyperbolic Cosine  -  Lee Mac
;; Args: x - real

(defun cosh ( x )
    (/ (+ (exp x) (exp (- x))) 2.0)
)

;; Hyperbolic Tangent  -  Lee Mac
;; Args: x - real

(defun tanh ( x )
    (/ (sinh x) (cosh x))
)

;; Area Hyperbolic Sine  -  Lee Mac
;; Args: x - real

(defun asinh ( x )
    (log (+ x (sqrt (1+ (* x x)))))
)

;; Area Hyperbolic Cosine  -  Lee Mac
;; Args: 1 <= x

(defun acosh ( x )
    (if (<= 1.0 x)
        (log (+ x (sqrt (1- (* x x)))))
    )
)

;; Area Hyperbolic Tangent  -  Lee Mac
;; Args: -1 < x < 1

(defun atanh ( x )
    (if (< (abs x) 1.0)
        (/ (log (/ (1+ x) (- 1.0 x))) 2.0)
    )
)