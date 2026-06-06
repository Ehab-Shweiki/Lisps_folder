;; Round  -  Lee Mac
;; Rounds 'n' to the nearest integer
(defun LM:round ( n )
    (fix (+ n (if (minusp n) -0.5 0.5)))
)

;; Round Multiple  -  Lee Mac
;; Rounds 'n' to the nearest multiple of 'm'
(defun LM:roundm ( n m )
    (* m (fix ((if (minusp n) - +) (/ n (float m)) 0.5)))
)

;; Round To  -  Lee Mac
;; Rounds 'n' to 'p' decimal places
(defun LM:roundto ( n p )
    (LM:roundm n (expt 10.0 (- p)))
)

;; Round Up  -  Lee Mac
;; Rounds 'n' up to the nearest 'm'
(defun LM:roundup ( n m )
    ((lambda ( r ) (cond ((equal 0.0 r 1e-8) n) ((< n 0) (- n r)) ((+ n (- m r))))) (rem n m))
)

;; Round Down  -  Lee Mac
;; Rounds 'n' down to the nearest 'm'
(defun LM:rounddown ( n m )
    ((lambda ( r ) (cond ((equal 0.0 r 1e-8) n) ((< n 0) (- n r m)) ((- n r)))) (rem n m))
)