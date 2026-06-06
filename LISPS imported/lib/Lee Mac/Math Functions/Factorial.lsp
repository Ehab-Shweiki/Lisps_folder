;; Factorial  -  Lee Mac
;; Args: n - positive integer

(defun n! ( n / r )
    (setq r n)
    (repeat (fix (- n 2)) (setq r (* r (setq n (1- n)))))
    (if (< r 2) 1 r)
)

;; Factorial (recursive version)  -  Lee Mac
;; Args: n - positive integer

(defun n!-rec ( n )
    (if (< n 2) 1 (* n (n!-rec (1- n))))
)

;; Factorial Division  -  Lee Mac
;; Args: n,k - positive integers

(defun n!/k! ( n k / m r )
    (cond
        (   (= n k) 1)
        (   (setq r (max n k) m r)
            (repeat (fix (1- (abs (- n k)))) (setq r (* r (setq m (1- m)))))
            (if (< k n) r (/ 1.0 r))
        )
    )
)

;; Factorial Multiplication  -  Lee Mac
;; Args: n,k - positive integers

(defun n!k! ( n k / m )
    (setq m (n! (min n k)))
    (* (n!/k! (max n k) (min n k)) m m)
)