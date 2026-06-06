;; Complex Addition  -  Lee Mac
;; Args: c1,c2 - complex numbers of the form a+bi = (a b)

(defun c+c ( c1 c2 )
    (mapcar '+ c1 c2)
)

;; Complex Subtraction  -  Lee Mac
;; Args: c1,c2 - complex numbers of the form a+bi = (a b)

(defun c-c ( c1 c2 )
    (mapcar '- c1 c2)
)

;; Complex Multiplication  -  Lee Mac
;; Args: c1,c2 - complex numbers of the form a+bi = (a b)

(defun cxc ( c1 c2 )
    (list
        (- (* (car c1) (car  c2)) (* (cadr c1) (cadr c2)))
        (+ (* (car c1) (cadr c2)) (* (cadr c1) (car  c2)))
    )
)

;; Complex Conjugate  -  Lee Mac
;; Args: c1 - complex number of the form a+bi = (a b)

(defun c_ ( c1 )
    (list (car c1) (- (cadr c1)))
)

;; Complex Division  -  Lee Mac
;; Args: c1,c2 - complex numbers of the form a+bi = (a b)

(defun c/c ( c1 c2 / d )
    (   (lambda ( d ) (mapcar '(lambda ( x ) (/ x d)) (cxc c1 (c_ c2))))
        (car (cxc c2 (c_ c2)))
    )
)

;; Complex Norm  -  Lee Mac
;; Args: c1 - complex number of the form a+bi = (a b)

(defun |c| ( c1 )
    (sqrt (apply '+ (mapcar '* c1 c1)))
)