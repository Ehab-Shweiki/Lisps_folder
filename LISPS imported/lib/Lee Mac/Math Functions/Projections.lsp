;; Project Point onto Line  -  Lee Mac
;; Projects pt onto the line defined by p1,p2

(defun LM:ProjectPointToLine ( pt p1 p2 / nm )
    (setq nm (mapcar '- p2 p1)
          p1 (trans p1 0 nm)
          pt (trans pt 0 nm)
    )
    (trans (list (car p1) (cadr p1) (caddr pt)) nm 0)
)

;; Project Point onto Plane  -  Lee Mac
;; Projects pt onto the plane defined by its origin and normal

(defun LM:ProjectPointToPlane ( pt org nm )
    (setq pt  (trans pt  0 nm)
          org (trans org 0 nm)
    )
    (trans (list (car pt) (cadr pt) (caddr org)) nm 0)
)

;; Reflect Point  -  Lee Mac
;; Returns the point obtained by reflecting 'pt' in the axis defined by points p1 & p2.

(defun LM:Reflect ( pt p1 p2 / ax )
    (setq ax (mapcar '- p1 p2)
          p1 (trans p1 0 ax)
          pt (trans pt 0 ax)
    )
    (trans (cons (- (+ (car p1) (car p1)) (car pt)) (cdr pt)) ax 0)
)