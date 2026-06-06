;;; CLOTHOID CURVE COMPUTATION METHODS - AUTOLISP IMPLEMENTATION
;;; ---------------------------------------------------------------
;;; This file defines various mathematical methods to compute clothoid (Euler spiral)
;;; coordinates using AutoLISP. All approximations are based on evaluating:
;;;   x(s) = ∫₀ˢ cos(k u²) du
;;;   y(s) = ∫₀ˢ sin(k u²) du
;;; where k is usually π/2.

(defun cos-ku2 (u k) (cos (* k u u)))
(defun sin-ku2 (u k) (sin (* k u u)))

;;; ------------------------------------------------------------------------
;;; 1. Trapezoidal Rule
;;; Approximates the integral:
;;; ∫₀ˢ f(u) du ≈ h/2 [f(0) + 2∑f(u_i) + f(s)] for i = 1 to n-1
;;; Suitable for basic approximations.
(defun clothoid-trapezoid (s k n / h i u sumx sumy)
  (setq h (/ s n)                                                  ; h = s / n  ... step size
        sumx 0.0
        sumy 0.0
        i 1)
  (while (< i n)
    (setq u (* i h))                                               ; u = i * h
    (setq sumx (+ sumx (cos-ku2 u k)))                             ; sumx += cos(k * u²) 
    (setq sumy (+ sumy (sin-ku2 u k)))                             ; sumy += sin(k * u²)
    (setq i (1+ i)))
  (setq sumx (+ (* 0.5 (cos-ku2 0 k)) sumx (* 0.5 (cos-ku2 s k)))) ; sumx += 0.5 * (cos(k * 0²) + cos(k * s²))
  (setq sumy (+ (* 0.5 (sin-ku2 0 k)) sumy (* 0.5 (sin-ku2 s k)))) ; sumy += 0.5 * (sin(k * 0²) + sin(k * s²))
  (list (* h sumx) (* h sumy))                                     ; (x, y) = (h * sumx, h * sumy)
)

;;; ------------------------------------------------------------------------
;;; 2. Simpson's Rule (n must be even)
;;; Approximates the integral:
;;; ∫₀ˢ f(u) du ≈ h/3 [f(0) + 4∑f(odd u_i) + 2∑f(even u_i) + f(s)]
;;; High accuracy with small n.
(defun clothoid-simpson (s k n / h i sumx sumy)
  ;; Ensure that n is even (Simpson's rule requires even number of intervals)
  (if (/= 0 (rem n 2)) (setq n (1+ n)))
  (setq h (/ s n)                                  ; h = s / n  ... step size
        sumx 0.0
        sumy 0.0
        i 1)
  ;; Main summation loop for interior terms (from i = 1 to n−1)
  (while (< i n)
    (setq u (* i h))                               ; u = i * h
    (setq w (if (= 0 (rem i 2)) 2 4))              ; w = 4 if odd, 2 if even
    (setq sumx (+ sumx (* w (cos-ku2 u k))))       ; sumx += w * cos(k u^2)
    (setq sumy (+ sumy (* w (sin-ku2 u k))))       ; sumy += w * sin(k u^2)
    (setq i (1+ i)))
  ;; Add endpoints: f₀ and fₙ (i.e., cos(0), cos(s))
  (setq sumx (+ (cos-ku2 0 k) sumx (cos-ku2 s k))) ; sumx += f(0) + f(s)
  (setq sumy (+ (sin-ku2 0 k) sumy (sin-ku2 s k))) ; sumx += f(0) + f(s)
  (list (* (/ h 3.0) sumx) (* (/ h 3.0) sumy))
)

;;; ------------------------------------------------------------------------
;;; 3. Runge-Kutta 4th Order Method
;;; Solves dx/ds = cos(θ(s)), dy/ds = sin(θ(s)), with θ(s) = 0.5 * k * s²
;;; Very accurate for trajectory computation.
(defun clothoid-rk4 (s k n / h x y i u theta k1 k2 k3 k4)
  (setq h (/ s n) x 0.0 y 0.0 u 0.0 i 0) 
  (while (< i n)
    (setq theta (* 0.5 k u u))                            ; θ(s) = 0.5 * k * u²
    (setq k1 (list (cos theta) (sin theta)))              ; k1 = (cos(θ), sin(θ))
    (setq theta (* 0.5 k (+ u (/ h 2.0)) (+ u (/ h 2.0))) ; θ(s) = 0.5k * (u + h/2)^2
          k2 (list (cos theta) (sin theta)))              ; k2 = (cos(θ), sin(θ))
    (setq theta (* 0.5 k (+ u (/ h 2.0)) (+ u (/ h 2.0))) ; θ(s) = 0.5k * (u + h/2)^2
          k3 (list (cos theta) (sin theta)))              ; k3 = (cos(θ), sin(θ))
    (setq theta (* 0.5 k (+ u h) (+ u h))                 ; θ(s) = 0.5k * (u + h)^2
          k4 (list (cos theta) (sin theta)))              ; k4 = (cos(θ), sin(θ))
    (setq x (+ x (* h (/ (+ (car k1) (* 2 (car k2)) (* 2 (car k3)) (car k4)) 6.0))))     ; x += h/6 * (k1 + 2*k2 + 2*k3 + k4)
    (setq y (+ y (* h (/ (+ (cadr k1) (* 2 (cadr k2)) (* 2 (cadr k3)) (cadr k4)) 6.0)))) ; y += h/6 * (k1 + 2*k2 + 2*k3 + k4)
    (setq u (+ u h) i (1+ i))) ; increment u and i
  (list x y)
)

;;; ------------------------------------------------------------------------
;;; 4. Gaussian Quadrature (2-point)
;;; ∫₀ˢ f(u) du ≈ (s/2) × [f(u₁) + f(u₂)] where u₁,u₂ are mapped from roots of Legendre poly
(defun clothoid-gauss (s k / u1 u2 w1 w2 x y)
  (setq u1 (* 0.5 s (- 1 (/ 1.0 (sqrt 3.0)))))                       ; u1 = s/2(1 - 1/√3)
  (setq u2 (* 0.5 s (+ 1 (/ 1.0 (sqrt 3.0)))))                       ; u2 =  s/2(1 + 1/√3)
  (setq w1 1.0 w2 1.0)                                               ; Weights for 2-point Gauss = 1
  (setq x (* 0.5 s (+ (* w1 (cos-ku2 u1 k)) (* w2 (cos-ku2 u2 k))))) ; x = 0.5s * (w1 * cos(u1) + w2 * cos(u2))
  (setq y (* 0.5 s (+ (* w1 (sin-ku2 u1 k)) (* w2 (sin-ku2 u2 k))))) ; y = 0.5s * (w1 * sin(u1) + w2 * sin(u2))
  (list x y) ; (x, y) coordinates of the clothoid end point
)

;;; ------------------------------------------------------------------------
;;; 5. Taylor Series Expansion
;;; Uses Fresnel integral series expansion:
;;; C(s) ≈ Σ (–1)ⁿ · k²ⁿ · s⁴ⁿ⁺¹ / [ (2n)! · (4n+1) ] 
;;; S(s) ≈ Σ (–1)ⁿ · k²ⁿ⁺¹ · s⁴ⁿ⁺³ / [ (2n+1)! · (4n+3) ]  
;;; Suitable for small s, as high-order terms grow quickly.
(defun clothoid-taylor (s k / x y i term)
  (setq x 0.0 y 0.0)
  (setq i 0)
  (while (< i 5)
    (setq term (/ (* (expt (- 1) i) (expt k (* 2 i)) (expt s (+ 1 (* 4 i))))
                  (* (factorial (* 2 i)) (+ 1 (* 4 i)))))                       ; term = (-1)^i * k^(2i) * s^(4i+1) / (2i)! * (4i+1)
    (setq x (+ x term))                                                         ; accumulate x
    (setq term (/ (* (expt -1 i) (expt k (+ (* 2 i) 1)) (expt s (+ 3 (* 4 i))))
                  (* (factorial (+ (* 2 i) 1)) (+ 3 (* 4 i)))))                 ; term = (-1)^i * k^(2i+1) * s^(4i+3) / (2i+1)! * (4i+3)
    (setq y (+ y term))                                                         ; accumulate y
    (setq i (1+ i)))
  (list x y)
)

;;; ------------------------------------------------------------------------
;;; 6. Fresnel Lookup Table
;;; Precomputes values for sampled s and uses linear interpolation
;;; Fast and approximate.
(setq *clothoid-table* nil)
(defun clothoid-build-table (k / s step i u x y)
  (setq *clothoid-table* nil step 0.5 i 0)
  (while (<= (* i step) 10.0)
    (setq u (* i step))                                                   ; u = i * step
    (setq x (car (clothoid-simpson u k 50)))                              ; x(s) using Simpson
    (setq y (cadr (clothoid-simpson u k 50)))                             ; y(s)
    (setq *clothoid-table* (append *clothoid-table* (list (list u x y)))) ; update table of (x,y)
    (setq i (1+ i)))
)
(defun clothoid-lookup (s k / i u1 u2 x1 x2 y1 y2)
  (if (null *clothoid-table*) (clothoid-build-table k))  ; Lazy build
  ;; Find interpolation bracket
  (setq i 0)
  (while (and (< i (1- (length *clothoid-table*)))
              (> s (car (nth i *clothoid-table*))))
    (setq i (1+ i)))
  (if (= i 0)
    (list 0 0) ; s < first table entry
    (progn
      ;; Linear interpolation between (u1,x1,y1) and (u2,x2,y2)
      (setq u1 (car (nth (1- i) *clothoid-table*))
            x1 (cadr (nth (1- i) *clothoid-table*))
            y1 (caddr (nth (1- i) *clothoid-table*))
            u2 (car (nth i *clothoid-table*))
            x2 (cadr (nth i *clothoid-table*))
            y2 (caddr (nth i *clothoid-table*)))
      (list (+ x1 (* (/ (- s u1) (- u2 u1)) (- x2 x1)))   ; x = x1 + (s-u1)/(u2-u1) * (x2-x1)
            (+ y1 (* (/ (- s u1) (- u2 u1)) (- y2 y1))))) ; y = y1 + (s-u1)/(u2-u1) * (y2-y1)
  )
)

;;; ------------------------------------------------------------------------
;;; 7. Adaptive Simpson's Method
;;; Recursively applies Simpson’s Rule on subintervals, refining until:
;;;   |I₂ - I₁| < ε, where   I₁ = trapezoidal approx, I₂ = Simpson approx
;;; Uses midpoint to subdivide interval and evaluate f(u) = [cos(k u²), sin(k u²)]
(defun clothoid-adaptive (s k / f recur)
  (defun f (u) (list (cos-ku2 u k) (sin-ku2 u k))) ; f(u) = [cos(k u²), sin(k u²)]
  (defun recur (a b eps / m fa fm fb I1 I2)
    (setq m (/ (+ a b) 2) ; midpoint of [a,b]
          fa (f a)
          fm (f m)
          fb (f b)
          ;; I1 = trapezoid approximation
          I1 (list (* (- b a) (/ (+ (car fa) (car fb)) 2))
                   (* (- b a) (/ (+ (cadr fa) (cadr fb)) 2)))
          ;; I2 = Simpson’s approximation
          I2 (list (* (/ (- b a) 6) (+ (car fa) (* 4 (car fm)) (car fb)))
                   (* (/ (- b a) 6) (+ (cadr fa) (* 4 (cadr fm)) (cadr fb)))))
    ;; Check error and recurse if needed
    (if (and (< (abs (- (car I1) (car I2))) eps)
             (< (abs (- (cadr I1) (cadr I2))) eps))
      I2
      (mapcar '+ (recur a m (/ eps 2)) (recur m b (/ eps 2)))))
  ;; Initial call over [0, s]
  (recur 0.0 s 1e-6)
)

;;; ------------------------------------------------------------------------
;;; 8. Clenshaw–Curtis Quadrature
;;; Uses substitution u = s/2(1 - cos(θ)), transformed to integrate over θ in [0, π]
;;; Then ∫₀ˢ f(u) du = (s/2) ∫₀^π f((s/2)(1 - cos θ)) sin θ dθ
;;; Approximated using: ∫₀^π f(θ) dθ ≈ (π/n) ∑ f(θᵢ)
;;; High precision and commonly used in spectral methods.
(defun clothoid-clenshaw (s k n / i x theta sumx sumy)
  (setq i 1 sumx 0.0 sumy 0.0)
  (while (<= i n)
    (setq theta (* pi (/ i n)))                          ; θᵢ = iπ/n
    (setq x (* 0.5 s (- 1 (cos theta))))                 ; uᵢ = s/2 (1 - cos θᵢ)
    ;; Sum: f(uᵢ) * sin(θᵢ)
    (setq sumx (+ sumx (* (sin theta) (cos-ku2 x k))))    
    (setq sumy (+ sumy (* (sin theta) (sin-ku2 x k))))    
    (setq i (1+ i)))
  ;; Multiply sum by Δθ = π/n and s/2 factor
  (list (* 0.5 s (/ pi n) sumx) (* 0.5 s (/ pi n) sumy)) ; 
)


;;; ------------------------------------------------------------------------
;;; DRAW CLOTHOID POLYLINE COMMAND
;;; Allows user to select a clothoid computation method, total length, number of segments,
;;; and orientation based on two selected entities (lines or arcs) representing tangents.
(defun c:DRAW_CLOTHOID ( / method s n i pt k func ptlist ent1 ent2 pt1a pt1b pt2a pt2b v1 v2 ang1 mat p)
  (setq method (strcase (getstring "\nMethod [TRAP/SIMP/RK4/GAUSS/TAYLOR/LOOKUP/ADAPT/CLENS]: ")))
  (setq s (getreal "\nTotal length (s): "))
  (setq n (getint "\nNumber of segments: "))
  (if (not s) (setq s 5.0))
  (if (not n) (setq n 50))
  (setq k (/ pi 2))

  ;; Select two lines/arcs representing the start and end tangents
  (prompt "\nSelect start tangent (line or arc): ")
  (setq ent1 (car (entsel)))
  (prompt "\nSelect end tangent (line or arc): ")
  (setq ent2 (car (entsel)))

  (if (and ent1 ent2)
    (progn
      (setq pt1a (cdr (assoc 10 (entget ent1))))
      (setq pt1b (cdr (assoc 11 (entget ent1))))
      (setq pt2a (cdr (assoc 10 (entget ent2))))
      (setq pt2b (cdr (assoc 11 (entget ent2))))

      ;; Direction vectors
      (setq v1 (mapcar '- pt1b pt1a))
      (setq v2 (mapcar '- pt2b pt2a))
      (setq ang1 (angle '(0 0 0) v1))

      ;; Select computation method
      (cond
        ((= method "TRAP") (setq func clothoid-trapezoid))
        ((= method "SIMP") (setq func clothoid-simpson))
        ((= method "RK4") (setq func clothoid-rk4))
        ((= method "GAUSS") (setq func clothoid-gauss))
        ((= method "TAYLOR") (setq func clothoid-taylor))
        ((= method "LOOKUP") (setq func clothoid-lookup))
        ((= method "ADAPT") (setq func clothoid-adaptive))
        ((= method "CLENS") (setq func clothoid-clenshaw))
        (T (prompt "\nUnknown method.")))

      (if func
        (progn
          (setq ptlist nil i 0)
          (while (<= i n)
            (setq val (* s (/ i n)))
            (setq pt (cond
                       ((member method '("TRAP" "SIMP" "RK4" "CLENS")) (func val k n))
                       ((member method '("ADAPT" "LOOKUP" "TAYLOR" "GAUSS")) (func val k))
                       (T '(0.0 0.0))))
            ;; Rotate and transform point
            (setq p (polar pt1a (+ ang1 (angle '(0 0 0) pt)) (distance '(0 0 0) pt)))
            (setq ptlist (append ptlist (list p)))
            (setq i (1+ i)))

          ;; Create polyline
          (entmakex (append
                     (list '(0 . "LWPOLYLINE")
                           '(100 . "AcDbEntity")
                           '(100 . "AcDbPolyline")
                           (cons 90 (length ptlist))
                           (cons 70 0))
                     (mapcar '(lambda (pt) (list 10 (car pt) (cadr pt))) ptlist)))
          (princ "\nClothoid drawn."))
        (prompt "\nNo method selected.")))
    (prompt "\nInvalid entity selections."))
  (princ))

;;; END OF FILE


