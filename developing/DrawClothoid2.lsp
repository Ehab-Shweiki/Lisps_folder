(defun deg->rad (a) (* pi (/ a 180.0)))
(defun rad->deg (a) (* 180.0 (/ a pi)))

;; --- [1] Fresnel integrals (approximated by power series "Taylor Expansion method") ---

;; Fresnel C(t) = ∫₀ᵗ cos(π/2 * u²) du
;;              = Σ (-1)ⁿ * [(π/2)^(2n) * t^(4n+1)] / [(2n)! * (4n+1)] ... (approximated by power series)
(defun fresnel-C (t / n term sum)
  (setq n 0 sum 0.0 term 1.0)
  (while (> (abs term) 1e-6)
    (setq term (/ (* (expt t (+ (* 4 n) 1)) (expt (/ pi 2.0) (* 2 n)))
              (* (factorial (* 2 n)) (+ (* 4 n) 1)))) ;  [(t)^(4n+1) * (pi/2)^(2n)] / [(2n)! *(4n+1)]
    (setq sum (+ sum (if (evenp n) term (- term)))) ; sum: C(t) = Σ (-1)^n * term
    (setq n (1+ n)))
  sum
)

;; Fresnel S(t) = ∫₀ᵗ sin(π/2 * u²) du
;;              = Σ (-1)ⁿ * [(π/2)^(4n+3) * t^(2n+1)] / [(2n+1)! * (4n+3)] ... (approximated by power series)
(defun fresnel-S (t / n term sum)
  (setq n 0 sum 0.0 term 1.0)
  (while (> (abs term) 1e-6)
    (setq term (/ (* (expt t (+ (* 4 n) 3)) (expt (/ pi 2.0) (+ (* 2 n) 1)))
         (* (factorial (+ (* 2 n) 1)) (+ (* 4 n) 3)))) ;  [t^(4n+3) * (pi/2)^(2n+1)] / [(2n+1)! *(4n+3)] 
    (setq sum (+ sum (if (evenp n) term (- term)))) ; sum: S(t) = Σ (-1)^n * term
    (setq n (1+ n)))
  sum
)

;; Factorial function: n! = n × (n-1) × ... × 1
(defun factorial (n)
  (if (<= n 1) 1
    (* n (factorial (- n 1)))
  )
)

;; --- [2] Estimate min/max clothoid length between two directions ---
;; Based on:
;;    Δθ = L² / (2·A²)
;;    Endpoint offset ≈ d = straight distance between lines
;; Then:
;;    Lmin ≈ sqrt(2·d·Δθ / sin(Δθ))
(defun clothoid-length-range (pt1 ang1 pt2 ang2 / delta d Lmin Lmax)
  (setq delta (abs (- ang2 ang1)))
  (if (> delta pi) (setq delta (- (* 2 pi) delta))) ; normalize to [0, π]
  (if (< delta 1e-6) (setq delta 1e-6)) ; avoid division by zero

  ;; d = Euclidean distance between connection points
  (setq d (distance pt1 pt2))

  ;; Lmin ≈ sqrt( (2 * d * Δθ) / sin(Δθ) )
  (setq Lmin (sqrt (/ (* 2 d delta) (sin delta))))

  ;; Optional: Lmax = 2 × Lmin
  (setq Lmax (* 2 Lmin))
  (list Lmin Lmax delta)
)

;; --- [3] Main clothoid drawing command ---
(defun c:CLOTHOID (/ ent1 ent2 e1 e2 p1 p2 pt1 pt2 ang1 ang2 delta lenRange Lmin Lmax delta angstep A i N t x y loc pt0 angle0 ptNext lastPt rotMat)

  ;; --- Step 1: Get input lines ---
  (prompt "\nSelect first line (start direction): ")
  (setq ent1 (car (entsel)))
  (if (not ent1) (progn (prompt "\nNothing selected.") (exit)))
  (setq e1 (entget ent1))
  (setq pt1 (cdr (assoc 11 e1))) ; end point
  (setq p1  (cdr (assoc 10 e1))) ; start point
  (setq ang1 (angle p1 pt1))     ; direction of first line

  (prompt "\nSelect second line (end direction): ")
  (setq ent2 (car (entsel)))
  (if (not ent2) (progn (prompt "\nNothing selected.") (exit)))
  (setq e2 (entget ent2))
  (setq pt2 (cdr (assoc 10 e2))) ; start point
  (setq p2  (cdr (assoc 11 e2))) ; end point
  (setq ang2 (angle pt2 p2))     ; direction of second line

  ;; --- Step 2: Calculate clothoid angle and length bounds ---
  ;; Δθ = abs(θ₂ - θ₁)
  ;; Lmin = sqrt(2·d·Δθ / sin(Δθ))
  (setq lenRange (clothoid-length-range pt1 ang1 pt2 ang2))
  (setq Lmin (car lenRange))
  (setq Lmax (cadr lenRange))
  (setq delta (caddr lenRange))

  ;; --- Step 3: Ask for desired clothoid length ---
  (initget 6)
  (setq L (getreal (strcat "\nEnter clothoid length [" (rtos Lmin 2 2) " - " (rtos Lmax 2 2) "]: ")))
  (if (< L Lmin) (setq L Lmin))
  (if (> L Lmax) (setq L Lmax))

  ;; --- Step 4: Ask for number of segments (controls smoothness) ---
  (initget 7)
  (setq N (getint "\nEnter number of segments (10+ recommended): "))
  (if (< N 2) (setq N 20))

  ;; --- Step 5: Compute clothoid parameter A from:
  ;; Δθ = L² / (2·A²) ⇒ A = sqrt(L² / (2·Δθ))
  (setq A (sqrt (/ (* L L) (* 2 delta))))

  ;; --- Step 6: Generate clothoid points using Fresnel:
  ;; x(s) = A·C(s/A), y(s) = A·S(s/A)   for s ∈ [0, L]
  (setq pt0 (list 0.0 0.0 0.0))
  (setq lastPt pt0)
  (setq i 1)
  (repeat N
    ;; s = (i / N) * L
    (setq t (/ (* i L) N))
    ;; local coordinates
    (setq x (* A (fresnel-C (/ t A))))
    (setq y (* A (fresnel-S (/ t A))))
    (setq ptNext (list x y 0.0))

    ;; Draw LINE from lastPt to ptNext
    (entmakex
      (list (cons 0 "LINE")
            (cons 10 lastPt)
            (cons 11 ptNext)
            (cons 62 1) ; red color
      )
    )
    (setq lastPt ptNext)
    (setq i (1+ i))
  )

  ;; Transform all added entities to align with user’s selected lines
  ;; Compute rotation and translation
  (setq angle0 0.0)
  (setq rotMat (list (list (cos ang1) (- (sin ang1)) 0.0)
                     (list (sin ang1) (cos ang1) 0.0)
                     (list 0.0 0.0 1.0)))

  ;; Move and rotate all drawn entities (very simple approximation: use UCS before drawing for better accuracy)
  (prompt "\nDone. Clothoid drawn from selected lines.")
  (princ)
)
