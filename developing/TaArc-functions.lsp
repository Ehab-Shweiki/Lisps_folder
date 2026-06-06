;;; ---------- Vector helpers ----------
(defun v+ (a b) (mapcar '+ a b))
(defun v- (a b) (mapcar '- a b))
(defun v* (a s) (mapcar '(lambda (x) (* x s)) a))
(defun vdot (a b) (+ (* (car a) (car b)) (* (cadr a) (cadr b))))
(defun vlen (v) (distance '(0 0 0) v)) ; faster than (sqrt (vlen2 v))
(defun vlen2 (v) (vdot v v))
(defun vunit (v) (if (zerop (vlen v)) '(0 0 0) (v* v (/ 1.0 (vlen v)))))

(defun vperpL (v) (list (- (cadr v)) (car v) 0.0)) ;; left normal (perpendicular to v in 2D space)
(defun vperpR (v) (list (cadr v) (- (car v)) 0.0)) ;; right normal (perpendicular to v in 2D space)
(defun tan (x) (if (not (equal 0.0 (cos x) 1e-10)) (/ (sin x) (cos x))))

(defun proj (a b)
  "Vector projection of a onto b
   proj = (a.b / |b|²) b   , where B̂ = b/|b| , proj = (comp)B̂ "
  (v* b (/ (vdot a b) (vlen2 b)))
)
(defun comp (a b)
  "Scalar projection of a onto b
   comp = (a.b / |b|)  "
  (/ (vdot a b) (vlen b))
)
(defun comp-on-unit (a b)
  "Scalar projection of a onto a unit vector (b)"
  "(vdot a b)"
  (+ (* (car a) (car b)) (* (cadr a) (cadr b)))
)

(defun proj-on-line (pt base u / t1) ; u must be unit
  "project point to infinite line (base, unit dir)"
  (setq t1 (vdot (v- pt base) u))
  (v+ base (v* u t1))
)

(defun line-intersection (pt1 dir1 pt2 dir2 / a b c d dx dy det t1)
  "Return intersection point of two infinite lines defined by (pt1,dir1) and (pt2,dir2).
   Returns NIL if lines are parallel or nearly parallel."
  ; L1​: P1​ + t*D1​ , L2​: P2 ​+ s*D2​
  ; P1 ​+ t*D1​ = P2​ + s*D2​
  ; => t = (x2​−x1​)d​−(y2​−y1​)c / (ad-bc)
  (setq a (car dir1) b (cadr dir1)
        c (car dir2) d (cadr dir2)
        dx (- (car pt2) (car pt1))
        dy (- (cadr pt2) (cadr pt1))
        det (- (* a d) (* b c)))
  (if (equal det 0.0 1e-12)
    nil ; lines are parallel
    (progn
      (setq t1 (/ (- (* dx d) (* dy c)) det)) ; t parameter along line 1
      (v+ pt1 (v* dir1 t1)) ; the intersection point
    )
  )
)

(defun get-ent-pair ()
  (setq ename (car ent1) pickpt (cadr ent1))) ; debug.
(defun curve-refpt+tan (ename pickpt / obj typ param seg p0 p1 dir bulge)
  ;Fix: modify handeling for curve arc/Pline segment. (as arc with C,R,start,end)
  (vl-load-com)
  (setq obj   (if (= (type ename) 'VLA-OBJECT)
                  ename
                  (vlax-ename->vla-object ename))
        typ   (vla-get-ObjectName obj))

  (cond
    ;; -------- LINE --------
    ((= typ "AcDbLine")
     (setq p0 (vlax-get obj 'StartPoint)
           p1 (vlax-get obj 'EndPoint)
           dir (vunit (v- p1 p0)))
     (list p0 dir)
    )

    ;; -------- LWPOLYLINE --------
    ((= typ "AcDbPolyline")
      (progn
        (setq param (vlax-curve-getParamAtPoint obj 
                        (vlax-curve-getClosestPointTo obj pickpt)) ;; parameter of picked point
              seg   (fix param) ; segment index
              p0    (vlax-curve-getPointAtParam obj seg)
              p1    (vlax-curve-getPointAtParam obj (1+ seg))
              bulge (vla-GetBulge obj seg))

        (if (equal bulge 0.0 1e-12)
          ;; straight segment
          (progn
            (setq dir (vunit (v- p1 p0)))
            (list p0 dir)
          )
          ;; arc segment
          (pline-segment-refpt+tan ename pickpt)
        )
      )
    )

     
    
    ;; -------- ARC --------
    ((= typ "AcDbArc")
     (setq p0 (vlax-get obj 'StartPoint))
     (setq dir (arc-tangent-at-point ename pickpt))
     (list p0 dir)
    )
    
    ;; -------- Others -------- Circle, SPLINE, ELLIPSE
    (t
     (princ "others")
      ; TODO.
     nil)
  )
)



(defun pline-segment-refpt+tan (ent pickpt / obj param seg p0 p1 bulge chord len theta ang R d perp mid center pt dir)
  "Return (list start-point tangent-vector) of the polyline segment
   containing the picked point PICKPT.
   for straight and arc segments"
  (vl-load-com)
  (setq obj (vlax-ename->vla-object ent))
  (if (= "AcDbPolyline" (vla-get-ObjectName obj))
    (progn
      (setq param (vlax-curve-getParamAtPoint obj 
                      (vlax-curve-getClosestPointTo obj pickpt)) ;; parameter of picked point
            seg   (fix param) ; segment index
            p0    (vlax-curve-getPointAtParam obj seg)
            p1    (vlax-curve-getPointAtParam obj (1+ seg))
            bulge (vla-GetBulge obj seg))

      (if (equal bulge 0.0 1e-12)
        ;; ---------- straight segment ----------
        (setq dir (vunit (v- p1 p0)))

        ;; ---------- arc segment ----------
        (progn
          (setq chord (v- p1 p0)
                len   (vlen chord)
                ;; use absolute bulge for geometry
                theta (* 4.0 (atan (abs bulge))) ; central angle (always positive)
                ang   (/ theta 2.0) ; half central angle
                R     (/ len 2.0 (sin ang))
                d     (* R (cos ang))) ; distance from chord mid-point to center
          ;; vector perpendicular to chord (for arc center)
          (setq perp  (if (> bulge 0.0) (vperpL chord) (vperpR chord))
                perp  (vunit perp))
          ;; center of the circular arc
          (setq mid   (v+ p0 (v* chord 0.5)) ; chord mid-point
                center (v+ mid (v* perp d))) ; true arc center
          ;; tangent = perpendicular to radius at picked point
          (setq pt     (vlax-curve-getClosestPointTo obj pickpt)
                dir    (if (> bulge 0.0)
                         (vperpL (v- pt center))
                         (vperpR (v- pt center))) ; tangent = ⟂ to radius
                dir    (vunit dir))
        )
      )
      (list p0 dir)
    )
  )
)

(defun arc-tangent-at-point (ent pickpt / obj cen start end param deriv dir)
  (vl-load-com)
  ;; Convert ename → vla-object
  (setq obj (vlax-ename->vla-object ent))

  ;; Get basic data
  (setq cen   (vlax-get obj 'Center)
        start (vlax-get obj 'StartPoint)
        end   (vlax-get obj 'EndPoint))

  ;; Find curve parameter of pick point
  (setq param (vlax-curve-getParamAtPoint obj 
                      (vlax-curve-getClosestPointTo obj pickpt)))

  ;; Get first derivative vector (tangent)
  (setq deriv (vlax-curve-getFirstDeriv obj param)
        dir (vunit deriv))

  ;; Return tangent as list (unit vector)
  dir
)

(defun c:ARC_2TAN_1PT (/ e1 e2 vla1 vla2 pt-dir1 pt-dir2 p1 p2 t1 t2 M arcMode pickLargeR? result cen rad Tpt1 Tpt2 cw)
  (vl-load-com)
  (prompt "\nSelect first tangent object (Line or Polyline): ")
  (setq e1 (entsel))
  (prompt "\nSelect second tangent object (Line or Polyline): ")
  (setq e2 (entsel))
  
  ;OPTIMIZE: filter by types for e1,e2.
  (if (and e1 e2)
    (progn
      (setq vla1 (vlax-ename->vla-object (car e1))
            vla2 (vlax-ename->vla-object (car e2)))

      ;;--- extract start point and direction for each object ---
      (setq pt-dir1 (curve-refpt+tan vla1 (cadr e1)))
      (setq pt-dir2 (curve-refpt+tan vla2 (cadr e2)))
      (setq p1 (car pt-dir1) t1 (cadr pt-dir1))
      (setq p2 (car pt-dir2) t2 (cadr pt-dir2))

      (if (and pt-dir1 pt-dir1)
        (progn
          ;; --- ask for a point on the arc
          (setq M (getpoint "\nPick point that arc must pass through: "))
          ;; --- Ask whether to use Internal or External arc ---
          (initget "Internal External")
          (setq arcMode (getkword "\nArc type [Internal/External] <Internal>: "))
          (princ (strcat "\nArc type: " arcMode))
          (setq pickLargeR?
                (or (null arcMode)  ; user pressed Enter
                    (= arcMode "Internal")))

          (setq result (circle-2tan-pt p1 t1 p2 t2 M pickLargeR?))
          (if result
            (progn
              (setq cen   (nth 0 result)
                    rad   (nth 1 result)
                    Tpt1  (nth 2 result)
                    Tpt2  (nth 3 result)
                    ang1 (angle cen Tpt1)
                    ang2 (angle cen Tpt2)
                    cw    (< (sin (- ang2 ang1)) 0.0))  ; quick direction test
              
              (make-arc cen rad ang1 ang2 cw)
              (prompt (strcat
                        "\nCenter: " (rtos (car cen) 2 3) ", "
                        (rtos (cadr cen) 2 3)
                        "   Radius: " (rtos rad 2 3)))
            )
            (prompt "\nNo valid circle found."))
        )
        (prompt "\nCould not extract tangent directions."))
    )
  )
  (princ)
)

(defun get-arc-ents-pt () ; debug.
  (progn
    (vl-load-com)
    (prompt "\nSelect first tangent object (Line or Polyline): ")
    (setq e1 (entsel-filteredDynamic "\nSelect first element: " allowed))
    
    (prompt "\nSelect second tangent object (Line or Polyline): ")
    (setq e2 (entsel-filteredDynamic "\nSelect second element: " allowed))
    
    
    (if (and e1 e2)
      (progn
        (setq vla1 (vlax-ename->vla-object (car e1))
              vla2 (vlax-ename->vla-object (car e2)))

        ;;--- extract start point and direction for each object ---
        (setq pt-dir1 (curve-refpt+tan vla1 (cadr e1)))
        (setq pt-dir2 (curve-refpt+tan vla2 (cadr e2)))
        (setq p1 (car pt-dir1) t1 (cadr pt-dir1))
        (setq p2 (car pt-dir2) t2 (cadr pt-dir2))
        (setq dir1 t1 dir2 t2)
        
        (if (and pt-dir1 pt-dir1)
            (setq M (getpoint "\nPick point that arc must pass through: ")))
      )
    )
  )) ; debug.
(defun p->pt_M->mouse ()
  setq p1 pt1 p2 pt2 M mouse) ; debug. ; c:TAARC: pt1 dir1 pt2 dir2 mouse
; (defun dir->t ()
;   (setq dir1 t1 dir2 t2)) ; debug. ; c:ARC_2TAN_1PT: p1 t1 p2 t2 M

(defun get-ent-bisect ()
  (setq pickpt (cadr (setq ent (entsel)))
        ename (car ent)
        b (cadr (curve-refpt+tan ename pickpt)))) ; debug.

;;; ---------- Main solver (no iteration) ----------
(defun circle-2tan-pt (p1 dir1 p2 dir2 M pickLargeR? / u1 u2 n1 n2 Q bis b listC A B1 D E aa bb cc disc srt t1 C R Tpt1 Tpt2)
  "Returns a list containing: center, radius, T1, T2) or nil
   R = |C - M|  and  R = (C-P1).n1
   C = Q + t1 b   , b = u1 + u2
  "
  ;; Returns: (list center radius T1 T2) or nil
  (setq u1 (vunit dir1)
        u2 (vunit dir2))
  ;; intersection of the two tangents
  (setq Q  (line-intersection p1 u1 p2 u2))
  (if (null Q) (progn (princ "\nLines are parallel/collinear.") (exit)))

  ;; unit normals to lines (any consistent choice)
  (setq n1 (vunit (vperpL u1))
        n2 (vunit (vperpL u2)))

  ;; two angle-bisector directions (use line directions)
  (setq bis (vl-remove nil
             (list
               (if (> (vlen (v+ u1 u2)) 1e-12) (vunit (v+ u1 u2)))
               (if (> (vlen (v- u1 u2)) 1e-12) (vunit (v- u1 u2))))))

  ;; --- Generate circle solutions from both bisectors ---
  (setq listC '())
  (foreach b bis
    ;; Solve [ (Q + t b - p1)·n1 ]² = |Q + t b - M|²
    ;; [(Q - p1).n1 + t b.n1 ]² = [(Q - M) + t b].[(Q - M) + t b]
    ;;          ...              = |Q - M|² + 2t [b.(Q - M)] + t² |b|²
    ;; Substitute by A,B,D,E scalers
    ;;          [ B + At ]²     = E + 2t D + t²
    ;;     B² + 2AB t + A² t² = E + 2D t + t²
    ;;     t² + [2(AB - D)] t + (B² - E) = 0
    (setq A (vdot b n1)         ; A = b.n1
          B1 (vdot (v- Q p1) n1) ; B = (Q - p1).n1
          D (vdot b (v- Q M))   ; D = b.(Q - M)
          E (vlen2 (v- Q M))    ; E = |Q - M|²
          aa (- (* A A) 1.0)        ; a = A² - 1
          bb (* 2.0 (- (* A B1) D)) ; b = 2(AB - D)
          cc (- (* B1 B1) E))        ; c = B² - E
    (cond
      ((> (abs a) 1e-12)
       ; Quadratic: a != 0
       (setq disc (- (* bb bb) (* 4.0 aa cc))) ; Discriminant (المميز)

       (cond
        ;; No real roots (strictly negative beyond tolerance) -> skip
        ((< disc (- 1e-10))
          ;; nothing to add; try other bisector
        )

        ;; Nearly tangent (discriminant ~ 0): clamp and take double root
        ((< (abs disc) 1e-10)
          (setq t1 (/ (- bb) (* 2.0 aa)))
          (setq C (v+ Q (v* b t1))  R (vlen (v- C M)))
          (if (and (equal (abs (comp-on-unit (v- C p1) n1)) R 1e-6)
                  (equal (abs (comp-on-unit (v- C p2) n2)) R 1e-6))
            (setq listC (cons (list C R) listC)))
        )

        ;; Two real roots
        (T
          (setq srt (sqrt disc))
          (setq t1 ())
          (foreach t1 (list (/ (+ (- bb) srt) (* 2.0 aa))
                          (/ (- (- bb) srt) (* 2.0 aa)))
            (setq C (v+ Q (v* b t1))  R (vlen (v- C M)))
            (if (and (equal (abs (comp-on-unit (v- C p1) n1)) R 1e-6)
                    (equal (abs (comp-on-unit (v- C p2) n2)) R 1e-6))
              (setq listC (cons (list C R) listC)))
          )
        )
       )
      )
      
      (T ; linear case: a≈0 ⇒ (bb)*t + cc = 0 ; means lines nearly parallel (theta ~ 0 or 180)
        ; if exactly parallel then Q is nill (checked before)
       (if (> (abs bb) 1e-12)
         (progn
           (setq t1 (/ (- cc) bb)
                 C (v+ Q (v* b t1)) 
                 R (vlen (v- C M)))
           ; verify both tangencies within tolerance:
           (if (and (equal (abs (comp-on-unit (v- C p1) n1)) R 1e-6)
                    (equal (abs (comp-on-unit (v- C p2) n2)) R 1e-6))
             (setq listC (cons (list C R) listC)))
         )
         ;; else: bb≈0 too -> no info (ignore this bisector)
       )
      )
    )
  )

  ;; --- Final Solution Selection ---
  (if (null listC) nil
    (progn
      ;; Sort ascending by center distance to M
      (setq listC (vl-sort listC '(lambda (a b) ; a,b are lists of (C,R)
                                     (< (vlen (v- (car a) M))
                                        (vlen (v- (car b) M))))))
      ;; pick one solution: according to smallest or largest
      (if pickLargeR?
        (setq C (car (last listC)) R (cadr (last listC)))  ; last = largest R (closest to Q)
        (setq C (car (car listC)) R (cadr (car listC))))   ; first = smallest R (far away from Q)
   
      ;; true tangency points = perpendicular foot from C to each line
      (list C R
            (proj-on-line C p1 u1)
            (proj-on-line C p2 u2))
    )
  )
)

(defun make-arc (cen r ang1 ang2 cw / tmp)
  ;; Reverse if clockwise
  (if cw (setq tmp ang1 ang1 ang2 ang2 tmp))
  
  ;; Create ARC
  (entmakex
    (list
      (cons 0 "ARC")
      (cons 10 (2d cen))
      (cons 40 r)
      (cons 50 ang1)
      (cons 51 ang2)))
)