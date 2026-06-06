(defun ConvertCurveToPolyline (ename / obj name num-segs param step pts i pt plineData)
  (vl-load-com)
  (setq obj (vlax-ename->vla-object ename))
  (setq name (vla-get-objectname obj))
  (setq num-segs 32) ; default segments

  (cond
    ;; CIRCLE or ELLIPSE or SPLINE (all support curve parameterization)
    ((member name '("AcDbSpline" "AcDbEllipse" "AcDbCircle"))
     (setq step (/ (- (vlax-curve-getEndParam ename)
                      (vlax-curve-getStartParam ename))
                   num-segs))
     (setq i 0
           pts '()
     )
     (while (<= i num-segs)
       (setq param (+ (vlax-curve-getStartParam ename) (* i step)))
       (setq pt (vlax-curve-getPointAtParam ename param))
       (setq pts (append pts (list pt)))
       (setq i (1+ i))
     )

     ;; Build polyline data
     (setq plineData
       (append
         (list
           (cons 0 "LWPOLYLINE")
           (cons 100 "AcDbEntity")
           (cons 100 "AcDbPolyline")
           (cons 90 (length pts))
           (cons 70 (if (equal (car pts) (last pts) 1e-6) 1 0)) ; closed if same point
         )
         (apply 'append (mapcar '(lambda (p) (list (cons 10 p) (cons 42 0.0))) pts))
       )
     )

     ;; Make polyline and delete original
     (entdel ename)
     (entmakex plineData)
    )
    (T ename) ; If unsupported type, just return original
  )
)

;-----------------------------

(defun ConvertCurveToPolyline_arcSegments (ename / obj name num-segs param step pts i pt bulgePts)
  (vl-load-com)
  (setq obj (vlax-ename->vla-object ename))
  (setq name (vla-get-objectname obj))
  (setq num-segs 16) ; fewer segments for smoother arcs

  (cond
    ((member name '("AcDbSpline" "AcDbEllipse" "AcDbCircle"))
     ;; 1. Collect points
     (setq step (/ (- (vlax-curve-getEndParam ename)
                      (vlax-curve-getStartParam ename))
                   num-segs))
     (setq i 0
           pts '()
     )
     (while (<= i num-segs)
       (setq param (+ (vlax-curve-getStartParam ename) (* i step)))
       (setq pt (vlax-curve-getPointAtParam ename param))
       (setq pts (append pts (list pt)))
       (setq i (1+ i))
     )

     ;; 2. Build bulge segments
     (setq i 0 bulgePts '())
     (while (< i (- (length pts) 2))
       (setq pt1 (nth i pts))
       (setq midpt (nth (1+ i) pts))
       (setq pt2 (nth (+ i 2) pts))
       (setq bulge (calc-bulge pt1 pt2 midpt))
       (setq bulgePts (append bulgePts (list (list pt1 bulge))))
       (setq i (1+ i))
     )
     ;; Last segment (add last point with 0 bulge)
     (setq bulgePts (append bulgePts (list (list (last pts) 0.0))))

     ;; 3. Create plineData
     (setq plineData
       (append
         (list
           (cons 0 "LWPOLYLINE")
           (cons 100 "AcDbEntity")
           (cons 100 "AcDbPolyline")
           (cons 90 (length bulgePts))
           (cons 70 0)
         )
         (apply 'append (mapcar '(lambda (x) (list (cons 10 (car x)) (cons 42 (cadr x)))) bulgePts))
       )
     )

     ;; Replace entity
     (entdel ename)
     (entmakex plineData)
    )
    (T ename) ; return original if unsupported
  )
)

; helper functions for ConvertCurveToPolyline_arcSegments function
;----------------------
(defun tan (x)
  (if (equal (cos x) 0.0 1e-8)
    nil
    (/ (sin x) (cos x))
  )
)
(defun calc-bulge (pt1 pt2 midpt / a b c ang dir bulge)
  ;; Calculates bulge from 3 points
  (setq a (distance pt1 midpt)
        b (distance midpt pt2)
        c (distance pt1 pt2)
  )
  (if (and (> a 0.0001) (> b 0.0001) (> c 0.0001))
    (progn
      (setq ang (* 4 (angle pt1 midpt)))
      (setq bulge
        (tan (/ (angle pt1 midpt) 2.0))
      )
      ;; Direction: positive if arc is counter-clockwise
      (setq dir (if (< (angle pt1 pt2) (angle pt1 midpt)) -1.0 1.0))
      (* bulge dir)
    )
    0.0
  )
)