(defun c:BD ( / ent ename pickpt obj param pickDist endDist option offset breakDist brkpt breakFromStart minOffset maxOffset)
  (vl-load-com)

  ;; Select polyline and point
  (setq ent (entsel "\nSelect polyline and click a point: "))
  (if (and ent (setq ename (car ent)) (setq pickpt (cadr ent)))
    (progn
      (setq ename (EnsurePolyline ename)) ;; <- convert if needed
    ;   (setq ename (ConvertLineOrArcToPolyline ename)) ;; <- convert if needed
	  (setq obj (vlax-ename->vla-object ename))

      ;; Validate it's a polyline
      (if (wcmatch (vla-get-objectname obj) "*Polyline")
        (progn
          ;; Get picked distance and total length
          (setq param (vlax-curve-getParamAtPoint ename (vlax-curve-getClosestPointTo ename pickpt)))
          (setq pickDist (vlax-curve-getDistAtParam ename param))
          (setq endDist (vlax-curve-getDistAtParam ename (vlax-curve-getEndParam ename)))

          ;; Prompt for option
          (initget "Start End Pick") ;; <--- here is your initget
          (setq option (getkword "\nBreak from [Start/End/Pick] <Auto nearest end>: "))
		  (setq option (if option (strcase option) nil))

          (cond
            ;; Option 1: Auto-detect or choose Start or End
            ((or (not option) (eq option "START") (eq option "END"))
             (setq breakFromStart
               (if (eq option "START")
                 T
                 (if (eq option "END")
                   nil
                   (< pickDist (/ endDist 2.0)) ; auto if nil
                 )
               )
             )

             ;; Define offset limits
             (setq minOffset 0.0)
             (setq maxOffset endDist)

             ;; Prompt and validate offset
             (while
               (progn
                 (setq offset (getreal
                   (strcat "\nEnter distance from "
                           (if breakFromStart "START" "END")
                           " (0.0 to "
                           (rtos maxOffset 2 2)
                           "): ")))
                 (or (< offset minOffset) (> offset maxOffset))
               )
               (princ "\nInvalid distance. Please enter a value within the allowed range.")
             )

             ;; Calculate breakDist
             (setq breakDist (if breakFromStart offset (- endDist offset)))
            )

            ;; Option 2: From pick point toward farthest end
            ((= option "PICK")
             (if (< pickDist (/ endDist 2.0))
               ;; Toward END
               (progn
                 (setq minOffset (* -1 pickDist))
                 (setq maxOffset (- endDist pickDist))
                 (while
                   (progn
                     (setq offset (getreal
                       (strcat "\nEnter distance from picked point toward END ("
                               (rtos minOffset 2 2) " to "
                               (rtos maxOffset 2 2) "): ")))
                     (or (< offset minOffset) (> offset maxOffset))
                   )
                   (princ "\nInvalid distance. Please enter a value within the allowed range.")
                 )
                 (setq breakDist (+ pickDist offset))
               )
               ;; Toward START
               (progn
                 (setq minOffset (* -1 (- endDist pickDist)))
                 (setq maxOffset pickDist)
                 (while
                   (progn
                     (setq offset (getreal
                       (strcat "\nEnter distance from picked point toward START ("
                               (rtos minOffset 2 2) " to "
                               (rtos maxOffset 2 2) "): ")))
                     (or (< offset minOffset) (> offset maxOffset))
                   )
                   (princ "\nInvalid distance. Please enter a value within the allowed range.")
                 )
                 (setq breakDist (+ pickDist offset))
               )
             )
            )

            ;; Invalid option
            (T (princ "\nInvalid option."))
          )

          ;; Get break point
          (setq brkpt (vlax-curve-getPointAtDist ename breakDist))

          ;; Break it
          (if brkpt
            (progn
              (setq oldSnap (getvar "OSMODE"))
			  (setvar "OSMODE" 0)  ;; turn off snaps temporarily  
              (command "_.BREAK" ename brkpt brkpt)
			  (setvar "OSMODE" oldSnap)
              (princ "\nPolyline broken at specified location."))
            (princ "\nCould not compute break point.")
          )
        )
        (princ "\nSelected object is not a polyline.")
      )
    )
    (princ "\nNo valid polyline selected.")
  )
)


;; Helper Functions
;;-----------------
(defun tan (x)
  (if (equal (cos x) 0.0 1e-6)
    nil ; or 1.0e+20 if you want a fallback
    (/ (sin x) (cos x))
  )
)

(defun EnsurePolyline (ename)
  (setq ename (ConvertLineOrArcToPolyline ename))
  (setq ename (ConvertCurveToPolyline ename))
  ename
)

(defun ConvertLineOrArcToPolyline (ename / ent edata newpline)
  (setq edata (entget ename))
  (cond
    ((= (cdr (assoc 0 edata)) "LINE")
     (setq newpline
       (entmakex
         (list
           (cons 0 "LWPOLYLINE")
           (cons 100 "AcDbEntity")
           (cons 100 "AcDbPolyline")
           (cons 90 2)
           (cons 70 0)
           (cons 10 (cdr (assoc 10 edata))) ; start point
           (cons 42 0.0)
           (cons 10 (cdr (assoc 11 edata))) ; end point
           (cons 42 0.0)
         )
       )
     )
    )
    ((= (cdr (assoc 0 edata)) "ARC")
      
     (setq  cen (cdr (assoc 10 edata))
            rad (cdr (assoc 40 edata))
            ang1 (cdr (assoc 50 edata))
            ang2 (cdr (assoc 51 edata)))
     ;; Normalize angle if needed
 	 (if (< ang2 ang1)
 	   (setq ang2 (+ ang2 (* 2 pi)))
 	 )
     
     (setq pt1 (polar cen ang1 rad)
		   pt2 (polar cen ang2 rad)
           delta (/ (- (if (< ang2 ang1) (+ ang2 (* 2 pi)) ang2) ang1) 2.0)
           bulge (tan (/ delta 2.0))
  	 )
     
	 (setq newpline
	   (entmakex
		 (list
			(cons 0 "LWPOLYLINE")
			(cons 100 "AcDbEntity")
			(cons 100 "AcDbPolyline")
			(cons 90 2)
			(cons 70 0)
			(cons 10 pt1)
			(cons 42 bulge)
			(cons 10 pt2)
			(cons 42 0.0)
		  )
		)
	  )
    )
  )
  ;; If a new polyline was made, delete old entity and return new name
  (if newpline
    (progn (entdel ename) newpline)
    ename
  )
)

;; Function to convert curves to polylines
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



(princ "\nType BD to run the BreakPlineAtDist command.")
(princ)