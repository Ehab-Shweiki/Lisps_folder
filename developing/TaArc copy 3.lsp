(defun pline-segment-refpt+tan (ent pickpt / obj param seg p0 p1 bulge chord len theta ang d perp mid center pt dir)
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
        ;; straight segment
        (setq dir (vunit (v- p1 p0)))

        ;; arc segment
        (progn
          (setq chord (v- p1 p0)
                len   (vlength chord)
                theta (* 4.0 (atan (abs bulge))) ; central angle (always positive)
                ang   (/ theta 2.0)) ; half central angle
          ;; vector perpendicular to chord (for arc center)
          (setq perp  (if (> bulge 0.0) (vperpL chord) (vperpR chord))
                perp  (vunit perp))
          ;; center of the circular arc
          (setq d     (* R (cos ang)) ; distance from chord mid-point to center
                mid   (v+ p0 (v* chord 0.5)) ; chord mid-point
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

(defun curve-refpt+tan (ename pickpt / obj typ param seg p0 p1 tvec dir bulge)
  ;Fix: dont handel for curve arc/Pline segment.
  (vl-load-com)
  (setq obj (vlax-ename->vla-object ename)
        typ   (vla-get-ObjectName obj))

  (cond
    ;; -------- LINE --------
    ((= typ "AcDbLine")
     (setq p0 (vlax-get obj 'StartPoint)
           p1 (vlax-get obj 'EndPoint))
           dir (vunit (v- p1 p0))
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
    
    ;; -------- Others --------
    (t
     (princ "others")
     nil)
  )
)

(defun arc-tangent-at-point (ent pickpt / obj cen start end param deriv vlen tan ccw)
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
  