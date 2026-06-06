(defun pline-picked-dist ( / ent ename pickpt obj param pickDist startDist endDist)
  "Prompt user to select a polyline and pick a point near it.
   Return distances from start and end vertices to the picked point."
  
  ;; Select polyline and pick point
  (setq ent (entsel "\nSelect polyline and click near one end: "))
  (setq ename (car ent))
  (setq pickpt (cadr ent))
  (setq obj (vlax-ename->vla-object ename))

  ;; Verify polyline
  (wcmatch (vla-get-objectname obj) "*Polyline")
          
  ;; Get parameter and distance of picked point
  (setq param (vlax-curve-getParamAtPoint ename (vlax-curve-getClosestPointTo ename pickpt)))
  (setq pickDist (vlax-curve-getDistAtParam ename param))

  ;; to Compare distance from both ends
  (setq startDist 0.0)
  (setq endDist (vlax-curve-getDistAtParam ename (vlax-curve-getEndParam ename)))
  
  ;; Print distances
  (princ (strcat "\nDistance from start: " (rtos (- pickDist startDist) 2 4)
                 "\nDistance from end: " (rtos (- endDist pickDist) 2 4)
          ))
  (princ)
  
  ; Return distances as list
  (list startDist pickDist endDist)
)

(defun get-vertices-pline (ent / ent obj coords i)
  "Return list of vertices of polyline ent"
  (vl-load-com)
  (setq obj (vlax-ename->vla-object ent))
  (if (= "AcDbPolyline" (vla-get-ObjectName obj))
    (progn
      (setq coords (vlax-get obj 'Coordinates)
            vers   '()
            i       0)
      (while (<= (+ i 1) (1- (length coords)))
        ; (setq vers (append vers (list (list (nth i coords) (nth (1+ i) coords) 0.0))))
        (setq vers (cons (list (nth i coords) (nth (1+ i) coords) 0.0) vers))
        
        ; (princ (strcat "\nVertex " (itoa (/ i 2)) ": "
        ;                (rtos (nth i coords) 2 4) ", "
        ;                (rtos (nth (1+ i) coords) 2 4)))
        (setq i (+ i 2))
      )
      vers
    )
  )
)

(defun get-vertex-at-pline (ent idx / obj coords i)
  "Return vertex at index idx (0-based) of polyline ent"
  (vl-load-com)
  (setq obj (vlax-ename->vla-object ent))
  (if (= "AcDbPolyline" (vla-get-ObjectName obj))
    (progn
      (setq coords (vlax-get obj 'Coordinates))
      ;or: (setq coords (vlax-safearray->list (vlax-variant-value (vla-get-Coordinates obj))))
      (setq i (* idx 2)) ; each vertex = 2 numbers
      (if (<= (+ i 1) (1- (length coords)))
        (list (nth i coords) (nth (1+ i) coords) 0.0)
      )
    )
  )
)

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
