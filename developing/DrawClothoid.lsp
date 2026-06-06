(defun c:ClothoidLink (/ ent1 ent2 obj1 obj2 type1 type2
                       start1 end1 dir1 rad1
                       start2 end2 dir2 rad2
                       L step A x y angle pts pt s xrot yrot)

  ;; --- Step: Select first entity ---
  (setq ent1 (entsel "\nSelect first entity (line/arc): "))
  (setq obj1 (vlax-ename->vla-object (car ent1)))
  (setq type1 (vla-get-ObjectName obj1))

  ;; --- Step: Select second entity ---
  (setq ent2 (entsel "\nSelect second entity (line/arc): "))
  (setq obj2 (vlax-ename->vla-object (car ent2)))
  (setq type2 (vla-get-ObjectName obj2))

  ;; --- Extract start/end points and direction ---
  (cond
    ((= type1 "AcDbLine")
     (setq start1 (vlax-get obj1 'StartPoint)
           end1   (vlax-get obj1 'EndPoint)
           dir1   (angle start1 end1)
           rad1   1.0e99) ; straight line = infinite radius
    )
    ((= type1 "AcDbArc")
     (setq center (vlax-get obj1 'Center)
           rad1   (vlax-get obj1 'Radius)
           ang1   (vlax-get obj1 'StartAngle)
           ang2   (vlax-get obj1 'EndAngle)
           dir1   (if (< ang1 ang2) ang1 ang2)
           start1 (polar center dir1 rad1)
           end1   (polar center ang2 rad1)
     )
    )
  )

  (cond
    ((= type2 "AcDbLine")
     (setq start2 (vlax-get obj2 'StartPoint)
           end2   (vlax-get obj2 'EndPoint)
           dir2   (angle start2 end2)
           rad2   1.0e99)
    )
    ((= type2 "AcDbArc")
     (setq center (vlax-get obj2 'Center)
           rad2   (vlax-get obj2 'Radius)
           ang1   (vlax-get obj2 'StartAngle)
           ang2   (vlax-get obj2 'EndAngle)
           dir2   (if (> ang2 ang1) ang2 ang1)
           start2 (polar center ang1 rad2)
           end2   (polar center ang2 rad2)
     )
    )
  )

  ;; --- Ask for clothoid length and step ---
  (setq L (getreal "\nEnter clothoid length (e.g., 20): "))
  (setq step (getreal "\nEnter segment step (e.g., 1.0): "))

  ;; --- Approximate clothoid (from rad1 to rad2) ---
  (setq A (sqrt (* rad2 L))) ; use final radius for now

  (setq s 0.0
        pts '())

  (while (<= s L)
    ;; clothoid parametric approximation:
    (setq x (- s (/ (expt s 5) (* 40.0 (expt rad2 2) (expt L 2)))))
    (setq y (- (/ (expt s 3) (* 6 rad2 L)) (/ (expt s 7) (* 336.0 (expt rad2 3) (expt L 3)))))

    ;; rotate to match dir1
    (setq xrot (- (* x (cos dir1)) (* y (sin dir1))))
    (setq yrot (+ (* x (sin dir1)) (* y (cos dir1))))

    ;; translate to start point
    (setq pt (list (+ (car start1) xrot) (+ (cadr start1) yrot)))
    (setq pts (append pts (list pt)))

    (setq s (+ s step))
  )

  ;; --- Draw polyline ---
  (command "._PLINE")
  (foreach p pts
    (command p)
  )
  (command "")

  (princ "\nClothoid created between the two entities.")
  (princ)
)
