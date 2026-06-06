;; Helper Functions
;------------------
(defun DrawRect (p1 p2 / doc ms p1 p2 x1 y1 x2 y2 pts pline base ang mirrorline newobj)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
        ms  (vla-get-ModelSpace doc))

  ;; Extract coordinates
  (setq x1 (car p1) y1 (cadr p1)
        x2 (car p2) y2 (cadr p2))

  ;; Define the rectangle vertices (closed polyline)
  (setq pts (vlax-make-safearray vlax-vbDouble '(0 . 9)))
  (vlax-safearray-fill pts (list x1 y1 x2 y1 x2 y2 x1 y2 x1 y1))
  
  ;; Add lightweight polyline
  (setq pline (vla-AddLightWeightPolyline ms pts))
  
  ;; Close it properly (just in case)
  (vla-put-Closed pline :vlax-true)
  (vla-update pline)
  
  (princ "\n✅ Rectangle created successfully.")
  (princ)
)

(defun get-bbox (ename / bbox obj minpt maxpt)
  "Return list of two points: (minPoint maxPoint) for entity ename"
  (vl-load-com)
  (setq obj (vlax-ename->vla-object ename))
  (vla-GetBoundingBox obj 'minpt 'maxpt)
  (setq bbox 
    (list (vlax-safearray->list minpt)
          (vlax-safearray->list maxpt))
  )
  bbox
)

;; Main Functions
;------------------
(defun c:ShowBBox ( / e bbox minpt maxpt)
  (if (setq e (car (entsel "\nSelect object: ")))
    (progn
      (setq bbox (get-bbox e)
            minpt (car bbox)
            maxpt (cadr bbox))
      (DrawRect minpt maxpt)
      (princ (strcat "\n✅ Bounding Box: " (rtos (car minpt) 2 3) "," (rtos (cadr minpt) 2 3)
                     "  →  " (rtos (car maxpt) 2 3) "," (rtos (cadr maxpt) 2 3)))
    )
  )
  (princ)
)

(defun c:ShowBBox-ss (/ ss i e bbox minpt maxpt)
  (if (setq ss (ssget '((0 . "*")))) ; prompt removed, just select anything
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq e (ssname ss i))
        (setq bbox (get-bbox e)
              minpt (car bbox)
              maxpt (cadr bbox))
        ;; Draw visual bbox as rectangle (2D: min/max)
        ;; Compute global bounding box for ss and draw one polyline box around all
        (if (= i 0)
          (progn
            (setq global-minpt (list (car minpt) (cadr minpt) (caddr minpt))) ;or (setq global-minpt minpt)
            (setq global-maxpt (list (car maxpt) (cadr maxpt) (caddr maxpt))) ;or (setq global-maxpt maxpt)
          )
          (progn ; update global min/max pts
            (setq global-minpt (mapcar 'min global-minpt minpt))
            (setq global-maxpt (mapcar 'max global-maxpt maxpt))
          )
          ; (progn
          ;   ;; Ensure global-minpt is the elementwise min of minpt and maxpt (in case the bbox is degenerate)
          ;   (setq global-minpt (list (min (car minpt) (car maxpt))
          ;                            (min (cadr minpt) (cadr maxpt))
          ;                            (min (caddr minpt) (caddr maxpt))))
          ;   (setq global-maxpt (list (max (car minpt) (car maxpt))
          ;                            (max (cadr minpt) (cadr maxpt))
          ;                            (max (caddr minpt) (caddr maxpt))))
          ; )
        )
        (setq i (1+ i))
      )
      ;; Draw global bounding box
      (DrawRect (list (car global-minpt) (cadr global-minpt))
                (list (car global-maxpt) (cadr global-maxpt)))
      ;; Print coordinates in a user-friendly way (show index too)
      (princ (strcat 
        "\n📦 BBox Min: (" (rtos (car global-minpt) 2 3) ", " (rtos (cadr global-minpt) 2 3) ")"
        "\n   BBox Max: (" (rtos (car global-maxpt) 2 3) ", " (rtos (cadr global-maxpt) 2 3) ")"
      ))
      
    )
    (princ "\nNo entities selected.")
  )
  (princ)
)

(defun tst()
  (setq e (car (entsel "\nSelect object: ")))
  (setq bbox (get-bbox e))
  (setq minpt (car bbox))
  (setq maxpt (cadr bbox))
)